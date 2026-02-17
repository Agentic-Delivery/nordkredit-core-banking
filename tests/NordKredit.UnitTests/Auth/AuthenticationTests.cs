using System.Net;
using System.Security.Claims;
using System.Text.Encodings.Web;
using Microsoft.AspNetCore.Authentication;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.AspNetCore.TestHost;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using NordKredit.Infrastructure;

namespace NordKredit.UnitTests.Auth;

/// <summary>
/// Integration tests for Azure AD B2C / Azure AD authentication and authorization.
/// SEC-BR-005: Verifies CICS CESN → Azure AD B2C/AD authentication replacement.
/// PSD2 Art. 97: Validates that unauthenticated requests are rejected (SCA enforcement).
/// DORA Art. 9: Confirms protection and prevention via strong authentication.
/// </summary>
public class AuthenticationTests : IClassFixture<AuthenticationTests.NordKreditWebApplicationFactory>
{
    private readonly NordKreditWebApplicationFactory _factory;

    public AuthenticationTests(NordKreditWebApplicationFactory factory)
    {
        _factory = factory;
    }

    // =================================================================
    // Health endpoint — must remain unauthenticated (infrastructure probe)
    // =================================================================

    [Fact]
    public async Task Health_NoAuth_Returns200()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.GetAsync("/health");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
    }

    // =================================================================
    // TransactionsController — requires CustomerAccess policy
    // SEC-BR-005: Unauthenticated requests must be rejected.
    // =================================================================

    [Fact]
    public async Task Transactions_NoAuth_Returns401()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.GetAsync("/api/transactions");

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task TransactionDetail_NoAuth_Returns401()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.GetAsync("/api/transactions/0000000000000001");

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task TransactionPost_NoAuth_Returns401()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.PostAsync("/api/transactions",
            new StringContent("{}", System.Text.Encoding.UTF8, "application/json"));

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task TransactionLast_NoAuth_Returns401()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.GetAsync("/api/transactions/last");

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task Transactions_WithB2CAuth_IsNotUnauthorized()
    {
        var client = _factory.CreateB2CAuthenticatedClient();

        var response = await client.GetAsync("/api/transactions");

        Assert.NotEqual(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task Transactions_WithAzureAdAuth_IsNotUnauthorized()
    {
        var client = _factory.CreateAzureAdAuthenticatedClient();

        var response = await client.GetAsync("/api/transactions");

        Assert.NotEqual(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    // =================================================================
    // CardsController — requires OperatorAccess policy (Azure AD only)
    // SEC-BR-001: Card operations restricted to admin/operator role.
    // =================================================================

    [Fact]
    public async Task Cards_NoAuth_Returns401()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.GetAsync("/api/cards");

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task CardDetail_NoAuth_Returns401()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.GetAsync("/api/cards/4000123456789010");

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task CardUpdate_NoAuth_Returns401()
    {
        var client = _factory.CreateUnauthenticatedClient();

        var response = await client.PutAsync("/api/cards/4000123456789010",
            new StringContent("{}", System.Text.Encoding.UTF8, "application/json"));

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task Cards_WithB2CAuth_Returns401Unauthorized()
    {
        // B2C tokens are not listed in the OperatorAccess policy authentication schemes,
        // so the policy challenges only the AzureAd scheme, which returns NoResult for B2C tokens.
        // This results in 401 (no authenticated identity from policy schemes), not 403.
        var client = _factory.CreateB2CAuthenticatedClient();

        var response = await client.GetAsync("/api/cards");

        Assert.Equal(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task Cards_WithAzureAdAuth_IsNotUnauthorized()
    {
        var client = _factory.CreateAzureAdAuthenticatedClient();

        var response = await client.GetAsync("/api/cards");

        Assert.NotEqual(HttpStatusCode.Unauthorized, response.StatusCode);
    }

    [Fact]
    public async Task Cards_WithAzureAdAuth_IsNotForbidden()
    {
        var client = _factory.CreateAzureAdAuthenticatedClient();

        var response = await client.GetAsync("/api/cards");

        Assert.NotEqual(HttpStatusCode.Forbidden, response.StatusCode);
    }

    // =================================================================
    // Test infrastructure
    // =================================================================

    public class NordKreditWebApplicationFactory : WebApplicationFactory<Program>
    {
        protected override void ConfigureWebHost(IWebHostBuilder builder)
        {
            builder.UseEnvironment("Testing");
            builder.ConfigureTestServices(services =>
            {
                // Replace real DbContext with InMemory for auth tests.
                var descriptor = services.SingleOrDefault(
                    d => d.ServiceType == typeof(DbContextOptions<NordKreditDbContext>));
                if (descriptor != null)
                {
                    services.Remove(descriptor);
                }

                services.AddDbContext<NordKreditDbContext>(options =>
                    options.UseInMemoryDatabase("AuthTests"));

                // Replace real Azure AD B2C and Azure AD auth with test schemes.
                // This allows testing auth policies without real identity providers.
                services.AddAuthentication("TestB2C")
                    .AddScheme<AuthenticationSchemeOptions, TestB2CAuthHandler>("TestB2C", _ => { })
                    .AddScheme<AuthenticationSchemeOptions, TestAzureAdAuthHandler>("TestAzureAd", _ => { });

                // Reconfigure authorization policies to use test schemes.
                services.AddAuthorizationBuilder()
                    .AddPolicy("CustomerAccess", policy =>
                    {
                        policy.AuthenticationSchemes.Clear();
                        policy.AuthenticationSchemes.Add("TestB2C");
                        policy.AuthenticationSchemes.Add("TestAzureAd");
                        policy.RequireAuthenticatedUser();
                    })
                    .AddPolicy("OperatorAccess", policy =>
                    {
                        policy.AuthenticationSchemes.Clear();
                        policy.AuthenticationSchemes.Add("TestAzureAd");
                        policy.RequireAuthenticatedUser();
                    });
            });
        }

        public HttpClient CreateUnauthenticatedClient() =>
            CreateClient();

        public HttpClient CreateB2CAuthenticatedClient()
        {
            var client = CreateClient();
            client.DefaultRequestHeaders.Add("X-Test-Auth-Scheme", "TestB2C");
            return client;
        }

        public HttpClient CreateAzureAdAuthenticatedClient()
        {
            var client = CreateClient();
            client.DefaultRequestHeaders.Add("X-Test-Auth-Scheme", "TestAzureAd");
            return client;
        }
    }

    private sealed class TestB2CAuthHandler : AuthenticationHandler<AuthenticationSchemeOptions>
    {
        public TestB2CAuthHandler(
            IOptionsMonitor<AuthenticationSchemeOptions> options,
            ILoggerFactory logger,
            UrlEncoder encoder)
            : base(options, logger, encoder)
        {
        }

        protected override Task<AuthenticateResult> HandleAuthenticateAsync()
        {
            if (!Request.Headers.TryGetValue("X-Test-Auth-Scheme", out var scheme) ||
                scheme != "TestB2C")
            {
                return Task.FromResult(AuthenticateResult.NoResult());
            }

            var claims = new[]
            {
                new Claim(ClaimTypes.NameIdentifier, "test-customer-001"),
                new Claim(ClaimTypes.Name, "Test Customer"),
            };
            var identity = new ClaimsIdentity(claims, "TestB2C");
            var principal = new ClaimsPrincipal(identity);
            var ticket = new AuthenticationTicket(principal, "TestB2C");
            return Task.FromResult(AuthenticateResult.Success(ticket));
        }
    }

    private sealed class TestAzureAdAuthHandler : AuthenticationHandler<AuthenticationSchemeOptions>
    {
        public TestAzureAdAuthHandler(
            IOptionsMonitor<AuthenticationSchemeOptions> options,
            ILoggerFactory logger,
            UrlEncoder encoder)
            : base(options, logger, encoder)
        {
        }

        protected override Task<AuthenticateResult> HandleAuthenticateAsync()
        {
            if (!Request.Headers.TryGetValue("X-Test-Auth-Scheme", out var scheme) ||
                scheme != "TestAzureAd")
            {
                return Task.FromResult(AuthenticateResult.NoResult());
            }

            var claims = new[]
            {
                new Claim(ClaimTypes.NameIdentifier, "test-operator-001"),
                new Claim(ClaimTypes.Name, "Test Operator"),
            };
            var identity = new ClaimsIdentity(claims, "TestAzureAd");
            var principal = new ClaimsPrincipal(identity);
            var ticket = new AuthenticationTicket(principal, "TestAzureAd");
            return Task.FromResult(AuthenticateResult.Success(ticket));
        }
    }
}
