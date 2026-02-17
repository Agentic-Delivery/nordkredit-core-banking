using System.Net;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.TestHost;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Diagnostics.HealthChecks;
using Microsoft.Extensions.Hosting;

namespace NordKredit.UnitTests.Telemetry;

public class HealthCheckTests
{
    [Fact]
    public async Task LiveEndpoint_returns_ok_with_healthy_status()
    {
        using var host = await CreateTestHost();
        var client = host.GetTestClient();

        var response = await client.GetAsync("/health/live");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var content = await response.Content.ReadAsStringAsync();
        Assert.Contains("Healthy", content);
    }

    [Fact]
    public async Task ReadyEndpoint_returns_ok_when_all_checks_healthy()
    {
        using var host = await CreateTestHost();
        var client = host.GetTestClient();

        var response = await client.GetAsync("/health/ready");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var content = await response.Content.ReadAsStringAsync();
        Assert.Contains("Healthy", content);
    }

    [Fact]
    public async Task ReadyEndpoint_returns_service_unavailable_when_check_unhealthy()
    {
        using var host = await CreateTestHost(dbHealthy: false);
        var client = host.GetTestClient();

        var response = await client.GetAsync("/health/ready");

        Assert.Equal(HttpStatusCode.ServiceUnavailable, response.StatusCode);
        var content = await response.Content.ReadAsStringAsync();
        Assert.Contains("Unhealthy", content);
    }

    private static async Task<IHost> CreateTestHost(bool dbHealthy = true)
    {
        var builder = new HostBuilder()
            .ConfigureWebHost(webHost =>
            {
                webHost.UseTestServer();
                webHost.ConfigureServices(services =>
                {
                    services.AddRouting();

                    var healthBuilder = services.AddHealthChecks();

                    if (dbHealthy)
                    {
                        healthBuilder.Add(new HealthCheckRegistration(
                            "database",
                            _ => new StubHealthCheck(HealthCheckResult.Healthy("DB OK")),
                            failureStatus: null,
                            tags: ["ready"]));
                    }
                    else
                    {
                        healthBuilder.Add(new HealthCheckRegistration(
                            "database",
                            _ => new StubHealthCheck(HealthCheckResult.Unhealthy("DB down")),
                            failureStatus: null,
                            tags: ["ready"]));
                    }
                });

                webHost.Configure(app =>
                {
                    app.UseRouting();
                    app.UseEndpoints(endpoints =>
                    {
                        endpoints.MapHealthChecks("/health/live", new Microsoft.AspNetCore.Diagnostics.HealthChecks.HealthCheckOptions
                        {
                            Predicate = _ => false,
                            ResponseWriter = WriteHealthResponse,
                        });

                        endpoints.MapHealthChecks("/health/ready", new Microsoft.AspNetCore.Diagnostics.HealthChecks.HealthCheckOptions
                        {
                            Predicate = check => check.Tags.Contains("ready"),
                            ResponseWriter = WriteHealthResponse,
                        });
                    });
                });
            });

        var host = builder.Build();
        await host.StartAsync();
        return host;
    }

    private static async Task WriteHealthResponse(
        HttpContext context,
        HealthReport report)
    {
        context.Response.ContentType = "application/json";
        var status = report.Status.ToString();
        await context.Response.WriteAsync($"{{\"status\":\"{status}\"}}");
    }

    private sealed class StubHealthCheck(HealthCheckResult result) : IHealthCheck
    {
        public Task<HealthCheckResult> CheckHealthAsync(
            HealthCheckContext context,
            CancellationToken cancellationToken = default) => Task.FromResult(result);
    }
}
