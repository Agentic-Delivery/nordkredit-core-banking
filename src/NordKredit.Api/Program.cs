using Microsoft.ApplicationInsights.Extensibility;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.AspNetCore.Diagnostics.HealthChecks;
using Microsoft.Extensions.Diagnostics.HealthChecks;
using Microsoft.Identity.Web;
using NordKredit.Api.Telemetry;
using NordKredit.Domain.Transactions;
using NordKredit.Infrastructure;
using NordKredit.Infrastructure.Messaging;
using NordKredit.Infrastructure.Transactions;
using AcctMgmtSqlRepo = NordKredit.Infrastructure.AccountManagement.SqlAccountManagementRepository;
using CardMgmtSqlCardRepo = NordKredit.Infrastructure.CardManagement.SqlCardRepository;
using CardMgmtSqlXrefRepo = NordKredit.Infrastructure.CardManagement.SqlCardCrossReferenceRepository;

var builder = WebApplication.CreateBuilder(args);

// Application Insights telemetry — DORA Art.11: ICT risk monitoring.
builder.Services.AddApplicationInsightsTelemetry();
builder.Services.AddSingleton<ITelemetryInitializer, NordKreditTelemetryInitializer>();

builder.Services.AddControllers();

// Authentication — replaces CICS CESN (sign-on transaction) and RACF credential validation.
// SEC-BR-005: CICS terminal auth → Azure AD B2C (customer) / Azure AD (internal operator).
// PSD2 Art. 97: Strong Customer Authentication — B2C supports MFA via conditional access policies.
// DORA Art. 9: Protection and prevention — strong authentication mechanisms.
builder.Services
    .AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
    .AddMicrosoftIdentityWebApi(builder.Configuration, "AzureAdB2C", JwtBearerDefaults.AuthenticationScheme);

builder.Services
    .AddAuthentication()
    .AddMicrosoftIdentityWebApi(builder.Configuration, "AzureAd", "AzureAd");

// Authorization policies — replaces CICS transaction-level security and COBOL CDEMO-USRTYP-USER role flag.
// SEC-BR-001: Role-based access control (Admin vs regular User).
// SEC-BR-004: Function key validation → HTTP method + endpoint authorization.
builder.Services.AddAuthorizationBuilder()
    // CustomerAccess: accepts tokens from either B2C (customers) or Azure AD (operators).
    // Maps to COBOL: regular users + admin can view transactions.
    .AddPolicy("CustomerAccess", policy =>
    {
        policy.AuthenticationSchemes.Add(JwtBearerDefaults.AuthenticationScheme);
        policy.AuthenticationSchemes.Add("AzureAd");
        policy.RequireAuthenticatedUser();
    })
    // OperatorAccess: accepts only Azure AD tokens (internal operators).
    // Maps to COBOL: CDEMO-USRTYP-ADMIN — only admin sees all cards (SEC-BR-001).
    .AddPolicy("OperatorAccess", policy =>
    {
        policy.AuthenticationSchemes.Add("AzureAd");
        policy.RequireAuthenticatedUser();
    });

// Transaction domain services (COTRN00C/01C/02C — online transaction processing)
builder.Services.AddScoped<TransactionValidationService>();
builder.Services.AddScoped<TransactionAddService>();
builder.Services.AddScoped<TransactionDetailService>();
builder.Services.AddScoped<TransactionListService>();

// Card Management domain services (COCRDLIC — card list, COCRDSLC — card detail, COCRDUPC — card update)
builder.Services.AddScoped<NordKredit.Domain.CardManagement.CardListService>();
builder.Services.AddScoped<NordKredit.Domain.CardManagement.CardDetailService>();
builder.Services.AddScoped<NordKredit.Domain.CardManagement.CardUpdateService>();

// Infrastructure bindings — Azure SQL repositories replace VSAM file I/O
builder.Services.AddScoped<ITransactionRepository, SqlTransactionRepository>();
builder.Services.AddScoped<ITransactionIdGenerator, SqlTransactionIdGenerator>();
builder.Services.AddScoped<ICardCrossReferenceRepository, SqlCardCrossReferenceRepository>();

// Card Management domain — Azure SQL repositories for card entity operations
builder.Services.AddScoped<NordKredit.Domain.CardManagement.ICardRepository, CardMgmtSqlCardRepo>();
builder.Services.AddScoped<NordKredit.Domain.CardManagement.ICardCrossReferenceRepository, CardMgmtSqlXrefRepo>();

// Account Management domain — Azure SQL repository for account lifecycle operations
// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD). Business rules: ACCT-BR-001 through ACCT-BR-008.
builder.Services.AddScoped<NordKredit.Domain.AccountManagement.IAccountManagementRepository, AcctMgmtSqlRepo>();

// EF Core DbContext — replaces Db2 for z/OS
builder.Services.AddDbContext<NordKreditDbContext>();

// Azure Service Bus messaging — replaces IBM MQ for Bankgirot and SWIFT payment clearing.
// PAY-BR-001: Payment clearing integration via Azure Service Bus queues/topics.
// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 (accurate records).
var serviceBusConnectionString = builder.Configuration.GetConnectionString("ServiceBus");
if (!string.IsNullOrWhiteSpace(serviceBusConnectionString))
{
    builder.Services.AddServiceBusMessaging(serviceBusConnectionString);
}

// Health checks — /health/ready (DB connectivity), /health/live (process alive).
// DORA Art.11: ICT risk monitoring and incident detection.
var healthChecksBuilder = builder.Services.AddHealthChecks();
var dbConnectionString = builder.Configuration.GetConnectionString("NordKreditDb");
if (!string.IsNullOrWhiteSpace(dbConnectionString))
{
    healthChecksBuilder.AddSqlServer(dbConnectionString, name: "database", tags: ["ready"]);
}

var app = builder.Build();

// Health check endpoints — unauthenticated infrastructure probes.
// /health/live: Process alive (no dependency checks) — Kubernetes liveness probe.
// /health/ready: DB connectivity — Kubernetes readiness probe.
app.MapHealthChecks("/health/live", new HealthCheckOptions
{
    Predicate = _ => false,
    ResponseWriter = WriteHealthResponse,
});
app.MapHealthChecks("/health/ready", new HealthCheckOptions
{
    Predicate = check => check.Tags.Contains("ready"),
    ResponseWriter = WriteHealthResponse,
});

// Authentication and authorization middleware — must be before MapControllers.
// SEC-BR-003: User session context management — JWT tokens replace CICS COMMAREA session state.
app.UseAuthentication();
app.UseAuthorization();

app.MapControllers();

app.Run();

// Required for WebApplicationFactory<Program> in integration tests.
public partial class Program
{
    private static async Task WriteHealthResponse(HttpContext context, HealthReport report)
    {
        context.Response.ContentType = "application/json";
        var status = report.Status.ToString();
        await context.Response.WriteAsync($"{{\"status\":\"{status}\"}}");
    }
}
