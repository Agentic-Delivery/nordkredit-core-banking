using Microsoft.ApplicationInsights.Channel;
using Microsoft.ApplicationInsights.DataContracts;
using Microsoft.ApplicationInsights.Extensibility;

namespace NordKredit.Api.Telemetry;

/// <summary>
/// Custom telemetry initializer that tags all requests with the NordKredit domain.
/// Maps API endpoint paths to business domains for monitoring and alerting.
/// DORA Art.11: ICT risk monitoring — domain-level observability.
/// </summary>
public class NordKreditTelemetryInitializer : ITelemetryInitializer
{
    internal const string DomainPropertyKey = "nordkredit.domain";

    public void Initialize(ITelemetry telemetry)
    {
        var domain = ResolveDomain(telemetry);

        if (telemetry is ISupportProperties supportProperties)
        {
            supportProperties.Properties[DomainPropertyKey] = domain;
        }
    }

    private static string ResolveDomain(ITelemetry telemetry)
    {
        if (telemetry is not RequestTelemetry request)
        {
            return "general";
        }

        var url = request.Url;
        if (url is null)
        {
            return "general";
        }

        var path = url.AbsolutePath.ToLowerInvariant();

        if (path.StartsWith("/api/transactions", StringComparison.Ordinal))
        {
            return "payments";
        }

        if (path.StartsWith("/api/cards", StringComparison.Ordinal))
        {
            return "card-management";
        }

        if (path.StartsWith("/health", StringComparison.Ordinal))
        {
            return "infrastructure";
        }

        return "general";
    }
}
