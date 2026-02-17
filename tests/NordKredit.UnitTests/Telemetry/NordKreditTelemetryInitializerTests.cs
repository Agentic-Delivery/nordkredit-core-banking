using Microsoft.ApplicationInsights.DataContracts;
using NordKredit.Api.Telemetry;

namespace NordKredit.UnitTests.Telemetry;

public class NordKreditTelemetryInitializerTests
{
    private readonly NordKreditTelemetryInitializer _sut = new();

    [Fact]
    public void Initialize_TransactionRequest_Tags_payments_domain()
    {
        var telemetry = new RequestTelemetry
        {
            Url = new Uri("https://localhost/api/transactions/123"),
        };

        _sut.Initialize(telemetry);

        Assert.Equal("payments", telemetry.Properties["nordkredit.domain"]);
    }

    [Fact]
    public void Initialize_CardsRequest_Tags_card_management_domain()
    {
        var telemetry = new RequestTelemetry
        {
            Url = new Uri("https://localhost/api/cards"),
        };

        _sut.Initialize(telemetry);

        Assert.Equal("card-management", telemetry.Properties["nordkredit.domain"]);
    }

    [Fact]
    public void Initialize_HealthRequest_Tags_infrastructure_domain()
    {
        var telemetry = new RequestTelemetry
        {
            Url = new Uri("https://localhost/health/ready"),
        };

        _sut.Initialize(telemetry);

        Assert.Equal("infrastructure", telemetry.Properties["nordkredit.domain"]);
    }

    [Fact]
    public void Initialize_UnknownPath_Tags_general_domain()
    {
        var telemetry = new RequestTelemetry
        {
            Url = new Uri("https://localhost/api/other"),
        };

        _sut.Initialize(telemetry);

        Assert.Equal("general", telemetry.Properties["nordkredit.domain"]);
    }

    [Fact]
    public void Initialize_NonRequestTelemetry_Tags_general_domain()
    {
        var telemetry = new TraceTelemetry("test message");

        _sut.Initialize(telemetry);

        Assert.Equal("general", telemetry.Properties["nordkredit.domain"]);
    }

    [Fact]
    public void Initialize_RequestWithNullUrl_Tags_general_domain()
    {
        var telemetry = new RequestTelemetry();

        _sut.Initialize(telemetry);

        Assert.Equal("general", telemetry.Properties["nordkredit.domain"]);
    }
}
