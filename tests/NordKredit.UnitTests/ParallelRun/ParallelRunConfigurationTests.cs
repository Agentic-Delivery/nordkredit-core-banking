using NordKredit.Domain.ParallelRun;

namespace NordKredit.UnitTests.ParallelRun;

/// <summary>
/// Unit tests for ParallelRunConfiguration — per-domain enablement and thresholds.
/// Regulations: DORA Art.11 (ICT system testing — configurable test parameters).
/// </summary>
public class ParallelRunConfigurationTests
{
    [Fact]
    public void IsDomainEnabled_EnabledDomain_ReturnsTrue()
    {
        var config = new ParallelRunConfiguration
        {
            EnabledDomains = new() { ["Transactions"] = true }
        };

        Assert.True(config.IsDomainEnabled("Transactions"));
    }

    [Fact]
    public void IsDomainEnabled_DisabledDomain_ReturnsFalse()
    {
        var config = new ParallelRunConfiguration
        {
            EnabledDomains = new() { ["Transactions"] = false }
        };

        Assert.False(config.IsDomainEnabled("Transactions"));
    }

    [Fact]
    public void IsDomainEnabled_UnknownDomain_ReturnsFalse()
    {
        var config = new ParallelRunConfiguration
        {
            EnabledDomains = new() { ["CardManagement"] = true }
        };

        Assert.False(config.IsDomainEnabled("Transactions"));
    }

    [Fact]
    public void DefaultThreshold_Is1Percent()
    {
        var config = new ParallelRunConfiguration();

        Assert.Equal(0.01, config.DivergenceThreshold);
    }

    [Fact]
    public void DefaultTimeout_Is30Seconds()
    {
        var config = new ParallelRunConfiguration();

        Assert.Equal(TimeSpan.FromSeconds(30), config.MainframeTimeout);
    }
}
