namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Configuration for parallel-run orchestration.
/// Supports per-domain enablement and divergence thresholds.
/// Regulations: DORA Art.11 (ICT system testing — configurable test parameters).
/// </summary>
public class ParallelRunConfiguration
{
    /// <summary>
    /// Domains enabled for parallel-run. Key = domain name (e.g., "CardManagement"),
    /// value = whether parallel-run is active for that domain.
    /// Domains not in this dictionary are treated as disabled (mainframe-only).
    /// </summary>
    public Dictionary<string, bool> EnabledDomains { get; init; } = [];

    /// <summary>
    /// Maximum acceptable divergence rate (0.0 to 1.0). When the divergence rate
    /// exceeds this threshold, an alert is raised.
    /// Default: 0.01 (1%).
    /// </summary>
    public double DivergenceThreshold { get; init; } = 0.01;

    /// <summary>
    /// Timeout for mainframe calls during parallel-run. If the mainframe does not
    /// respond within this duration, the comparison is skipped and logged.
    /// Default: 30 seconds.
    /// </summary>
    public TimeSpan MainframeTimeout { get; init; } = TimeSpan.FromSeconds(30);

    /// <summary>
    /// Checks whether a given domain is enabled for parallel-run.
    /// </summary>
    public bool IsDomainEnabled(string domain) =>
        EnabledDomains.TryGetValue(domain, out bool enabled) && enabled;
}
