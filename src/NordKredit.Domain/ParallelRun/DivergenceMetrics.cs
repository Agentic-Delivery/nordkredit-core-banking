namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Aggregated divergence metrics for a domain during parallel-run.
/// Used for monitoring, alerting, and DORA audit trail.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class DivergenceMetrics
{
    /// <summary>The domain these metrics apply to (e.g., "CardManagement").</summary>
    public required string Domain { get; init; }

    /// <summary>Total number of comparisons performed.</summary>
    public required int TotalComparisons { get; init; }

    /// <summary>Number of comparisons that matched.</summary>
    public required int MatchCount { get; init; }

    /// <summary>Number of comparisons that diverged.</summary>
    public required int DivergenceCount { get; init; }

    /// <summary>
    /// Match rate (0.0 to 1.0). Calculated as MatchCount / TotalComparisons.
    /// Returns 1.0 if no comparisons have been performed.
    /// </summary>
    public double MatchRate => TotalComparisons == 0 ? 1.0 : (double)MatchCount / TotalComparisons;

    /// <summary>
    /// Divergence rate (0.0 to 1.0). Calculated as DivergenceCount / TotalComparisons.
    /// Returns 0.0 if no comparisons have been performed.
    /// </summary>
    public double DivergenceRate => TotalComparisons == 0 ? 0.0 : (double)DivergenceCount / TotalComparisons;

    /// <summary>Whether the divergence rate exceeds the configured threshold.</summary>
    public required bool ThresholdExceeded { get; init; }

    /// <summary>Breakdown of divergences by category.</summary>
    public required IReadOnlyDictionary<DivergenceCategory, int> CategoryBreakdown { get; init; }

    /// <summary>Distinct account IDs affected by divergences.</summary>
    public required IReadOnlyList<string> AffectedAccounts { get; init; }

    /// <summary>Time window for these metrics.</summary>
    public required DateTimeOffset WindowStart { get; init; }

    /// <summary>Time window end for these metrics.</summary>
    public required DateTimeOffset WindowEnd { get; init; }
}
