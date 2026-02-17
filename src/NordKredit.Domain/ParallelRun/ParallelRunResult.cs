namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Result of a parallel-run execution for a single request.
/// Contains the mainframe response (system of record), Azure response, and comparison result.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class ParallelRunResult
{
    /// <summary>The response from the mainframe (system of record during parallel-run).</summary>
    public required string? MainframeResponse { get; init; }

    /// <summary>The response from the Azure system.</summary>
    public required string? AzureResponse { get; init; }

    /// <summary>The comparison result, or null if comparison was skipped.</summary>
    public required ComparisonResult? Comparison { get; init; }

    /// <summary>Whether the parallel-run executed successfully (both systems responded).</summary>
    public required bool BothSystemsResponded { get; init; }

    /// <summary>
    /// Whether the mainframe call timed out. When true, the Azure response is returned
    /// but no comparison is performed.
    /// </summary>
    public required bool MainframeTimedOut { get; init; }

    /// <summary>Error message if something went wrong (mainframe error, comparison error, etc.).</summary>
    public string? ErrorMessage { get; init; }

    /// <summary>Correlation ID for end-to-end tracing.</summary>
    public required string CorrelationId { get; init; }

    /// <summary>Duration of the parallel-run execution.</summary>
    public required TimeSpan Duration { get; init; }
}
