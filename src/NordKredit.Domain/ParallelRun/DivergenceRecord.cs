namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// A persisted record of a parallel-run comparison for audit trail.
/// Stored for DORA Art.11 compliance (ICT system testing documentation).
/// Regulations: DORA Art.11, FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public class DivergenceRecord
{
    /// <summary>Unique identifier for this record.</summary>
    public required string Id { get; init; }

    /// <summary>The domain that was compared.</summary>
    public required string Domain { get; init; }

    /// <summary>The operation that was compared.</summary>
    public required string Operation { get; init; }

    /// <summary>Correlation ID for tracing.</summary>
    public required string CorrelationId { get; init; }

    /// <summary>Whether the outputs matched.</summary>
    public required bool IsMatch { get; init; }

    /// <summary>The mainframe response (serialized JSON).</summary>
    public required string? MainframeResponse { get; init; }

    /// <summary>The Azure response (serialized JSON).</summary>
    public required string? AzureResponse { get; init; }

    /// <summary>Field-level divergences (serialized JSON).</summary>
    public required string? DivergencesJson { get; init; }

    /// <summary>Account ID affected (for metrics tracking), if applicable.</summary>
    public string? AccountId { get; init; }

    /// <summary>Timestamp of the comparison.</summary>
    public required DateTimeOffset RecordedAt { get; init; }
}
