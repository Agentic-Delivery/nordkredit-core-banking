namespace NordKredit.Functions.Batch;

/// <summary>
/// Result of the daily batch transaction posting function.
/// COBOL source: CBTRN02C.cbl:424-579 — output summary of the posting run.
/// Contains aggregate counts for monitoring and alerting.
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public class TransactionPostingResult
{
    /// <summary>Total number of validated transactions processed.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of transactions successfully posted.</summary>
    public required int PostedCount { get; init; }

    /// <summary>Number of invalid transactions skipped (already rejected).</summary>
    public required int SkippedCount { get; init; }

    /// <summary>Number of transactions that failed during posting.</summary>
    public required int FailedCount { get; init; }

    /// <summary>
    /// True if any failures exist — replaces COBOL RETURN-CODE = 8 (error status).
    /// </summary>
    public bool HasErrors => FailedCount > 0;
}
