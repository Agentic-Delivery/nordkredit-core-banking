namespace NordKredit.Domain.Transactions;

/// <summary>
/// Result of the batch transaction posting service.
/// COBOL source: CBTRN02C.cbl:424-579 — aggregate summary of the posting run.
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting).
/// </summary>
public class TransactionPostingServiceResult
{
    /// <summary>Total number of validated transactions processed.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of transactions successfully posted.</summary>
    public required int PostedCount { get; init; }

    /// <summary>Number of invalid transactions skipped (already rejected).</summary>
    public required int SkippedCount { get; init; }

    /// <summary>Number of transactions that failed during posting.</summary>
    public required int FailedCount { get; init; }
}
