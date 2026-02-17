using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Result of the daily batch transaction posting function.
/// COBOL source: CBTRN02C.cbl:424-579 — output summary of the posting run.
/// Contains all posting results plus aggregate counts for monitoring.
/// If any transactions were skipped, HasWarnings is true (replaces COBOL RETURN-CODE = 4).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public class TransactionPostingResult
{
    /// <summary>All posting results from this batch run.</summary>
    public required IReadOnlyList<PostedTransactionResult> Results { get; init; }

    /// <summary>Total number of validated transactions processed.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of transactions that were successfully posted.</summary>
    public required int PostedCount { get; init; }

    /// <summary>Number of transactions that were skipped (validation failures).</summary>
    public required int SkippedCount { get; init; }

    /// <summary>
    /// True if any transactions were skipped — replaces COBOL RETURN-CODE = 4 (warning status).
    /// </summary>
    public bool HasWarnings => SkippedCount > 0;
}
