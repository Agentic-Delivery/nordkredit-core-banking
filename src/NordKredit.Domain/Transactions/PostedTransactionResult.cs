namespace NordKredit.Domain.Transactions;

/// <summary>
/// Result of posting a single transaction in the daily batch.
/// COBOL source: CBTRN02C.cbl:424-579 — output status per transaction.
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), PSD2 Art.94 (retention).
/// </summary>
public class PostedTransactionResult
{
    /// <summary>Transaction ID that was processed.</summary>
    public required string TransactionId { get; init; }

    /// <summary>Whether the transaction was successfully posted.</summary>
    public required bool IsPosted { get; init; }

    /// <summary>Reason for skipping if not posted (e.g., validation failure).</summary>
    public string? SkipReason { get; init; }
}
