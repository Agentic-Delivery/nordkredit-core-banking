using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Result of the daily batch credit limit and expiration validation function.
/// COBOL source: CBTRN02C.cbl:370-422 — output summary of the validation run.
/// Contains all validation results plus aggregate counts for monitoring.
/// If any rejections exist, HasWarnings is true (replaces COBOL RETURN-CODE = 4).
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
/// </summary>
public class TransactionCreditValidationResult
{
    /// <summary>All validation results from this batch run.</summary>
    public required IReadOnlyList<ValidatedTransaction> Results { get; init; }

    /// <summary>Total number of verified transactions processed.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of transactions that passed validation.</summary>
    public required int ValidCount { get; init; }

    /// <summary>Number of transactions that were rejected.</summary>
    public required int RejectedCount { get; init; }

    /// <summary>
    /// True if any rejections exist — replaces COBOL RETURN-CODE = 4 (warning status).
    /// </summary>
    public bool HasWarnings => RejectedCount > 0;
}
