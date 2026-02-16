using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Result of the daily batch card verification function.
/// COBOL source: CBTRN01C.cbl — output summary of the verification run.
/// Contains all verification results plus aggregate counts for monitoring.
/// Regulations: PSD2 Art.97, FFFS 2014:5 Ch.4 §3, AML/KYC.
/// </summary>
public class CardVerificationResult
{
    /// <summary>All verification results from this batch run.</summary>
    public required IReadOnlyList<VerifiedTransaction> Results { get; init; }

    /// <summary>Total number of daily transactions processed.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of transactions that passed verification.</summary>
    public required int VerifiedCount { get; init; }

    /// <summary>Number of transactions that failed verification.</summary>
    public required int FailedCount { get; init; }
}
