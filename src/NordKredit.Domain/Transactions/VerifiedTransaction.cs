namespace NordKredit.Domain.Transactions;

/// <summary>
/// Result of card verification for a daily transaction.
/// COBOL source: CBTRN01C.cbl:164-186 — output of the verification loop.
/// Each daily transaction is verified and marked with a status indicating
/// whether it passed card cross-reference and account existence checks.
/// Regulations: PSD2 Art.97 (transaction authorization), FFFS 2014:5 Ch.4 §3,
/// AML/KYC (source verification).
/// </summary>
public class VerifiedTransaction
{
    /// <summary>The daily transaction that was verified.</summary>
    public required DailyTransaction Transaction { get; init; }

    /// <summary>Whether the transaction passed all verification checks.</summary>
    public required bool IsVerified { get; init; }

    /// <summary>Failure reason if verification failed; null when verified.</summary>
    public string? FailureReason { get; init; }

    /// <summary>Account ID from the cross-reference lookup (null if card not found).</summary>
    public string? AccountId { get; init; }

    /// <summary>Customer ID from the cross-reference lookup (null if card not found).</summary>
    public int? CustomerId { get; init; }
}
