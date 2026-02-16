namespace NordKredit.Domain.Transactions;

/// <summary>
/// Result of credit limit and account expiration validation for a single transaction.
/// COBOL source: CBTRN02C.cbl:370-422 — output of the validation loop.
/// Improvement over COBOL: collects ALL validation failures instead of "last wins" behavior.
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
/// </summary>
public class ValidatedTransaction
{
    /// <summary>The verified transaction that was validated.</summary>
    public required VerifiedTransaction VerifiedTransaction { get; init; }

    /// <summary>Whether the transaction passed all validation checks.</summary>
    public required bool IsValid { get; init; }

    /// <summary>
    /// All rejection reasons if validation failed.
    /// Improvement over COBOL: collects all failures instead of last-wins behavior.
    /// </summary>
    public required IReadOnlyList<DailyReject> Rejections { get; init; }
}
