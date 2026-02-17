using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Step 2 of the daily batch pipeline: credit limit and expiration validation.
/// COBOL source: CBTRN02C.cbl:370-422.
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk).
/// </summary>
public interface ICreditValidationStep
{
    Task<TransactionCreditValidationResult> RunAsync(
        IReadOnlyList<VerifiedTransaction> verifiedTransactions,
        CancellationToken cancellationToken = default);
}
