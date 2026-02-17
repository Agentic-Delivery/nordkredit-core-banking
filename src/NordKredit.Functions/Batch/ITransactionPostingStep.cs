using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Step 3 of the daily batch pipeline: transaction posting with balance updates.
/// COBOL source: CBTRN02C.cbl:424-579.
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), PSD2 Art.94 (retention).
/// </summary>
public interface ITransactionPostingStep
{
    Task<TransactionPostingResult> RunAsync(
        IReadOnlyList<ValidatedTransaction> validatedTransactions,
        CancellationToken cancellationToken = default);
}
