namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for daily transaction input records.
/// COBOL source: Sequential DALYTRAN file, read by CBTRN01C (verification) and CBTRN02C (posting).
/// </summary>
public interface IDailyTransactionRepository
{
    Task<IReadOnlyList<DailyTransaction>> GetUnprocessedAsync(CancellationToken cancellationToken = default);

    Task AddAsync(DailyTransaction dailyTransaction, CancellationToken cancellationToken = default);

    Task MarkAsProcessedAsync(string transactionId, CancellationToken cancellationToken = default);
}
