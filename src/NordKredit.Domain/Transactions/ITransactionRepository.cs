namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for posted transaction records.
/// COBOL source: VSAM TRANSACT file, accessed by COTRN00C/01C/02C.
/// </summary>
public interface ITransactionRepository
{
    Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default);

    Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(
        string accountId,
        int pageSize,
        string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default);

    Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default);
}
