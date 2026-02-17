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

    /// <summary>
    /// Retrieves a page of transactions ordered by ID using keyset pagination.
    /// COBOL: COTRN00C.cbl:279-328 — PROCESS-PAGE-FORWARD.
    /// </summary>
    Task<IReadOnlyList<Transaction>> GetPageAsync(
        int pageSize,
        string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves the most recently added transaction.
    /// COBOL: COTRN02C.cbl — COPY-LAST-TRAN-DATA (PF5 handler).
    /// </summary>
    Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves transactions within a processing timestamp date range, ordered by CardNumber then Id.
    /// COBOL: CBTRN03C.cbl:159-373 — report generation reads TRANSACT sequentially.
    /// The sort order matches the COBOL assumption that the file is sorted by card number.
    /// </summary>
    Task<IReadOnlyList<Transaction>> GetByDateRangeAsync(
        DateTime startDate,
        DateTime endDate,
        CancellationToken cancellationToken = default);
}
