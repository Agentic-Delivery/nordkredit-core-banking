namespace NordKredit.Domain.Transactions;

/// <summary>
/// Unit of work abstraction for atomic multi-repository operations.
/// Required by TransactionPostingService to wrap category balance upsert,
/// account balance update, and transaction write in a single database transaction.
/// COBOL source: CBTRN02C.cbl:424-579 — posting section (non-atomic in COBOL; fixed here).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting).
/// </summary>
public interface IUnitOfWork
{
    Task BeginTransactionAsync(CancellationToken cancellationToken = default);
    Task CommitAsync(CancellationToken cancellationToken = default);
    Task RollbackAsync(CancellationToken cancellationToken = default);
}
