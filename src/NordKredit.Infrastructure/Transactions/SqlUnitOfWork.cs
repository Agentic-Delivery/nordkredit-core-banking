using Microsoft.EntityFrameworkCore.Storage;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// EF Core implementation of <see cref="IUnitOfWork"/>.
/// Wraps NordKreditDbContext database transactions for atomic multi-repository operations.
/// Used by TransactionPostingService to ensure category balance upsert, account balance update,
/// and transaction record write are committed or rolled back as a unit.
/// Fixes non-atomic posting behavior in COBOL source CBTRN02C.cbl:424-579.
/// </summary>
public class SqlUnitOfWork : IUnitOfWork
{
    private readonly NordKreditDbContext _dbContext;
    private IDbContextTransaction? _transaction;

    public SqlUnitOfWork(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task BeginTransactionAsync(CancellationToken cancellationToken = default) =>
        _transaction = await _dbContext.Database.BeginTransactionAsync(cancellationToken);

    public async Task CommitAsync(CancellationToken cancellationToken = default)
    {
        if (_transaction is not null)
        {
            await _transaction.CommitAsync(cancellationToken);
            await _transaction.DisposeAsync();
            _transaction = null;
        }
    }

    public async Task RollbackAsync(CancellationToken cancellationToken = default)
    {
        if (_transaction is not null)
        {
            await _transaction.RollbackAsync(cancellationToken);
            await _transaction.DisposeAsync();
            _transaction = null;
        }
    }
}
