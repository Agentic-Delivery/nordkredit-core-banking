using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Azure SQL implementation of <see cref="ITransactionRepository"/>.
/// Replaces VSAM TRANSACT file browse/read/write operations.
/// </summary>
public class SqlTransactionRepository : ITransactionRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlTransactionRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default)
    {
        return await _dbContext.Transactions
            .FirstOrDefaultAsync(t => t.Id == transactionId, cancellationToken);
    }

    public async Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(
        string accountId,
        int pageSize,
        string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
    {
        // Keyset pagination matching COBOL VSAM STARTBR/READNEXT pattern
        IQueryable<Transaction> query = _dbContext.Transactions
            .Join(
                _dbContext.CardCrossReferences,
                t => t.CardNumber,
                x => x.CardNumber,
                (t, x) => new { Transaction = t, Xref = x })
            .Where(j => j.Xref.AccountId == accountId)
            .Select(j => j.Transaction)
            .OrderBy(t => t.Id);

        if (!string.IsNullOrEmpty(startAfterTransactionId))
        {
            query = query.Where(t => t.Id.CompareTo(startAfterTransactionId) > 0)
                .OrderBy(t => t.Id);
        }

        return await query
            .Take(pageSize)
            .ToListAsync(cancellationToken);
    }

    public async Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
    {
        await _dbContext.Transactions.AddAsync(transaction, cancellationToken);
        await _dbContext.SaveChangesAsync(cancellationToken);
    }

    public async Task<IReadOnlyList<Transaction>> GetPageAsync(
        int pageSize,
        string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
    {
        IQueryable<Transaction> query = _dbContext.Transactions.OrderBy(t => t.Id);

        if (!string.IsNullOrEmpty(startAfterTransactionId))
        {
            query = query.Where(t => t.Id.CompareTo(startAfterTransactionId) > 0)
                .OrderBy(t => t.Id);
        }

        return await query
            .Take(pageSize)
            .ToListAsync(cancellationToken);
    }

    public async Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default)
    {
        return await _dbContext.Transactions
            .OrderByDescending(t => t.Id)
            .FirstOrDefaultAsync(cancellationToken);
    }
}
