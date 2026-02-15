using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Azure SQL implementation of <see cref="IDailyTransactionRepository"/>.
/// Replaces sequential DALYTRAN file read operations used by CBTRN01C/CBTRN02C.
/// </summary>
public class SqlDailyTransactionRepository : IDailyTransactionRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlDailyTransactionRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<IReadOnlyList<DailyTransaction>> GetUnprocessedAsync(CancellationToken cancellationToken = default)
    {
        return await _dbContext.DailyTransactions
            .OrderBy(d => d.Id)
            .ToListAsync(cancellationToken);
    }

    public async Task AddAsync(DailyTransaction dailyTransaction, CancellationToken cancellationToken = default)
    {
        await _dbContext.DailyTransactions.AddAsync(dailyTransaction, cancellationToken);
        await _dbContext.SaveChangesAsync(cancellationToken);
    }

    public async Task MarkAsProcessedAsync(string transactionId, CancellationToken cancellationToken = default)
    {
        var entity = await _dbContext.DailyTransactions
            .FirstOrDefaultAsync(d => d.Id == transactionId, cancellationToken);

        if (entity is not null)
        {
            _dbContext.DailyTransactions.Remove(entity);
            await _dbContext.SaveChangesAsync(cancellationToken);
        }
    }
}
