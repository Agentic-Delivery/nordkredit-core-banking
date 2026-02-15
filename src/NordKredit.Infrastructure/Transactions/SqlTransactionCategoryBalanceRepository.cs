using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Azure SQL implementation of <see cref="ITransactionCategoryBalanceRepository"/>.
/// Replaces VSAM TRAN-CAT-BAL keyed file operations.
/// </summary>
public class SqlTransactionCategoryBalanceRepository : ITransactionCategoryBalanceRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlTransactionCategoryBalanceRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<TransactionCategoryBalance?> GetAsync(
        string accountId,
        string typeCode,
        int categoryCode,
        CancellationToken cancellationToken = default)
    {
        return await _dbContext.TransactionCategoryBalances
            .FirstOrDefaultAsync(
                b => b.AccountId == accountId && b.TypeCode == typeCode && b.CategoryCode == categoryCode,
                cancellationToken);
    }

    public async Task<IReadOnlyList<TransactionCategoryBalance>> GetByAccountIdAsync(
        string accountId,
        CancellationToken cancellationToken = default)
    {
        return await _dbContext.TransactionCategoryBalances
            .Where(b => b.AccountId == accountId)
            .OrderBy(b => b.TypeCode)
            .ThenBy(b => b.CategoryCode)
            .ToListAsync(cancellationToken);
    }

    public async Task UpsertAsync(TransactionCategoryBalance balance, CancellationToken cancellationToken = default)
    {
        var existing = await GetAsync(balance.AccountId, balance.TypeCode, balance.CategoryCode, cancellationToken);

        if (existing is null)
        {
            await _dbContext.TransactionCategoryBalances.AddAsync(balance, cancellationToken);
        }
        else
        {
            existing.Balance = balance.Balance;
        }

        await _dbContext.SaveChangesAsync(cancellationToken);
    }
}
