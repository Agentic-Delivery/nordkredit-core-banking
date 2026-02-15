using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Azure SQL implementation of <see cref="IAccountRepository"/>.
/// Replaces VSAM ACCTFILE keyed file read/write operations for transaction processing.
/// </summary>
public class SqlAccountRepository : IAccountRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlAccountRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
    {
        return await _dbContext.Accounts
            .FirstOrDefaultAsync(a => a.Id == accountId, cancellationToken);
    }

    public async Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
    {
        _dbContext.Accounts.Update(account);
        await _dbContext.SaveChangesAsync(cancellationToken);
    }
}
