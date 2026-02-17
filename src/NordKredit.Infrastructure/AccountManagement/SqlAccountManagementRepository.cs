using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.AccountManagement;

namespace NordKredit.Infrastructure.AccountManagement;

/// <summary>
/// Azure SQL implementation of <see cref="IAccountManagementRepository"/>.
/// Replaces VSAM ACCTFILE keyed file operations for account management.
/// Business rule: ACCT-BR-001 (account record data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, GDPR Art. 17.
/// </summary>
public class SqlAccountManagementRepository : IAccountManagementRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlAccountManagementRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
    {
        return await _dbContext.ManagedAccounts
            .FirstOrDefaultAsync(a => a.Id == accountId, cancellationToken);
    }

    public async Task<IReadOnlyList<Account>> GetAllAsync(int pageSize, string? afterAccountId, CancellationToken cancellationToken = default)
    {
        var query = _dbContext.ManagedAccounts.AsQueryable();

        if (!string.IsNullOrEmpty(afterAccountId))
        {
            query = query.Where(a => string.Compare(a.Id, afterAccountId, StringComparison.Ordinal) > 0);
        }

        return await query
            .OrderBy(a => a.Id)
            .Take(pageSize)
            .ToListAsync(cancellationToken);
    }

    public async Task AddAsync(Account account, CancellationToken cancellationToken = default)
    {
        _dbContext.ManagedAccounts.Add(account);
        await _dbContext.SaveChangesAsync(cancellationToken);
    }

    public async Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
    {
        _dbContext.ManagedAccounts.Update(account);
        await _dbContext.SaveChangesAsync(cancellationToken);
    }
}
