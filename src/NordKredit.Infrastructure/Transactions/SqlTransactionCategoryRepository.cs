using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Azure SQL implementation of <see cref="ITransactionCategoryRepository"/>.
/// Replaces VSAM TRANCATG indexed file random-read operations.
/// COBOL source: CVTRA04Y.cpy (TRAN-CAT-RECORD).
/// </summary>
public class SqlTransactionCategoryRepository : ITransactionCategoryRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlTransactionCategoryRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<TransactionCategory?> GetAsync(
        string typeCode, int categoryCode, CancellationToken cancellationToken = default)
    {
        return await _dbContext.TransactionCategories
            .FirstOrDefaultAsync(c => c.TypeCode == typeCode && c.CategoryCode == categoryCode,
                cancellationToken);
    }
}
