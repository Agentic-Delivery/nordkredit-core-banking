using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Azure SQL implementation of <see cref="ITransactionTypeRepository"/>.
/// Replaces VSAM TRANTYPE indexed file random-read operations.
/// COBOL source: CVTRA03Y.cpy (TRAN-TYPE-RECORD).
/// </summary>
public class SqlTransactionTypeRepository : ITransactionTypeRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlTransactionTypeRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<TransactionType?> GetByCodeAsync(string typeCode, CancellationToken cancellationToken = default)
    {
        return await _dbContext.TransactionTypes
            .FirstOrDefaultAsync(t => t.TypeCode == typeCode, cancellationToken);
    }
}
