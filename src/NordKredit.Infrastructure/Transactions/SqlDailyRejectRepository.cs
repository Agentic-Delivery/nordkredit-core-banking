using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// SQL Server implementation of <see cref="IDailyRejectRepository"/>.
/// Maps to DailyRejects table in Azure SQL.
/// COBOL source: CBTRN02C.cbl:370-422 — rejected records written with fail codes.
/// </summary>
public class SqlDailyRejectRepository : IDailyRejectRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlDailyRejectRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task AddAsync(DailyReject reject, CancellationToken cancellationToken = default)
    {
        await _dbContext.DailyRejects.AddAsync(reject, cancellationToken);
        await _dbContext.SaveChangesAsync(cancellationToken);
    }
}
