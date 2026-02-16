using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.Transactions;

namespace NordKredit.Infrastructure.Transactions;

/// <summary>
/// Azure SQL implementation of <see cref="ICardCrossReferenceRepository"/>.
/// Replaces VSAM CARDXREF keyed file read operations.
/// </summary>
public class SqlCardCrossReferenceRepository : ICardCrossReferenceRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlCardCrossReferenceRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    public async Task<CardCrossReference?> GetByCardNumberAsync(
        string cardNumber,
        CancellationToken cancellationToken = default)
    {
        return await _dbContext.CardCrossReferences
            .FirstOrDefaultAsync(x => x.CardNumber == cardNumber, cancellationToken);
    }

    public async Task<CardCrossReference?> GetByAccountIdAsync(
        string accountId,
        CancellationToken cancellationToken = default)
    {
        return await _dbContext.CardCrossReferences
            .FirstOrDefaultAsync(x => x.AccountId == accountId, cancellationToken);
    }
}
