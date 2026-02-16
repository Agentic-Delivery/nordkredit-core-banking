using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.CardManagement;
using TransactionXref = NordKredit.Domain.Transactions.CardCrossReference;

namespace NordKredit.Infrastructure.CardManagement;

/// <summary>
/// Azure SQL implementation of <see cref="ICardCrossReferenceRepository"/> for the Card Management domain.
/// Replaces VSAM CARDXREF file operations. COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD).
/// Supports card-to-customer lookups required for AML/KYC compliance.
/// </summary>
public class SqlCardCrossReferenceRepository : ICardCrossReferenceRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlCardCrossReferenceRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    /// <inheritdoc />
    public async Task<CardCrossReference?> GetByCardNumberAsync(
        string cardNumber,
        CancellationToken cancellationToken = default)
    {
        var entity = await _dbContext.CardCrossReferences
            .FirstOrDefaultAsync(x => x.CardNumber == cardNumber, cancellationToken);

        return entity is null ? null : MapToDomain(entity);
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<CardCrossReference>> GetByCustomerIdAsync(
        int customerId,
        CancellationToken cancellationToken = default)
    {
        var entities = await _dbContext.CardCrossReferences
            .Where(x => x.CustomerId == customerId)
            .OrderBy(x => x.CardNumber)
            .ToListAsync(cancellationToken);

        return entities.Select(MapToDomain).ToList();
    }

    private static CardCrossReference MapToDomain(TransactionXref entity)
    {
        return new CardCrossReference
        {
            CardNumber = entity.CardNumber,
            CustomerId = entity.CustomerId,
            AccountId = entity.AccountId
        };
    }
}
