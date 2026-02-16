using Microsoft.EntityFrameworkCore;
using NordKredit.Domain.CardManagement;

namespace NordKredit.Infrastructure.CardManagement;

/// <summary>
/// Azure SQL implementation of <see cref="ICardRepository"/>.
/// Replaces VSAM CARDDAT (primary key) and CARDAIX (alternate index) file operations.
/// COBOL source: COCRDLIC.cbl (list), COCRDSLC.cbl (detail), COCRDUPC.cbl (update).
/// </summary>
public class SqlCardRepository : ICardRepository
{
    private readonly NordKreditDbContext _dbContext;

    public SqlCardRepository(NordKreditDbContext dbContext)
    {
        _dbContext = dbContext;
    }

    /// <inheritdoc />
    public async Task<Card?> GetByCardNumberAsync(
        string cardNumber,
        CancellationToken cancellationToken = default)
    {
        return await _dbContext.Cards
            .FirstOrDefaultAsync(c => c.CardNumber == cardNumber, cancellationToken);
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<Card>> GetByAccountIdAsync(
        string accountId,
        CancellationToken cancellationToken = default)
    {
        return await _dbContext.Cards
            .Where(c => c.AccountId == accountId)
            .OrderBy(c => c.CardNumber)
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<Card>> GetPageForwardAsync(
        int pageSize,
        string? startAfterCardNumber = null,
        CancellationToken cancellationToken = default)
    {
        // Keyset pagination replacing COBOL STARTBR + READNEXT on CARDDAT
        IQueryable<Card> query = _dbContext.Cards.OrderBy(c => c.CardNumber);

        if (!string.IsNullOrEmpty(startAfterCardNumber))
        {
            query = query.Where(c => c.CardNumber.CompareTo(startAfterCardNumber) > 0)
                .OrderBy(c => c.CardNumber);
        }

        return await query
            .Take(pageSize)
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public async Task<IReadOnlyList<Card>> GetPageBackwardAsync(
        int pageSize,
        string? startBeforeCardNumber = null,
        CancellationToken cancellationToken = default)
    {
        // Keyset pagination replacing COBOL STARTBR + READPREV on CARDDAT (PF7)
        IQueryable<Card> query = _dbContext.Cards.OrderByDescending(c => c.CardNumber);

        if (!string.IsNullOrEmpty(startBeforeCardNumber))
        {
            query = query.Where(c => c.CardNumber.CompareTo(startBeforeCardNumber) < 0)
                .OrderByDescending(c => c.CardNumber);
        }

        return await query
            .Take(pageSize)
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public async Task UpdateAsync(Card card, CancellationToken cancellationToken = default)
    {
        // Optimistic concurrency via rowversion — translates EF Core's
        // DbUpdateConcurrencyException to domain-level ConcurrencyConflictException.
        // COBOL: COCRDUPC.cbl:1427-1449 — READ with UPDATE lock + RESP check.
        try
        {
            _dbContext.Cards.Update(card);
            await _dbContext.SaveChangesAsync(cancellationToken);
        }
        catch (DbUpdateConcurrencyException ex)
        {
            throw new ConcurrencyConflictException(
                "Record changed by some one else. Please review", ex);
        }
    }
}
