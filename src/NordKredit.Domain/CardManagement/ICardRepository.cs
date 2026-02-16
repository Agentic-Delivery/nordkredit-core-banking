namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Repository for card records.
/// COBOL source: CVACT02Y.cpy (CARD-RECORD), VSAM CARDDAT (primary key) and CARDAIX (alternate index).
/// Used by COCRDLIC (list), COCRDSLC (detail), COCRDUPC (update).
/// Regulations: GDPR Art. 5(1)(c)(d), PSD2 Art. 97.
/// </summary>
public interface ICardRepository
{
    /// <summary>
    /// Retrieves a card by its card number (primary key lookup).
    /// COBOL: VSAM CARDDAT keyed read by CARD-NUM.
    /// </summary>
    Task<Card?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all cards for an account (alternate index lookup).
    /// COBOL: VSAM CARDAIX browse by CARD-ACCT-ID.
    /// </summary>
    Task<IReadOnlyList<Card>> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves a page of cards ordered by card number (forward pagination).
    /// COBOL: COCRDLIC.cbl — PROCESS-PAGE-FORWARD (STARTBR/READNEXT).
    /// </summary>
    Task<IReadOnlyList<Card>> GetPageForwardAsync(
        int pageSize,
        string? startAfterCardNumber = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves a page of cards ordered by card number descending (backward pagination).
    /// COBOL: COCRDLIC.cbl — PROCESS-PAGE-BACKWARD (STARTBR/READPREV).
    /// </summary>
    Task<IReadOnlyList<Card>> GetPageBackwardAsync(
        int pageSize,
        string? startBeforeCardNumber = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates a card record with optimistic concurrency check.
    /// COBOL: COCRDUPC.cbl — REWRITE CARD-RECORD.
    /// Uses RowVersion (SQL rowversion) instead of COBOL field-by-field comparison.
    /// </summary>
    Task UpdateAsync(Card card, CancellationToken cancellationToken = default);
}
