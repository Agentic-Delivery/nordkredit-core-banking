namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Repository for card records.
/// COBOL source: VSAM CARDDAT file (primary key), CARDAIX alternate index (account ID).
/// Accessed by COCRDLIC (list), COCRDSLC (detail), COCRDUPC (update).
/// </summary>
public interface ICardRepository
{
    /// <summary>
    /// Reads a single card by its card number.
    /// COBOL: VSAM READ on CARDDAT by primary key.
    /// </summary>
    Task<Card?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Reads all cards linked to an account.
    /// COBOL: VSAM browse on CARDAIX alternate index by CARD-ACCT-ID.
    /// </summary>
    Task<IReadOnlyList<Card>> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves a forward page of cards ordered by card number (keyset pagination).
    /// COBOL: COCRDLIC.cbl — PROCESS-PAGE-FORWARD (STARTBR + READNEXT).
    /// </summary>
    Task<IReadOnlyList<Card>> GetPageForwardAsync(
        int pageSize,
        string? startAfterCardNumber = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves a backward page of cards ordered by card number descending (keyset pagination).
    /// COBOL: COCRDLIC.cbl — PROCESS-PAGE-BACKWARD (STARTBR + READPREV).
    /// </summary>
    Task<IReadOnlyList<Card>> GetPageBackwardAsync(
        int pageSize,
        string? startBeforeCardNumber = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates an existing card record with optimistic concurrency check.
    /// COBOL: COCRDUPC.cbl — REWRITE CARD-RECORD (field-by-field comparison replaced by rowversion).
    /// </summary>
    Task UpdateAsync(Card card, CancellationToken cancellationToken = default);
}
