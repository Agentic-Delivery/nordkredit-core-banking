namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Retrieves and formats a single card for read-only detail view.
/// COBOL source: COCRDSLC.cbl:608-812 (CICS program — card detail lookup).
/// Replaces CICS READ on CARDDAT (primary key) and CARDAIX (alternate index).
/// Regulations: PSD2 Art. 97, GDPR Art. 15, FFFS 2014:5 Ch. 8 section 4.
/// </summary>
public class CardDetailService
{
    private readonly ICardRepository _cardRepository;

    public CardDetailService(ICardRepository cardRepository)
    {
        _cardRepository = cardRepository;
    }

    /// <summary>
    /// Retrieves a card by its card number (primary key lookup).
    /// COBOL: COCRDSLC.cbl:736-774 — 9100-GETCARD-BYACCTCARD (READ CARDDAT).
    /// </summary>
    public async Task<CardDetailResponse?> GetByCardNumberAsync(
        string cardNumber,
        CancellationToken cancellationToken = default)
    {
        var card = await _cardRepository.GetByCardNumberAsync(cardNumber, cancellationToken);
        return card is null ? null : MapToResponse(card);
    }

    /// <summary>
    /// Retrieves the first card for an account (alternate index lookup).
    /// COBOL: COCRDSLC.cbl:779-812 — 9150-GETCARD-BYACCT (READ CARDAIX).
    /// Returns first card by key order, matching COBOL CICS READ behavior on alternate index.
    /// </summary>
    public async Task<CardDetailResponse?> GetByAccountIdAsync(
        string accountId,
        CancellationToken cancellationToken = default)
    {
        var cards = await _cardRepository.GetByAccountIdAsync(accountId, cancellationToken);
        var card = cards.Count > 0 ? cards[0] : null;
        return card is null ? null : MapToResponse(card);
    }

    private static CardDetailResponse MapToResponse(Card card) => new()
    {
        CardNumber = card.CardNumber,
        AccountId = card.AccountId,
        EmbossedName = card.EmbossedName,
        ExpirationDate = card.ExpirationDate.ToString("yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture),
        ActiveStatus = card.ActiveStatus
    };
}
