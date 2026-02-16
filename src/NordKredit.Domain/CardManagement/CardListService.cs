using System.Globalization;

namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Lists cards with keyset pagination and optional filtering.
/// COBOL source: COCRDLIC.cbl:1123-1411 (PROCESS-PAGE-FORWARD/BACKWARD, FILTER-RECORDS).
/// Replaces CICS STARTBR/READNEXT/READPREV on CARDDAT file with SQL keyset queries.
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access), FFFS 2014:5 Ch. 8 §4.
/// </summary>
public class CardListService
{
    /// <summary>
    /// Page size matching COBOL WS-MAX-SCREEN-LINES = 7.
    /// COBOL: COCRDLIC.cbl:178 — 05 WS-MAX-SCREEN-LINES PIC 9(02) VALUE 7.
    /// </summary>
    internal const int PageSize = 7;

    private readonly ICardRepository _cardRepository;

    public CardListService(ICardRepository cardRepository)
    {
        _cardRepository = cardRepository;
    }

    /// <summary>
    /// Retrieves a forward page of cards with optional filtering.
    /// COBOL: COCRDLIC.cbl:1129-1163 — 9000-READ-FORWARD (STARTBR + READNEXT).
    /// Reads PageSize + 1 records to detect next page existence.
    /// </summary>
    /// <param name="afterCardNumber">Keyset cursor — card number to start after. Null for first page.</param>
    /// <param name="accountId">Optional account ID filter. COBOL: 9500-FILTER-RECORDS.</param>
    /// <param name="cardNumber">Optional card number filter. COBOL: 9500-FILTER-RECORDS.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    public async Task<CardListResponse> GetCardsForwardAsync(
        string? afterCardNumber = null,
        string? accountId = null,
        string? cardNumber = null,
        CancellationToken cancellationToken = default)
    {
        // Read PageSize + 1 to detect if next page exists (COBOL 7+1 pattern)
        var records = await _cardRepository.GetPageForwardAsync(
            PageSize + 1,
            afterCardNumber,
            cancellationToken);

        // Apply in-memory filters matching COBOL 9500-FILTER-RECORDS
        var filtered = ApplyFilters(records, accountId, cardNumber);

        var hasNextPage = filtered.Count > PageSize;
        var pageCards = hasNextPage ? filtered.GetRange(0, PageSize) : filtered;

        if (pageCards.Count == 0)
        {
            // COBOL: COCRDLIC.cbl:1139-1141 — 'NO RECORDS FOUND FOR THIS SEARCH CONDITION'
            return new CardListResponse
            {
                Cards = [],
                HasNextPage = false,
                HasPreviousPage = afterCardNumber is not null,
                Message = "NO RECORDS FOUND FOR THIS SEARCH CONDITION"
            };
        }

        var items = MapToItems(pageCards);

        return new CardListResponse
        {
            Cards = items,
            HasNextPage = hasNextPage,
            HasPreviousPage = afterCardNumber is not null,
            FirstCardNumber = items[0].CardNumber,
            LastCardNumber = items[^1].CardNumber
        };
    }

    /// <summary>
    /// Retrieves a backward page of cards with optional filtering.
    /// COBOL: COCRDLIC.cbl:1264-1292 — 9100-READ-BACKWARD (STARTBR + READPREV).
    /// Reads PageSize + 1 records to detect previous page existence.
    /// </summary>
    /// <param name="beforeCardNumber">Keyset cursor — card number to start before.</param>
    /// <param name="accountId">Optional account ID filter. COBOL: 9500-FILTER-RECORDS.</param>
    /// <param name="cardNumber">Optional card number filter. COBOL: 9500-FILTER-RECORDS.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    public async Task<CardListResponse> GetCardsBackwardAsync(
        string beforeCardNumber,
        string? accountId = null,
        string? cardNumber = null,
        CancellationToken cancellationToken = default)
    {
        // Read PageSize + 1 to detect if previous page exists
        var records = await _cardRepository.GetPageBackwardAsync(
            PageSize + 1,
            beforeCardNumber,
            cancellationToken);

        // Apply in-memory filters matching COBOL 9500-FILTER-RECORDS
        var filtered = ApplyFilters(records, accountId, cardNumber);

        var hasPreviousPage = filtered.Count > PageSize;
        var pageCards = hasPreviousPage ? filtered.GetRange(0, PageSize) : filtered;

        // Reverse to ascending order (backward reads in descending)
        pageCards = [.. pageCards.OrderBy(c => c.CardNumber)];

        if (pageCards.Count == 0)
        {
            return new CardListResponse
            {
                Cards = [],
                HasNextPage = true,
                HasPreviousPage = false,
                Message = "NO RECORDS FOUND FOR THIS SEARCH CONDITION"
            };
        }

        var items = MapToItems(pageCards);

        return new CardListResponse
        {
            Cards = items,
            HasNextPage = true,
            HasPreviousPage = hasPreviousPage,
            FirstCardNumber = items[0].CardNumber,
            LastCardNumber = items[^1].CardNumber
        };
    }

    /// <summary>
    /// Applies account and card number filters matching COBOL 9500-FILTER-RECORDS.
    /// COBOL: COCRDLIC.cbl:1382-1400.
    /// </summary>
    private static List<Card> ApplyFilters(
        IReadOnlyList<Card> records,
        string? accountId,
        string? cardNumber)
    {
        IEnumerable<Card> result = records;

        if (!string.IsNullOrWhiteSpace(accountId))
        {
            result = result.Where(c => c.AccountId == accountId);
        }

        if (!string.IsNullOrWhiteSpace(cardNumber))
        {
            result = result.Where(c => c.CardNumber == cardNumber);
        }

        return [.. result];
    }

    private static List<CardListItem> MapToItems(IEnumerable<Card> cards) =>
        [.. cards.Select(c => new CardListItem
        {
            CardNumber = c.CardNumber,
            AccountId = c.AccountId,
            EmbossedName = c.EmbossedName,
            ExpirationDate = c.ExpirationDate.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture),
            ActiveStatus = c.ActiveStatus
        })];
}
