namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Lists cards with keyset pagination and optional filtering.
/// COBOL source: COCRDLIC.cbl:1123-1411 (CICS program — paginated card list with forward/backward browsing).
/// Regulations: PSD2 Art. 97 (SCA for card data access), GDPR Art. 15 (right of access).
/// </summary>
public class CardListService
{
    /// <summary>
    /// Page size matching COBOL hardcoded 7-record display.
    /// COBOL: COCRDLIC.cbl:178 — WS-MAX-SCREEN-LINES PIC 9(02) VALUE 7.
    /// </summary>
    internal const int PageSize = 7;

    private readonly ICardRepository _cardRepository;

    public CardListService(ICardRepository cardRepository)
    {
        _cardRepository = cardRepository;
    }

    /// <summary>
    /// Retrieves a page of cards using keyset pagination with optional filtering.
    /// Reads PageSize + 1 records to detect next page (COBOL 7+1 pattern).
    /// COBOL: PROCESS-PAGE-FORWARD (COCRDLIC.cbl:1129-1163), PROCESS-PAGE-BACKWARD (1264-1292).
    /// Filtering: 9500-FILTER-RECORDS (1382-1397) — account ID and/or card number, AND logic.
    /// </summary>
    public async Task<CardListResponse> GetCardsAsync(
        string? afterCardNumber = null,
        string? beforeCardNumber = null,
        string? accountId = null,
        string? cardNumber = null,
        CancellationToken cancellationToken = default)
    {
        IReadOnlyList<Card> records;
        bool isBackward = !string.IsNullOrEmpty(beforeCardNumber);
        bool hasPreviousPage = !string.IsNullOrEmpty(afterCardNumber);

        if (isBackward)
        {
            // Backward pagination — COBOL: STARTBR + READPREV (PF7)
            // Read PageSize + 1 to detect previous page existence
            records = await _cardRepository.GetPageBackwardAsync(
                PageSize + 1, beforeCardNumber, cancellationToken);

            // Check if there are more records before this page
            bool hasMoreBefore = records.Count > PageSize;
            var pageRecords = hasMoreBefore
                ? records.Take(PageSize).Reverse().ToList()
                : records.Reverse().ToList();

            // Apply filters — COBOL: 9500-FILTER-RECORDS
            var filtered = ApplyFilters(pageRecords, accountId, cardNumber);

            if (filtered.Count == 0)
            {
                return EmptyResponse();
            }

            return new CardListResponse
            {
                Cards = MapToItems(filtered),
                HasNextPage = true, // Can always go forward after going backward
                HasPreviousPage = hasMoreBefore,
                FirstCardNumber = filtered[0].CardNumber,
                LastCardNumber = filtered[^1].CardNumber
            };
        }
        else
        {
            // Forward pagination — COBOL: STARTBR + READNEXT (PF8)
            // Read PageSize + 1 to detect next page (COBOL 7+1 pattern)
            records = await _cardRepository.GetPageForwardAsync(
                PageSize + 1, afterCardNumber, cancellationToken);

            bool hasNextPage = records.Count > PageSize;
            List<Card> pageRecords = hasNextPage
                ? [.. records.Take(PageSize)]
                : [.. records];

            // Apply filters — COBOL: 9500-FILTER-RECORDS
            var filtered = ApplyFilters(pageRecords, accountId, cardNumber);

            if (filtered.Count == 0)
            {
                return EmptyResponse();
            }

            return new CardListResponse
            {
                Cards = MapToItems(filtered),
                HasNextPage = hasNextPage,
                HasPreviousPage = hasPreviousPage,
                FirstCardNumber = filtered[0].CardNumber,
                LastCardNumber = filtered[^1].CardNumber
            };
        }
    }

    /// <summary>
    /// Applies account ID and/or card number filters (AND logic).
    /// COBOL: 9500-FILTER-RECORDS (COCRDLIC.cbl:1382-1397).
    /// </summary>
    private static List<Card> ApplyFilters(
        List<Card> cards,
        string? accountId,
        string? cardNumber)
    {
        IEnumerable<Card> filtered = cards;

        if (!string.IsNullOrWhiteSpace(accountId))
        {
            filtered = filtered.Where(c => c.AccountId == accountId);
        }

        if (!string.IsNullOrWhiteSpace(cardNumber))
        {
            filtered = filtered.Where(c => c.CardNumber == cardNumber);
        }

        return filtered.ToList();
    }

    /// <summary>
    /// Maps Card entities to CardListItem DTOs.
    /// CVV code is NEVER included in list response (PCI-DSS).
    /// </summary>
    private static IReadOnlyList<CardListItem> MapToItems(List<Card> cards) =>
        [.. cards.Select(c => new CardListItem
        {
            CardNumber = c.CardNumber,
            AccountId = c.AccountId,
            EmbossedName = c.EmbossedName,
            ExpirationDate = c.ExpirationDate,
            ActiveStatus = c.ActiveStatus
        })];

    /// <summary>
    /// Returns empty response with "NO RECORDS FOUND" message.
    /// COBOL: COCRDLIC.cbl:1139-1141.
    /// </summary>
    private static CardListResponse EmptyResponse() => new()
    {
        Cards = [],
        HasNextPage = false,
        HasPreviousPage = false,
        Message = "NO RECORDS FOUND FOR THIS SEARCH CONDITION"
    };
}
