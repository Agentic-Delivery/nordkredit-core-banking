using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardListService — paginated card list with filtering.
/// COBOL source: COCRDLIC.cbl:1123-1411 (CICS program — paginated card list).
/// Regulations: PSD2 Art. 97 (SCA for card data), GDPR Art. 15 (right of access).
/// Covers: keyset pagination (forward/backward), account/card filtering, empty results, page boundaries.
/// </summary>
public class CardListServiceTests
{
    private readonly StubCardRepository _repo = new();
    private readonly CardListService _sut;

    public CardListServiceTests()
    {
        _sut = new CardListService(_repo);
    }

    // ===================================================================
    // First page — no cursor (COCRDLIC.cbl:1123-1163, initial display)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_15Cards_NoCursor_ReturnsFirst7()
    {
        // GIVEN the CARDDAT table contains 15 cards
        _repo.SeedCards(15);

        // WHEN GET /api/cards is called with no filters or cursor
        var result = await _sut.GetCardsAsync();

        // THEN the first 7 cards are returned (page size = 7, matching COBOL WS-MAX-SCREEN-LINES)
        Assert.Equal(7, result.Cards.Count);
        Assert.Equal("4000000000000001", result.Cards[0].CardNumber);
        Assert.Equal("4000000000000007", result.Cards[6].CardNumber);
    }

    [Fact]
    public async Task GetCardsAsync_15Cards_NoCursor_HasNextPageTrue()
    {
        // GIVEN the CARDDAT table contains 15 cards
        _repo.SeedCards(15);

        // WHEN GET /api/cards is called with no cursor
        var result = await _sut.GetCardsAsync();

        // THEN hasNextPage is true
        Assert.True(result.HasNextPage);
    }

    [Fact]
    public async Task GetCardsAsync_15Cards_NoCursor_ReturnsCursors()
    {
        // GIVEN the CARDDAT table contains 15 cards
        _repo.SeedCards(15);

        // WHEN GET /api/cards is called with no cursor
        var result = await _sut.GetCardsAsync();

        // THEN response includes firstCardNumber and lastCardNumber
        Assert.Equal("4000000000000001", result.FirstCardNumber);
        Assert.Equal("4000000000000007", result.LastCardNumber);
    }

    [Fact]
    public async Task GetCardsAsync_NoCursor_HasPreviousPageFalse()
    {
        // GIVEN the first page is requested
        _repo.SeedCards(15);

        // WHEN no cursor is provided
        var result = await _sut.GetCardsAsync();

        // THEN hasPreviousPage is false
        Assert.False(result.HasPreviousPage);
    }

    [Fact]
    public async Task GetCardsAsync_ReturnsCardFieldsWithoutCvv()
    {
        // GIVEN a card exists
        _repo.SeedCards(1);

        // WHEN the card list is retrieved
        var result = await _sut.GetCardsAsync();

        // THEN card fields are returned (CVV code NEVER included — PCI-DSS)
        Assert.Single(result.Cards);
        var item = result.Cards[0];
        Assert.Equal("4000000000000001", item.CardNumber);
        Assert.Equal("12345678901", item.AccountId);
        Assert.Equal("CARD HOLDER 1", item.EmbossedName);
        Assert.Equal(new DateOnly(2027, 12, 31), item.ExpirationDate);
        Assert.Equal('Y', item.ActiveStatus);
    }

    // ===================================================================
    // Forward pagination — afterCardNumber cursor (COCRDLIC.cbl:1129-1163, PF8)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_WithAfterCursor_ReturnsNextPage()
    {
        // GIVEN a nextCursor from a previous response
        _repo.SeedCards(15);

        // WHEN GET /api/cards?afterCardNumber={cursor} is called
        var result = await _sut.GetCardsAsync(afterCardNumber: "4000000000000007");

        // THEN the next 7 cards are returned
        Assert.Equal(7, result.Cards.Count);
        Assert.Equal("4000000000000008", result.Cards[0].CardNumber);
        Assert.Equal("4000000000000014", result.Cards[6].CardNumber);
    }

    [Fact]
    public async Task GetCardsAsync_WithAfterCursor_HasPreviousPageTrue()
    {
        // GIVEN navigating forward
        _repo.SeedCards(15);

        // WHEN afterCardNumber cursor is provided
        var result = await _sut.GetCardsAsync(afterCardNumber: "4000000000000007");

        // THEN hasPreviousPage is true
        Assert.True(result.HasPreviousPage);
    }

    [Fact]
    public async Task GetCardsAsync_LastPage_HasNextPageFalse()
    {
        // GIVEN we're on the last page
        _repo.SeedCards(15);

        // WHEN requesting the third page (only 1 record left)
        var result = await _sut.GetCardsAsync(afterCardNumber: "4000000000000014");

        // THEN hasNextPage is false
        Assert.False(result.HasNextPage);
        Assert.Single(result.Cards);
    }

    // ===================================================================
    // Backward pagination — beforeCardNumber cursor (COCRDLIC.cbl:1264-1292, PF7)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_WithBeforeCursor_ReturnsPreviousPage()
    {
        // GIVEN the user requests backward navigation
        _repo.SeedCards(15);

        // WHEN GET /api/cards?beforeCardNumber={cursor} is called
        var result = await _sut.GetCardsAsync(beforeCardNumber: "4000000000000008");

        // THEN the previous 7 cards are returned in ascending order
        Assert.Equal(7, result.Cards.Count);
        Assert.Equal("4000000000000001", result.Cards[0].CardNumber);
        Assert.Equal("4000000000000007", result.Cards[6].CardNumber);
    }

    [Fact]
    public async Task GetCardsAsync_WithBeforeCursor_HasNextPageTrue()
    {
        // GIVEN navigating backward from page 2
        _repo.SeedCards(15);

        // WHEN beforeCardNumber cursor is provided
        var result = await _sut.GetCardsAsync(beforeCardNumber: "4000000000000008");

        // THEN hasNextPage is true (can go forward again)
        Assert.True(result.HasNextPage);
    }

    // ===================================================================
    // Account ID filter (COCRDLIC.cbl:1382-1397, 9500-FILTER-RECORDS)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_AccountIdFilter_ReturnsOnlyMatchingCards()
    {
        // GIVEN account filter "12345678901"
        _repo.SeedCardsWithAccounts();

        // WHEN GET /api/cards?accountId=12345678901 is called
        var result = await _sut.GetCardsAsync(accountId: "12345678901");

        // THEN only cards belonging to that account are returned
        Assert.All(result.Cards, c => Assert.Equal("12345678901", c.AccountId));
    }

    // ===================================================================
    // Card number filter (COCRDLIC.cbl:1392-1397, 9500-FILTER-RECORDS)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_CardNumberFilter_ReturnsMatchingCard()
    {
        // GIVEN card number filter
        _repo.SeedCards(15);

        // WHEN GET /api/cards?cardNumber=4000000000000005 is called
        var result = await _sut.GetCardsAsync(cardNumber: "4000000000000005");

        // THEN only the matching card is returned
        Assert.Single(result.Cards);
        Assert.Equal("4000000000000005", result.Cards[0].CardNumber);
    }

    // ===================================================================
    // Combined filters (COCRDLIC.cbl:1382-1397 — AND logic)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_BothFilters_ReturnsBothMatching()
    {
        // GIVEN both account and card filters
        _repo.SeedCardsWithAccounts();

        // WHEN GET /api/cards?accountId=12345678901&cardNumber=4000000000000001 is called
        var result = await _sut.GetCardsAsync(
            accountId: "12345678901",
            cardNumber: "4000000000000001");

        // THEN only cards matching BOTH filters are returned
        Assert.Single(result.Cards);
        Assert.Equal("4000000000000001", result.Cards[0].CardNumber);
        Assert.Equal("12345678901", result.Cards[0].AccountId);
    }

    // ===================================================================
    // No records match (COCRDLIC.cbl:1139-1141 — "NO RECORDS FOUND...")
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_NoMatchingRecords_ReturnsEmptyWithMessage()
    {
        // GIVEN no records match
        _repo.SeedCards(5);

        // WHEN GET /api/cards?accountId=99999999999 is called
        var result = await _sut.GetCardsAsync(accountId: "99999999999");

        // THEN empty array with message
        Assert.Empty(result.Cards);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", result.Message);
    }

    // ===================================================================
    // Empty table (edge case)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_EmptyTable_ReturnsEmptyWithMessage()
    {
        // GIVEN the CARDDAT table is empty

        // WHEN GET /api/cards is called
        var result = await _sut.GetCardsAsync();

        // THEN empty array with message
        Assert.Empty(result.Cards);
        Assert.False(result.HasNextPage);
        Assert.False(result.HasPreviousPage);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", result.Message);
    }

    // ===================================================================
    // Exactly 7 records (edge case — COCRDLIC.cbl:1145-1163)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_Exactly7Records_HasNextPageFalse()
    {
        // COBOL reads 8th record to detect next page
        // With exactly 7, the 8th read fails → next-page = NO
        _repo.SeedCards(7);

        var result = await _sut.GetCardsAsync();

        Assert.Equal(7, result.Cards.Count);
        Assert.False(result.HasNextPage);
    }

    [Fact]
    public async Task GetCardsAsync_8Records_HasNextPageTrue()
    {
        // With 8 records, the 8th read succeeds → next-page = YES
        _repo.SeedCards(8);

        var result = await _sut.GetCardsAsync();

        Assert.Equal(7, result.Cards.Count);
        Assert.True(result.HasNextPage);
        Assert.Equal("4000000000000007", result.LastCardNumber);
    }

    // ===================================================================
    // Cursor beyond last record (edge case)
    // ===================================================================

    [Fact]
    public async Task GetCardsAsync_CursorBeyondLastRecord_ReturnsEmptyWithMessage()
    {
        _repo.SeedCards(5);

        var result = await _sut.GetCardsAsync(afterCardNumber: "9999999999999999");

        Assert.Empty(result.Cards);
        Assert.False(result.HasNextPage);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", result.Message);
    }
}

/// <summary>
/// In-memory test double for ICardRepository supporting pagination and filtering.
/// </summary>
internal sealed class StubCardRepository : ICardRepository
{
    private readonly List<Card> _cards = [];

    public void SeedCards(int count)
    {
        for (var i = 1; i <= count; i++)
        {
            _cards.Add(new Card
            {
                CardNumber = $"4000000000000{i:D3}",
                AccountId = "12345678901",
                CvvCode = "123",
                EmbossedName = $"CARD HOLDER {i}",
                ExpirationDate = new DateOnly(2027, 12, 31),
                ActiveStatus = 'Y'
            });
        }
    }

    public void SeedCardsWithAccounts()
    {
        _cards.Add(new Card
        {
            CardNumber = "4000000000000001",
            AccountId = "12345678901",
            CvvCode = "123",
            EmbossedName = "ALICE ANDERSSON",
            ExpirationDate = new DateOnly(2027, 12, 31),
            ActiveStatus = 'Y'
        });
        _cards.Add(new Card
        {
            CardNumber = "4000000000000002",
            AccountId = "12345678901",
            CvvCode = "456",
            EmbossedName = "ALICE ANDERSSON",
            ExpirationDate = new DateOnly(2028, 6, 30),
            ActiveStatus = 'Y'
        });
        _cards.Add(new Card
        {
            CardNumber = "4000000000000003",
            AccountId = "98765432109",
            CvvCode = "789",
            EmbossedName = "BJÖRK ÖBERG",
            ExpirationDate = new DateOnly(2027, 3, 31),
            ActiveStatus = 'N'
        });
    }

    public Task<Card?> GetByCardNumberAsync(
        string cardNumber, CancellationToken cancellationToken = default) =>
        Task.FromResult(_cards.FirstOrDefault(c => c.CardNumber == cardNumber));

    public Task<IReadOnlyList<Card>> GetByAccountIdAsync(
        string accountId, CancellationToken cancellationToken = default)
    {
        IReadOnlyList<Card> result = [.. _cards
            .Where(c => c.AccountId == accountId)
            .OrderBy(c => c.CardNumber)];
        return Task.FromResult(result);
    }

    public Task<IReadOnlyList<Card>> GetPageForwardAsync(
        int pageSize,
        string? startAfterCardNumber = null,
        CancellationToken cancellationToken = default)
    {
        IEnumerable<Card> query = _cards.OrderBy(c => c.CardNumber);

        if (!string.IsNullOrEmpty(startAfterCardNumber))
        {
            query = query.Where(c =>
                string.Compare(c.CardNumber, startAfterCardNumber, StringComparison.Ordinal) > 0);
        }

        IReadOnlyList<Card> result = [.. query.Take(pageSize)];
        return Task.FromResult(result);
    }

    public Task<IReadOnlyList<Card>> GetPageBackwardAsync(
        int pageSize,
        string? startBeforeCardNumber = null,
        CancellationToken cancellationToken = default)
    {
        IEnumerable<Card> query = _cards.OrderByDescending(c => c.CardNumber);

        if (!string.IsNullOrEmpty(startBeforeCardNumber))
        {
            query = query.Where(c =>
                string.Compare(c.CardNumber, startBeforeCardNumber, StringComparison.Ordinal) < 0);
        }

        IReadOnlyList<Card> result = [.. query.Take(pageSize)];
        return Task.FromResult(result);
    }

    public Task UpdateAsync(Card card, CancellationToken cancellationToken = default) =>
        Task.CompletedTask;
}
