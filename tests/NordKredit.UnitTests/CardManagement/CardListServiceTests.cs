using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardListService — paginated card list with filtering.
/// COBOL source: COCRDLIC.cbl:1123-1411 (READ-FORWARD/READ-BACKWARD/FILTER-RECORDS).
/// CARD-BR-001: 7 scenarios covering pagination, filtering, empty results.
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).
/// </summary>
public class CardListServiceTests
{
    private readonly StubCardRepository _repo = new();
    private readonly CardListService _service;

    public CardListServiceTests()
    {
        _service = new CardListService(_repo);
    }

    // ===================================================================
    // CARD-BR-001 Scenario 1: First page without filters
    // COBOL: COCRDLIC.cbl:1129-1163 — 9000-READ-FORWARD, page size = 7
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_15Cards_ReturnsFirst7()
    {
        // GIVEN 15 cards in CARDDAT
        AddCards(15);

        // WHEN GET /api/cards (no filters, no cursor)
        var result = await _service.GetCardsForwardAsync();

        // THEN first 7 cards returned
        Assert.Equal(7, result.Cards.Count);
    }

    [Fact]
    public async Task GetCardsForward_15Cards_HasNextPageTrue()
    {
        AddCards(15);

        var result = await _service.GetCardsForwardAsync();

        Assert.True(result.HasNextPage);
    }

    [Fact]
    public async Task GetCardsForward_15Cards_HasPreviousPageFalseOnFirstPage()
    {
        AddCards(15);

        var result = await _service.GetCardsForwardAsync();

        Assert.False(result.HasPreviousPage);
    }

    [Fact]
    public async Task GetCardsForward_15Cards_ReturnsFirstAndLastCardNumbers()
    {
        AddCards(15);

        var result = await _service.GetCardsForwardAsync();

        Assert.Equal("4000000000000001", result.FirstCardNumber);
        Assert.Equal("4000000000000007", result.LastCardNumber);
    }

    [Fact]
    public async Task GetCardsForward_Exactly7Cards_ReturnsAllWithNoNextPage()
    {
        // COBOL: WS-MAX-SCREEN-LINES PIC 9(02) VALUE 7
        AddCards(7);

        var result = await _service.GetCardsForwardAsync();

        Assert.Equal(7, result.Cards.Count);
        Assert.False(result.HasNextPage);
    }

    // ===================================================================
    // CARD-BR-001 Scenario 2: Navigate forward to next page
    // COBOL: COCRDLIC.cbl — READNEXT with cursor
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_WithCursor_ReturnsNextPage()
    {
        AddCards(15);

        // WHEN afterCardNumber = last card of page 1
        var result = await _service.GetCardsForwardAsync(
            afterCardNumber: "4000000000000007");

        // THEN next 7 cards returned (cards 8-14, plus hasNextPage from card 15)
        Assert.Equal(7, result.Cards.Count);
        Assert.Equal("4000000000000008", result.FirstCardNumber);
        Assert.Equal("4000000000000014", result.LastCardNumber);
    }

    [Fact]
    public async Task GetCardsForward_WithCursor_HasPreviousPageTrue()
    {
        AddCards(15);

        var result = await _service.GetCardsForwardAsync(
            afterCardNumber: "4000000000000007");

        Assert.True(result.HasPreviousPage);
    }

    // ===================================================================
    // CARD-BR-001 Scenario 3: Navigate backward
    // COBOL: COCRDLIC.cbl:1264-1292 — 9100-READ-BACKWARD
    // ===================================================================

    [Fact]
    public async Task GetCardsBackward_ReturnsPage()
    {
        AddCards(15);

        // WHEN beforeCardNumber = first card of page 2
        var result = await _service.GetCardsBackwardAsync(
            beforeCardNumber: "4000000000000008");

        // THEN previous 7 cards in ascending order
        Assert.Equal(7, result.Cards.Count);
        Assert.Equal("4000000000000001", result.FirstCardNumber);
        Assert.Equal("4000000000000007", result.LastCardNumber);
    }

    [Fact]
    public async Task GetCardsBackward_ResultsInAscendingOrder()
    {
        AddCards(15);

        var result = await _service.GetCardsBackwardAsync(
            beforeCardNumber: "4000000000000008");

        // Cards must be in ascending order (re-sorted after backward read)
        for (int i = 1; i < result.Cards.Count; i++)
        {
            Assert.True(
                string.Compare(result.Cards[i - 1].CardNumber, result.Cards[i].CardNumber, StringComparison.Ordinal) < 0);
        }
    }

    [Fact]
    public async Task GetCardsBackward_HasNextPageTrue()
    {
        AddCards(15);

        var result = await _service.GetCardsBackwardAsync(
            beforeCardNumber: "4000000000000008");

        // There are cards after this page
        Assert.True(result.HasNextPage);
    }

    // ===================================================================
    // CARD-BR-001 Scenario 4: Filter by account ID
    // COBOL: COCRDLIC.cbl:1385-1390 — 9500-FILTER-RECORDS (account filter)
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_AccountFilter_ReturnsOnlyMatchingCards()
    {
        // GIVEN cards for two accounts
        _repo.AddCard(CreateCard("4000000000000001", "12345678901"));
        _repo.AddCard(CreateCard("4000000000000002", "99999999999"));
        _repo.AddCard(CreateCard("4000000000000003", "12345678901"));

        // WHEN filter by account "12345678901"
        var result = await _service.GetCardsForwardAsync(accountId: "12345678901");

        // THEN only cards for that account
        Assert.Equal(2, result.Cards.Count);
        Assert.All(result.Cards, c => Assert.Equal("12345678901", c.AccountId));
    }

    // ===================================================================
    // CARD-BR-001 Scenario 5: Filter by card number
    // COBOL: COCRDLIC.cbl:1392-1397 — 9500-FILTER-RECORDS (card filter)
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_CardNumberFilter_ReturnsOnlyMatchingCard()
    {
        AddCards(5);

        var result = await _service.GetCardsForwardAsync(cardNumber: "4000000000000003");

        Assert.Single(result.Cards);
        Assert.Equal("4000000000000003", result.Cards[0].CardNumber);
    }

    // ===================================================================
    // CARD-BR-001 Scenario 6: No records match filter
    // COBOL: COCRDLIC.cbl:1139-1141 — 'NO RECORDS FOUND FOR THIS SEARCH CONDITION'
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_NoMatchingRecords_ReturnsEmptyWithMessage()
    {
        AddCards(5);

        var result = await _service.GetCardsForwardAsync(accountId: "99999999999");

        Assert.Empty(result.Cards);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", result.Message);
    }

    [Fact]
    public async Task GetCardsForward_NoMatchingRecords_HasNextPageFalse()
    {
        AddCards(5);

        var result = await _service.GetCardsForwardAsync(accountId: "99999999999");

        Assert.False(result.HasNextPage);
    }

    // ===================================================================
    // CARD-BR-001 Scenario 7: Combined account and card filter
    // COBOL: 9500-FILTER-RECORDS — both filters applied
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_BothFilters_ReturnsOnlyMatchingBoth()
    {
        _repo.AddCard(CreateCard("4000000000000001", "12345678901"));
        _repo.AddCard(CreateCard("4000000000000002", "99999999999"));
        _repo.AddCard(CreateCard("4000000000000003", "12345678901"));

        // Filter by account AND card number
        var result = await _service.GetCardsForwardAsync(
            accountId: "12345678901",
            cardNumber: "4000000000000001");

        Assert.Single(result.Cards);
        Assert.Equal("4000000000000001", result.Cards[0].CardNumber);
        Assert.Equal("12345678901", result.Cards[0].AccountId);
    }

    [Fact]
    public async Task GetCardsForward_BothFiltersNoMatch_ReturnsEmptyWithMessage()
    {
        _repo.AddCard(CreateCard("4000000000000001", "12345678901"));
        _repo.AddCard(CreateCard("4000000000000002", "99999999999"));

        var result = await _service.GetCardsForwardAsync(
            accountId: "12345678901",
            cardNumber: "4000000000000002");

        Assert.Empty(result.Cards);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", result.Message);
    }

    // ===================================================================
    // Edge case: Fewer than 7 records (partial page)
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_FewerThan7Cards_ReturnsAllWithNoNextPage()
    {
        AddCards(3);

        var result = await _service.GetCardsForwardAsync();

        Assert.Equal(3, result.Cards.Count);
        Assert.False(result.HasNextPage);
    }

    // ===================================================================
    // PCI-DSS: CVV must NOT appear in list response
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_CvvExcludedFromResponse()
    {
        _repo.AddCard(CreateCard("4000000000000001", "12345678901"));

        var result = await _service.GetCardsForwardAsync();

        var item = result.Cards[0];
        Assert.Null(item.GetType().GetProperty("CvvCode"));
    }

    // ===================================================================
    // Card list items include expected fields
    // ===================================================================

    [Fact]
    public async Task GetCardsForward_ItemsContainAllExpectedFields()
    {
        _repo.AddCard(new Card
        {
            CardNumber = "4000123456789012",
            AccountId = "12345678901",
            CvvCode = "123",
            EmbossedName = "JOHN DOE",
            ExpirationDate = new DateOnly(2027, 12, 31),
            ActiveStatus = 'Y',
            RowVersion = [1, 2, 3, 4, 5, 6, 7, 8]
        });

        var result = await _service.GetCardsForwardAsync();

        var item = result.Cards[0];
        Assert.Equal("4000123456789012", item.CardNumber);
        Assert.Equal("12345678901", item.AccountId);
        Assert.Equal("JOHN DOE", item.EmbossedName);
        Assert.Equal("2027-12-31", item.ExpirationDate);
        Assert.Equal('Y', item.ActiveStatus);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private void AddCards(int count)
    {
        for (int i = 1; i <= count; i++)
        {
            _repo.AddCard(CreateCard(
                $"4000000000000{i:D3}",
                "12345678901"));
        }
    }

    private static Card CreateCard(string cardNumber, string accountId) => new()
    {
        CardNumber = cardNumber,
        AccountId = accountId,
        CvvCode = "123",
        EmbossedName = "TEST CARD",
        ExpirationDate = new DateOnly(2027, 12, 31),
        ActiveStatus = 'Y',
        RowVersion = [1, 2, 3, 4, 5, 6, 7, 8]
    };

    /// <summary>
    /// In-memory stub implementing keyset pagination.
    /// </summary>
    internal sealed class StubCardRepository : ICardRepository
    {
        private readonly List<Card> _cards = [];

        public void AddCard(Card card) => _cards.Add(card);

        public Task<Card?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default) =>
            Task.FromResult(_cards.FirstOrDefault(c => c.CardNumber == cardNumber));

        public Task<IReadOnlyList<Card>> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
        {
            IReadOnlyList<Card> result = [.. _cards.Where(c => c.AccountId == accountId).OrderBy(c => c.CardNumber)];
            return Task.FromResult(result);
        }

        public Task<IReadOnlyList<Card>> GetPageForwardAsync(int pageSize, string? startAfterCardNumber = null, CancellationToken cancellationToken = default)
        {
            var query = _cards.OrderBy(c => c.CardNumber).AsEnumerable();
            if (!string.IsNullOrEmpty(startAfterCardNumber))
            {
                query = query.Where(c => string.Compare(c.CardNumber, startAfterCardNumber, StringComparison.Ordinal) > 0);
            }
            IReadOnlyList<Card> result = [.. query.Take(pageSize)];
            return Task.FromResult(result);
        }

        public Task<IReadOnlyList<Card>> GetPageBackwardAsync(int pageSize, string? startBeforeCardNumber = null, CancellationToken cancellationToken = default)
        {
            var query = _cards.OrderByDescending(c => c.CardNumber).AsEnumerable();
            if (!string.IsNullOrEmpty(startBeforeCardNumber))
            {
                query = query.Where(c => string.Compare(c.CardNumber, startBeforeCardNumber, StringComparison.Ordinal) < 0);
            }
            IReadOnlyList<Card> result = [.. query.Take(pageSize)];
            return Task.FromResult(result);
        }

        public Task UpdateAsync(Card card, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }
}
