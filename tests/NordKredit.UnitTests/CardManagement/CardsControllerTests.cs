using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardsController HTTP response mapping.
/// COBOL source: COCRDLIC.cbl:1123-1411 (list), COCRDSLC.cbl:608-812 (detail).
/// CARD-BR-001: Pagination and filtering. CARD-BR-002: Selection routing.
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access), FFFS 2014:5 Ch. 8 §4.
/// </summary>
public class CardsControllerTests
{
    private readonly StubCardRepository _cardRepo = new();
    private readonly CardsController _controller;

    public CardsControllerTests()
    {
        var detailService = new CardDetailService(_cardRepo);
        var listService = new CardListService(_cardRepo);
        var updateService = new CardUpdateService(_cardRepo);
        _controller = new CardsController(detailService, listService, updateService);
    }

    // ===================================================================
    // GET /api/cards — List cards (CARD-BR-001)
    // COCRDLIC.cbl:1123-1411 — pagination and filtering
    // ===================================================================

    [Fact]
    public async Task ListCards_NoFiltersNoCursor_Returns200Ok()
    {
        // GIVEN 15 cards exist
        AddCards(15);

        // WHEN GET /api/cards
        var result = await _controller.ListCards(null, null, null, null, CancellationToken.None);

        // THEN 200 OK
        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task ListCards_NoFiltersNoCursor_ReturnsFirst7Cards()
    {
        // GIVEN 15 cards exist
        AddCards(15);

        // WHEN GET /api/cards
        var result = await _controller.ListCards(null, null, null, null, CancellationToken.None);

        // THEN first 7 cards returned with pagination metadata
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal(7, body.Cards.Count);
        Assert.True(body.HasNextPage);
        Assert.False(body.HasPreviousPage);
        Assert.NotNull(body.FirstCardNumber);
        Assert.NotNull(body.LastCardNumber);
    }

    [Fact]
    public async Task ListCards_WithAfterCursor_ReturnsNextPage()
    {
        // GIVEN 15 cards exist
        AddCards(15);

        // WHEN GET /api/cards?afterCardNumber=4000000000000007
        var result = await _controller.ListCards("4000000000000007", null, null, null, CancellationToken.None);

        // THEN next page of cards returned
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal(7, body.Cards.Count);
        Assert.True(body.HasPreviousPage);
    }

    [Fact]
    public async Task ListCards_WithBeforeCursor_ReturnsPreviousPage()
    {
        // GIVEN 15 cards exist
        AddCards(15);

        // WHEN GET /api/cards?beforeCardNumber=4000000000000008
        var result = await _controller.ListCards(null, "4000000000000008", null, null, CancellationToken.None);

        // THEN previous page of cards returned
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal(7, body.Cards.Count);
    }

    [Fact]
    public async Task ListCards_FilterByAccount_ReturnsOnlyMatchingCards()
    {
        // GIVEN cards for two accounts
        _cardRepo.AddCard(CreateTestCard("4000000000000001", "12345678901"));
        _cardRepo.AddCard(CreateTestCard("4000000000000002", "99999999999"));
        _cardRepo.AddCard(CreateTestCard("4000000000000003", "12345678901"));

        // WHEN GET /api/cards?accountId=12345678901
        var result = await _controller.ListCards(null, null, "12345678901", null, CancellationToken.None);

        // THEN only cards for that account
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal(2, body.Cards.Count);
        Assert.All(body.Cards, c => Assert.Equal("12345678901", c.AccountId));
    }

    [Fact]
    public async Task ListCards_FilterByBothAccountAndCard_ReturnsMatchingBoth()
    {
        // GIVEN cards with different accounts and numbers
        _cardRepo.AddCard(CreateTestCard("4000123456789012", "12345678901"));
        _cardRepo.AddCard(CreateTestCard("4000123456789099", "12345678901"));
        _cardRepo.AddCard(CreateTestCard("4000999999999999", "99999999999"));

        // WHEN GET /api/cards?accountId=12345678901&cardNumber=4000123456789012
        var result = await _controller.ListCards(null, null, "12345678901", "4000123456789012", CancellationToken.None);

        // THEN only card matching BOTH filters
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Single(body.Cards);
        Assert.Equal("4000123456789012", body.Cards[0].CardNumber);
        Assert.Equal("12345678901", body.Cards[0].AccountId);
    }

    [Fact]
    public async Task ListCards_NoMatchingRecords_ReturnsEmptyWithMessage()
    {
        // GIVEN no cards for account "99999999999"
        _cardRepo.AddCard(CreateTestCard("4000000000000001", "12345678901"));

        // WHEN GET /api/cards?accountId=99999999999
        var result = await _controller.ListCards(null, null, "99999999999", null, CancellationToken.None);

        // THEN empty array with message
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Empty(body.Cards);
        Assert.Equal("NO RECORDS FOUND FOR THIS SEARCH CONDITION", body.Message);
    }

    [Fact]
    public async Task ListCards_InvalidAccountFilter_Returns400()
    {
        // GIVEN invalid account filter "ABC"
        // WHEN GET /api/cards?accountId=ABC
        var result = await _controller.ListCards(null, null, "ABC", null, CancellationToken.None);

        // THEN 400 Bad Request with validation error
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    [Fact]
    public async Task ListCards_InvalidCardNumberFilter_Returns400()
    {
        // GIVEN invalid card number filter "XYZ"
        // WHEN GET /api/cards?cardNumber=XYZ
        var result = await _controller.ListCards(null, null, null, "XYZ", CancellationToken.None);

        // THEN 400 Bad Request
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    // ===================================================================
    // CARD-BR-002: Selection routing ('S' → GET detail, 'U' → PUT update)
    // These are documented through the existing GET/PUT endpoints.
    // 'S' maps to GET /api/cards/{cardNumber}
    // 'U' maps to PUT /api/cards/{cardNumber}
    // The list response provides cardNumber for URL construction.
    // ===================================================================

    [Fact]
    public async Task ListCards_ResponseContainsCardNumbersForBr002Routing()
    {
        // GIVEN cards exist (CARD-BR-002: list provides card numbers for selection)
        _cardRepo.AddCard(CreateTestCard("4000123456789012", "12345678901"));

        // WHEN GET /api/cards
        var result = await _controller.ListCards(null, null, null, null, CancellationToken.None);

        // THEN each card has cardNumber for 'S' → GET /api/cards/{cardNumber} routing
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardListResponse>(ok.Value);
        Assert.Equal("4000123456789012", body.Cards[0].CardNumber);
    }

    // ===================================================================
    // GET /api/cards/{cardNumber} — 200 OK (COCRDSLC.cbl:736-774)
    // ===================================================================

    [Fact]
    public async Task GetCard_ExistingCard_Returns200Ok()
    {
        // GIVEN card "4000123456789012" exists for account "12345678901"
        _cardRepo.AddCard(CreateTestCard());

        // WHEN GET /api/cards/4000123456789012
        var result = await _controller.GetCard("4000123456789012", CancellationToken.None);

        // THEN 200 OK
        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task GetCard_ExistingCard_ReturnsCardDetailWithAllFields()
    {
        // GIVEN card "4000123456789012" exists
        _cardRepo.AddCard(CreateTestCard());

        // WHEN GET /api/cards/4000123456789012
        var result = await _controller.GetCard("4000123456789012", CancellationToken.None);

        // THEN response contains cardNumber, accountId, embossedName, expirationDate, activeStatus
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardDetailResponse>(ok.Value);
        Assert.Equal("4000123456789012", body.CardNumber);
        Assert.Equal("12345678901", body.AccountId);
        Assert.Equal("JOHN DOE", body.EmbossedName);
        Assert.Equal("2027-12-31", body.ExpirationDate);
        Assert.Equal('Y', body.ActiveStatus);
    }

    [Fact]
    public async Task GetCard_ExistingCard_ExcludesCvvFromResponse()
    {
        // GIVEN card with CVV exists (PCI-DSS: CVV must not be in response)
        _cardRepo.AddCard(CreateTestCard());

        // WHEN GET /api/cards/4000123456789012
        var result = await _controller.GetCard("4000123456789012", CancellationToken.None);

        // THEN CvvCode is not present in response DTO
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardDetailResponse>(ok.Value);
        Assert.Null(body.GetType().GetProperty("CvvCode"));
    }

    // ===================================================================
    // GET /api/cards/{cardNumber} — 404 Not Found (DFHRESP(NOTFND))
    // ===================================================================

    [Fact]
    public async Task GetCard_NonExistentCard_Returns404NotFound()
    {
        // GIVEN no card with number "9999999999999999" exists
        // WHEN GET /api/cards/9999999999999999
        var result = await _controller.GetCard("9999999999999999", CancellationToken.None);

        // THEN 404 with "Did not find cards for this search condition"
        var notFound = Assert.IsType<NotFoundObjectResult>(result);
        var body = notFound.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Did not find cards for this search condition", message);
    }

    // ===================================================================
    // GET /api/cards/{cardNumber} — 400 Bad Request (validation errors)
    // COBOL: COCRDSLC.cbl:685-724 — 2220-EDIT-CARD
    // ===================================================================

    [Fact]
    public async Task GetCard_InvalidCardNumber_Returns400BadRequest()
    {
        // GIVEN invalid card number "ABC"
        // WHEN GET /api/cards/ABC
        var result = await _controller.GetCard("ABC", CancellationToken.None);

        // THEN 400 with validation error
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    [Fact]
    public async Task GetCard_ShortCardNumber_Returns400WithValidationMessage()
    {
        // GIVEN card number "12345" (not 16 digits)
        // WHEN GET /api/cards/12345
        var result = await _controller.GetCard("12345", CancellationToken.None);

        // THEN 400 with "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", message);
    }

    [Fact]
    public async Task GetCard_AllZerosCardNumber_Returns400()
    {
        // GIVEN all-zeros card number (COBOL: treated as blank)
        // WHEN GET /api/cards/0000000000000000
        var result = await _controller.GetCard("0000000000000000", CancellationToken.None);

        // THEN 400 — all zeros treated as blank per COBOL logic
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Card number not provided", message);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private void AddCards(int count)
    {
        for (int i = 1; i <= count; i++)
        {
            _cardRepo.AddCard(CreateTestCard(
                $"4000000000000{i:D3}",
                "12345678901"));
        }
    }

    private static Card CreateTestCard(
        string cardNumber = "4000123456789012",
        string accountId = "12345678901") => new()
        {
            CardNumber = cardNumber,
            AccountId = accountId,
            CvvCode = "123",
            EmbossedName = "JOHN DOE",
            ExpirationDate = new DateOnly(2027, 12, 31),
            ActiveStatus = 'Y',
            RowVersion = [1, 2, 3, 4, 5, 6, 7, 8]
        };

    /// <summary>
    /// In-memory stub for ICardRepository with keyset pagination support.
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
