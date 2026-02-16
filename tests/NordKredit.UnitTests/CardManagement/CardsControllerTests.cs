using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardsController HTTP response mapping.
/// COBOL source: COCRDSLC.cbl:608-812 — card detail lookup.
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access), FFFS 2014:5 Ch. 8 section 4.
/// </summary>
public class CardsControllerTests
{
    private readonly StubCardRepository _cardRepo = new();
    private readonly CardsController _controller;

    public CardsControllerTests()
    {
        var detailService = new CardDetailService(_cardRepo);
        _controller = new CardsController(detailService);
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
    // GET /api/cards?accountId={id} — 200 OK (COCRDSLC.cbl:779-812)
    // ===================================================================

    [Fact]
    public async Task GetCardByAccount_ExistingAccount_Returns200Ok()
    {
        // GIVEN account "12345678901" has cards
        _cardRepo.AddCard(CreateTestCard());

        // WHEN GET /api/cards?accountId=12345678901
        var result = await _controller.GetCardByAccount("12345678901", CancellationToken.None);

        // THEN 200 OK with card detail
        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task GetCardByAccount_MultipleCards_ReturnsFirstByKeyOrder()
    {
        // GIVEN account "12345678901" has multiple cards
        _cardRepo.AddCard(CreateTestCard("4000123456789012", "12345678901"));
        _cardRepo.AddCard(CreateTestCard("4000123456789099", "12345678901"));

        // WHEN GET /api/cards?accountId=12345678901
        var result = await _controller.GetCardByAccount("12345678901", CancellationToken.None);

        // THEN first card by key order is returned (COBOL: CICS READ returns first match)
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardDetailResponse>(ok.Value);
        Assert.Equal("4000123456789012", body.CardNumber);
    }

    // ===================================================================
    // GET /api/cards?accountId={id} — 404 Not Found
    // ===================================================================

    [Fact]
    public async Task GetCardByAccount_NoCardsForAccount_Returns404()
    {
        // GIVEN no cards for account "99999999999"
        // WHEN GET /api/cards?accountId=99999999999
        var result = await _controller.GetCardByAccount("99999999999", CancellationToken.None);

        // THEN 404 with "Did not find cards for this search condition"
        var notFound = Assert.IsType<NotFoundObjectResult>(result);
        var body = notFound.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Did not find cards for this search condition", message);
    }

    // ===================================================================
    // GET /api/cards?accountId={id} — 400 Bad Request (validation errors)
    // COBOL: COCRDSLC.cbl:647-683 — 2210-EDIT-ACCOUNT
    // ===================================================================

    [Fact]
    public async Task GetCardByAccount_InvalidAccountId_Returns400()
    {
        // GIVEN invalid account ID "ABC12345678"
        // WHEN GET /api/cards?accountId=ABC12345678
        var result = await _controller.GetCardByAccount("ABC12345678", CancellationToken.None);

        // THEN 400 with "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", message);
    }

    [Fact]
    public async Task GetCardByAccount_MissingAccountId_Returns400()
    {
        // GIVEN no accountId parameter
        // WHEN GET /api/cards
        var result = await _controller.GetCardByAccount(null, CancellationToken.None);

        // THEN 400 with "Account number not provided"
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Account number not provided", message);
    }

    [Fact]
    public async Task GetCardByAccount_EmptyAccountId_Returns400()
    {
        // GIVEN empty accountId
        // WHEN GET /api/cards?accountId=
        var result = await _controller.GetCardByAccount("", CancellationToken.None);

        // THEN 400 with "Account number not provided"
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Account number not provided", message);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

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
    /// In-memory stub for ICardRepository.
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

        public Task<IReadOnlyList<Card>> GetPageForwardAsync(int pageSize, string? startAfterCardNumber = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Card>>([]);

        public Task<IReadOnlyList<Card>> GetPageBackwardAsync(int pageSize, string? startBeforeCardNumber = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Card>>([]);

        public Task UpdateAsync(Card card, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }
}
