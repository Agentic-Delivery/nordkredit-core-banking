using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardsController PUT endpoint — card update with state machine and concurrency.
/// COBOL source: COCRDUPC.cbl:275-290 (state machine), 1420-1523 (write processing + concurrency).
/// Business rules: CARD-BR-007 (state machine), CARD-BR-008 (optimistic concurrency).
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 §4 (operational risk management).
/// </summary>
public class CardsControllerUpdateTests
{
    private readonly StubCardRepository _cardRepo = new();
    private readonly CardsController _controller;

    public CardsControllerUpdateTests()
    {
        var detailService = new CardDetailService(_cardRepo);
        var listService = new CardListService(_cardRepo);
        var updateService = new CardUpdateService(_cardRepo);
        _controller = new CardsController(detailService, listService, updateService);
    }

    // ===================================================================
    // PUT /api/cards/{cardNumber} — 200 OK: Successful update
    // COBOL: CHANGES-OKAYED-AND-DONE (COCRDUPC.cbl:998-1000)
    // ===================================================================

    [Fact]
    public async Task UpdateCard_ValidChanges_Returns200OkWithUpdatedCard()
    {
        // GIVEN card "4000123456789012" exists with name "JOHN DOE"
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        // WHEN PUT with valid ETag
        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN 200 OK with updated card data and new ETag
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardUpdateResponse>(ok.Value);
        Assert.Equal("JANE DOE", body.EmbossedName);
        Assert.Equal("4000123456789012", body.CardNumber);
    }

    [Fact]
    public async Task UpdateCard_ValidChanges_ReturnsETagInResponse()
    {
        // GIVEN card exists
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        // WHEN PUT with valid ETag
        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN response includes ETag
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<CardUpdateResponse>(ok.Value);
        Assert.NotNull(body.ETag);
        Assert.NotEmpty(body.ETag);
    }

    // ===================================================================
    // PUT /api/cards/{cardNumber} — 200 OK: No change detected
    // COBOL: SHOW-DETAILS with "No change detected" (COCRDUPC.cbl 1200-EDIT-MAP-INPUTS)
    // ===================================================================

    [Fact]
    public async Task UpdateCard_NoChanges_Returns200WithNoChangeMessage()
    {
        // GIVEN card with name "JOHN DOE"
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        // WHEN PUT with identical values (case-insensitive)
        var request = new CardUpdateRequest
        {
            EmbossedName = "john doe",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN 200 with "No change detected with respect to values fetched."
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = ok.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("No change detected with respect to values fetched.", message);
    }

    // ===================================================================
    // PUT /api/cards/{cardNumber} — 400 Bad Request: Validation errors
    // COBOL: CHANGES-NOT-OK (COCRDUPC.cbl:806-947)
    // ===================================================================

    [Fact]
    public async Task UpdateCard_InvalidFields_Returns400WithAllErrors()
    {
        // GIVEN card exists
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        // WHEN PUT with invalid name (numbers)
        var request = new CardUpdateRequest
        {
            EmbossedName = "JOHN123",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN 400 with validation errors
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var errors = body.GetType().GetProperty("Errors")?.GetValue(body) as IReadOnlyList<string>;
        Assert.NotNull(errors);
        Assert.Contains("Card name can only contain alphabets and spaces", errors);
    }

    [Fact]
    public async Task UpdateCard_InvalidCardNumber_Returns400()
    {
        // GIVEN invalid card number
        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("ABC", request, CancellationToken.None);

        // THEN 400 with card number validation error
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", message);
    }

    // ===================================================================
    // PUT /api/cards/{cardNumber} — 404 Not Found
    // ===================================================================

    [Fact]
    public async Task UpdateCard_CardNotFound_Returns404()
    {
        // GIVEN no card exists
        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("9999999999999999", request, CancellationToken.None);

        // THEN 404 Not Found
        Assert.IsType<NotFoundObjectResult>(result);
    }

    // ===================================================================
    // PUT /api/cards/{cardNumber} — 409 Conflict: Concurrency conflict
    // COBOL: DATA-WAS-CHANGED-BEFORE-UPDATE (COCRDUPC.cbl:1498-1523)
    // ===================================================================

    [Fact]
    public async Task UpdateCard_ConcurrencyConflict_Returns409WithRefreshedData()
    {
        // GIVEN card exists and another user subsequently updates it
        var card = CreateTestCard();
        _cardRepo.AddCard(card);
        _cardRepo.ThrowConcurrencyExceptionOnNextUpdate();

        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        // WHEN PUT with stale ETag
        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN 409 Conflict with "Record changed by some one else. Please review"
        var conflict = Assert.IsType<ConflictObjectResult>(result);
        var body = conflict.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Record changed by some one else. Please review", message);
    }

    [Fact]
    public async Task UpdateCard_ConcurrencyConflict_ReturnsRefreshedCardDataWithETag()
    {
        // GIVEN card exists with concurrency conflict
        var card = CreateTestCard();
        _cardRepo.AddCard(card);
        _cardRepo.ThrowConcurrencyExceptionOnNextUpdate();

        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        // WHEN PUT with stale ETag
        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN response body contains refreshed card data with new ETag
        var conflict = Assert.IsType<ConflictObjectResult>(result);
        var body = conflict.Value;
        Assert.NotNull(body);
        var cardData = body.GetType().GetProperty("Card")?.GetValue(body);
        Assert.NotNull(cardData);
    }

    // ===================================================================
    // PUT /api/cards/{cardNumber} — 428 Precondition Required: Missing ETag
    // ===================================================================

    [Fact]
    public async Task UpdateCard_MissingIfMatchHeader_Returns428PreconditionRequired()
    {
        // GIVEN PUT without If-Match header
        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        // WHEN PUT without If-Match header (no header set)
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN 428 Precondition Required
        var statusResult = Assert.IsType<ObjectResult>(result);
        Assert.Equal(428, statusResult.StatusCode);
        var body = statusResult.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("ETag required for update", message);
    }

    // ===================================================================
    // PUT /api/cards/{cardNumber} — 500: Write failure
    // COBOL: LOCKED-BUT-UPDATE-FAILED (COCRDUPC.cbl:993-996)
    // ===================================================================

    [Fact]
    public async Task UpdateCard_WriteFailure_Returns500()
    {
        // GIVEN card exists but write will fail
        var card = CreateTestCard();
        _cardRepo.AddCard(card);
        _cardRepo.ThrowDbUpdateExceptionOnNextUpdate();

        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        SetIfMatchHeader("AQIDBAUGB wg=");
        var result = await _controller.UpdateCard("4000123456789012", request, CancellationToken.None);

        // THEN 500 with "Update of record failed"
        var statusResult = Assert.IsType<ObjectResult>(result);
        Assert.Equal(500, statusResult.StatusCode);
        var body = statusResult.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Update of record failed", message);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private void SetIfMatchHeader(string etagValue)
    {
        _controller.ControllerContext = new ControllerContext
        {
            HttpContext = new DefaultHttpContext()
        };
        _controller.Request.Headers.IfMatch = etagValue;
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
    /// In-memory stub for ICardRepository with concurrency simulation.
    /// </summary>
    internal sealed class StubCardRepository : ICardRepository
    {
        private readonly List<Card> _cards = [];
        private bool _throwConcurrencyException;
        private bool _throwDbUpdateException;

        public void AddCard(Card card) => _cards.Add(card);

        public void ThrowConcurrencyExceptionOnNextUpdate() => _throwConcurrencyException = true;

        public void ThrowDbUpdateExceptionOnNextUpdate() => _throwDbUpdateException = true;

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

        public Task UpdateAsync(Card card, CancellationToken cancellationToken = default)
        {
            if (_throwConcurrencyException)
            {
                _throwConcurrencyException = false;
                throw new ConcurrencyConflictException("Simulated concurrency conflict");
            }

            if (_throwDbUpdateException)
            {
                _throwDbUpdateException = false;
                throw new InvalidOperationException("Simulated write failure");
            }

            return Task.CompletedTask;
        }
    }
}
