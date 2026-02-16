using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardUpdateService — the card update workflow state machine.
/// COBOL source: COCRDUPC.cbl:275-290 (state machine), 429-543 (main EVALUATE),
///               948-1027 (action decisions), 1420-1523 (write processing + concurrency).
/// Business rules: CARD-BR-007 (state machine), CARD-BR-008 (optimistic concurrency).
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 §4 (operational risk management).
/// </summary>
public class CardUpdateServiceTests
{
    private readonly MockCardRepository _cardRepo = new();
    private readonly CardUpdateService _service;

    public CardUpdateServiceTests()
    {
        _service = new CardUpdateService(_cardRepo);
    }

    // ===================================================================
    // CARD-BR-007: Successful update (CHANGES-OKAYED-AND-DONE)
    // COBOL: COCRDUPC.cbl:998-1000
    // ===================================================================

    [Fact]
    public async Task UpdateCard_ValidChanges_ReturnsSuccess()
    {
        // GIVEN card "4000123456789012" exists with name "JOHN DOE"
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        // WHEN PUT with embossedName "JANE DOE" and valid rowVersion
        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN success with updated card data
        Assert.True(result.IsSuccess);
        Assert.NotNull(result.Card);
        Assert.Equal("JANE DOE", result.Card.EmbossedName);
    }

    [Fact]
    public async Task UpdateCard_ValidChanges_UpdatesAllFields()
    {
        // GIVEN card exists
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        // WHEN updating name, status, and expiry
        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'N',
            ExpiryMonth = 6,
            ExpiryYear = 2028
        };

        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN all fields updated (expiry day clamped from 31 to 30 for June)
        Assert.True(result.IsSuccess);
        Assert.Equal("JANE DOE", result.Card!.EmbossedName);
        Assert.Equal('N', result.Card.ActiveStatus);
        Assert.Equal(new DateOnly(2028, 6, 30), result.Card.ExpirationDate);
    }

    [Fact]
    public async Task UpdateCard_ValidChanges_CarriesExpiryDayFromOriginal()
    {
        // GIVEN card with expiry day = 15
        var card = CreateTestCard();
        card.ExpirationDate = new DateOnly(2027, 12, 15);
        _cardRepo.AddCard(card);

        // WHEN updating expiry month and year
        var request = new CardUpdateRequest
        {
            EmbossedName = "JOHN DOE UPDATED",
            ActiveStatus = 'Y',
            ExpiryMonth = 6,
            ExpiryYear = 2028
        };

        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN expiry day is carried from original record (15), not reset
        Assert.True(result.IsSuccess);
        Assert.Equal(15, result.Card!.ExpirationDate.Day);
    }

    // ===================================================================
    // CARD-BR-007: No change detected (SHOW-DETAILS with message)
    // COBOL: COCRDUPC.cbl 1200-EDIT-MAP-INPUTS
    // ===================================================================

    [Fact]
    public async Task UpdateCard_NoChangesDetected_ReturnsNoChangeWithMessage()
    {
        // GIVEN card "4000123456789012" with name "JOHN DOE"
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        // WHEN PUT with identical values (case-insensitive for name)
        var request = new CardUpdateRequest
        {
            EmbossedName = "john doe",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN no-change result with message
        Assert.True(result.IsNoChange);
        Assert.Equal("No change detected with respect to values fetched.", result.Message);
        Assert.NotNull(result.Card);
    }

    // ===================================================================
    // CARD-BR-007: Validation failure (CHANGES-NOT-OK)
    // COBOL: COCRDUPC.cbl:806-947
    // ===================================================================

    [Fact]
    public async Task UpdateCard_InvalidFields_ReturnsValidationFailure()
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

        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN validation failure with errors
        Assert.True(result.IsValidationFailure);
        Assert.Contains("Card name can only contain alphabets and spaces", result.ValidationErrors);
    }

    [Fact]
    public async Task UpdateCard_MultipleInvalidFields_ReturnsAllErrors()
    {
        // GIVEN card exists
        var card = CreateTestCard();
        _cardRepo.AddCard(card);

        // WHEN PUT with multiple invalid fields
        var request = new CardUpdateRequest
        {
            EmbossedName = "JOHN123",
            ActiveStatus = 'X',
            ExpiryMonth = 13,
            ExpiryYear = 2100
        };

        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN all errors returned (not just first)
        Assert.True(result.IsValidationFailure);
        Assert.Equal(4, result.ValidationErrors.Count);
    }

    // ===================================================================
    // CARD-BR-007: Card not found
    // ===================================================================

    [Fact]
    public async Task UpdateCard_CardNotFound_ReturnsNotFound()
    {
        // GIVEN no card with number "9999999999999999" exists
        var request = new CardUpdateRequest
        {
            EmbossedName = "JANE DOE",
            ActiveStatus = 'Y',
            ExpiryMonth = 12,
            ExpiryYear = 2027
        };

        var result = await _service.UpdateCardAsync(
            "9999999999999999", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN not found
        Assert.True(result.IsNotFound);
    }

    // ===================================================================
    // CARD-BR-008: Concurrency conflict (DATA-WAS-CHANGED-BEFORE-UPDATE)
    // COBOL: COCRDUPC.cbl:1498-1523
    // ===================================================================

    [Fact]
    public async Task UpdateCard_ConcurrencyConflict_ReturnsConflictWithRefreshedData()
    {
        // GIVEN card exists and another user subsequently modifies it
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
        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN 409 Conflict with refreshed card data
        Assert.True(result.IsConflict);
        Assert.Equal("Record changed by some one else. Please review", result.Message);
        Assert.NotNull(result.Card);
    }

    // ===================================================================
    // CARD-BR-008: Write failure (LOCKED-BUT-UPDATE-FAILED)
    // COBOL: COCRDUPC.cbl:993-996
    // ===================================================================

    [Fact]
    public async Task UpdateCard_WriteFailure_ReturnsWriteFailure()
    {
        // GIVEN card exists but database write will fail
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

        // WHEN PUT
        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, [1, 2, 3, 4, 5, 6, 7, 8], CancellationToken.None);

        // THEN write failure
        Assert.True(result.IsWriteFailure);
        Assert.Equal("Update of record failed", result.Message);
    }

    // ===================================================================
    // CARD-BR-008: Rowversion is set before save
    // ===================================================================

    [Fact]
    public async Task UpdateCard_SetsRowVersionFromETagBeforeSave()
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

        byte[] etag = [10, 20, 30, 40, 50, 60, 70, 80];

        // WHEN PUT with specific ETag
        var result = await _service.UpdateCardAsync(
            "4000123456789012", request, etag, CancellationToken.None);

        // THEN rowVersion is set to the provided ETag value before save
        Assert.True(result.IsSuccess);
        Assert.Equal(etag, _cardRepo.LastUpdatedCard!.RowVersion);
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
    /// Mock repository for testing CardUpdateService with concurrency control.
    /// </summary>
    private sealed class MockCardRepository : ICardRepository
    {
        private readonly List<Card> _cards = [];
        private bool _throwConcurrencyException;
        private bool _throwDbUpdateException;

        public Card? LastUpdatedCard { get; private set; }

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
            LastUpdatedCard = card;

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
