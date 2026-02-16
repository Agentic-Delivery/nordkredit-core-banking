using NordKredit.Domain.CardManagement;

namespace NordKredit.UnitTests.CardManagement;

/// <summary>
/// Unit tests for CardDetailService — card detail lookup by card number or account ID.
/// COBOL source: COCRDSLC.cbl:608-812 (card detail lookup logic).
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).
/// </summary>
public class CardDetailServiceTests
{
    private readonly StubCardRepository _cardRepo = new();
    private readonly CardDetailService _service;

    public CardDetailServiceTests()
    {
        _service = new CardDetailService(_cardRepo);
    }

    // ===================================================================
    // GetByCardNumberAsync — Primary key lookup (CICS READ on CARDDAT)
    // COBOL: COCRDSLC.cbl:736-774 — 9100-GETCARD-BYACCTCARD
    // ===================================================================

    [Fact]
    public async Task GetByCardNumber_ExistingCard_ReturnsCardDetail()
    {
        // GIVEN card "4000123456789012" exists
        _cardRepo.AddCard(CreateTestCard());

        // WHEN lookup by card number
        var result = await _service.GetByCardNumberAsync("4000123456789012");

        // THEN card detail is returned
        Assert.NotNull(result);
        Assert.Equal("4000123456789012", result.CardNumber);
    }

    [Fact]
    public async Task GetByCardNumber_ExistingCard_ReturnsAllFields()
    {
        // GIVEN card "4000123456789012" exists with all fields populated
        _cardRepo.AddCard(CreateTestCard());

        // WHEN lookup by card number
        var result = await _service.GetByCardNumberAsync("4000123456789012");

        // THEN all fields are populated correctly
        Assert.NotNull(result);
        Assert.Equal("4000123456789012", result.CardNumber);
        Assert.Equal("12345678901", result.AccountId);
        Assert.Equal("JOHN DOE", result.EmbossedName);
        Assert.Equal("2027-12-31", result.ExpirationDate);
        Assert.Equal('Y', result.ActiveStatus);
    }

    [Fact]
    public async Task GetByCardNumber_ExistingCard_ExcludesCvvCode()
    {
        // GIVEN card with CVV "123" exists (PCI-DSS: CVV must not be returned)
        _cardRepo.AddCard(CreateTestCard());

        // WHEN lookup by card number
        var result = await _service.GetByCardNumberAsync("4000123456789012");

        // THEN response type does not have a CvvCode property
        Assert.NotNull(result);
        var cvvProperty = result.GetType().GetProperty("CvvCode");
        Assert.Null(cvvProperty);
    }

    [Fact]
    public async Task GetByCardNumber_NonExistentCard_ReturnsNull()
    {
        // GIVEN no card with number "9999999999999999" exists
        // WHEN lookup by card number
        var result = await _service.GetByCardNumberAsync("9999999999999999");

        // THEN null is returned (COBOL: DFHRESP(NOTFND))
        Assert.Null(result);
    }

    [Fact]
    public async Task GetByCardNumber_ExpirationDateFormat_IsYYYYMMDD()
    {
        // GIVEN card with expiration date 2027-12-31
        _cardRepo.AddCard(CreateTestCard());

        // WHEN lookup by card number
        var result = await _service.GetByCardNumberAsync("4000123456789012");

        // THEN expiration date is in YYYY-MM-DD format (COBOL: CARD-EXPIRAION-DATE PIC X(10))
        Assert.NotNull(result);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", result.ExpirationDate);
    }

    // ===================================================================
    // GetByAccountIdAsync — Alternate index lookup (CICS READ on CARDAIX)
    // COBOL: COCRDSLC.cbl:779-812 — 9150-GETCARD-BYACCT
    // ===================================================================

    [Fact]
    public async Task GetByAccountId_AccountWithCards_ReturnsFirstCard()
    {
        // GIVEN account "12345678901" has cards
        _cardRepo.AddCard(CreateTestCard());
        _cardRepo.AddCard(CreateTestCard("4000123456789099", "12345678901"));

        // WHEN lookup by account ID
        var result = await _service.GetByAccountIdAsync("12345678901");

        // THEN first card by key order is returned (COBOL: CICS READ returns first match)
        Assert.NotNull(result);
        Assert.Equal("12345678901", result.AccountId);
    }

    [Fact]
    public async Task GetByAccountId_NoCardsForAccount_ReturnsNull()
    {
        // GIVEN no cards for account "99999999999"
        // WHEN lookup by account ID
        var result = await _service.GetByAccountIdAsync("99999999999");

        // THEN null is returned (COBOL: DFHRESP(NOTFND))
        Assert.Null(result);
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
