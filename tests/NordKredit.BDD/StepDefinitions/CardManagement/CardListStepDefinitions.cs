using System.Globalization;
using NordKredit.Domain.CardManagement;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.CardManagement;

/// <summary>
/// Step definitions for card list BDD scenarios (CARD-BR-001).
/// COBOL source: COCRDLIC.cbl:1123-1411 (PROCESS-PAGE-FORWARD/BACKWARD, FILTER-RECORDS).
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).
/// </summary>
[Binding]
[Scope(Feature = "Card list display with pagination and filtering")]
public sealed class CardListStepDefinitions
{
    private readonly StubCardRepository _cardRepo = new();
    private CardListService _service = null!;
    private CardListResponse _response = null!;

    [BeforeScenario]
    public void SetUp() =>
        _service = new CardListService(_cardRepo);

    [Given(@"the card repository contains the following cards")]
    public void GivenTheCardRepositoryContainsTheFollowingCards(Table table)
    {
        foreach (var row in table.Rows)
        {
            _cardRepo.AddCard(new Card
            {
                CardNumber = row["CardNumber"],
                AccountId = row["AccountId"],
                EmbossedName = row["EmbossedName"],
                ExpirationDate = DateOnly.Parse(row["ExpirationDate"], CultureInfo.InvariantCulture),
                ActiveStatus = row["ActiveStatus"][0],
                CvvCode = "000",
                RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
            });
        }
    }

    [When(@"I request the first page of cards without filters")]
    public async Task WhenIRequestTheFirstPageOfCardsWithoutFilters() =>
        _response = await _service.GetCardsForwardAsync();

    [When(@"I request the next page using the last card number as cursor")]
    public async Task WhenIRequestTheNextPageUsingTheLastCardNumberAsCursor()
    {
        var cursor = _response.LastCardNumber;
        _response = await _service.GetCardsForwardAsync(afterCardNumber: cursor);
    }

    [When(@"I request the previous page using the first card number as cursor")]
    public async Task WhenIRequestThePreviousPageUsingTheFirstCardNumberAsCursor()
    {
        var cursor = _response.FirstCardNumber;
        _response = await _service.GetCardsBackwardAsync(beforeCardNumber: cursor!);
    }

    [When(@"I request cards filtered by account ID ""([^""]*)""$")]
    public async Task WhenIRequestCardsFilteredByAccountId(string accountId) =>
        _response = await _service.GetCardsForwardAsync(accountId: accountId);

    [When(@"I request cards filtered by card number ""([^""]*)""$")]
    public async Task WhenIRequestCardsFilteredByCardNumber(string cardNumber) =>
        _response = await _service.GetCardsForwardAsync(cardNumber: cardNumber);

    [When(@"I request cards filtered by account ID ""([^""]*)"" and card number ""([^""]*)""")]
    public async Task WhenIRequestCardsFilteredByAccountIdAndCardNumber(string accountId, string cardNumber) =>
        _response = await _service.GetCardsForwardAsync(accountId: accountId, cardNumber: cardNumber);

    [Then(@"the response contains (\d+) cards?")]
    public void ThenTheResponseContainsNCards(int expectedCount) =>
        Assert.Equal(expectedCount, _response.Cards.Count);

    [Then(@"the response indicates a next page exists")]
    public void ThenTheResponseIndicatesANextPageExists() =>
        Assert.True(_response.HasNextPage);

    [Then(@"the response indicates no next page exists")]
    public void ThenTheResponseIndicatesNoNextPageExists() =>
        Assert.False(_response.HasNextPage);

    [Then(@"the response indicates a previous page exists")]
    public void ThenTheResponseIndicatesAPreviousPageExists() =>
        Assert.True(_response.HasPreviousPage);

    [Then(@"the response indicates no previous page exists")]
    public void ThenTheResponseIndicatesNoPreviousPageExists() =>
        Assert.False(_response.HasPreviousPage);

    [Then(@"the response contains card ""(.*)""")]
    public void ThenTheResponseContainsCard(string cardNumber) =>
        Assert.Contains(_response.Cards, c => c.CardNumber == cardNumber);

    [Then(@"the response message is ""(.*)""")]
    public void ThenTheResponseMessageIs(string expectedMessage) =>
        Assert.Equal(expectedMessage, _response.Message);

    /// <summary>
    /// In-memory stub repository for BDD scenarios.
    /// Matches COBOL VSAM CARDDAT + CARDAIX behavior.
    /// </summary>
    internal sealed class StubCardRepository : ICardRepository
    {
        private readonly List<Card> _cards = [];

        public void AddCard(Card card) => _cards.Add(card);

        public Task<Card?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default) =>
            Task.FromResult(_cards.FirstOrDefault(c => c.CardNumber == cardNumber));

        public Task<IReadOnlyList<Card>> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
        {
            IReadOnlyList<Card> result = [.. _cards
                .Where(c => c.AccountId == accountId)
                .OrderBy(c => c.CardNumber)];
            return Task.FromResult(result);
        }

        public Task<IReadOnlyList<Card>> GetPageForwardAsync(int pageSize, string? startAfterCardNumber = null, CancellationToken cancellationToken = default)
        {
            var query = _cards.OrderBy(c => c.CardNumber).AsEnumerable();
            if (startAfterCardNumber is not null)
            {
                query = query.Where(c => string.Compare(c.CardNumber, startAfterCardNumber, StringComparison.Ordinal) > 0);
            }

            IReadOnlyList<Card> result = [.. query.Take(pageSize)];
            return Task.FromResult(result);
        }

        public Task<IReadOnlyList<Card>> GetPageBackwardAsync(int pageSize, string? startBeforeCardNumber = null, CancellationToken cancellationToken = default)
        {
            var query = _cards.OrderByDescending(c => c.CardNumber).AsEnumerable();
            if (startBeforeCardNumber is not null)
            {
                query = query.Where(c => string.Compare(c.CardNumber, startBeforeCardNumber, StringComparison.Ordinal) < 0);
            }

            IReadOnlyList<Card> result = [.. query.Take(pageSize)];
            return Task.FromResult(result);
        }

        public Task UpdateAsync(Card card, CancellationToken cancellationToken = default)
        {
            var existing = _cards.FindIndex(c => c.CardNumber == card.CardNumber);
            if (existing >= 0)
            {
                _cards[existing] = card;
            }

            return Task.CompletedTask;
        }
    }
}
