using System.Globalization;
using NordKredit.Domain.CardManagement;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.CardManagement;

/// <summary>
/// Step definitions for card detail BDD scenarios (CARD-BR-003, CARD-BR-009).
/// COBOL source: COCRDSLC.cbl:608-812 (card detail lookup — primary key and alternate index).
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).
/// </summary>
[Binding]
[Scope(Feature = "Card detail lookup by card number or account")]
public sealed class CardDetailStepDefinitions
{
    private readonly StubCardRepository _cardRepo = new();
    private CardDetailService _service = null!;
    private CardDetailResponse? _response;

    [BeforeScenario]
    public void SetUp() =>
        _service = new CardDetailService(_cardRepo);

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
                CvvCode = "123",
                RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
            });
        }
    }

    [Given(@"the card ""(.*)"" has CVV code ""(.*)""")]
    public void GivenTheCardHasCvvCode(string cardNumber, string cvvCode)
    {
        var card = _cardRepo.GetByCardNumberAsync(cardNumber).Result;
        if (card is not null)
        {
            card.CvvCode = cvvCode;
        }
    }

    [When(@"I request card detail for card number ""(.*)""")]
    public async Task WhenIRequestCardDetailForCardNumber(string cardNumber) =>
        _response = await _service.GetByCardNumberAsync(cardNumber);

    [When(@"I request card detail for account ID ""(.*)""")]
    public async Task WhenIRequestCardDetailForAccountId(string accountId) =>
        _response = await _service.GetByAccountIdAsync(accountId);

    [Then(@"the card detail response contains card number ""(.*)""")]
    public void ThenTheCardDetailResponseContainsCardNumber(string expectedCardNumber)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedCardNumber, _response.CardNumber);
    }

    [Then(@"the card detail response contains account ID ""(.*)""")]
    public void ThenTheCardDetailResponseContainsAccountId(string expectedAccountId)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedAccountId, _response.AccountId);
    }

    [Then(@"the card detail response contains embossed name ""(.*)""")]
    public void ThenTheCardDetailResponseContainsEmbossedName(string expectedName)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedName, _response.EmbossedName);
    }

    [Then(@"the card detail response contains expiration date ""(.*)""")]
    public void ThenTheCardDetailResponseContainsExpirationDate(string expectedDate)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedDate, _response.ExpirationDate);
    }

    [Then(@"the card detail response contains active status '(.*)'")]
    public void ThenTheCardDetailResponseContainsActiveStatus(char expectedStatus)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedStatus, _response.ActiveStatus);
    }

    [Then(@"the card detail response is not null")]
    public void ThenTheCardDetailResponseIsNotNull() =>
        Assert.NotNull(_response);

    [Then(@"the card detail response is null")]
    public void ThenTheCardDetailResponseIsNull() =>
        Assert.Null(_response);

    [Then(@"the card detail response does not contain a CVV code")]
    public void ThenTheCardDetailResponseDoesNotContainACvvCode()
    {
        // CardDetailResponse has no CVV field by design (PCI-DSS compliance).
        Assert.NotNull(_response);
        var properties = typeof(CardDetailResponse).GetProperties();
        Assert.DoesNotContain(properties, p => p.Name == "CvvCode");
    }

    /// <summary>
    /// In-memory stub repository for card detail BDD scenarios.
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

        public Task<IReadOnlyList<Card>> GetPageForwardAsync(int pageSize, string? startAfterCardNumber = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Card>>([]);

        public Task<IReadOnlyList<Card>> GetPageBackwardAsync(int pageSize, string? startBeforeCardNumber = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Card>>([]);

        public Task UpdateAsync(Card card, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }
}
