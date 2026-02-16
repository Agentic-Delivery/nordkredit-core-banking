using System.Globalization;
using NordKredit.Domain.CardManagement;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.CardManagement;

/// <summary>
/// Step definitions for card update BDD scenarios (CARD-BR-006, CARD-BR-007, CARD-BR-008).
/// COBOL source: COCRDUPC.cbl:275-290 (state machine), 429-543 (main EVALUATE),
///               806-947 (field validation), 1420-1523 (write processing + concurrency).
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 §4 (operational risk).
/// </summary>
[Binding]
[Scope(Feature = "Card update workflow with state machine and optimistic concurrency")]
public sealed class CardUpdateStepDefinitions
{
    private readonly StubCardRepository _cardRepo = new();
    private CardUpdateService _service = null!;
    private CardUpdateRequest _request = null!;
    private string _cardNumber = string.Empty;
    private byte[] _rowVersion = [];
    private CardUpdateResult _result = null!;

    [BeforeScenario]
    public void SetUp() =>
        _service = new CardUpdateService(_cardRepo);

    [Given(@"the card repository contains a card with")]
    public void GivenTheCardRepositoryContainsACardWith(Table table)
    {
        var row = table.Rows[0];
        _cardRepo.AddCard(new Card
        {
            CardNumber = row["CardNumber"],
            AccountId = row["AccountId"],
            EmbossedName = row["EmbossedName"],
            ExpirationDate = DateOnly.Parse(row["ExpirationDate"], CultureInfo.InvariantCulture),
            ActiveStatus = row["ActiveStatus"][0],
            CvvCode = "123",
            RowVersion = Convert.FromBase64String(row["RowVersion"])
        });
    }

    [Given(@"the card repository will throw a concurrency conflict on update")]
    public void GivenTheCardRepositoryWillThrowAConcurrencyConflictOnUpdate() =>
        _cardRepo.ThrowOnUpdate = StubCardRepository.UpdateFailureMode.ConcurrencyConflict;

    [Given(@"the card repository will throw a write failure on update")]
    public void GivenTheCardRepositoryWillThrowAWriteFailureOnUpdate() =>
        _cardRepo.ThrowOnUpdate = StubCardRepository.UpdateFailureMode.WriteFailure;

    [When(@"I update card ""(.*)"" with")]
    public void WhenIUpdateCardWith(string cardNumber, Table table)
    {
        var row = table.Rows[0];
        _cardNumber = cardNumber;
        _request = new CardUpdateRequest
        {
            EmbossedName = row["EmbossedName"],
            ActiveStatus = row["ActiveStatus"][0],
            ExpiryMonth = int.Parse(row["ExpiryMonth"], CultureInfo.InvariantCulture),
            ExpiryYear = int.Parse(row["ExpiryYear"], CultureInfo.InvariantCulture)
        };
    }

    [When(@"I provide the row version ""(.*)""")]
    public async Task WhenIProvideTheRowVersion(string base64RowVersion)
    {
        _rowVersion = Convert.FromBase64String(base64RowVersion);
        _result = await _service.UpdateCardAsync(
            _cardNumber,
            _request,
            _rowVersion);
    }

    [Then(@"the update result is success")]
    public void ThenTheUpdateResultIsSuccess() =>
        Assert.True(_result.IsSuccess);

    [Then(@"the update result is no change")]
    public void ThenTheUpdateResultIsNoChange() =>
        Assert.True(_result.IsNoChange);

    [Then(@"the update result is validation failure")]
    public void ThenTheUpdateResultIsValidationFailure() =>
        Assert.True(_result.IsValidationFailure);

    [Then(@"the update result is not found")]
    public void ThenTheUpdateResultIsNotFound() =>
        Assert.True(_result.IsNotFound);

    [Then(@"the update result is conflict")]
    public void ThenTheUpdateResultIsConflict() =>
        Assert.True(_result.IsConflict);

    [Then(@"the update result is write failure")]
    public void ThenTheUpdateResultIsWriteFailure() =>
        Assert.True(_result.IsWriteFailure);

    [Then(@"the updated card has embossed name ""(.*)""")]
    public void ThenTheUpdatedCardHasEmbossedName(string expectedName)
    {
        Assert.NotNull(_result.Card);
        Assert.Equal(expectedName, _result.Card.EmbossedName);
    }

    [Then(@"the update message is ""(.*)""")]
    public void ThenTheUpdateMessageIs(string expectedMessage) =>
        Assert.Equal(expectedMessage, _result.Message);

    [Then(@"the validation errors contain ""(.*)""")]
    public void ThenTheValidationErrorsContain(string expectedError) =>
        Assert.Contains(_result.ValidationErrors, e => e == expectedError);

    /// <summary>
    /// In-memory stub repository for card update BDD scenarios.
    /// Supports simulating concurrency conflicts and write failures.
    /// </summary>
    internal sealed class StubCardRepository : ICardRepository
    {
        private readonly List<Card> _cards = [];

        public enum UpdateFailureMode { None, ConcurrencyConflict, WriteFailure }

        public UpdateFailureMode ThrowOnUpdate { get; set; }

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

        public Task UpdateAsync(Card card, CancellationToken cancellationToken = default)
        {
            return ThrowOnUpdate switch
            {
                UpdateFailureMode.None => CompleteUpdate(card),
                UpdateFailureMode.ConcurrencyConflict => throw new ConcurrencyConflictException("Simulated concurrency conflict"),
                UpdateFailureMode.WriteFailure => throw new InvalidOperationException("Simulated write failure"),
                _ => throw new InvalidOperationException($"Unknown failure mode: {ThrowOnUpdate}")
            };
        }

        private Task CompleteUpdate(Card card)
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
