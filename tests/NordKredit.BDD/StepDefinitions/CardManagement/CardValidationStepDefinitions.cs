using NordKredit.Domain.CardManagement;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.CardManagement;

/// <summary>
/// Step definitions for account and card number validation BDD scenarios (CARD-BR-004, CARD-BR-005).
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-724, COCRDUPC.cbl:721-800.
/// Regulations: FFFS 2014:5 Ch. 4 §3 (operational risk — input validation).
/// </summary>
[Binding]
public sealed class CardValidationStepDefinitions
{
    private CardValidationResult _result = null!;

    [When(@"I validate account number ""(.*)"" as required")]
    public void WhenIValidateAccountNumberAsRequired(string accountNumber) =>
        _result = CardValidationService.ValidateAccountNumber(accountNumber, required: true);

    [When(@"I validate account number ""(.*)"" as optional")]
    public void WhenIValidateAccountNumberAsOptional(string accountNumber) =>
        _result = CardValidationService.ValidateAccountNumber(accountNumber, required: false);

    [When(@"I validate card number ""(.*)"" as required")]
    public void WhenIValidateCardNumberAsRequired(string cardNumber) =>
        _result = CardValidationService.ValidateCardNumber(cardNumber, required: true);

    [When(@"I validate card number ""(.*)"" as optional")]
    public void WhenIValidateCardNumberAsOptional(string cardNumber) =>
        _result = CardValidationService.ValidateCardNumber(cardNumber, required: false);

    [Then(@"the validation result is valid")]
    public void ThenTheValidationResultIsValid() =>
        Assert.True(_result.IsValid);

    [Then(@"the validation result is invalid")]
    public void ThenTheValidationResultIsInvalid() =>
        Assert.False(_result.IsValid);

    [Then(@"the validation error is ""(.*)""")]
    public void ThenTheValidationErrorIs(string expectedError) =>
        Assert.Equal(expectedError, _result.ErrorMessage);
}
