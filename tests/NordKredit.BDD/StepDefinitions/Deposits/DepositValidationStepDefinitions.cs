using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for deposit account input validation BDD scenarios (DEP-BR-001).
/// COBOL source: CVACT01Y.cpy — account field validation.
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3 (operational risk — input validation), PSD2 Art. 97.
/// </summary>
[Binding]
[Scope(Feature = "Deposit account input validation")]
public sealed class DepositValidationStepDefinitions
{
    private DepositValidationResult _result = null!;

    [When(@"I validate deposit account ID ""(.*)""")]
    public void WhenIValidateDepositAccountId(string accountId) =>
        _result = DepositValidationService.ValidateAccountId(accountId);

    [When(@"I validate disclosure group ID ""(.*)""")]
    public void WhenIValidateDisclosureGroupId(string groupId) =>
        _result = DepositValidationService.ValidateDisclosureGroupId(groupId);

    [Then(@"the deposit validation result is valid")]
    public void ThenTheDepositValidationResultIsValid() =>
        Assert.True(_result.IsValid);

    [Then(@"the deposit validation result is invalid")]
    public void ThenTheDepositValidationResultIsInvalid() =>
        Assert.False(_result.IsValid);

    [Then(@"the deposit validation error is ""(.*)""")]
    public void ThenTheDepositValidationErrorIs(string expectedError) =>
        Assert.Equal(expectedError, _result.ErrorMessage);
}
