using NordKredit.Domain.AccountManagement;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.AccountManagement;

/// <summary>
/// Step definitions for account ID validation BDD scenarios (ACCT-BR-002).
/// COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760.
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3.
/// </summary>
[Binding]
public sealed class AccountValidationStepDefinitions
{
    private AccountValidationResult _result = null!;

    [When(@"I validate account ID ""(.*)"" as required")]
    public void WhenIValidateAccountIdAsRequired(string accountId) =>
        _result = AccountValidationService.ValidateAccountId(accountId, required: true);

    [When(@"I validate account ID ""(.*)"" as optional")]
    public void WhenIValidateAccountIdAsOptional(string accountId) =>
        _result = AccountValidationService.ValidateAccountId(accountId, required: false);

    [Then(@"the account validation result is valid")]
    public void ThenTheAccountValidationResultIsValid() =>
        Assert.True(_result.IsValid);

    [Then(@"the account validation result is invalid")]
    public void ThenTheAccountValidationResultIsInvalid() =>
        Assert.False(_result.IsValid);

    [Then(@"the account validation error is ""(.*)""")]
    public void ThenTheAccountValidationErrorIs(string expectedError) =>
        Assert.Equal(expectedError, _result.ErrorMessage);
}
