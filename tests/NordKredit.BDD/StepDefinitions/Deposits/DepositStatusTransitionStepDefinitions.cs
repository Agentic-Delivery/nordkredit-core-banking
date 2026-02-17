using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for deposit account lifecycle state machine BDD scenarios (DEP-BR-009, DEP-BR-008).
/// COBOL source: CVACT01Y.cpy (ACCT-ACTIVE-STATUS PIC X(01) — 'Y'/'N').
/// Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 5(1)(e),
///              GDPR Art. 17 (right to erasure on closure), Deposit Guarantee Directive.
/// </summary>
[Binding]
[Scope(Feature = "Deposit account lifecycle state machine")]
public sealed class DepositStatusTransitionStepDefinitions
{
    private DepositAccount _account = null!;
    private DepositValidationResult _result = null!;

    [Given(@"a deposit account with status ""(.*)""")]
    public void GivenADepositAccountWithStatus(string status) =>
        _account = new DepositAccount
        {
            Id = "12345678901",
            Status = Enum.Parse<DepositAccountStatus>(status),
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };

    [When(@"I transition the account to ""(.*)""")]
    public void WhenITransitionTheAccountTo(string targetStatus) =>
        _result = _account.TransitionTo(Enum.Parse<DepositAccountStatus>(targetStatus));

    [Then(@"the transition result is valid")]
    public void ThenTheTransitionResultIsValid() =>
        Assert.True(_result.IsValid);

    [Then(@"the transition result is invalid")]
    public void ThenTheTransitionResultIsInvalid() =>
        Assert.False(_result.IsValid);

    [Then(@"the account status is ""(.*)""")]
    public void ThenTheAccountStatusIs(string expectedStatus) =>
        Assert.Equal(expectedStatus, _account.Status.ToString());

    [Then(@"the account has a closed date")]
    public void ThenTheAccountHasAClosedDate() =>
        Assert.NotNull(_account.ClosedDate);

    [Then(@"the transition error is ""(.*)""")]
    public void ThenTheTransitionErrorIs(string expectedError) =>
        Assert.Equal(expectedError, _result.ErrorMessage);
}
