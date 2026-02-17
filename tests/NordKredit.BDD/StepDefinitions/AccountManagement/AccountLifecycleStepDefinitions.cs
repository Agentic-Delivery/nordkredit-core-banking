using NordKredit.Domain.AccountManagement;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.AccountManagement;

/// <summary>
/// Step definitions for account lifecycle state machine BDD scenarios (ACCT-BR-005).
/// COBOL source: COCRDUPC.cbl:845-876, CBTRN02C.cbl:414-420.
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97, GDPR Art. 17.
/// </summary>
[Binding]
public sealed class AccountLifecycleStepDefinitions
{
    private Account _account = null!;
    private AccountValidationResult _result = null!;

    [Given(@"an account with status ""(.*)""")]
    public void GivenAnAccountWithStatus(string status) =>
        _account = new Account
        {
            Id = "12345678901",
            Status = Enum.Parse<AccountStatus>(status),
            HolderName = "TEST HOLDER",
            OpenedDate = DateTime.UtcNow
        };

    [When(@"I transition the account to ""(.*)""")]
    public void WhenITransitionTheAccountTo(string targetStatus) =>
        _result = _account.TransitionTo(Enum.Parse<AccountStatus>(targetStatus));

    [Then(@"the transition succeeds")]
    public void ThenTheTransitionSucceeds() =>
        Assert.True(_result.IsValid);

    [Then(@"the transition fails")]
    public void ThenTheTransitionFails() =>
        Assert.False(_result.IsValid);

    [Then(@"the account status is ""(.*)""")]
    public void ThenTheAccountStatusIs(string expectedStatus) =>
        Assert.Equal(Enum.Parse<AccountStatus>(expectedStatus), _account.Status);

    [Then(@"the account has a closed date")]
    public void ThenTheAccountHasAClosedDate() =>
        Assert.NotNull(_account.ClosedDate);

    [Then(@"the transition error is ""(.*)""")]
    public void ThenTheTransitionErrorIs(string expectedError) =>
        Assert.Equal(expectedError, _result.ErrorMessage);
}
