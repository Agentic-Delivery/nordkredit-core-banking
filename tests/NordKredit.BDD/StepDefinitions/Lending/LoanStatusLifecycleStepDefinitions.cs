using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for loan status lifecycle BDD scenarios (LND-BR-008).
/// COBOL source: Dedicated program (inferred from LND-BR-008).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Inkassolagen 1974:182.
/// </summary>
[Binding]
[Scope(Feature = "Loan status lifecycle state machine")]
public sealed class LoanStatusLifecycleStepDefinitions
{
    private Loan _loan = null!;
    private LoanValidationResult _transitionResult = null!;

    [Given(@"a loan with status ""(.*)""")]
    public void GivenALoanWithStatus(string status)
    {
        _loan = new Loan
        {
            AccountId = "12345678901",
            ActiveStatus = Enum.Parse<LoanStatus>(status)
        };
    }

    [When(@"I transition the loan to ""(.*)""")]
    public void WhenITransitionTheLoanTo(string targetStatus) =>
        _transitionResult = _loan.TransitionTo(Enum.Parse<LoanStatus>(targetStatus));

    [Then(@"the status transition is valid")]
    public void ThenTheStatusTransitionIsValid() =>
        Assert.True(_transitionResult.IsValid);

    [Then(@"the status transition is invalid")]
    public void ThenTheStatusTransitionIsInvalid() =>
        Assert.False(_transitionResult.IsValid);

    [Then(@"the loan status is ""(.*)""")]
    public void ThenTheLoanStatusIs(string expectedStatus) =>
        Assert.Equal(Enum.Parse<LoanStatus>(expectedStatus), _loan.ActiveStatus);

    [Then(@"the transition error is ""(.*)""")]
    public void ThenTheTransitionErrorIs(string expectedError) =>
        Assert.Equal(expectedError, _transitionResult.ErrorMessage);
}
