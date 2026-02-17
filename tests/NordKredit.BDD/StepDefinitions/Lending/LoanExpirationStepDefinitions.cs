using System.Globalization;
using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for loan expiration enforcement BDD scenarios (LND-BR-009).
/// COBOL source: CBTRN02C.cbl:414-420.
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk management).
/// </summary>
[Binding]
[Scope(Feature = "Loan expiration enforcement")]
public sealed class LoanExpirationStepDefinitions
{
    private Loan _loan = null!;
    private bool _isExpired;

    [Given(@"a loan with expiration date ""(.*)""")]
    public void GivenALoanWithExpirationDate(string expirationDate)
    {
        _loan = new Loan
        {
            AccountId = "12345678901",
            ExpirationDate = DateTime.Parse(expirationDate, CultureInfo.InvariantCulture)
        };
    }

    [Given(@"a loan with no expiration date")]
    public void GivenALoanWithNoExpirationDate()
    {
        _loan = new Loan
        {
            AccountId = "12345678901",
            ExpirationDate = null
        };
    }

    [When(@"I check if the loan is expired as of ""(.*)""")]
    public void WhenICheckIfTheLoanIsExpiredAsOf(string asOfDate) =>
        _isExpired = _loan.IsExpired(DateTime.Parse(asOfDate, CultureInfo.InvariantCulture));

    [Then(@"the loan is not expired")]
    public void ThenTheLoanIsNotExpired() =>
        Assert.False(_isExpired);

    [Then(@"the loan is expired")]
    public void ThenTheLoanIsExpired() =>
        Assert.True(_isExpired);
}
