using System.Globalization;
using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for credit limit enforcement BDD scenarios (LND-BR-002).
/// COBOL source: CBTRN02C.cbl:403-413 (1500-B-LOOKUP-ACCT).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10.
/// </summary>
[Binding]
[Scope(Feature = "Loan credit limit enforcement")]
public sealed class LoanCreditLimitStepDefinitions
{
    private Loan _loan = null!;
    private bool _wouldExceed;

    [Given(@"a loan account with the following details")]
    public void GivenALoanAccountWithTheFollowingDetails(Table table)
    {
        var row = table.Rows[0];
        _loan = new Loan
        {
            AccountId = row["AccountId"],
            CreditLimit = decimal.Parse(row["CreditLimit"], CultureInfo.InvariantCulture),
            CurrentCycleCredit = decimal.Parse(row["CurrentCycleCredit"], CultureInfo.InvariantCulture),
            CurrentCycleDebit = decimal.Parse(row["CurrentCycleDebit"], CultureInfo.InvariantCulture)
        };
    }

    [When(@"I check if transaction amount (.+) would exceed the credit limit")]
    public void WhenICheckIfTransactionAmountWouldExceedTheCreditLimit(decimal amount) =>
        _wouldExceed = _loan.WouldExceedCreditLimit(amount);

    [Then(@"the credit limit check result is ""not exceeded""")]
    public void ThenTheCreditLimitCheckResultIsNotExceeded() =>
        Assert.False(_wouldExceed);

    [Then(@"the credit limit check result is ""exceeded""")]
    public void ThenTheCreditLimitCheckResultIsExceeded() =>
        Assert.True(_wouldExceed);

    [Then(@"the available credit is (.+)")]
    public void ThenTheAvailableCreditIs(decimal expected) =>
        Assert.Equal(expected, _loan.AvailableCredit);
}
