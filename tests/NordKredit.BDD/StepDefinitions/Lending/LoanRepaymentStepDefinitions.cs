using System.Globalization;
using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for loan repayment processing BDD scenarios (LND-BR-005).
/// COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, Art. 89.
/// </summary>
[Binding]
[Scope(Feature = "Loan repayment and transaction processing")]
public sealed class LoanRepaymentStepDefinitions
{
    private Loan _loan = null!;

    [Given(@"a loan account with the following balances")]
    public void GivenALoanAccountWithTheFollowingBalances(Table table)
    {
        var row = table.Rows[0];
        _loan = new Loan
        {
            AccountId = row["AccountId"],
            CurrentBalance = decimal.Parse(row["CurrentBalance"], CultureInfo.InvariantCulture),
            CurrentCycleCredit = decimal.Parse(row["CurrentCycleCredit"], CultureInfo.InvariantCulture),
            CurrentCycleDebit = decimal.Parse(row["CurrentCycleDebit"], CultureInfo.InvariantCulture)
        };
    }

    [When(@"I apply a transaction of (.+) to the loan")]
    public void WhenIApplyATransactionToTheLoan(decimal amount) =>
        _loan.ApplyTransaction(amount);

    [Then(@"the current balance is (.+)")]
    public void ThenTheCurrentBalanceIs(decimal expected) =>
        Assert.Equal(expected, _loan.CurrentBalance);

    [Then(@"the current cycle credit is (.+)")]
    public void ThenTheCurrentCycleCreditIs(decimal expected) =>
        Assert.Equal(expected, _loan.CurrentCycleCredit);

    [Then(@"the current cycle debit is (.+)")]
    public void ThenTheCurrentCycleDebitIs(decimal expected) =>
        Assert.Equal(expected, _loan.CurrentCycleDebit);
}
