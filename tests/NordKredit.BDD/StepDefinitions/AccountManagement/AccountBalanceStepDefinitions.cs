using NordKredit.Domain.AccountManagement;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.AccountManagement;

/// <summary>
/// Step definitions for account balance management BDD scenarios (ACCT-BR-004, ACCT-BR-007).
/// COBOL source: CBTRN02C.cbl:403-413 (credit limit), CBTRN02C.cbl:545-560 (balance update).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 7, PSD2 Art. 64.
/// </summary>
[Binding]
public sealed class AccountBalanceStepDefinitions
{
    private Account _account = null!;
    private bool _creditLimitCheckResult;

    [Given(@"an account with balance ([\d.-]+) and cycle credit ([\d.-]+) and cycle debit ([\d.-]+)")]
    public void GivenAnAccountWithBalanceAndCycles(decimal balance, decimal cycleCredit, decimal cycleDebit) =>
        _account = new Account
        {
            Id = "12345678901",
            CurrentBalance = balance,
            CurrentCycleCredit = cycleCredit,
            CurrentCycleDebit = cycleDebit
        };

    [Given(@"an account with credit limit ([\d.-]+) and cycle credit ([\d.-]+) and cycle debit ([\d.-]+)")]
    public void GivenAnAccountWithCreditLimitAndCycles(decimal creditLimit, decimal cycleCredit, decimal cycleDebit) =>
        _account = new Account
        {
            Id = "12345678901",
            CreditLimit = creditLimit,
            CurrentCycleCredit = cycleCredit,
            CurrentCycleDebit = cycleDebit
        };

    [When(@"I apply a transaction of ([\d.-]+)")]
    public void WhenIApplyATransactionOf(decimal amount) =>
        _account.ApplyTransaction(amount);

    [When(@"I check if a transaction of ([\d.-]+) would exceed the credit limit")]
    public void WhenICheckIfTransactionWouldExceedCreditLimit(decimal amount) =>
        _creditLimitCheckResult = _account.WouldExceedCreditLimit(amount);

    [Then(@"the account balance is ([\d.-]+)")]
    public void ThenTheAccountBalanceIs(decimal expectedBalance) =>
        Assert.Equal(expectedBalance, _account.CurrentBalance);

    [Then(@"the cycle credit is ([\d.-]+)")]
    public void ThenTheCycleCreditIs(decimal expectedCycleCredit) =>
        Assert.Equal(expectedCycleCredit, _account.CurrentCycleCredit);

    [Then(@"the cycle debit is ([\d.-]+)")]
    public void ThenTheCycleDebitIs(decimal expectedCycleDebit) =>
        Assert.Equal(expectedCycleDebit, _account.CurrentCycleDebit);

    [Then(@"the credit limit check returns (true|false)")]
    public void ThenTheCreditLimitCheckReturns(bool expected) =>
        Assert.Equal(expected, _creditLimitCheckResult);
}
