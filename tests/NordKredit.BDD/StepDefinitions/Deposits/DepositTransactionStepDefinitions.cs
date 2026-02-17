using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for deposit transaction posting BDD scenarios (DEP-BR-003).
/// COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64.
/// </summary>
[Binding]
[Scope(Feature = "Deposit transaction posting")]
public sealed class DepositTransactionStepDefinitions
{
    private DepositAccount _account = null!;

    [Given(@"a deposit account with balance (.+)")]
    public void GivenADepositAccountWithBalance(decimal balance) =>
        _account = new DepositAccount
        {
            Id = "12345678901",
            Status = DepositAccountStatus.Active,
            CurrentBalance = balance,
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };

    [When(@"I apply a transaction of (.+)")]
    public void WhenIApplyATransactionOf(decimal amount) =>
        _account.ApplyTransaction(amount);

    [Then(@"the account balance is (.+)")]
    public void ThenTheAccountBalanceIs(decimal expectedBalance) =>
        Assert.Equal(expectedBalance, _account.CurrentBalance);

    [Then(@"the current cycle credit is (.+)")]
    public void ThenTheCurrentCycleCreditIs(decimal expectedCredit) =>
        Assert.Equal(expectedCredit, _account.CurrentCycleCredit);

    [Then(@"the current cycle debit is (.+)")]
    public void ThenTheCurrentCycleDebitIs(decimal expectedDebit) =>
        Assert.Equal(expectedDebit, _account.CurrentCycleDebit);
}
