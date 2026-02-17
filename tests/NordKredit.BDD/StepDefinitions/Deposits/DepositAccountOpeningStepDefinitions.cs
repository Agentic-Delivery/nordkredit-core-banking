using System.Globalization;
using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for deposit account opening BDD scenarios (DEP-BR-002).
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), WRITE ACCOUNT-RECORD to ACCTFILE.
/// Regulations: FSA FFFS 2014:5 Ch. 3, AML 2017:11, Deposit Guarantee Directive.
/// </summary>
[Binding]
[Scope(Feature = "Deposit account opening")]
public sealed class DepositAccountOpeningStepDefinitions
{
    private DepositAccount _account = null!;

    [Given(@"I create a deposit account with")]
    public void GivenICreateADepositAccountWith(Table table)
    {
        var row = table.Rows[0];
        _account = new DepositAccount
        {
            Id = row["AccountId"],
            HolderName = row["HolderName"],
            ProductType = Enum.Parse<DepositProductType>(row["ProductType"]),
            DisclosureGroupId = row["DisclosureGroupId"],
            Status = DepositAccountStatus.Active,
            OpenedDate = DateTime.UtcNow,
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };
    }

    [Given(@"I create a deposit account with maturity date ""(.*)"" and")]
    public void GivenICreateADepositAccountWithMaturityDateAnd(string maturityDate, Table table)
    {
        var row = table.Rows[0];
        _account = new DepositAccount
        {
            Id = row["AccountId"],
            HolderName = row["HolderName"],
            ProductType = Enum.Parse<DepositProductType>(row["ProductType"]),
            DisclosureGroupId = row["DisclosureGroupId"],
            Status = DepositAccountStatus.Active,
            OpenedDate = DateTime.UtcNow,
            MaturityDate = DateTime.Parse(maturityDate, CultureInfo.InvariantCulture),
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };
    }

    [Then(@"the new account has status ""(.*)""")]
    public void ThenTheNewAccountHasStatus(string expectedStatus) =>
        Assert.Equal(expectedStatus, _account.Status.ToString());

    [Then(@"the new account has balance (.+)")]
    public void ThenTheNewAccountHasBalance(decimal expectedBalance) =>
        Assert.Equal(expectedBalance, _account.CurrentBalance);

    [Then(@"the new account has current cycle credit (.+)")]
    public void ThenTheNewAccountHasCurrentCycleCredit(decimal expected) =>
        Assert.Equal(expected, _account.CurrentCycleCredit);

    [Then(@"the new account has current cycle debit (.+)")]
    public void ThenTheNewAccountHasCurrentCycleDebit(decimal expected) =>
        Assert.Equal(expected, _account.CurrentCycleDebit);

    [Then(@"the new account has accrued interest (.+)")]
    public void ThenTheNewAccountHasAccruedInterest(decimal expected) =>
        Assert.Equal(expected, _account.AccruedInterest);

    [Then(@"the new account has an opened date")]
    public void ThenTheNewAccountHasAnOpenedDate() =>
        Assert.NotEqual(default, _account.OpenedDate);

    [Then(@"the new account has maturity date ""(.*)""")]
    public void ThenTheNewAccountHasMaturityDate(string expectedDate)
    {
        Assert.NotNull(_account.MaturityDate);
        Assert.Equal(expectedDate, _account.MaturityDate.Value.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture));
    }

    [Then(@"the new account has holder name ""(.*)""")]
    public void ThenTheNewAccountHasHolderName(string expectedName) =>
        Assert.Equal(expectedName, _account.HolderName);
}
