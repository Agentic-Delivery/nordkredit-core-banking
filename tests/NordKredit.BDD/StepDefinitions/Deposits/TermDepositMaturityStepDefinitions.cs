using System.Globalization;
using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for term deposit maturity and renewal BDD scenarios (DEP-BR-005).
/// COBOL source: CVACT01Y.cpy (ACCT-EXPIRAION-DATE), CBTRN02C.cbl:414-420.
/// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive, PSD2 Art. 57.
/// </summary>
[Binding]
[Scope(Feature = "Term deposit maturity and renewal processing")]
public sealed class TermDepositMaturityStepDefinitions
{
    private DepositAccount _account = null!;
    private TermDeposit _termDeposit = null!;
    private bool _isMatured;

    [Given(@"a deposit account with maturity date ""(.*)""")]
    public void GivenADepositAccountWithMaturityDate(string maturityDate) =>
        _account = new DepositAccount
        {
            Id = "12345678901",
            Status = DepositAccountStatus.Active,
            ProductType = DepositProductType.TermDeposit,
            MaturityDate = DateTime.Parse(maturityDate, CultureInfo.InvariantCulture),
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };

    [Given(@"a deposit account without a maturity date")]
    public void GivenADepositAccountWithoutAMaturityDate() =>
        _account = new DepositAccount
        {
            Id = "12345678901",
            Status = DepositAccountStatus.Active,
            ProductType = DepositProductType.DemandSavings,
            MaturityDate = null,
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };

    [Given(@"a term deposit for account ""(.*)"" with")]
    public void GivenATermDepositForAccountWith(string accountId, Table table)
    {
        var row = table.Rows[0];
        _termDeposit = new TermDeposit
        {
            AccountId = accountId,
            TermMonths = int.Parse(row["TermMonths"], CultureInfo.InvariantCulture),
            FixedRate = decimal.Parse(row["FixedRate"], CultureInfo.InvariantCulture),
            PrincipalAmount = decimal.Parse(row["PrincipalAmount"], CultureInfo.InvariantCulture),
            RenewalInstruction = Enum.Parse<RenewalInstruction>(row["RenewalInstruction"]),
            LinkedAccountId = row.ContainsKey("LinkedAccountId") ? row["LinkedAccountId"] : null,
            StartDate = DateTime.Parse(row["StartDate"], CultureInfo.InvariantCulture)
        };
    }

    [When(@"I check maturity as of ""(.*)""")]
    public void WhenICheckMaturityAsOf(string asOfDate) =>
        _isMatured = _account.IsMatured(DateTime.Parse(asOfDate, CultureInfo.InvariantCulture));

    [Then(@"the account is matured")]
    public void ThenTheAccountIsMatured() =>
        Assert.True(_isMatured);

    [Then(@"the account is not matured")]
    public void ThenTheAccountIsNotMatured() =>
        Assert.False(_isMatured);

    [Then(@"the term deposit has term months (\d+)")]
    public void ThenTheTermDepositHasTermMonths(int expected) =>
        Assert.Equal(expected, _termDeposit.TermMonths);

    [Then(@"the term deposit has fixed rate (.+)")]
    public void ThenTheTermDepositHasFixedRate(decimal expected) =>
        Assert.Equal(expected, _termDeposit.FixedRate);

    [Then(@"the term deposit has principal amount (.+)")]
    public void ThenTheTermDepositHasPrincipalAmount(decimal expected) =>
        Assert.Equal(expected, _termDeposit.PrincipalAmount);

    [Then(@"the term deposit has renewal instruction ""(.*)""")]
    public void ThenTheTermDepositHasRenewalInstruction(string expected) =>
        Assert.Equal(expected, _termDeposit.RenewalInstruction.ToString());

    [Then(@"the term deposit has linked account ID ""(.*)""")]
    public void ThenTheTermDepositHasLinkedAccountId(string expected) =>
        Assert.Equal(expected, _termDeposit.LinkedAccountId);
}
