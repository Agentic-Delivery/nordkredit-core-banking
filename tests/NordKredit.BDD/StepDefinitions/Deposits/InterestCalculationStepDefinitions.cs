using System.Globalization;
using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for deposit interest calculation BDD scenarios (DEP-BR-004, DEP-BR-006).
/// COBOL source: Dedicated interest calculation batch program, CVTRA02Y.cpy (disclosure group records).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive, PSD2 Art. 57.
/// </summary>
[Binding]
[Scope(Feature = "Deposit interest calculation with tiered rates")]
public sealed class InterestCalculationStepDefinitions
{
    private SavingsProduct _product = null!;
    private decimal _dailyInterest;

    [Given(@"a savings product with annual rate (.+) and day count basis (\d+)")]
    public void GivenASavingsProductWithAnnualRateAndDayCountBasis(decimal annualRate, int dayCountBasis) =>
        _product = new SavingsProduct
        {
            ProductId = "TEST",
            Description = "Test Product",
            AnnualRate = annualRate,
            DayCountBasis = dayCountBasis
        };

    [Given(@"a tiered savings product with")]
    public void GivenATieredSavingsProductWith(Table table)
    {
        var row = table.Rows[0];
        _product = new SavingsProduct
        {
            ProductId = "TIERED",
            Description = "Tiered Product",
            AnnualRate = decimal.Parse(row["AnnualRate"], CultureInfo.InvariantCulture),
            Tier1Limit = decimal.Parse(row["Tier1Limit"], CultureInfo.InvariantCulture),
            Tier2Rate = decimal.Parse(row["Tier2Rate"], CultureInfo.InvariantCulture),
            Tier2Limit = decimal.Parse(row["Tier2Limit"], CultureInfo.InvariantCulture),
            Tier3Rate = decimal.Parse(row["Tier3Rate"], CultureInfo.InvariantCulture),
            DayCountBasis = int.Parse(row["DayCountBasis"], CultureInfo.InvariantCulture)
        };
    }

    [When(@"I calculate daily deposit interest on balance (.+)")]
    public void WhenICalculateDailyDepositInterestOnBalance(decimal balance) =>
        _dailyInterest = InterestCalculation.CalculateDailyInterest(balance, _product);

    [Then(@"the daily interest is (.+)")]
    public void ThenTheDailyInterestIs(decimal expected) =>
        Assert.Equal(expected, _dailyInterest);
}
