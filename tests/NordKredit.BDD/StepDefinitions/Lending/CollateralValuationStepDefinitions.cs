using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for collateral management and LTV ratio BDD scenarios (LND-BR-006).
/// COBOL source: Dedicated program (inferred VSAM COLLATERAL file).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8, CRR Art. 194-217, Finansinspektionen mortgage cap (85%).
/// </summary>
[Binding]
[Scope(Feature = "Collateral management and LTV ratio calculation")]
public sealed class CollateralValuationStepDefinitions
{
    private Collateral _collateral = null!;
    private decimal _ltvRatio;

    [Given(@"a collateral valued at (.+)")]
    public void GivenACollateralValuedAt(decimal value)
    {
        _collateral = new Collateral
        {
            CollateralId = "COL000000001",
            LoanAccountId = "12345678901",
            Type = CollateralType.RealEstate,
            Value = value,
            ValuationDate = DateTime.UtcNow,
            Description = "Residential property",
            Status = CollateralStatus.Active
        };
    }

    [When(@"I calculate the LTV ratio for a loan balance of (.+)")]
    public void WhenICalculateTheLtvRatioForALoanBalanceOf(decimal loanBalance) =>
        _ltvRatio = _collateral.CalculateLtvRatio(loanBalance);

    [Then(@"the LTV ratio is (\d+\.\d+)")]
    public void ThenTheLtvRatioIs(decimal expected) =>
        Assert.Equal(expected, _ltvRatio);

    [Then(@"the LTV ratio is maximum")]
    public void ThenTheLtvRatioIsMaximum() =>
        Assert.Equal(decimal.MaxValue, _ltvRatio);
}
