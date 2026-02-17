using NordKredit.Domain.Lending;

namespace NordKredit.UnitTests.Lending;

/// <summary>
/// Tests for the Collateral entity.
/// COBOL source: Dedicated program not yet in repository.
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8, CRR Art. 194-217.
/// </summary>
public class CollateralTests
{
    [Fact]
    public void Collateral_ShouldStoreAllFields()
    {
        var collateral = new Collateral
        {
            CollateralId = "COL-00000001",
            LoanAccountId = "12345678901",
            Type = CollateralType.RealEstate,
            Value = 3000000.00m,
            ValuationDate = new DateTime(2025, 6, 15),
            Description = "Apartment in Stockholm",
            Status = CollateralStatus.Active
        };

        Assert.Equal("COL-00000001", collateral.CollateralId);
        Assert.Equal("12345678901", collateral.LoanAccountId);
        Assert.Equal(CollateralType.RealEstate, collateral.Type);
        Assert.Equal(3000000.00m, collateral.Value);
        Assert.Equal(new DateTime(2025, 6, 15), collateral.ValuationDate);
        Assert.Equal("Apartment in Stockholm", collateral.Description);
        Assert.Equal(CollateralStatus.Active, collateral.Status);
    }

    [Fact]
    public void Collateral_StringProperties_DefaultToEmpty()
    {
        var collateral = new Collateral();

        Assert.Equal(string.Empty, collateral.CollateralId);
        Assert.Equal(string.Empty, collateral.LoanAccountId);
        Assert.Equal(string.Empty, collateral.Description);
    }

    [Fact]
    public void Collateral_Defaults_StatusIsActive()
    {
        var collateral = new Collateral();

        Assert.Equal(CollateralStatus.Active, collateral.Status);
    }

    [Fact]
    public void Collateral_Value_PreservesLargeAmounts()
    {
        // COBOL: PIC S9(12)V99 — max 999,999,999,999.99
        var collateral = new Collateral { Value = 999999999999.99m };

        Assert.Equal(999999999999.99m, collateral.Value);
    }

    [Fact]
    public void Collateral_Description_ShouldSupportSwedishCharacters()
    {
        var collateral = new Collateral { Description = "Fastighet i Malmö, Skåne" };

        Assert.Contains("ö", collateral.Description);
        Assert.Contains("å", collateral.Description);
    }

    // =================================================================
    // LND-BR-006: LTV ratio calculation
    // =================================================================

    [Fact]
    public void CalculateLtv_StandardMortgage_ReturnsCorrectPercentage()
    {
        // Scenario 1: LTV = 2,000,000 / 3,000,000 * 100 = 66.67%
        var collateral = new Collateral { Value = 3000000.00m };

        var ltv = collateral.CalculateLtvRatio(2000000.00m);

        Assert.Equal(66.67m, Math.Round(ltv, 2));
    }

    [Fact]
    public void CalculateLtv_AtRegulatoryLimit_Returns85Percent()
    {
        // Swedish mortgage cap: 85% LTV (Finansinspektionen)
        var collateral = new Collateral { Value = 1000000.00m };

        var ltv = collateral.CalculateLtvRatio(850000.00m);

        Assert.Equal(85.00m, ltv);
    }

    [Fact]
    public void CalculateLtv_ZeroCollateralValue_ReturnsMaxValue()
    {
        // Edge case: zero collateral means infinite LTV
        var collateral = new Collateral { Value = 0m };

        var ltv = collateral.CalculateLtvRatio(100000.00m);

        Assert.Equal(decimal.MaxValue, ltv);
    }

    [Fact]
    public void CalculateLtv_ZeroLoanBalance_ReturnsZero()
    {
        var collateral = new Collateral { Value = 3000000.00m };

        var ltv = collateral.CalculateLtvRatio(0m);

        Assert.Equal(0m, ltv);
    }
}
