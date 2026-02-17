using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Deposits;

/// <summary>
/// Tests for the SavingsProduct entity.
/// COBOL source: CVTRA02Y.cpy (disclosure group records).
/// Business rule: DEP-BR-006 (savings product configuration and tiered rates).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive.
/// </summary>
public class SavingsProductTests
{
    [Fact]
    public void SavingsProduct_ShouldStoreAllFields()
    {
        var product = new SavingsProduct
        {
            ProductId = "SAV-STD",
            Description = "Standard Savings Account",
            ProductType = DepositProductType.DemandSavings,
            AnnualRate = 0.025m,
            Tier1Limit = 100000m,
            Tier2Rate = 0.015m,
            Tier2Limit = 500000m,
            Tier3Rate = 0.020m,
            MinimumBalance = 0m,
            MaxWithdrawalsPerMonth = 0,
            TermMonths = null,
            DayCountBasis = 365,
            InterestPostingFrequencyMonths = 1
        };

        Assert.Equal("SAV-STD", product.ProductId);
        Assert.Equal("Standard Savings Account", product.Description);
        Assert.Equal(DepositProductType.DemandSavings, product.ProductType);
        Assert.Equal(0.025m, product.AnnualRate);
        Assert.Equal(100000m, product.Tier1Limit);
        Assert.Equal(0.015m, product.Tier2Rate);
        Assert.Equal(500000m, product.Tier2Limit);
        Assert.Equal(0.020m, product.Tier3Rate);
        Assert.Equal(0m, product.MinimumBalance);
        Assert.Equal(0, product.MaxWithdrawalsPerMonth);
        Assert.Null(product.TermMonths);
        Assert.Equal(365, product.DayCountBasis);
        Assert.Equal(1, product.InterestPostingFrequencyMonths);
    }

    [Fact]
    public void SavingsProduct_StringProperties_DefaultToEmpty()
    {
        var product = new SavingsProduct();

        Assert.Equal(string.Empty, product.ProductId);
        Assert.Equal(string.Empty, product.Description);
    }

    [Fact]
    public void SavingsProduct_Defaults_DayCountBasisIs365()
    {
        var product = new SavingsProduct();

        Assert.Equal(365, product.DayCountBasis);
    }

    [Fact]
    public void SavingsProduct_Defaults_MonthlyInterestPosting()
    {
        var product = new SavingsProduct();

        Assert.Equal(1, product.InterestPostingFrequencyMonths);
    }

    [Fact]
    public void SavingsProduct_TermDeposit_HasTermMonths()
    {
        var product = new SavingsProduct
        {
            ProductType = DepositProductType.TermDeposit,
            TermMonths = 12,
            InterestPostingFrequencyMonths = 0
        };

        Assert.Equal(DepositProductType.TermDeposit, product.ProductType);
        Assert.Equal(12, product.TermMonths);
        Assert.Equal(0, product.InterestPostingFrequencyMonths);
    }

    [Theory]
    [InlineData(DepositProductType.DemandSavings)]
    [InlineData(DepositProductType.TermDeposit)]
    [InlineData(DepositProductType.ChildrensSavings)]
    [InlineData(DepositProductType.BusinessDeposit)]
    public void SavingsProduct_SupportsAllProductTypes(DepositProductType productType)
    {
        var product = new SavingsProduct { ProductType = productType };

        Assert.Equal(productType, product.ProductType);
    }
}
