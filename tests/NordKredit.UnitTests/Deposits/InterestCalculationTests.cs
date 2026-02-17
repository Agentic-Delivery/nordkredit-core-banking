using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Deposits;

/// <summary>
/// Tests for the InterestCalculation domain service.
/// COBOL source: Dedicated interest calculation batch program (not yet in repository).
/// Business rule: DEP-BR-004 (interest calculation and accrual).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive, PSD2 Art. 57.
/// </summary>
public class InterestCalculationTests
{
    // =================================================================
    // DEP-BR-004: Daily interest accrual on positive balance
    // =================================================================

    [Fact]
    public void CalculateDailyInterest_PositiveBalance_ReturnsCorrectInterest()
    {
        // GIVEN: Balance 100,000 SEK, 2.5% annual rate, ACT/365
        // Daily interest = 100000.00 × (0.025 / 365) = 6.8493
        var product = CreateProduct(annualRate: 0.025m, dayCountBasis: 365);

        var interest = InterestCalculation.CalculateDailyInterest(100000.00m, product);

        Assert.Equal(6.8493m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_ACT360_ReturnsCorrectInterest()
    {
        // GIVEN: Balance 100,000 SEK, 2.5% annual rate, ACT/360
        // Daily interest = 100000.00 × (0.025 / 360) = 6.9444
        var product = CreateProduct(annualRate: 0.025m, dayCountBasis: 360);

        var interest = InterestCalculation.CalculateDailyInterest(100000.00m, product);

        Assert.Equal(6.9444m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_ZeroBalance_ReturnsZero()
    {
        var product = CreateProduct(annualRate: 0.025m);

        var interest = InterestCalculation.CalculateDailyInterest(0m, product);

        Assert.Equal(0m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_NegativeBalance_ReturnsZero()
    {
        // Negative deposit balances should not accrue interest
        var product = CreateProduct(annualRate: 0.025m);

        var interest = InterestCalculation.CalculateDailyInterest(-1000m, product);

        Assert.Equal(0m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_Uses4DecimalPrecision()
    {
        // Intermediate precision must be 4 decimal places
        var product = CreateProduct(annualRate: 0.01m, dayCountBasis: 365);

        // 50000 × (0.01 / 365) = 1.36986... → rounded to 1.3699
        var interest = InterestCalculation.CalculateDailyInterest(50000m, product);

        Assert.Equal(1.3699m, interest);
    }

    // =================================================================
    // DEP-BR-006: Tiered interest rate calculation
    // =================================================================

    [Fact]
    public void CalculateDailyInterest_TieredRates_CalculatesPerTier()
    {
        // GIVEN: Balance 500,000 SEK with tiers:
        //   Tier 1: 0-100,000 at 1.00%
        //   Tier 2: 100,001-500,000 at 1.50%
        // Daily = (100000 × 0.01/365) + (400000 × 0.015/365)
        //       = 2.7397 + 16.4384 = 19.1781
        var product = CreateTieredProduct(
            baseRate: 0.01m,
            tier1Limit: 100000m,
            tier2Rate: 0.015m,
            tier2Limit: 500000m);

        var interest = InterestCalculation.CalculateDailyInterest(500000m, product);

        Assert.Equal(19.1781m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_TieredRates_BalanceInTier1Only()
    {
        // Balance within Tier 1 — only base rate applies
        var product = CreateTieredProduct(
            baseRate: 0.01m,
            tier1Limit: 100000m,
            tier2Rate: 0.015m,
            tier2Limit: 500000m);

        // 50000 × (0.01 / 365) = 1.3699
        var interest = InterestCalculation.CalculateDailyInterest(50000m, product);

        Assert.Equal(1.3699m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_TieredRates_BalanceInAllThreeTiers()
    {
        // Balance exceeds Tier 2 — Tier 3 rate applies to excess
        var product = CreateTieredProduct(
            baseRate: 0.01m,
            tier1Limit: 100000m,
            tier2Rate: 0.015m,
            tier2Limit: 500000m,
            tier3Rate: 0.02m);

        // 600,000 SEK:
        //   Tier 1: 100000 × 0.01/365 = 2.7397
        //   Tier 2: 400000 × 0.015/365 = 16.4384
        //   Tier 3: 100000 × 0.02/365 = 5.4795
        //   Total = 24.6576
        var interest = InterestCalculation.CalculateDailyInterest(600000m, product);

        Assert.Equal(24.6576m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_TieredRates_ExactlyAtTier1Limit()
    {
        var product = CreateTieredProduct(
            baseRate: 0.01m,
            tier1Limit: 100000m,
            tier2Rate: 0.015m,
            tier2Limit: 500000m);

        // 100000 × 0.01/365 = 2.7397
        var interest = InterestCalculation.CalculateDailyInterest(100000m, product);

        Assert.Equal(2.7397m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_SmallBalance_PreservesPrecision()
    {
        // Very small balance — precision matters
        var product = CreateProduct(annualRate: 0.005m, dayCountBasis: 365);

        // 100 × 0.005/365 = 0.001369... → 0.0014
        var interest = InterestCalculation.CalculateDailyInterest(100m, product);

        Assert.Equal(0.0014m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_LargeBalance_PreservesPrecision()
    {
        // COBOL PIC S9(10)V99 max balance
        var product = CreateProduct(annualRate: 0.025m, dayCountBasis: 365);

        // 9999999999.99 × 0.025/365 = 684931.5068...
        var interest = InterestCalculation.CalculateDailyInterest(9999999999.99m, product);

        Assert.Equal(684931.5068m, interest);
    }

    // =================================================================
    // Helpers
    // =================================================================

    private static SavingsProduct CreateProduct(decimal annualRate, int dayCountBasis = 365) => new()
    {
        ProductId = "TEST",
        AnnualRate = annualRate,
        DayCountBasis = dayCountBasis
    };

    private static SavingsProduct CreateTieredProduct(
        decimal baseRate,
        decimal tier1Limit,
        decimal tier2Rate,
        decimal tier2Limit,
        decimal? tier3Rate = null) => new()
        {
            ProductId = "TEST-TIERED",
            AnnualRate = baseRate,
            Tier1Limit = tier1Limit,
            Tier2Rate = tier2Rate,
            Tier2Limit = tier2Limit,
            Tier3Rate = tier3Rate,
            DayCountBasis = 365
        };
}
