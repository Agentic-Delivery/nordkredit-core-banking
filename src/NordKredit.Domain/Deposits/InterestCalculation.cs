namespace NordKredit.Domain.Deposits;

/// <summary>
/// Domain service for deposit interest calculation.
/// COBOL source: Dedicated interest calculation batch program (not yet in repository).
/// Business rule: DEP-BR-004 (interest calculation and accrual).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive, PSD2 Art. 57.
///
/// Calculates daily interest based on account balance, product rate schedule,
/// and day-count convention. Supports tiered rates per DEP-BR-006.
/// </summary>
public static class InterestCalculation
{
    /// <summary>
    /// Calculates daily interest for a deposit account based on its savings product.
    /// Uses 4 decimal intermediate precision.
    /// Business rule: DEP-BR-004.
    ///
    /// For tiered rates:
    ///   Tier 1: balance up to Tier1Limit at base AnnualRate
    ///   Tier 2: balance from Tier1Limit to Tier2Limit at Tier2Rate
    ///   Tier 3: balance above Tier2Limit at Tier3Rate
    /// </summary>
    /// <param name="balance">Current deposit balance. COBOL: ACCT-CURR-BAL.</param>
    /// <param name="product">Savings product with rate schedule.</param>
    /// <returns>Daily interest amount with 4 decimal precision.</returns>
    public static decimal CalculateDailyInterest(decimal balance, SavingsProduct product)
    {
        if (balance <= 0)
        {
            return 0m;
        }

        if (product.Tier1Limit.HasValue && product.Tier2Rate.HasValue)
        {
            return CalculateTieredInterest(balance, product);
        }

        return Round4(balance * product.AnnualRate / product.DayCountBasis);
    }

    private static decimal CalculateTieredInterest(decimal balance, SavingsProduct product)
    {
        var totalInterest = 0m;
        var remaining = balance;

        // Tier 1: base rate up to Tier1Limit
        var tier1Balance = Math.Min(remaining, product.Tier1Limit!.Value);
        totalInterest += Round4(tier1Balance * product.AnnualRate / product.DayCountBasis);
        remaining -= tier1Balance;

        if (remaining <= 0)
        {
            return totalInterest;
        }

        // Tier 2: Tier2Rate from Tier1Limit to Tier2Limit
        if (product.Tier2Limit.HasValue)
        {
            var tier2Balance = Math.Min(remaining, product.Tier2Limit.Value - product.Tier1Limit.Value);
            totalInterest += Round4(tier2Balance * product.Tier2Rate!.Value / product.DayCountBasis);
            remaining -= tier2Balance;
        }
        else
        {
            totalInterest += Round4(remaining * product.Tier2Rate!.Value / product.DayCountBasis);
            return totalInterest;
        }

        if (remaining <= 0)
        {
            return totalInterest;
        }

        // Tier 3: Tier3Rate above Tier2Limit
        if (product.Tier3Rate.HasValue)
        {
            totalInterest += Round4(remaining * product.Tier3Rate.Value / product.DayCountBasis);
        }

        return totalInterest;
    }

    private static decimal Round4(decimal value) =>
        Math.Round(value, 4, MidpointRounding.AwayFromZero);
}
