namespace NordKredit.Domain.Deposits;

/// <summary>
/// Savings product configuration with tiered interest rates.
/// COBOL source: CVTRA02Y.cpy (disclosure group records).
/// Business rule: DEP-BR-006 (savings product configuration and tiered rates).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive.
/// </summary>
public class SavingsProduct
{
    /// <summary>
    /// Product/disclosure group identifier.
    /// COBOL: ACCT-GROUP-ID PIC X(10).
    /// </summary>
    public string ProductId { get; set; } = string.Empty;

    /// <summary>Product description.</summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>Deposit product type classification.</summary>
    public DepositProductType ProductType { get; set; } = DepositProductType.DemandSavings;

    /// <summary>
    /// Base annual interest rate (as decimal, e.g. 0.025 for 2.5%).
    /// COBOL: Disclosure group rate field.
    /// Business rule: DEP-BR-004.
    /// </summary>
    public decimal AnnualRate { get; set; }

    /// <summary>
    /// Tier 1 balance limit. Interest at base rate applies up to this balance.
    /// Null if no tiered rates.
    /// Business rule: DEP-BR-006.
    /// </summary>
    public decimal? Tier1Limit { get; set; }

    /// <summary>Tier 2 annual rate applied from Tier1Limit to Tier2Limit.</summary>
    public decimal? Tier2Rate { get; set; }

    /// <summary>Tier 2 balance limit.</summary>
    public decimal? Tier2Limit { get; set; }

    /// <summary>Tier 3 annual rate applied above Tier2Limit.</summary>
    public decimal? Tier3Rate { get; set; }

    /// <summary>
    /// Minimum balance required for the product.
    /// Business rule: DEP-BR-006.
    /// </summary>
    public decimal MinimumBalance { get; set; }

    /// <summary>
    /// Maximum withdrawals per month (0 = unlimited).
    /// Business rule: DEP-BR-006.
    /// </summary>
    public int MaxWithdrawalsPerMonth { get; set; }

    /// <summary>
    /// Term length in months for term deposits. Null for demand deposits.
    /// Business rule: DEP-BR-005.
    /// </summary>
    public int? TermMonths { get; set; }

    /// <summary>
    /// Day-count basis for interest calculation (360 or 365).
    /// Business rule: DEP-BR-004.
    /// </summary>
    public int DayCountBasis { get; set; } = 365;

    /// <summary>
    /// Interest posting frequency in months (1 = monthly, 3 = quarterly, 0 = at maturity).
    /// Business rule: DEP-BR-004.
    /// </summary>
    public int InterestPostingFrequencyMonths { get; set; } = 1;
}
