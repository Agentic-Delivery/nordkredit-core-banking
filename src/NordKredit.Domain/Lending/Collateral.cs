namespace NordKredit.Domain.Lending;

/// <summary>
/// Collateral record representing an asset pledged as security for a loan.
/// COBOL source: Dedicated program not yet in repository (inferred VSAM file structure).
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8 (assets), CRR Art. 194-217 (credit risk mitigation),
///              GDPR Art. 5(1)(e) (storage limitation).
/// </summary>
public class Collateral
{
    /// <summary>Unique collateral identifier. COBOL: COLLATERAL-ID PIC X(12).</summary>
    public string CollateralId { get; set; } = string.Empty;

    /// <summary>Foreign key to the loan account. COBOL: COLLATERAL-ACCT-ID PIC 9(11).</summary>
    public string LoanAccountId { get; set; } = string.Empty;

    /// <summary>
    /// Collateral asset type.
    /// COBOL: COLLATERAL-TYPE PIC X(02) — RE, VH, SC, GR.
    /// </summary>
    public CollateralType Type { get; set; }

    /// <summary>
    /// Current assessed value in SEK.
    /// COBOL: COLLATERAL-VALUE PIC S9(12)V99. Maps to SQL decimal(14,2).
    /// </summary>
    public decimal Value { get; set; }

    /// <summary>
    /// Date of last valuation. COBOL: COLLATERAL-VALUATION-DATE PIC X(10).
    /// Regulations: FSA FFFS 2014:5 Ch. 8 (periodic revaluation required).
    /// </summary>
    public DateTime ValuationDate { get; set; }

    /// <summary>
    /// Free-text asset description. COBOL: COLLATERAL-DESCRIPTION PIC X(80).
    /// Supports Swedish characters (Å, Ä, Ö) via nvarchar.
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Collateral record lifecycle status.
    /// COBOL: COLLATERAL-STATUS PIC X(01) — A=Active, R=Released, E=Expired.
    /// </summary>
    public CollateralStatus Status { get; set; } = CollateralStatus.Active;

    /// <summary>
    /// Calculates the loan-to-value (LTV) ratio.
    /// LTV = loanBalance / collateralValue × 100.
    /// Business rule: LND-BR-006.
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Finansinspektionen mortgage cap (85%).
    /// </summary>
    /// <param name="loanBalance">The outstanding loan balance.</param>
    /// <returns>LTV as a percentage. Returns decimal.MaxValue if collateral value is zero.</returns>
    public decimal CalculateLtvRatio(decimal loanBalance)
    {
        if (loanBalance == 0m)
        {
            return 0m;
        }

        if (Value == 0m)
        {
            return decimal.MaxValue;
        }

        return loanBalance / Value * 100m;
    }
}
