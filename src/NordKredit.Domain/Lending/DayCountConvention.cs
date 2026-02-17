namespace NordKredit.Domain.Lending;

/// <summary>
/// Day count convention for interest calculation.
/// COBOL source: Dedicated program not yet in repository (inferred from ACCT-GROUP-ID).
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 19 (APR calculation).
/// </summary>
public enum DayCountConvention
{
    /// <summary>Actual/360 — actual days elapsed divided by 360. Common for SEK lending.</summary>
    Actual360,

    /// <summary>30/360 — assumes 30 days per month, 360 days per year. Used for monthly calculations.</summary>
    Thirty360
}
