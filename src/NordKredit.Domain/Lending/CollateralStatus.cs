namespace NordKredit.Domain.Lending;

/// <summary>
/// Collateral record lifecycle status.
/// COBOL source: Inferred COLLATERAL-STATUS PIC X(01) — A=Active, R=Released, E=Expired.
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 8, GDPR Art. 5(1)(e).
/// </summary>
public enum CollateralStatus
{
    /// <summary>Collateral is active and counted in regulatory capital calculations.</summary>
    Active,

    /// <summary>Collateral has been released (loan paid off). No longer counted in capital calculations.</summary>
    Released,

    /// <summary>Collateral has expired (e.g., guarantee past validity date).</summary>
    Expired
}
