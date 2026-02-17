namespace NordKredit.Domain.Lending;

/// <summary>
/// Collateral asset type classification.
/// COBOL source: Dedicated program not yet in repository.
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8, CRR Art. 194-217.
/// </summary>
public enum CollateralType
{
    /// <summary>Real estate property. Revaluation: annual. LTV limit: 85% (Finansinspektionen mortgage cap).</summary>
    RealEstate,

    /// <summary>Motor vehicle. Revaluation: annual (depreciation model). LTV limit: 80% (institution policy).</summary>
    Vehicle,

    /// <summary>Financial securities (stocks, bonds). Revaluation: daily (mark-to-market). LTV limit: 70% (volatility haircut).</summary>
    Securities,

    /// <summary>Third-party guarantee. Revaluation: at issuance + annual review. Binary: valid/expired.</summary>
    Guarantee,

    /// <summary>Other collateral types not covered above.</summary>
    Other
}
