namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Result of the collateral valuation batch step.
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8 (assets), CRR Art. 194-217.
/// </summary>
public class CollateralValuationResult
{
    /// <summary>Loans with updated collateral/LTV information.</summary>
    public required IReadOnlyList<ValuatedLoan> ValuatedLoans { get; init; }

    /// <summary>Total number of loans evaluated.</summary>
    public required int TotalProcessed { get; init; }

    /// <summary>Number of loans with acceptable LTV ratios.</summary>
    public required int WithinLtvLimitCount { get; init; }

    /// <summary>Number of loans exceeding LTV limits (flagged for review).</summary>
    public required int ExceedingLtvLimitCount { get; init; }

    /// <summary>Number of collateral records with stale valuations.</summary>
    public required int StaleValuationCount { get; init; }

    public bool HasWarnings => ExceedingLtvLimitCount > 0 || StaleValuationCount > 0;

    /// <summary>
    /// A loan with its collateral valuation assessment.
    /// </summary>
    public class ValuatedLoan
    {
        public required string AccountId { get; init; }
        public required decimal CurrentBalance { get; init; }
        public required decimal TotalCollateralValue { get; init; }
        public required decimal LtvRatio { get; init; }
        public required bool IsWithinLtvLimit { get; init; }
        public required bool HasStaleValuation { get; init; }
    }
}
