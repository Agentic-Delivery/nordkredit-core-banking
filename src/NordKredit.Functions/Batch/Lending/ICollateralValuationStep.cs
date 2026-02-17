namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Step 2 of the lending batch pipeline: collateral valuation updates.
/// Reviews collateral records, flags stale valuations, and calculates LTV ratios.
/// COBOL source: Dedicated program not yet in repository (inferred VSAM COLLATERAL file).
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8 (assets), CRR Art. 194-217 (credit risk mitigation).
/// </summary>
public interface ICollateralValuationStep
{
    Task<CollateralValuationResult> RunAsync(
        IReadOnlyList<AmortizationProcessingResult.ProcessedLoan> processedLoans,
        CancellationToken cancellationToken = default);
}
