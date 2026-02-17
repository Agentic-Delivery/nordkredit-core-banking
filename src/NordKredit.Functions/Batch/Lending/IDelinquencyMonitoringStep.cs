namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Step 3 of the lending batch pipeline: delinquency monitoring.
/// Identifies loans with missed payments, transitions statuses, and flags for AML screening.
/// COBOL source: Dedicated program not yet in repository (inferred from LND-BR-008).
/// Business rule: LND-BR-008 (delinquency management).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Inkassolagen 1974:182,
///              AML/KYC (delinquent accounts feed AML screening — regulatory requirement).
/// </summary>
public interface IDelinquencyMonitoringStep
{
    Task<DelinquencyMonitoringResult> RunAsync(
        IReadOnlyList<CollateralValuationResult.ValuatedLoan> valuatedLoans,
        CancellationToken cancellationToken = default);
}
