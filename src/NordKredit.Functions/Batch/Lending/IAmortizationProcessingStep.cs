namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Step 1 of the lending batch pipeline: amortization schedule processing.
/// Processes due amortization payments for all active term loans.
/// COBOL source: Nightly interest/amortization batch (inferred from CVACT01Y.cpy).
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Consumer Credit Directive Art. 10.
/// </summary>
public interface IAmortizationProcessingStep
{
    Task<AmortizationProcessingResult> RunAsync(CancellationToken cancellationToken = default);
}
