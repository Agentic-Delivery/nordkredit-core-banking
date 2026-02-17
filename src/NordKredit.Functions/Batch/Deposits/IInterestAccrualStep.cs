namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Step 1 of the deposits batch pipeline: nightly interest accrual.
/// COBOL source: Dedicated interest calculation batch program.
/// Business rule: DEP-BR-004 (interest calculation and accrual).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6 (financial reporting/interest calculation).
/// </summary>
public interface IInterestAccrualStep
{
    Task<InterestAccrualResult> RunAsync(CancellationToken cancellationToken = default);
}
