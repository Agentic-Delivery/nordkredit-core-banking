namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Step 2 of the deposits batch pipeline: monthly statement generation.
/// COBOL source: Dedicated statement generation batch program.
/// Business rule: DEP-BR-004 (interest posting), DEP-BR-001 (account data).
/// Regulations: FSA FFFS 2014:5 Ch. 7 (financial reporting), PSD2 Art. 57.
/// </summary>
public interface IStatementGenerationStep
{
    Task<StatementGenerationResult> RunAsync(CancellationToken cancellationToken = default);
}
