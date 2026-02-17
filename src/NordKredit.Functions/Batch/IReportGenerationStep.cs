namespace NordKredit.Functions.Batch;

/// <summary>
/// Step 4 of the daily batch pipeline: transaction report generation.
/// COBOL source: CBTRN03C.cbl:159-373.
/// Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility).
/// </summary>
public interface IReportGenerationStep
{
    Task<TransactionReportFunctionResult> RunAsync(
        DateTime startDate,
        DateTime endDate,
        int pageSize = 20,
        CancellationToken cancellationToken = default);
}
