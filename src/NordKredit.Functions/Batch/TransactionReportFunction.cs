using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Batch transaction report function — replaces CBTRN03C.cbl:159-373.
/// Activity function for the daily batch pipeline (step 3 after transaction posting).
/// Generates a detailed transaction report for a configurable date range with
/// type/category description lookups, page totals, account totals, and grand total.
/// COBOL source: CBTRN03C.cbl:159-373 (report generation).
/// Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility),
/// AML 2017:11 Para.3 (monitoring).
/// </summary>
public partial class TransactionReportFunction : IReportGenerationStep
{
    private readonly TransactionDetailReportService _reportService;
    private readonly ILogger<TransactionReportFunction> _logger;

    public TransactionReportFunction(
        TransactionDetailReportService reportService,
        ILogger<TransactionReportFunction> logger)
    {
        _reportService = reportService;
        _logger = logger;
    }

    /// <summary>
    /// Executes the daily transaction report generation.
    /// COBOL: CBTRN03C.cbl main program (lines 159-373).
    /// Replaces COBOL DISPLAY with structured logging to Application Insights.
    /// Throws on lookup failures (replaces COBOL ABEND 999).
    /// </summary>
    public async Task<TransactionReportFunctionResult> RunAsync(
        DateTime startDate,
        DateTime endDate,
        int pageSize = 20,
        CancellationToken cancellationToken = default)
    {
        // COBOL: DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN03C'
        LogBatchStarted(_logger, startDate, endDate);

        var serviceResult = await _reportService
            .GenerateReportAsync(startDate, endDate, pageSize, cancellationToken);

        // COBOL: DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN03C'
        LogBatchCompleted(_logger, serviceResult.TotalTransactions,
            serviceResult.PageCount, serviceResult.AccountGroupCount, serviceResult.GrandTotal);

        return new TransactionReportFunctionResult
        {
            TotalTransactions = serviceResult.TotalTransactions,
            DetailLineCount = serviceResult.DetailLineCount,
            PageCount = serviceResult.PageCount,
            AccountGroupCount = serviceResult.AccountGroupCount,
            GrandTotal = serviceResult.GrandTotal
        };
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Start of execution of TransactionReportFunction (replaces CBTRN03C). Date range: {StartDate:yyyy-MM-dd} to {EndDate:yyyy-MM-dd}")]
    private static partial void LogBatchStarted(ILogger logger, DateTime startDate, DateTime endDate);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "End of execution of TransactionReportFunction. Transactions: {TransactionCount}, Pages: {PageCount}, AccountGroups: {AccountGroupCount}, GrandTotal: {GrandTotal}")]
    private static partial void LogBatchCompleted(ILogger logger, int transactionCount, int pageCount, int accountGroupCount, decimal grandTotal);
}
