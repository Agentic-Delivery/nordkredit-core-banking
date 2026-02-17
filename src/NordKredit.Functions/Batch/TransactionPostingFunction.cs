using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Batch transaction posting function — replaces CBTRN02C.cbl:424-579 (posting section).
/// Activity function for the daily batch pipeline (step 3 after credit validation).
/// Posts validated transactions: upserts category balances, updates account balances,
/// and writes transaction records atomically.
/// COBOL source: CBTRN02C.cbl:424-579 (posting), CBTRN02C.cbl:467-542 (TCATBAL upsert).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public partial class TransactionPostingFunction : ITransactionPostingStep
{
    private readonly TransactionPostingService _postingService;
    private readonly ILogger<TransactionPostingFunction> _logger;

    public TransactionPostingFunction(
        TransactionPostingService postingService,
        ILogger<TransactionPostingFunction> logger)
    {
        _postingService = postingService;
        _logger = logger;
    }

    /// <summary>
    /// Executes the transaction posting batch.
    /// COBOL: CBTRN02C.cbl posting section (lines 424-579).
    /// Replaces COBOL DISPLAY with structured logging to Application Insights.
    /// Returns error status (HasErrors) if any posting failures (replaces RETURN-CODE = 8).
    /// </summary>
    public async Task<TransactionPostingResult> RunAsync(
        IReadOnlyList<ValidatedTransaction> validatedTransactions,
        CancellationToken cancellationToken = default)
    {
        // COBOL: DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C — POSTING'
        LogBatchStarted(_logger, validatedTransactions.Count);

        var serviceResult = await _postingService
            .PostTransactionsAsync(validatedTransactions, cancellationToken);

        // COBOL: DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C — POSTING'
        LogBatchCompleted(_logger, serviceResult.TotalProcessed,
            serviceResult.PostedCount, serviceResult.SkippedCount, serviceResult.FailedCount);

        return new TransactionPostingResult
        {
            TotalProcessed = serviceResult.TotalProcessed,
            PostedCount = serviceResult.PostedCount,
            SkippedCount = serviceResult.SkippedCount,
            FailedCount = serviceResult.FailedCount
        };
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of execution of TransactionPostingFunction (replaces CBTRN02C posting). Input count: {InputCount}")]
    private static partial void LogBatchStarted(ILogger logger, int inputCount);

    [LoggerMessage(Level = LogLevel.Information, Message = "End of execution of TransactionPostingFunction. TotalProcessed: {TotalProcessed}, Posted: {Posted}, Skipped: {Skipped}, Failed: {Failed}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int posted, int skipped, int failed);
}
