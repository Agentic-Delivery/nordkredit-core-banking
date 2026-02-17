using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Batch transaction posting function — replaces CBTRN02C.cbl:424-579.
/// Activity function for the daily batch pipeline (step 3 after credit validation).
/// Posts valid transactions, updates account balances, and upserts category balances.
/// COBOL source: CBTRN02C.cbl:424-579.
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public partial class TransactionPostingFunction
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
    /// Returns warning status (HasWarnings) if any transactions were skipped (replaces RETURN-CODE = 4).
    /// </summary>
    public async Task<TransactionPostingResult> RunAsync(
        IReadOnlyList<ValidatedTransaction> validatedTransactions,
        CancellationToken cancellationToken = default)
    {
        // COBOL: DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C — POSTING'
        LogBatchStarted(_logger, validatedTransactions.Count);

        var results = await _postingService
            .PostTransactionsAsync(validatedTransactions, cancellationToken);

        var postedCount = results.Count(r => r.IsPosted);
        var skippedCount = results.Count(r => !r.IsPosted);

        // COBOL: DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C — POSTING'
        LogBatchCompleted(_logger, results.Count, postedCount, skippedCount);

        return new TransactionPostingResult
        {
            Results = results,
            TotalProcessed = results.Count,
            PostedCount = postedCount,
            SkippedCount = skippedCount
        };
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of execution of TransactionPostingFunction (replaces CBTRN02C posting). Input count: {InputCount}")]
    private static partial void LogBatchStarted(ILogger logger, int inputCount);

    [LoggerMessage(Level = LogLevel.Information, Message = "End of execution of TransactionPostingFunction. TotalProcessed: {TotalProcessed}, Posted: {Posted}, Skipped: {Skipped}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int posted, int skipped);
}
