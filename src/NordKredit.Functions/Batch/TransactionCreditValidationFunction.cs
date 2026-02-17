using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Batch credit limit and account expiration validation function — replaces CBTRN02C.cbl:370-422.
/// Activity function for the daily batch pipeline (step 2 after card verification).
/// Validates verified transactions against credit limit and account expiration.
/// Rejected transactions are written to the DailyRejects table with coded reasons.
/// COBOL source: CBTRN02C.cbl:370-422.
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
/// </summary>
public partial class TransactionCreditValidationFunction : ICreditValidationStep
{
    private readonly TransactionCreditValidationService _validationService;
    private readonly ILogger<TransactionCreditValidationFunction> _logger;

    public TransactionCreditValidationFunction(
        TransactionCreditValidationService validationService,
        ILogger<TransactionCreditValidationFunction> logger)
    {
        _validationService = validationService;
        _logger = logger;
    }

    /// <summary>
    /// Executes the credit limit and expiration validation batch.
    /// COBOL: CBTRN02C.cbl validation section (lines 370-422).
    /// Replaces COBOL DISPLAY with structured logging to Application Insights.
    /// Returns warning status (HasWarnings) if any rejections exist (replaces RETURN-CODE = 4).
    /// </summary>
    public async Task<TransactionCreditValidationResult> RunAsync(
        IReadOnlyList<VerifiedTransaction> verifiedTransactions,
        CancellationToken cancellationToken = default)
    {
        // COBOL: DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C — VALIDATION'
        LogBatchStarted(_logger, verifiedTransactions.Count);

        var results = await _validationService
            .ValidateTransactionsAsync(verifiedTransactions, cancellationToken);

        var validCount = results.Count(r => r.IsValid);
        var rejectedCount = results.Count(r => !r.IsValid);

        // COBOL: DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C — VALIDATION'
        LogBatchCompleted(_logger, results.Count, validCount, rejectedCount);

        return new TransactionCreditValidationResult
        {
            Results = results,
            TotalProcessed = results.Count,
            ValidCount = validCount,
            RejectedCount = rejectedCount
        };
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of execution of TransactionCreditValidationFunction (replaces CBTRN02C validation). Input count: {InputCount}")]
    private static partial void LogBatchStarted(ILogger logger, int inputCount);

    [LoggerMessage(Level = LogLevel.Information, Message = "End of execution of TransactionCreditValidationFunction. TotalProcessed: {TotalProcessed}, Valid: {Valid}, Rejected: {Rejected}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int valid, int rejected);
}
