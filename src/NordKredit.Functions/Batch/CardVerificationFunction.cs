using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Batch daily transaction card verification function — replaces CBTRN01C.cbl.
/// Activity function for the daily batch pipeline (Durable Functions orchestration).
/// Reads daily transaction records, verifies each card number against the
/// cross-reference file, and confirms the linked account exists.
/// Verification only — no balance updates (CBTRN02C handles posting).
/// COBOL source: CBTRN01C.cbl:154-250.
/// Regulations: PSD2 Art.97 (transaction authorization), FFFS 2014:5 Ch.4 §3
/// (operational risk), AML/KYC (source verification).
/// </summary>
public partial class CardVerificationFunction : ICardVerificationStep
{
    private readonly CardVerificationService _cardVerificationService;
    private readonly ILogger<CardVerificationFunction> _logger;

    public CardVerificationFunction(
        CardVerificationService cardVerificationService,
        ILogger<CardVerificationFunction> logger)
    {
        _cardVerificationService = cardVerificationService;
        _logger = logger;
    }

    /// <summary>
    /// Executes the daily card verification batch.
    /// COBOL: CBTRN01C.cbl MAIN-PARA (lines 154-196).
    /// Replaces COBOL DISPLAY with structured logging to Application Insights.
    /// Replaces ABEND 999 with proper exception propagation.
    /// </summary>
    public async Task<CardVerificationResult> RunAsync(CancellationToken cancellationToken = default)
    {
        // COBOL: DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN01C'
        LogBatchStarted(_logger);

        var results = await _cardVerificationService
            .VerifyDailyTransactionsAsync(cancellationToken);

        var verifiedCount = results.Count(r => r.IsVerified);
        var failedCount = results.Count(r => !r.IsVerified);

        // COBOL: DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN01C'
        LogBatchCompleted(_logger, results.Count, verifiedCount, failedCount);

        return new CardVerificationResult
        {
            Results = results,
            TotalProcessed = results.Count,
            VerifiedCount = verifiedCount,
            FailedCount = failedCount
        };
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of execution of CardVerificationFunction (replaces CBTRN01C)")]
    private static partial void LogBatchStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information, Message = "End of execution of CardVerificationFunction. TotalProcessed: {TotalProcessed}, Verified: {Verified}, Failed: {Failed}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int verified, int failed);
}
