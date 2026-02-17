using NordKredit.Domain.Transactions;

namespace NordKredit.Functions.Batch;

/// <summary>
/// Orchestrates the daily batch pipeline: verification → validation → posting → reporting.
/// Replaces JCL job chain: CBTRN01C → CBTRN02C → CBTRN03C.
/// Coordinates step execution order, filters results between steps, handles errors,
/// and monitors SLA compliance (06:00 deadline).
/// COBOL source: JCL job chain nightly schedule.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// Business rules: TRN-BR-005 through TRN-BR-009 (pipeline integration).
/// </summary>
public partial class DailyBatchOrchestrator
{
    private static readonly TimeOnly _slaDeadline = new(6, 0);

    private readonly ICardVerificationStep _verificationStep;
    private readonly ICreditValidationStep _validationStep;
    private readonly ITransactionPostingStep _postingStep;
    private readonly IReportGenerationStep _reportStep;
    private readonly TimeProvider _timeProvider;
    private readonly ILogger<DailyBatchOrchestrator> _logger;

    public DailyBatchOrchestrator(
        ICardVerificationStep verificationStep,
        ICreditValidationStep validationStep,
        ITransactionPostingStep postingStep,
        IReportGenerationStep reportStep,
        TimeProvider timeProvider,
        ILogger<DailyBatchOrchestrator> logger)
    {
        _verificationStep = verificationStep;
        _validationStep = validationStep;
        _postingStep = postingStep;
        _reportStep = reportStep;
        _timeProvider = timeProvider;
        _logger = logger;
    }

    /// <summary>
    /// Executes the complete daily batch pipeline.
    /// Steps run in order: (1) card verification → (2) credit/expiration validation →
    /// (3) transaction posting → (4) report generation.
    /// Failed transactions are filtered between steps. Unrecoverable errors halt the pipeline.
    /// </summary>
    public async Task<DailyBatchResult> RunAsync(CancellationToken cancellationToken = default)
    {
        var startedAt = _timeProvider.GetUtcNow();
        LogPipelineStarted(_logger);

        CardVerificationResult? verificationResult = null;
        TransactionCreditValidationResult? validationResult = null;
        TransactionPostingResult? postingResult = null;
        TransactionReportFunctionResult? reportResult = null;

        try
        {
            // Step 1: Card verification (CBTRN01C)
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "CardVerification", 1);
            verificationResult = await _verificationStep.RunAsync(cancellationToken);
            LogStepCompleted(_logger, "CardVerification", 1,
                verificationResult.TotalProcessed, verificationResult.VerifiedCount, verificationResult.FailedCount);

            // Filter: only pass verified transactions to step 2
            IReadOnlyList<VerifiedTransaction> verifiedTransactions =
                [.. verificationResult.Results.Where(r => r.IsVerified)];
            LogFilteredResults(_logger, "CardVerification", verificationResult.Results.Count, verifiedTransactions.Count);

            // Step 2: Credit limit / expiration validation (CBTRN02C validation)
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "CreditValidation", 2);
            validationResult = await _validationStep.RunAsync(verifiedTransactions, cancellationToken);
            LogStepCompleted(_logger, "CreditValidation", 2,
                validationResult.TotalProcessed, validationResult.ValidCount, validationResult.RejectedCount);

            // Filter: only pass valid transactions to step 3
            IReadOnlyList<ValidatedTransaction> validTransactions =
                [.. validationResult.Results.Where(r => r.IsValid)];
            LogFilteredResults(_logger, "CreditValidation", validationResult.Results.Count, validTransactions.Count);

            // Step 3: Transaction posting (CBTRN02C posting)
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "TransactionPosting", 3);
            postingResult = await _postingStep.RunAsync(validTransactions, cancellationToken);
            LogStepCompleted(_logger, "TransactionPosting", 3,
                postingResult.TotalProcessed, postingResult.PostedCount, postingResult.FailedCount);

            // Step 4: Report generation (CBTRN03C)
            cancellationToken.ThrowIfCancellationRequested();
            var reportDate = startedAt.UtcDateTime.Date;
            LogStepStarted(_logger, "ReportGeneration", 4);
            reportResult = await _reportStep.RunAsync(reportDate, reportDate, 20, cancellationToken);
            LogStepCompleted(_logger, "ReportGeneration", 4,
                reportResult.TotalTransactions, reportResult.PageCount, reportResult.AccountGroupCount);
        }
        catch (OperationCanceledException)
        {
            throw;
        }
        catch (Exception ex)
        {
            var failedStep = DetermineFailedStep(verificationResult, validationResult, postingResult);
            var completedAt = _timeProvider.GetUtcNow();
            var slaBreached = IsSlaBreached(completedAt);

            LogPipelineFailed(_logger, failedStep, ex.Message);

            if (slaBreached)
            {
                LogSlaBreached(_logger, completedAt);
            }

            return new DailyBatchResult
            {
                Success = false,
                FailedStep = failedStep,
                ErrorMessage = ex.Message,
                CardVerificationResult = verificationResult,
                CreditValidationResult = validationResult,
                PostingResult = postingResult,
                ReportResult = null,
                StartedAt = startedAt,
                CompletedAt = completedAt,
                SlaBreached = slaBreached
            };
        }

        var pipelineCompletedAt = _timeProvider.GetUtcNow();
        var pipelineSlaBreached = IsSlaBreached(pipelineCompletedAt);
        var duration = pipelineCompletedAt - startedAt;

        LogPipelineCompleted(_logger, duration.TotalSeconds);

        if (pipelineSlaBreached)
        {
            LogSlaBreached(_logger, pipelineCompletedAt);
        }

        return new DailyBatchResult
        {
            Success = true,
            CardVerificationResult = verificationResult,
            CreditValidationResult = validationResult,
            PostingResult = postingResult,
            ReportResult = reportResult,
            StartedAt = startedAt,
            CompletedAt = pipelineCompletedAt,
            SlaBreached = pipelineSlaBreached
        };
    }

    private static string DetermineFailedStep(
        CardVerificationResult? verification,
        TransactionCreditValidationResult? validation,
        TransactionPostingResult? posting)
    {
        if (verification is null)
        {
            return "CardVerification";
        }

        if (validation is null)
        {
            return "CreditValidation";
        }

        if (posting is null)
        {
            return "TransactionPosting";
        }

        return "ReportGeneration";
    }

    private static bool IsSlaBreached(DateTimeOffset completedAt)
    {
        var completionTime = TimeOnly.FromTimeSpan(completedAt.UtcDateTime.TimeOfDay);
        return completionTime >= _slaDeadline;
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Daily batch pipeline started (replaces JCL job chain CBTRN01C → CBTRN02C → CBTRN03C)")]
    private static partial void LogPipelineStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Pipeline step {StepName} (#{StepNumber}) started")]
    private static partial void LogStepStarted(ILogger logger, string stepName, int stepNumber);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Pipeline step {StepName} (#{StepNumber}) completed. Processed: {Processed}, Passed: {Passed}, Failed: {Failed}")]
    private static partial void LogStepCompleted(ILogger logger, string stepName, int stepNumber,
        int processed, int passed, int failed);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Filtered results after {StepName}: {InputCount} → {OutputCount} passed to next step")]
    private static partial void LogFilteredResults(ILogger logger, string stepName, int inputCount, int outputCount);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Daily batch pipeline completed successfully. Duration: {DurationSeconds:F1}s")]
    private static partial void LogPipelineCompleted(ILogger logger, double durationSeconds);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Daily batch pipeline FAILED at step {StepName}: {ErrorMessage}")]
    private static partial void LogPipelineFailed(ILogger logger, string stepName, string errorMessage);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "SLA BREACH: Daily batch pipeline completed after 06:00 deadline. Completion time: {CompletionTime}")]
    private static partial void LogSlaBreached(ILogger logger, DateTimeOffset completionTime);
}
