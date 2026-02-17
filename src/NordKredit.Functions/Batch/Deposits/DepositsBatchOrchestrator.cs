namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Orchestrates the deposits batch pipeline: interest accrual → statement generation.
/// Replaces JCL job chain for deposit interest calculation and monthly statement batch.
/// Coordinates step execution order, handles errors, and monitors SLA compliance (06:00 deadline).
/// COBOL source: JCL job chain nightly schedule.
/// Business rules: DEP-BR-004 (interest accrual), DEP-BR-001 (account data).
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management),
///              FSA FFFS 2014:5 Ch. 7 (financial reporting/interest calculation).
/// </summary>
public partial class DepositsBatchOrchestrator
{
    private static readonly TimeOnly _slaDeadline = new(6, 0);

    private readonly IInterestAccrualStep _interestAccrualStep;
    private readonly IStatementGenerationStep _statementGenerationStep;
    private readonly TimeProvider _timeProvider;
    private readonly ILogger<DepositsBatchOrchestrator> _logger;

    public DepositsBatchOrchestrator(
        IInterestAccrualStep interestAccrualStep,
        IStatementGenerationStep statementGenerationStep,
        TimeProvider timeProvider,
        ILogger<DepositsBatchOrchestrator> logger)
    {
        _interestAccrualStep = interestAccrualStep;
        _statementGenerationStep = statementGenerationStep;
        _timeProvider = timeProvider;
        _logger = logger;
    }

    /// <summary>
    /// Executes the deposits batch pipeline.
    /// Steps run in order: (1) interest accrual → (2) statement generation.
    /// Unrecoverable errors halt the pipeline.
    /// </summary>
    public async Task<DepositsBatchResult> RunAsync(CancellationToken cancellationToken = default)
    {
        var startedAt = _timeProvider.GetUtcNow();
        LogPipelineStarted(_logger);

        InterestAccrualResult? interestResult = null;
        StatementGenerationResult? statementResult;

        try
        {
            // Step 1: Nightly interest accrual — DEP-BR-004
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "InterestAccrual", 1);
            interestResult = await _interestAccrualStep.RunAsync(cancellationToken);
            LogStepCompleted(_logger, "InterestAccrual", 1,
                interestResult.TotalProcessed, interestResult.AccruedCount, interestResult.SkippedCount);

            // Step 2: Monthly statement generation — FSA FFFS 2014:5 Ch. 7
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "StatementGeneration", 2);
            statementResult = await _statementGenerationStep.RunAsync(cancellationToken);
            LogStepCompleted(_logger, "StatementGeneration", 2,
                statementResult.TotalProcessed, statementResult.StatementsGenerated, statementResult.SkippedCount);
        }
        catch (OperationCanceledException)
        {
            throw;
        }
        catch (Exception ex)
        {
            var failedStep = interestResult is null ? "InterestAccrual" : "StatementGeneration";
            var completedAt = _timeProvider.GetUtcNow();
            var slaBreached = IsSlaBreached(completedAt);

            LogPipelineFailed(_logger, failedStep, ex.Message);

            if (slaBreached)
            {
                LogSlaBreached(_logger, completedAt);
            }

            return new DepositsBatchResult
            {
                Success = false,
                FailedStep = failedStep,
                ErrorMessage = ex.Message,
                InterestAccrualResult = interestResult,
                StatementGenerationResult = null,
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

        return new DepositsBatchResult
        {
            Success = true,
            InterestAccrualResult = interestResult,
            StatementGenerationResult = statementResult,
            StartedAt = startedAt,
            CompletedAt = pipelineCompletedAt,
            SlaBreached = pipelineSlaBreached
        };
    }

    private static bool IsSlaBreached(DateTimeOffset completedAt)
    {
        var completionTime = TimeOnly.FromTimeSpan(completedAt.UtcDateTime.TimeOfDay);
        return completionTime >= _slaDeadline;
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Deposits batch pipeline started (replaces JCL deposit interest/statement batch)")]
    private static partial void LogPipelineStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Pipeline step {StepName} (#{StepNumber}) started")]
    private static partial void LogStepStarted(ILogger logger, string stepName, int stepNumber);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Pipeline step {StepName} (#{StepNumber}) completed. Processed: {Processed}, Passed: {Passed}, Skipped: {Skipped}")]
    private static partial void LogStepCompleted(ILogger logger, string stepName, int stepNumber,
        int processed, int passed, int skipped);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Deposits batch pipeline completed successfully. Duration: {DurationSeconds:F1}s")]
    private static partial void LogPipelineCompleted(ILogger logger, double durationSeconds);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Deposits batch pipeline FAILED at step {StepName}: {ErrorMessage}")]
    private static partial void LogPipelineFailed(ILogger logger, string stepName, string errorMessage);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "SLA BREACH: Deposits batch pipeline completed after 06:00 deadline. Completion time: {CompletionTime}")]
    private static partial void LogSlaBreached(ILogger logger, DateTimeOffset completionTime);
}
