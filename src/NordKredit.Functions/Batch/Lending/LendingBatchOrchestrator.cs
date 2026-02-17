namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Orchestrates the lending batch pipeline: amortization → collateral valuation → delinquency monitoring.
/// Coordinates step execution order, filters results between steps, handles errors,
/// and monitors SLA compliance (06:00 deadline).
/// Per ADR-001: plain C# orchestrator pattern.
/// COBOL source: Nightly lending batch job chain.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management),
///              FSA FFFS 2014:5 Ch. 6 (credit risk).
/// Business rules: LND-BR-004, LND-BR-006, LND-BR-008.
/// </summary>
public partial class LendingBatchOrchestrator
{
    private static readonly TimeOnly _slaDeadline = new(6, 0);

    private readonly IAmortizationProcessingStep _amortizationStep;
    private readonly ICollateralValuationStep _collateralValuationStep;
    private readonly IDelinquencyMonitoringStep _delinquencyMonitoringStep;
    private readonly TimeProvider _timeProvider;
    private readonly ILogger<LendingBatchOrchestrator> _logger;

    public LendingBatchOrchestrator(
        IAmortizationProcessingStep amortizationStep,
        ICollateralValuationStep collateralValuationStep,
        IDelinquencyMonitoringStep delinquencyMonitoringStep,
        TimeProvider timeProvider,
        ILogger<LendingBatchOrchestrator> logger)
    {
        _amortizationStep = amortizationStep;
        _collateralValuationStep = collateralValuationStep;
        _delinquencyMonitoringStep = delinquencyMonitoringStep;
        _timeProvider = timeProvider;
        _logger = logger;
    }

    /// <summary>
    /// Executes the complete lending batch pipeline.
    /// Steps run in order: (1) amortization processing → (2) collateral valuation →
    /// (3) delinquency monitoring.
    /// Failed loans are filtered between steps. Unrecoverable errors halt the pipeline.
    /// </summary>
    public async Task<LendingBatchResult> RunAsync(CancellationToken cancellationToken = default)
    {
        var startedAt = _timeProvider.GetUtcNow();
        LogPipelineStarted(_logger);

        AmortizationProcessingResult? amortizationResult = null;
        CollateralValuationResult? collateralResult = null;
        DelinquencyMonitoringResult? delinquencyResult = null;

        try
        {
            // Step 1: Amortization processing
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "AmortizationProcessing", 1);
            amortizationResult = await _amortizationStep.RunAsync(cancellationToken);
            LogStepCompleted(_logger, "AmortizationProcessing", 1,
                amortizationResult.TotalProcessed, amortizationResult.SuccessCount, amortizationResult.FailedCount);

            // Filter: only pass successfully processed loans to step 2
            IReadOnlyList<AmortizationProcessingResult.ProcessedLoan> successfulLoans =
                [.. amortizationResult.ProcessedLoans.Where(l => l.IsSuccess)];
            LogFilteredResults(_logger, "AmortizationProcessing", amortizationResult.ProcessedLoans.Count, successfulLoans.Count);

            // Step 2: Collateral valuation
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "CollateralValuation", 2);
            collateralResult = await _collateralValuationStep.RunAsync(successfulLoans, cancellationToken);
            LogStepCompleted(_logger, "CollateralValuation", 2,
                collateralResult.TotalProcessed, collateralResult.WithinLtvLimitCount, collateralResult.ExceedingLtvLimitCount);

            // Step 3: Delinquency monitoring (receives all valuated loans)
            cancellationToken.ThrowIfCancellationRequested();
            LogStepStarted(_logger, "DelinquencyMonitoring", 3);
            delinquencyResult = await _delinquencyMonitoringStep.RunAsync(collateralResult.ValuatedLoans, cancellationToken);
            LogStepCompleted(_logger, "DelinquencyMonitoring", 3,
                delinquencyResult.TotalProcessed, delinquencyResult.NewlyDelinquentCount, delinquencyResult.FlaggedForAmlScreeningCount);
        }
        catch (OperationCanceledException)
        {
            throw;
        }
        catch (Exception ex)
        {
            var failedStep = DetermineFailedStep(amortizationResult, collateralResult);
            var completedAt = _timeProvider.GetUtcNow();
            var slaBreached = IsSlaBreached(completedAt);

            LogPipelineFailed(_logger, failedStep, ex.Message);

            if (slaBreached)
            {
                LogSlaBreached(_logger, completedAt);
            }

            return new LendingBatchResult
            {
                Success = false,
                FailedStep = failedStep,
                ErrorMessage = ex.Message,
                AmortizationResult = amortizationResult,
                CollateralValuationResult = collateralResult,
                DelinquencyMonitoringResult = null,
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

        return new LendingBatchResult
        {
            Success = true,
            AmortizationResult = amortizationResult,
            CollateralValuationResult = collateralResult,
            DelinquencyMonitoringResult = delinquencyResult,
            StartedAt = startedAt,
            CompletedAt = pipelineCompletedAt,
            SlaBreached = pipelineSlaBreached
        };
    }

    private static string DetermineFailedStep(
        AmortizationProcessingResult? amortization,
        CollateralValuationResult? collateral)
    {
        if (amortization is null)
        {
            return "AmortizationProcessing";
        }

        if (collateral is null)
        {
            return "CollateralValuation";
        }

        return "DelinquencyMonitoring";
    }

    private static bool IsSlaBreached(DateTimeOffset completedAt)
    {
        var completionTime = TimeOnly.FromTimeSpan(completedAt.UtcDateTime.TimeOfDay);
        return completionTime >= _slaDeadline;
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Lending batch pipeline started (amortization → collateral valuation → delinquency monitoring)")]
    private static partial void LogPipelineStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Lending pipeline step {StepName} (#{StepNumber}) started")]
    private static partial void LogStepStarted(ILogger logger, string stepName, int stepNumber);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Lending pipeline step {StepName} (#{StepNumber}) completed. Processed: {Processed}, Passed: {Passed}, Flagged: {Flagged}")]
    private static partial void LogStepCompleted(ILogger logger, string stepName, int stepNumber,
        int processed, int passed, int flagged);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Filtered results after {StepName}: {InputCount} → {OutputCount} passed to next step")]
    private static partial void LogFilteredResults(ILogger logger, string stepName, int inputCount, int outputCount);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Lending batch pipeline completed successfully. Duration: {DurationSeconds:F1}s")]
    private static partial void LogPipelineCompleted(ILogger logger, double durationSeconds);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Lending batch pipeline FAILED at step {StepName}: {ErrorMessage}")]
    private static partial void LogPipelineFailed(ILogger logger, string stepName, string errorMessage);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "SLA BREACH: Lending batch pipeline completed after 06:00 deadline. Completion time: {CompletionTime}")]
    private static partial void LogSlaBreached(ILogger logger, DateTimeOffset completionTime);
}
