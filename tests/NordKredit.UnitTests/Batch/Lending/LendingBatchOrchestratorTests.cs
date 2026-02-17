using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Functions.Batch.Lending;

namespace NordKredit.UnitTests.Batch.Lending;

/// <summary>
/// Unit tests for LendingBatchOrchestrator — lending batch pipeline coordinator.
/// Per ADR-001: plain C# orchestrator pattern with SLA monitoring.
/// Verifies step sequencing, result filtering, error handling, and SLA monitoring.
/// Business rules: LND-BR-004, LND-BR-006, LND-BR-008.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// </summary>
public class LendingBatchOrchestratorTests
{
    private readonly StubAmortizationStep _amortizationStep = new();
    private readonly StubCollateralValuationStep _collateralStep = new();
    private readonly StubDelinquencyMonitoringStep _delinquencyStep = new();
    private readonly FakeLendingTimeProvider _timeProvider = new(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero));

    private LendingBatchOrchestrator CreateOrchestrator() => new(
        _amortizationStep,
        _collateralStep,
        _delinquencyStep,
        _timeProvider,
        NullLogger<LendingBatchOrchestrator>.Instance);

    // ===================================================================
    // AC-1: Steps execute in order: amortization → collateral → delinquency
    // ===================================================================

    [Fact]
    public async Task RunAsync_AllStepsSucceed_ReturnsSuccessWithAllResults()
    {
        _amortizationStep.Result = CreateAmortizationResult(5, 4, 1, 0);
        _collateralStep.Result = CreateCollateralResult(4, 3, 1, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(4, 1, 1, 2);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
        Assert.Null(result.FailedStep);
        Assert.Null(result.ErrorMessage);
        Assert.NotNull(result.AmortizationResult);
        Assert.NotNull(result.CollateralValuationResult);
        Assert.NotNull(result.DelinquencyMonitoringResult);
    }

    // ===================================================================
    // AC-2: Only successful loans passed from step 1 to step 2
    // ===================================================================

    [Fact]
    public async Task RunAsync_MixedAmortization_OnlySuccessfulPassedToCollateral()
    {
        var successful = CreateProcessedLoan("00000000001", true);
        var failed = CreateProcessedLoan("00000000002", false);
        _amortizationStep.Result = new AmortizationProcessingResult
        {
            ProcessedLoans = [successful, failed],
            TotalProcessed = 2,
            SuccessCount = 1,
            SkippedCount = 0,
            FailedCount = 1,
            TotalInterestAccrued = 16.67m
        };
        _collateralStep.Result = CreateCollateralResult(1, 1, 0, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(1, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        await orchestrator.RunAsync();

        Assert.Single(_collateralStep.ReceivedInput);
        Assert.Equal("00000000001", _collateralStep.ReceivedInput[0].AccountId);
    }

    // ===================================================================
    // AC-3: All valuated loans passed from step 2 to step 3
    // ===================================================================

    [Fact]
    public async Task RunAsync_AllValuatedLoans_PassedToDelinquency()
    {
        _amortizationStep.Result = CreateAmortizationResult(2, 2, 0, 0);
        var v1 = CreateValuatedLoan("00000000001", true);
        var v2 = CreateValuatedLoan("00000000002", false);
        _collateralStep.Result = new CollateralValuationResult
        {
            ValuatedLoans = [v1, v2],
            TotalProcessed = 2,
            WithinLtvLimitCount = 1,
            ExceedingLtvLimitCount = 1,
            StaleValuationCount = 0
        };
        _delinquencyStep.Result = CreateDelinquencyResult(2, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        await orchestrator.RunAsync();

        Assert.Equal(2, _delinquencyStep.ReceivedInput.Count);
    }

    // ===================================================================
    // AC-4: Failure in step 1 — pipeline halts
    // ===================================================================

    [Fact]
    public async Task RunAsync_AmortizationFails_HaltsWithError()
    {
        _amortizationStep.ThrowOnRun = new InvalidOperationException("Loan repository unavailable");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("AmortizationProcessing", result.FailedStep);
        Assert.Equal("Loan repository unavailable", result.ErrorMessage);
        Assert.Null(result.AmortizationResult);
        Assert.Null(result.CollateralValuationResult);
        Assert.Null(result.DelinquencyMonitoringResult);
    }

    // ===================================================================
    // AC-4: Failure in step 2 — preserves step 1
    // ===================================================================

    [Fact]
    public async Task RunAsync_CollateralFails_HaltsWithErrorAndPreservesStep1()
    {
        _amortizationStep.Result = CreateAmortizationResult(3, 3, 0, 0);
        _collateralStep.ThrowOnRun = new InvalidOperationException("Collateral lookup failed");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("CollateralValuation", result.FailedStep);
        Assert.NotNull(result.AmortizationResult);
        Assert.Null(result.CollateralValuationResult);
        Assert.Null(result.DelinquencyMonitoringResult);
    }

    // ===================================================================
    // AC-4: Failure in step 3 — preserves steps 1 and 2
    // ===================================================================

    [Fact]
    public async Task RunAsync_DelinquencyFails_HaltsWithErrorAndPreservesSteps1And2()
    {
        _amortizationStep.Result = CreateAmortizationResult(3, 3, 0, 0);
        _collateralStep.Result = CreateCollateralResult(3, 2, 1, 0);
        _delinquencyStep.ThrowOnRun = new InvalidOperationException("Status transition failed");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("DelinquencyMonitoring", result.FailedStep);
        Assert.NotNull(result.AmortizationResult);
        Assert.NotNull(result.CollateralValuationResult);
        Assert.Null(result.DelinquencyMonitoringResult);
    }

    // ===================================================================
    // AC-5: SLA compliance — pipeline finishes before 06:00
    // ===================================================================

    [Fact]
    public async Task RunAsync_CompletesBeforeDeadline_SlaNotBreached()
    {
        _amortizationStep.Result = CreateAmortizationResult(1, 1, 0, 0);
        _collateralStep.Result = CreateCollateralResult(1, 1, 0, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(1, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.SlaBreached);
    }

    // ===================================================================
    // AC-5: SLA breach — pipeline finishes after 06:00
    // ===================================================================

    [Fact]
    public async Task RunAsync_CompletesAfterDeadline_SlaBreached()
    {
        _timeProvider.SetUtcNow(new DateTimeOffset(2026, 1, 16, 5, 59, 0, TimeSpan.Zero));
        _amortizationStep.Result = CreateAmortizationResult(1, 1, 0, 0);
        _amortizationStep.OnRun = () => _timeProvider.Advance(TimeSpan.FromMinutes(5));
        _collateralStep.Result = CreateCollateralResult(1, 1, 0, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(1, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.SlaBreached);
    }

    // ===================================================================
    // Timing — StartedAt and CompletedAt are recorded
    // ===================================================================

    [Fact]
    public async Task RunAsync_RecordsTiming()
    {
        _amortizationStep.Result = CreateAmortizationResult(1, 1, 0, 0);
        _collateralStep.Result = CreateCollateralResult(1, 1, 0, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(1, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.Equal(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero), result.StartedAt);
        Assert.True(result.CompletedAt >= result.StartedAt);
    }

    // ===================================================================
    // Empty pipeline — no loans to process, still succeeds
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoLoans_CompletesSuccessfully()
    {
        _amortizationStep.Result = CreateAmortizationResult(0, 0, 0, 0);
        _collateralStep.Result = CreateCollateralResult(0, 0, 0, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(0, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
    }

    // ===================================================================
    // All amortizations fail → empty set to collateral valuation
    // ===================================================================

    [Fact]
    public async Task RunAsync_AllAmortizationsFail_EmptySetToCollateral()
    {
        _amortizationStep.Result = new AmortizationProcessingResult
        {
            ProcessedLoans = [CreateProcessedLoan("00000000001", false)],
            TotalProcessed = 1,
            SuccessCount = 0,
            SkippedCount = 0,
            FailedCount = 1,
            TotalInterestAccrued = 0m
        };
        _collateralStep.Result = CreateCollateralResult(0, 0, 0, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(0, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
        Assert.Empty(_collateralStep.ReceivedInput);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        _amortizationStep.Result = CreateAmortizationResult(1, 1, 0, 0);

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var orchestrator = CreateOrchestrator();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => orchestrator.RunAsync(cts.Token));
    }

    // ===================================================================
    // Timing on failure — StartedAt and CompletedAt still recorded
    // ===================================================================

    [Fact]
    public async Task RunAsync_FailedStep_StillRecordsTiming()
    {
        _amortizationStep.ThrowOnRun = new InvalidOperationException("fail");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.Equal(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero), result.StartedAt);
        Assert.True(result.CompletedAt >= result.StartedAt);
    }

    // ===================================================================
    // Duration computed correctly
    // ===================================================================

    [Fact]
    public async Task RunAsync_Duration_CalculatedFromTiming()
    {
        _amortizationStep.Result = CreateAmortizationResult(1, 1, 0, 0);
        _amortizationStep.OnRun = () => _timeProvider.Advance(TimeSpan.FromSeconds(30));
        _collateralStep.Result = CreateCollateralResult(1, 1, 0, 0);
        _delinquencyStep.Result = CreateDelinquencyResult(1, 0, 0, 0);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Duration >= TimeSpan.FromSeconds(30));
    }

    // ===================================================================
    // SLA breach on failure
    // ===================================================================

    [Fact]
    public async Task RunAsync_FailureAfterDeadline_SlaBreached()
    {
        _timeProvider.SetUtcNow(new DateTimeOffset(2026, 1, 16, 6, 30, 0, TimeSpan.Zero));
        _amortizationStep.ThrowOnRun = new InvalidOperationException("fail");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.True(result.SlaBreached);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static AmortizationProcessingResult CreateAmortizationResult(
        int total, int success, int skipped, int failed)
    {
        var loans = new List<AmortizationProcessingResult.ProcessedLoan>();
        for (var i = 0; i < success; i++)
        {
            loans.Add(CreateProcessedLoan($"S{i:D11}", true));
        }

        for (var i = 0; i < failed; i++)
        {
            loans.Add(CreateProcessedLoan($"F{i:D11}", false));
        }

        return new AmortizationProcessingResult
        {
            ProcessedLoans = loans,
            TotalProcessed = total,
            SuccessCount = success,
            SkippedCount = skipped,
            FailedCount = failed,
            TotalInterestAccrued = success * 16.67m
        };
    }

    private static CollateralValuationResult CreateCollateralResult(
        int total, int withinLtv, int exceedingLtv, int staleValuations)
    {
        var loans = new List<CollateralValuationResult.ValuatedLoan>();
        for (var i = 0; i < withinLtv; i++)
        {
            loans.Add(CreateValuatedLoan($"W{i:D11}", true));
        }

        for (var i = 0; i < exceedingLtv; i++)
        {
            loans.Add(CreateValuatedLoan($"E{i:D11}", false));
        }

        return new CollateralValuationResult
        {
            ValuatedLoans = loans,
            TotalProcessed = total,
            WithinLtvLimitCount = withinLtv,
            ExceedingLtvLimitCount = exceedingLtv,
            StaleValuationCount = staleValuations
        };
    }

    private static DelinquencyMonitoringResult CreateDelinquencyResult(
        int total, int newlyDelinquent, int flaggedForAml, int elevatedRisk) => new()
        {
            TotalProcessed = total,
            NewlyDelinquentCount = newlyDelinquent,
            FlaggedForAmlScreeningCount = flaggedForAml,
            ElevatedRiskCount = elevatedRisk
        };

    private static AmortizationProcessingResult.ProcessedLoan CreateProcessedLoan(
        string accountId, bool isSuccess) => new()
        {
            AccountId = accountId,
            InterestAmount = isSuccess ? 16.67m : 0m,
            CurrentBalance = isSuccess ? 100_016.67m : 100_000m,
            IsSuccess = isSuccess,
            FailureReason = isSuccess ? null : "Processing failed"
        };

    private static CollateralValuationResult.ValuatedLoan CreateValuatedLoan(
        string accountId, bool isWithinLimit) => new()
        {
            AccountId = accountId,
            CurrentBalance = 500_000m,
            TotalCollateralValue = 700_000m,
            LtvRatio = isWithinLimit ? 71.43m : 90.00m,
            IsWithinLtvLimit = isWithinLimit,
            HasStaleValuation = false
        };
}

// ===================================================================
// Test doubles — stub implementations for lending orchestrator step interfaces
// ===================================================================

internal sealed class StubAmortizationStep : IAmortizationProcessingStep
{
    public AmortizationProcessingResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public Action? OnRun { get; set; }

    public Task<AmortizationProcessingResult> RunAsync(CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        OnRun?.Invoke();
        if (ThrowOnRun is not null)
        {
            throw ThrowOnRun;
        }

        return Task.FromResult(Result!);
    }
}

internal sealed class StubCollateralValuationStep : ICollateralValuationStep
{
    public CollateralValuationResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public IReadOnlyList<AmortizationProcessingResult.ProcessedLoan> ReceivedInput { get; private set; } = [];

    public Task<CollateralValuationResult> RunAsync(
        IReadOnlyList<AmortizationProcessingResult.ProcessedLoan> processedLoans,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        ReceivedInput = processedLoans;
        if (ThrowOnRun is not null)
        {
            throw ThrowOnRun;
        }

        return Task.FromResult(Result!);
    }
}

internal sealed class StubDelinquencyMonitoringStep : IDelinquencyMonitoringStep
{
    public DelinquencyMonitoringResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public IReadOnlyList<CollateralValuationResult.ValuatedLoan> ReceivedInput { get; private set; } = [];

    public Task<DelinquencyMonitoringResult> RunAsync(
        IReadOnlyList<CollateralValuationResult.ValuatedLoan> valuatedLoans,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        ReceivedInput = valuatedLoans;
        if (ThrowOnRun is not null)
        {
            throw ThrowOnRun;
        }

        return Task.FromResult(Result!);
    }
}

/// <summary>
/// Fake TimeProvider for testing time-dependent SLA checks in lending batch.
/// </summary>
internal sealed class FakeLendingTimeProvider : TimeProvider
{
    private DateTimeOffset _utcNow;

    public FakeLendingTimeProvider(DateTimeOffset utcNow)
    {
        _utcNow = utcNow;
    }

    public override DateTimeOffset GetUtcNow() => _utcNow;

    public void SetUtcNow(DateTimeOffset value) => _utcNow = value;

    public void Advance(TimeSpan duration) => _utcNow += duration;
}
