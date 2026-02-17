using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Functions.Batch.Deposits;

namespace NordKredit.UnitTests.Batch.Deposits;

/// <summary>
/// Unit tests for DepositsBatchOrchestrator — deposits batch pipeline coordinator.
/// Replaces JCL job chain for deposit interest and statement processing.
/// Verifies step sequencing, error handling, and SLA monitoring.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// Business rules: DEP-BR-004 (interest accrual), DEP-BR-001 (account data).
/// </summary>
public class DepositsBatchOrchestratorTests
{
    private readonly StubInterestAccrualStep _interestStep = new();
    private readonly StubStatementGenerationStep _statementStep = new();
    private readonly FakeTimeProvider _timeProvider = new(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero));

    private DepositsBatchOrchestrator CreateOrchestrator() => new(
        _interestStep,
        _statementStep,
        _timeProvider,
        NullLogger<DepositsBatchOrchestrator>.Instance);

    // ===================================================================
    // AC-1: Both steps succeed → returns success with all results
    // ===================================================================

    [Fact]
    public async Task RunAsync_AllStepsSucceed_ReturnsSuccessWithAllResults()
    {
        _interestStep.Result = CreateInterestResult(10, 8, 1, 1, 5.4795m);
        _statementStep.Result = CreateStatementResult(10, 8, 2, 6, 32.877m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
        Assert.Null(result.FailedStep);
        Assert.Null(result.ErrorMessage);
        Assert.NotNull(result.InterestAccrualResult);
        Assert.NotNull(result.StatementGenerationResult);
    }

    // ===================================================================
    // AC-2: Interest accrual step fails → pipeline halts
    // ===================================================================

    [Fact]
    public async Task RunAsync_InterestStepFails_HaltsWithError()
    {
        _interestStep.ThrowOnRun = new InvalidOperationException("Deposit account source is unavailable");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("InterestAccrual", result.FailedStep);
        Assert.Equal("Deposit account source is unavailable", result.ErrorMessage);
        Assert.Null(result.InterestAccrualResult);
        Assert.Null(result.StatementGenerationResult);
    }

    // ===================================================================
    // AC-3: Statement generation step fails → halts, preserves step 1
    // ===================================================================

    [Fact]
    public async Task RunAsync_StatementStepFails_HaltsWithErrorPreservesStep1()
    {
        _interestStep.Result = CreateInterestResult(10, 8, 1, 1, 5.4795m);
        _statementStep.ThrowOnRun = new InvalidOperationException("Statement generation failed");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("StatementGeneration", result.FailedStep);
        Assert.NotNull(result.InterestAccrualResult);
        Assert.Null(result.StatementGenerationResult);
    }

    // ===================================================================
    // AC-4: SLA compliance — pipeline finishes before 06:00
    // Batch SLA: nightly batch must complete by 06:00
    // ===================================================================

    [Fact]
    public async Task RunAsync_CompletesBeforeDeadline_SlaNotBreached()
    {
        _interestStep.Result = CreateInterestResult(1, 1, 0, 0, 0.6849m);
        _statementStep.Result = CreateStatementResult(1, 1, 0, 1, 0.6849m);

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
        _interestStep.Result = CreateInterestResult(1, 1, 0, 0, 0.6849m);
        _interestStep.OnRun = () => _timeProvider.Advance(TimeSpan.FromMinutes(5));
        _statementStep.Result = CreateStatementResult(1, 1, 0, 1, 0.6849m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.SlaBreached);
    }

    // ===================================================================
    // AC-6: SLA breach on failure path
    // ===================================================================

    [Fact]
    public async Task RunAsync_FailsAfterDeadline_SlaBreached()
    {
        _timeProvider.SetUtcNow(new DateTimeOffset(2026, 1, 16, 6, 30, 0, TimeSpan.Zero));
        _interestStep.ThrowOnRun = new InvalidOperationException("fail");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.True(result.SlaBreached);
    }

    // ===================================================================
    // Timing — StartedAt and CompletedAt are recorded
    // ===================================================================

    [Fact]
    public async Task RunAsync_RecordsTiming()
    {
        _interestStep.Result = CreateInterestResult(1, 1, 0, 0, 0.6849m);
        _statementStep.Result = CreateStatementResult(1, 1, 0, 1, 0.6849m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.Equal(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero), result.StartedAt);
        Assert.True(result.CompletedAt >= result.StartedAt);
    }

    // ===================================================================
    // Timing on failure — still recorded
    // ===================================================================

    [Fact]
    public async Task RunAsync_FailedStep_StillRecordsTiming()
    {
        _interestStep.ThrowOnRun = new InvalidOperationException("fail");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.Equal(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero), result.StartedAt);
        Assert.True(result.CompletedAt >= result.StartedAt);
    }

    // ===================================================================
    // Empty pipeline — no accounts to process, still succeeds
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoAccounts_CompletesSuccessfully()
    {
        _interestStep.Result = CreateInterestResult(0, 0, 0, 0, 0m);
        _statementStep.Result = CreateStatementResult(0, 0, 0, 0, 0m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        _interestStep.Result = CreateInterestResult(1, 1, 0, 0, 0.6849m);

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var orchestrator = CreateOrchestrator();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => orchestrator.RunAsync(cts.Token));
    }

    // ===================================================================
    // Duration calculation
    // ===================================================================

    [Fact]
    public async Task RunAsync_DurationCalculatedCorrectly()
    {
        _interestStep.Result = CreateInterestResult(1, 1, 0, 0, 0.6849m);
        _interestStep.OnRun = () => _timeProvider.Advance(TimeSpan.FromMinutes(10));
        _statementStep.Result = CreateStatementResult(1, 1, 0, 1, 0.6849m);
        _statementStep.OnRun = () => _timeProvider.Advance(TimeSpan.FromMinutes(5));

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.Equal(TimeSpan.FromMinutes(15), result.Duration);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static InterestAccrualResult CreateInterestResult(
        int total, int accrued, int skipped, int failed, decimal totalInterest) =>
        new()
        {
            TotalProcessed = total,
            AccruedCount = accrued,
            SkippedCount = skipped,
            FailedCount = failed,
            TotalInterestAccrued = totalInterest
        };

    private static StatementGenerationResult CreateStatementResult(
        int total, int generated, int skipped, int interestPosted, decimal totalInterestPosted) =>
        new()
        {
            TotalProcessed = total,
            StatementsGenerated = generated,
            SkippedCount = skipped,
            InterestPostedCount = interestPosted,
            TotalInterestPosted = totalInterestPosted
        };
}

// ===================================================================
// Test doubles — stub implementations for deposits orchestrator step interfaces
// ===================================================================

internal sealed class StubInterestAccrualStep : IInterestAccrualStep
{
    public InterestAccrualResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public Action? OnRun { get; set; }

    public Task<InterestAccrualResult> RunAsync(CancellationToken cancellationToken = default)
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

internal sealed class StubStatementGenerationStep : IStatementGenerationStep
{
    public StatementGenerationResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public Action? OnRun { get; set; }

    public Task<StatementGenerationResult> RunAsync(CancellationToken cancellationToken = default)
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
