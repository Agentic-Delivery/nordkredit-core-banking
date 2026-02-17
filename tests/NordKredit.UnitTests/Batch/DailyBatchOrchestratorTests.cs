using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;
using NordKredit.Functions.Batch;

namespace NordKredit.UnitTests.Batch;

/// <summary>
/// Unit tests for DailyBatchOrchestrator — nightly batch pipeline coordinator.
/// Replaces JCL job chain: CBTRN01C → CBTRN02C → CBTRN03C.
/// Verifies step sequencing, result filtering, error handling, and SLA monitoring.
/// Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management).
/// Business rules: TRN-BR-005 through TRN-BR-009 (pipeline integration).
/// </summary>
public class DailyBatchOrchestratorTests
{
    private readonly StubCardVerificationStep _verificationStep = new();
    private readonly StubCreditValidationStep _validationStep = new();
    private readonly StubTransactionPostingStep _postingStep = new();
    private readonly StubReportStep _reportStep = new();
    private readonly FakeTimeProvider _timeProvider = new(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero));

    private DailyBatchOrchestrator CreateOrchestrator() => new(
        _verificationStep,
        _validationStep,
        _postingStep,
        _reportStep,
        _timeProvider,
        NullLogger<DailyBatchOrchestrator>.Instance);

    // ===================================================================
    // AC-1: Steps execute in order: verification → validation → posting → reporting
    // JCL chain: CBTRN01C → CBTRN02C → CBTRN03C
    // ===================================================================

    [Fact]
    public async Task RunAsync_AllStepsSucceed_ReturnsSuccessWithAllResults()
    {
        _verificationStep.Result = CreateVerificationResult(5, 4, 1);
        _validationStep.Result = CreateValidationResult(4, 3, 1);
        _postingStep.Result = CreatePostingResult(3, 3, 0, 0);
        _reportStep.Result = CreateReportResult(3, 3, 1, 1, 300.00m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
        Assert.Null(result.FailedStep);
        Assert.Null(result.ErrorMessage);
        Assert.NotNull(result.CardVerificationResult);
        Assert.NotNull(result.CreditValidationResult);
        Assert.NotNull(result.PostingResult);
        Assert.NotNull(result.ReportResult);
    }

    // ===================================================================
    // AC-2: Only verified transactions passed to step 2
    // CBTRN01C output filtering — failed verifications excluded
    // ===================================================================

    [Fact]
    public async Task RunAsync_MixedVerification_OnlyVerifiedPassedToValidation()
    {
        var verified = CreateVerifiedTransaction("TXN001", true);
        var failed = CreateVerifiedTransaction("TXN002", false);
        _verificationStep.Result = new CardVerificationResult
        {
            Results = [verified, failed],
            TotalProcessed = 2,
            VerifiedCount = 1,
            FailedCount = 1
        };
        _validationStep.Result = CreateValidationResult(1, 1, 0);
        _postingStep.Result = CreatePostingResult(1, 1, 0, 0);
        _reportStep.Result = CreateReportResult(1, 1, 1, 1, 100.00m);

        var orchestrator = CreateOrchestrator();
        await orchestrator.RunAsync();

        // Validation step should only receive the verified transaction
        Assert.Single(_validationStep.ReceivedInput);
        Assert.Equal("TXN001", _validationStep.ReceivedInput[0].Transaction.Id);
    }

    // ===================================================================
    // AC-3: Only approved transactions posted, rejected written to rejects
    // CBTRN02C validation filtering — rejected transactions excluded from posting
    // ===================================================================

    [Fact]
    public async Task RunAsync_MixedValidation_OnlyValidPassedToPosting()
    {
        _verificationStep.Result = CreateVerificationResult(2, 2, 0);
        var valid = CreateValidatedTransaction("TXN001", true);
        var rejected = CreateValidatedTransaction("TXN002", false);
        _validationStep.Result = new TransactionCreditValidationResult
        {
            Results = [valid, rejected],
            TotalProcessed = 2,
            ValidCount = 1,
            RejectedCount = 1
        };
        _postingStep.Result = CreatePostingResult(1, 1, 0, 0);
        _reportStep.Result = CreateReportResult(1, 1, 1, 1, 100.00m);

        var orchestrator = CreateOrchestrator();
        await orchestrator.RunAsync();

        // Posting step should only receive the valid transaction
        Assert.Single(_postingStep.ReceivedInput);
        Assert.Equal("TXN001", _postingStep.ReceivedInput[0].VerifiedTransaction.Transaction.Id);
    }

    // ===================================================================
    // AC-4: Unrecoverable error in step 1 — pipeline halts
    // Replaces COBOL ABEND 999
    // ===================================================================

    [Fact]
    public async Task RunAsync_VerificationStepFails_HaltsWithError()
    {
        _verificationStep.ThrowOnRun = new InvalidOperationException("Daily transaction source is unavailable");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("CardVerification", result.FailedStep);
        Assert.Equal("Daily transaction source is unavailable", result.ErrorMessage);
        Assert.Null(result.CardVerificationResult);
        Assert.Null(result.CreditValidationResult);
        Assert.Null(result.PostingResult);
        Assert.Null(result.ReportResult);
    }

    // ===================================================================
    // AC-4: Unrecoverable error in step 2 — pipeline halts
    // ===================================================================

    [Fact]
    public async Task RunAsync_ValidationStepFails_HaltsWithErrorAndPreservesStep1()
    {
        _verificationStep.Result = CreateVerificationResult(3, 3, 0);
        _validationStep.ThrowOnRun = new InvalidOperationException("Account lookup failed");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("CreditValidation", result.FailedStep);
        Assert.NotNull(result.CardVerificationResult);
        Assert.Null(result.CreditValidationResult);
        Assert.Null(result.PostingResult);
    }

    // ===================================================================
    // AC-4: Unrecoverable error in step 3 — pipeline halts
    // ===================================================================

    [Fact]
    public async Task RunAsync_PostingStepFails_HaltsWithErrorAndPreservesSteps1And2()
    {
        _verificationStep.Result = CreateVerificationResult(3, 3, 0);
        _validationStep.Result = CreateValidationResult(3, 3, 0);
        _postingStep.ThrowOnRun = new InvalidOperationException("Database connection lost");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("TransactionPosting", result.FailedStep);
        Assert.NotNull(result.CardVerificationResult);
        Assert.NotNull(result.CreditValidationResult);
        Assert.Null(result.PostingResult);
    }

    // ===================================================================
    // AC-4: Unrecoverable error in step 4 — pipeline halts
    // ===================================================================

    [Fact]
    public async Task RunAsync_ReportStepFails_HaltsWithErrorAndPreservesSteps1Through3()
    {
        _verificationStep.Result = CreateVerificationResult(3, 3, 0);
        _validationStep.Result = CreateValidationResult(3, 3, 0);
        _postingStep.Result = CreatePostingResult(3, 3, 0, 0);
        _reportStep.ThrowOnRun = new InvalidOperationException("Report generation failed");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.False(result.Success);
        Assert.Equal("ReportGeneration", result.FailedStep);
        Assert.NotNull(result.CardVerificationResult);
        Assert.NotNull(result.CreditValidationResult);
        Assert.NotNull(result.PostingResult);
        Assert.Null(result.ReportResult);
    }

    // ===================================================================
    // AC-5: SLA compliance — pipeline finishes before 06:00
    // Batch SLA: nightly batch must complete by 06:00
    // ===================================================================

    [Fact]
    public async Task RunAsync_CompletesBeforeDeadline_SlaNotBreached()
    {
        // Starts at 02:00, completes at 02:00 (instant in test)
        _verificationStep.Result = CreateVerificationResult(1, 1, 0);
        _validationStep.Result = CreateValidationResult(1, 1, 0);
        _postingStep.Result = CreatePostingResult(1, 1, 0, 0);
        _reportStep.Result = CreateReportResult(1, 1, 1, 1, 100.00m);

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
        // Start at 05:59, advance to 06:04 during execution
        _timeProvider.SetUtcNow(new DateTimeOffset(2026, 1, 16, 5, 59, 0, TimeSpan.Zero));
        _verificationStep.Result = CreateVerificationResult(1, 1, 0);
        _verificationStep.OnRun = () => _timeProvider.Advance(TimeSpan.FromMinutes(5));
        _validationStep.Result = CreateValidationResult(1, 1, 0);
        _postingStep.Result = CreatePostingResult(1, 1, 0, 0);
        _reportStep.Result = CreateReportResult(1, 1, 1, 1, 100.00m);

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
        _verificationStep.Result = CreateVerificationResult(1, 1, 0);
        _validationStep.Result = CreateValidationResult(1, 1, 0);
        _postingStep.Result = CreatePostingResult(1, 1, 0, 0);
        _reportStep.Result = CreateReportResult(1, 1, 1, 1, 100.00m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.Equal(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero), result.StartedAt);
        Assert.True(result.CompletedAt >= result.StartedAt);
    }

    // ===================================================================
    // Empty pipeline — no transactions to process, still succeeds
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoTransactions_CompletesSuccessfully()
    {
        _verificationStep.Result = CreateVerificationResult(0, 0, 0);
        _validationStep.Result = CreateValidationResult(0, 0, 0);
        _postingStep.Result = CreatePostingResult(0, 0, 0, 0);
        _reportStep.Result = CreateReportResult(0, 0, 0, 0, 0m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
    }

    // ===================================================================
    // All verifications fail — empty set passed to validation
    // ===================================================================

    [Fact]
    public async Task RunAsync_AllVerificationsFail_EmptySetToValidation()
    {
        _verificationStep.Result = new CardVerificationResult
        {
            Results = [CreateVerifiedTransaction("TXN001", false)],
            TotalProcessed = 1,
            VerifiedCount = 0,
            FailedCount = 1
        };
        _validationStep.Result = CreateValidationResult(0, 0, 0);
        _postingStep.Result = CreatePostingResult(0, 0, 0, 0);
        _reportStep.Result = CreateReportResult(0, 0, 0, 0, 0m);

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.True(result.Success);
        Assert.Empty(_validationStep.ReceivedInput);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        _verificationStep.Result = CreateVerificationResult(1, 1, 0);

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
        _verificationStep.ThrowOnRun = new InvalidOperationException("fail");

        var orchestrator = CreateOrchestrator();
        var result = await orchestrator.RunAsync();

        Assert.Equal(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero), result.StartedAt);
        Assert.True(result.CompletedAt >= result.StartedAt);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static CardVerificationResult CreateVerificationResult(int total, int verified, int failed)
    {
        var results = new List<VerifiedTransaction>();
        for (var i = 0; i < verified; i++)
        {
            results.Add(CreateVerifiedTransaction($"V{i}", true));
        }

        for (var i = 0; i < failed; i++)
        {
            results.Add(CreateVerifiedTransaction($"F{i}", false));
        }

        return new CardVerificationResult
        {
            Results = results,
            TotalProcessed = total,
            VerifiedCount = verified,
            FailedCount = failed
        };
    }

    private static TransactionCreditValidationResult CreateValidationResult(int total, int valid, int rejected)
    {
        var results = new List<ValidatedTransaction>();
        for (var i = 0; i < valid; i++)
        {
            results.Add(CreateValidatedTransaction($"V{i}", true));
        }

        for (var i = 0; i < rejected; i++)
        {
            results.Add(CreateValidatedTransaction($"R{i}", false));
        }

        return new TransactionCreditValidationResult
        {
            Results = results,
            TotalProcessed = total,
            ValidCount = valid,
            RejectedCount = rejected
        };
    }

    private static TransactionPostingResult CreatePostingResult(int total, int posted, int skipped, int failed) =>
        new()
        {
            TotalProcessed = total,
            PostedCount = posted,
            SkippedCount = skipped,
            FailedCount = failed
        };

    private static TransactionReportFunctionResult CreateReportResult(
        int totalTxns, int detailLines, int pages, int accountGroups, decimal grandTotal) =>
        new()
        {
            TotalTransactions = totalTxns,
            DetailLineCount = detailLines,
            PageCount = pages,
            AccountGroupCount = accountGroups,
            GrandTotal = grandTotal
        };

    private static VerifiedTransaction CreateVerifiedTransaction(string id, bool isVerified) =>
        new()
        {
            Transaction = new DailyTransaction
            {
                Id = id,
                CardNumber = "4000000000000001",
                TypeCode = "01",
                CategoryCode = 1001,
                Source = "BATCH",
                Description = "Test",
                Amount = 100.00m,
                MerchantId = 1,
                MerchantName = "Test",
                MerchantCity = "Stockholm",
                MerchantZip = "11122",
                OriginationTimestamp = new DateTime(2026, 1, 15, 10, 0, 0),
                ProcessingTimestamp = new DateTime(2026, 1, 16, 6, 0, 0)
            },
            IsVerified = isVerified,
            FailureReason = isVerified ? null : "Verification failed",
            AccountId = isVerified ? "00000000001" : null,
            CustomerId = isVerified ? 100000001 : null
        };

    private static ValidatedTransaction CreateValidatedTransaction(string id, bool isValid) =>
        new()
        {
            VerifiedTransaction = CreateVerifiedTransaction(id, true),
            IsValid = isValid,
            Rejections = isValid ? [] : [new DailyReject
            {
                TransactionId = id,
                CardNumber = "4000000000000001",
                AccountId = "00000000001",
                RejectCode = 102,
                RejectReason = "Over credit limit",
                TransactionAmount = 100.00m,
                RejectedAt = new DateTime(2026, 1, 16)
            }]
        };
}

// ===================================================================
// Test doubles — stub implementations for orchestrator step interfaces
// ===================================================================

internal sealed class StubCardVerificationStep : ICardVerificationStep
{
    public CardVerificationResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public Action? OnRun { get; set; }

    public Task<CardVerificationResult> RunAsync(CancellationToken cancellationToken = default)
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

internal sealed class StubCreditValidationStep : ICreditValidationStep
{
    public TransactionCreditValidationResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public IReadOnlyList<VerifiedTransaction> ReceivedInput { get; private set; } = [];

    public Task<TransactionCreditValidationResult> RunAsync(
        IReadOnlyList<VerifiedTransaction> verifiedTransactions,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        ReceivedInput = verifiedTransactions;
        if (ThrowOnRun is not null)
        {
            throw ThrowOnRun;
        }

        return Task.FromResult(Result!);
    }
}

internal sealed class StubTransactionPostingStep : ITransactionPostingStep
{
    public TransactionPostingResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }
    public IReadOnlyList<ValidatedTransaction> ReceivedInput { get; private set; } = [];

    public Task<TransactionPostingResult> RunAsync(
        IReadOnlyList<ValidatedTransaction> validatedTransactions,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        ReceivedInput = validatedTransactions;
        if (ThrowOnRun is not null)
        {
            throw ThrowOnRun;
        }

        return Task.FromResult(Result!);
    }
}

internal sealed class StubReportStep : IReportGenerationStep
{
    public TransactionReportFunctionResult? Result { get; set; }
    public Exception? ThrowOnRun { get; set; }

    public Task<TransactionReportFunctionResult> RunAsync(
        DateTime startDate,
        DateTime endDate,
        int pageSize = 20,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        if (ThrowOnRun is not null)
        {
            throw ThrowOnRun;
        }

        return Task.FromResult(Result!);
    }
}

/// <summary>
/// Fake TimeProvider for testing time-dependent SLA checks.
/// </summary>
internal sealed class FakeTimeProvider : TimeProvider
{
    private DateTimeOffset _utcNow;

    public FakeTimeProvider(DateTimeOffset utcNow)
    {
        _utcNow = utcNow;
    }

    public override DateTimeOffset GetUtcNow() => _utcNow;

    public void SetUtcNow(DateTimeOffset value) => _utcNow = value;

    public void Advance(TimeSpan duration) => _utcNow += duration;
}
