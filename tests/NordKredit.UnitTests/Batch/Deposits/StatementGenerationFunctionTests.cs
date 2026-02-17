using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Deposits;
using NordKredit.Functions.Batch.Deposits;

namespace NordKredit.UnitTests.Batch.Deposits;

/// <summary>
/// Unit tests for StatementGenerationFunction — monthly statement generation batch.
/// Business rule: DEP-BR-004 (interest posting), DEP-BR-001 (account data).
/// Regulations: FSA FFFS 2014:5 Ch. 7 (financial reporting), PSD2 Art. 57.
/// </summary>
public class StatementGenerationFunctionTests
{
    private readonly StubDepositAccountRepository _accountRepo = new();

    private StatementGenerationFunction CreateFunction() => new(
        _accountRepo,
        NullLogger<StatementGenerationFunction>.Instance);

    // ===================================================================
    // AC-1: Account with activity → statement generated
    // ===================================================================

    [Fact]
    public async Task RunAsync_AccountWithActivity_GeneratesStatement()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            CurrentCycleCredit = 500m,
            CurrentCycleDebit = -200m,
            AccruedInterest = 0m
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(1, result.StatementsGenerated);
        Assert.Equal(0, result.SkippedCount);
    }

    // ===================================================================
    // AC-2: Account with no activity → skipped
    // ===================================================================

    [Fact]
    public async Task RunAsync_AccountWithNoActivity_Skipped()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000002",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            CurrentCycleCredit = 0m,
            CurrentCycleDebit = 0m,
            AccruedInterest = 0m
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(0, result.StatementsGenerated);
        Assert.Equal(1, result.SkippedCount);
    }

    // ===================================================================
    // AC-3: Account with accrued interest → interest posted to balance
    // DEP-BR-004: Interest posting
    // ===================================================================

    [Fact]
    public async Task RunAsync_AccountWithAccruedInterest_PostsInterestToBalance()
    {
        var account = new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            CurrentCycleCredit = 0m,
            CurrentCycleDebit = 0m,
            AccruedInterest = 5.4795m
        };
        _accountRepo.AddActive(account);

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.StatementsGenerated);
        Assert.Equal(1, result.InterestPostedCount);
        Assert.Equal(5.4795m, result.TotalInterestPosted);
        // Balance updated: 10000 + 5.4795 = 10005.4795
        Assert.Equal(10005.4795m, account.CurrentBalance);
        Assert.Equal(0m, account.AccruedInterest);
    }

    // ===================================================================
    // AC-4: Multiple accounts — mixed outcomes
    // ===================================================================

    [Fact]
    public async Task RunAsync_MultipleAccounts_CorrectCounts()
    {
        // Account 1: Has activity (credit), no accrued interest
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            CurrentCycleCredit = 500m,
            AccruedInterest = 0m
        });

        // Account 2: No activity → skipped
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000002",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 5000m,
            AccruedInterest = 0m
        });

        // Account 3: Has accrued interest → posted
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000003",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 20000m,
            AccruedInterest = 3.5m
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(3, result.TotalProcessed);
        Assert.Equal(2, result.StatementsGenerated);
        Assert.Equal(1, result.SkippedCount);
        Assert.Equal(1, result.InterestPostedCount);
        Assert.Equal(3.5m, result.TotalInterestPosted);
    }

    // ===================================================================
    // AC-5: No active accounts → empty result
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoActiveAccounts_ReturnsEmptyResult()
    {
        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(0, result.TotalProcessed);
        Assert.Equal(0, result.StatementsGenerated);
        Assert.Equal(0, result.SkippedCount);
        Assert.Equal(0, result.InterestPostedCount);
        Assert.Equal(0m, result.TotalInterestPosted);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            CurrentCycleCredit = 500m
        });

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var function = CreateFunction();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => function.RunAsync(cts.Token));
    }

    // ===================================================================
    // AC-6: Account with debit activity only → statement generated
    // ===================================================================

    [Fact]
    public async Task RunAsync_AccountWithDebitOnly_GeneratesStatement()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            CurrentCycleCredit = 0m,
            CurrentCycleDebit = -200m,
            AccruedInterest = 0m
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.StatementsGenerated);
    }
}
