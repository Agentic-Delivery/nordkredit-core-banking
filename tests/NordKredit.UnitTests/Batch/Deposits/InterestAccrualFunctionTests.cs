using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Deposits;
using NordKredit.Functions.Batch.Deposits;

namespace NordKredit.UnitTests.Batch.Deposits;

/// <summary>
/// Unit tests for InterestAccrualFunction — nightly interest accrual batch.
/// COBOL source: Dedicated interest calculation batch program.
/// Business rule: DEP-BR-004 (interest calculation and accrual).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive.
/// </summary>
public class InterestAccrualFunctionTests
{
    private readonly StubDepositAccountRepository _accountRepo = new();
    private readonly StubSavingsProductRepository _productRepo = new();

    private InterestAccrualFunction CreateFunction() => new(
        _accountRepo,
        _productRepo,
        NullLogger<InterestAccrualFunction>.Instance);

    // ===================================================================
    // AC-1: Active account with matching product → interest accrued
    // DEP-BR-004: Interest calculation uses decimal types
    // ===================================================================

    [Fact]
    public async Task RunAsync_ActiveAccountWithProduct_AccruesInterest()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            DisclosureGroupId = "SAVINGS01"
        });
        _productRepo.Add(new SavingsProduct
        {
            ProductId = "SAVINGS01",
            AnnualRate = 0.025m,
            DayCountBasis = 365
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(1, result.AccruedCount);
        Assert.Equal(0, result.SkippedCount);
        Assert.Equal(0, result.FailedCount);
        // 10000 * 0.025 / 365 = 0.6849 (rounded to 4 decimal places)
        Assert.Equal(0.6849m, result.TotalInterestAccrued);
    }

    // ===================================================================
    // AC-2: Account with zero balance → skipped
    // ===================================================================

    [Fact]
    public async Task RunAsync_ZeroBalanceAccount_SkippedNotAccrued()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000002",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 0m,
            DisclosureGroupId = "SAVINGS01"
        });
        _productRepo.Add(new SavingsProduct
        {
            ProductId = "SAVINGS01",
            AnnualRate = 0.025m,
            DayCountBasis = 365
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(0, result.AccruedCount);
        Assert.Equal(1, result.SkippedCount);
        Assert.Equal(0m, result.TotalInterestAccrued);
    }

    // ===================================================================
    // AC-3: Account with missing product → failed
    // ===================================================================

    [Fact]
    public async Task RunAsync_MissingProduct_CountedAsFailed()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000003",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 5000m,
            DisclosureGroupId = "UNKNOWN"
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(0, result.AccruedCount);
        Assert.Equal(0, result.SkippedCount);
        Assert.Equal(1, result.FailedCount);
        Assert.Equal(0m, result.TotalInterestAccrued);
    }

    // ===================================================================
    // AC-4: Multiple accounts — mixed outcomes
    // ===================================================================

    [Fact]
    public async Task RunAsync_MultipleAccounts_CorrectCounts()
    {
        _productRepo.Add(new SavingsProduct
        {
            ProductId = "SAVINGS01",
            AnnualRate = 0.025m,
            DayCountBasis = 365
        });

        // Account 1: Normal, will accrue
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            DisclosureGroupId = "SAVINGS01"
        });

        // Account 2: Zero balance, will be skipped
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000002",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 0m,
            DisclosureGroupId = "SAVINGS01"
        });

        // Account 3: Missing product, will fail
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000003",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 5000m,
            DisclosureGroupId = "UNKNOWN"
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(3, result.TotalProcessed);
        Assert.Equal(1, result.AccruedCount);
        Assert.Equal(1, result.SkippedCount);
        Assert.Equal(1, result.FailedCount);
        Assert.Equal(0.6849m, result.TotalInterestAccrued);
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
        Assert.Equal(0, result.AccruedCount);
        Assert.Equal(0, result.SkippedCount);
        Assert.Equal(0, result.FailedCount);
        Assert.Equal(0m, result.TotalInterestAccrued);
    }

    // ===================================================================
    // AC-6: Interest uses decimal types — never floating-point
    // DEP-BR-004: COBOL S9(04)V99 → decimal precision
    // ===================================================================

    [Fact]
    public async Task RunAsync_InterestUsesDecimalPrecision()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 100000m,
            DisclosureGroupId = "SAVINGS01"
        });
        _productRepo.Add(new SavingsProduct
        {
            ProductId = "SAVINGS01",
            AnnualRate = 0.0333m, // 3.33% — chosen to produce fractional result
            DayCountBasis = 365
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        // 100000 * 0.0333 / 365 = 9.1233 (rounded to 4 decimal places)
        Assert.Equal(9.1233m, result.TotalInterestAccrued);
    }

    // ===================================================================
    // AC-7: Tiered interest calculation
    // DEP-BR-006: Tiered rate schedule
    // ===================================================================

    [Fact]
    public async Task RunAsync_TieredProduct_CalculatesTieredInterest()
    {
        _accountRepo.AddActive(new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 150000m,
            DisclosureGroupId = "TIERED01"
        });
        _productRepo.Add(new SavingsProduct
        {
            ProductId = "TIERED01",
            AnnualRate = 0.02m,
            Tier1Limit = 100000m,
            Tier2Rate = 0.03m,
            DayCountBasis = 365
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        // Tier 1: 100000 * 0.02 / 365 = 5.4795
        // Tier 2: 50000 * 0.03 / 365 = 4.1096
        // Total: 9.5891
        Assert.Equal(9.5891m, result.TotalInterestAccrued);
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
            DisclosureGroupId = "SAVINGS01"
        });
        _productRepo.Add(new SavingsProduct
        {
            ProductId = "SAVINGS01",
            AnnualRate = 0.025m,
            DayCountBasis = 365
        });

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var function = CreateFunction();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => function.RunAsync(cts.Token));
    }

    // ===================================================================
    // AC-8: Account accrued interest is updated after batch
    // ===================================================================

    [Fact]
    public async Task RunAsync_AccruesInterestOnAccountEntity()
    {
        var account = new DepositAccount
        {
            Id = "00000000001",
            Status = DepositAccountStatus.Active,
            CurrentBalance = 10000m,
            AccruedInterest = 1.5m, // Pre-existing accrued interest
            DisclosureGroupId = "SAVINGS01"
        };
        _accountRepo.AddActive(account);
        _productRepo.Add(new SavingsProduct
        {
            ProductId = "SAVINGS01",
            AnnualRate = 0.025m,
            DayCountBasis = 365
        });

        var function = CreateFunction();
        await function.RunAsync();

        // Accrued interest should be previous + daily: 1.5 + 0.6849 = 2.1849
        Assert.Equal(2.1849m, account.AccruedInterest);
    }
}
