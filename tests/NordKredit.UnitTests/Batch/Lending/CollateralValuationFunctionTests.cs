using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Lending;
using NordKredit.Functions.Batch.Lending;

namespace NordKredit.UnitTests.Batch.Lending;

/// <summary>
/// Unit tests for CollateralValuationFunction — collateral LTV assessment batch.
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8, CRR Art. 194-217.
/// </summary>
public class CollateralValuationFunctionTests
{
    private readonly StubCollateralRepository _collateralRepository = new();
    private readonly FakeTimeProvider _timeProvider = new(new DateTimeOffset(2026, 1, 16, 2, 0, 0, TimeSpan.Zero));

    private CollateralValuationFunction CreateFunction() => new(
        _collateralRepository,
        _timeProvider,
        NullLogger<CollateralValuationFunction>.Instance);

    // ===================================================================
    // AC-1: LTV ratio calculated correctly
    // ===================================================================

    [Fact]
    public async Task RunAsync_LoanWithCollateral_CalculatesLtvRatio()
    {
        var loan = CreateProcessedLoan("00000000001", 500_000m);
        _collateralRepository.SetCollateral("00000000001",
        [
            CreateCollateral("C001", "00000000001", 700_000m, CollateralType.RealEstate)
        ]);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        // 500,000 / 700,000 * 100 = 71.43
        Assert.Equal(71.43m, valuated.LtvRatio);
        Assert.True(valuated.IsWithinLtvLimit);
    }

    [Fact]
    public async Task RunAsync_LtvExceedsLimit_FlaggedAsExceeding()
    {
        var loan = CreateProcessedLoan("00000000001", 900_000m);
        _collateralRepository.SetCollateral("00000000001",
        [
            CreateCollateral("C001", "00000000001", 1_000_000m, CollateralType.RealEstate)
        ]);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        // 900,000 / 1,000,000 * 100 = 90.00 > 85%
        Assert.Equal(90.00m, valuated.LtvRatio);
        Assert.False(valuated.IsWithinLtvLimit);
        Assert.Equal(1, result.ExceedingLtvLimitCount);
    }

    // ===================================================================
    // AC-2: No collateral → MaxValue LTV
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoCollateral_LtvIsMaxValue()
    {
        var loan = CreateProcessedLoan("00000000001", 100_000m);
        _collateralRepository.SetCollateral("00000000001", []);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        Assert.Equal(decimal.MaxValue, valuated.LtvRatio);
        Assert.False(valuated.IsWithinLtvLimit);
    }

    [Fact]
    public async Task RunAsync_ZeroBalanceNoCollateral_LtvIsZero()
    {
        var loan = CreateProcessedLoan("00000000001", 0m);
        _collateralRepository.SetCollateral("00000000001", []);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        Assert.Equal(0m, valuated.LtvRatio);
        Assert.True(valuated.IsWithinLtvLimit);
    }

    // ===================================================================
    // AC-3: Stale valuations detected
    // ===================================================================

    [Fact]
    public async Task RunAsync_StaleValuation_Flagged()
    {
        var loan = CreateProcessedLoan("00000000001", 500_000m);
        var staleCollateral = CreateCollateral("C001", "00000000001", 700_000m, CollateralType.RealEstate);
        staleCollateral.ValuationDate = new DateTime(2024, 6, 1); // > 365 days old
        _collateralRepository.SetCollateral("00000000001", [staleCollateral]);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        Assert.True(valuated.HasStaleValuation);
        Assert.Equal(1, result.StaleValuationCount);
    }

    [Fact]
    public async Task RunAsync_RecentValuation_NotStale()
    {
        var loan = CreateProcessedLoan("00000000001", 500_000m);
        var freshCollateral = CreateCollateral("C001", "00000000001", 700_000m, CollateralType.RealEstate);
        freshCollateral.ValuationDate = new DateTime(2025, 6, 1); // < 365 days old
        _collateralRepository.SetCollateral("00000000001", [freshCollateral]);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        Assert.False(valuated.HasStaleValuation);
        Assert.Equal(0, result.StaleValuationCount);
    }

    // ===================================================================
    // AC-4: Only active collateral counted
    // ===================================================================

    [Fact]
    public async Task RunAsync_ReleasedCollateral_NotCounted()
    {
        var loan = CreateProcessedLoan("00000000001", 500_000m);
        var active = CreateCollateral("C001", "00000000001", 400_000m, CollateralType.RealEstate);
        var released = CreateCollateral("C002", "00000000001", 300_000m, CollateralType.Vehicle);
        released.Status = CollateralStatus.Released;
        _collateralRepository.SetCollateral("00000000001", [active, released]);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        Assert.Equal(400_000m, valuated.TotalCollateralValue);
    }

    // ===================================================================
    // AC-5: Multiple collateral records summed
    // ===================================================================

    [Fact]
    public async Task RunAsync_MultipleCollateral_SumsValues()
    {
        var loan = CreateProcessedLoan("00000000001", 500_000m);
        _collateralRepository.SetCollateral("00000000001",
        [
            CreateCollateral("C001", "00000000001", 300_000m, CollateralType.RealEstate),
            CreateCollateral("C002", "00000000001", 400_000m, CollateralType.Vehicle),
        ]);

        var function = CreateFunction();
        var result = await function.RunAsync([loan]);

        var valuated = Assert.Single(result.ValuatedLoans);
        Assert.Equal(700_000m, valuated.TotalCollateralValue);
        // 500,000 / 700,000 * 100 = 71.43
        Assert.Equal(71.43m, valuated.LtvRatio);
    }

    // ===================================================================
    // AC-6: Multiple loans processed
    // ===================================================================

    [Fact]
    public async Task RunAsync_MultipleLoans_AggregatesResults()
    {
        var loan1 = CreateProcessedLoan("00000000001", 500_000m);
        var loan2 = CreateProcessedLoan("00000000002", 900_000m);
        _collateralRepository.SetCollateral("00000000001",
        [
            CreateCollateral("C001", "00000000001", 700_000m, CollateralType.RealEstate)
        ]);
        _collateralRepository.SetCollateral("00000000002",
        [
            CreateCollateral("C002", "00000000002", 1_000_000m, CollateralType.RealEstate)
        ]);

        var function = CreateFunction();
        var result = await function.RunAsync([loan1, loan2]);

        Assert.Equal(2, result.TotalProcessed);
        Assert.Equal(1, result.WithinLtvLimitCount);
        Assert.Equal(1, result.ExceedingLtvLimitCount);
    }

    // ===================================================================
    // AC-7: Empty input succeeds
    // ===================================================================

    [Fact]
    public async Task RunAsync_EmptyInput_CompletesSuccessfully()
    {
        var function = CreateFunction();
        var result = await function.RunAsync([]);

        Assert.Equal(0, result.TotalProcessed);
        Assert.Empty(result.ValuatedLoans);
        Assert.False(result.HasWarnings);
    }

    // ===================================================================
    // AC-8: Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        var loan = CreateProcessedLoan("00000000001", 500_000m);
        _collateralRepository.SetCollateral("00000000001",
        [
            CreateCollateral("C001", "00000000001", 700_000m, CollateralType.RealEstate)
        ]);

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var function = CreateFunction();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => function.RunAsync([loan], cts.Token));
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static AmortizationProcessingResult.ProcessedLoan CreateProcessedLoan(
        string accountId, decimal balance) => new()
        {
            AccountId = accountId,
            InterestAmount = 16.67m,
            CurrentBalance = balance,
            IsSuccess = true
        };

    private static Collateral CreateCollateral(
        string id, string loanAccountId, decimal value, CollateralType type) => new()
        {
            CollateralId = id,
            LoanAccountId = loanAccountId,
            Value = value,
            Type = type,
            ValuationDate = new DateTime(2025, 12, 1),
            Description = "Test collateral",
            Status = CollateralStatus.Active
        };
}

// ===================================================================
// Test doubles
// ===================================================================

internal sealed class StubCollateralRepository : ICollateralRepository
{
    private readonly Dictionary<string, IReadOnlyList<Collateral>> _collateralByLoan = [];

    public void SetCollateral(string loanAccountId, IReadOnlyList<Collateral> collaterals) =>
        _collateralByLoan[loanAccountId] = collaterals;

    public Task<Collateral?> GetByIdAsync(string collateralId, CancellationToken cancellationToken = default) =>
        Task.FromResult<Collateral?>(null);

    public Task<IReadOnlyList<Collateral>> GetByLoanAccountIdAsync(string loanAccountId, CancellationToken cancellationToken = default) =>
        Task.FromResult(_collateralByLoan.TryGetValue(loanAccountId, out var collaterals)
            ? collaterals
            : ([]));

    public Task AddAsync(Collateral collateral, CancellationToken cancellationToken = default) =>
        Task.CompletedTask;

    public Task UpdateAsync(Collateral collateral, CancellationToken cancellationToken = default) =>
        Task.CompletedTask;
}
