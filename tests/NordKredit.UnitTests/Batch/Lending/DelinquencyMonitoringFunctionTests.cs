using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Lending;
using NordKredit.Functions.Batch.Lending;

namespace NordKredit.UnitTests.Batch.Lending;

/// <summary>
/// Unit tests for DelinquencyMonitoringFunction — delinquency detection and AML flagging batch.
/// Business rule: LND-BR-008 (delinquency management).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), AML/KYC (delinquent accounts feed AML screening).
/// </summary>
public class DelinquencyMonitoringFunctionTests
{
    private readonly StubLoanRepository _loanRepository = new();

    private DelinquencyMonitoringFunction CreateFunction() => new(
        _loanRepository,
        NullLogger<DelinquencyMonitoringFunction>.Instance);

    // ===================================================================
    // AC-1: Loan exceeding LTV with stale valuation → Delinquent
    // ===================================================================

    [Fact]
    public async Task RunAsync_ExceedingLtvAndStale_TransitionsToDelinquent()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 95m, isWithinLtvLimit: false, hasStaleValuation: true);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated]);

        Assert.Equal(1, result.NewlyDelinquentCount);
        Assert.Equal(1, result.FlaggedForAmlScreeningCount);
        Assert.Equal(LoanStatus.Delinquent, loan.ActiveStatus);
    }

    // ===================================================================
    // AC-2: Exceeding LTV but fresh valuation → NOT delinquent
    // ===================================================================

    [Fact]
    public async Task RunAsync_ExceedingLtvButFreshValuation_NoTransition()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 95m, isWithinLtvLimit: false, hasStaleValuation: false);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated]);

        Assert.Equal(0, result.NewlyDelinquentCount);
        Assert.Equal(LoanStatus.Active, loan.ActiveStatus);
    }

    // ===================================================================
    // AC-3: Within LTV limit → NOT delinquent
    // ===================================================================

    [Fact]
    public async Task RunAsync_WithinLtvLimit_NoTransition()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 70m, isWithinLtvLimit: true, hasStaleValuation: true);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated]);

        Assert.Equal(0, result.NewlyDelinquentCount);
        Assert.Equal(LoanStatus.Active, loan.ActiveStatus);
    }

    // ===================================================================
    // AC-4: Already delinquent loan → NOT re-transitioned
    // ===================================================================

    [Fact]
    public async Task RunAsync_AlreadyDelinquent_NotReTransitioned()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Delinquent);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 95m, isWithinLtvLimit: false, hasStaleValuation: true);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated]);

        Assert.Equal(0, result.NewlyDelinquentCount);
        Assert.Equal(0, result.FlaggedForAmlScreeningCount);
    }

    // ===================================================================
    // AC-5: AML screening flagged for all newly delinquent loans
    // ===================================================================

    [Fact]
    public async Task RunAsync_MultipleNewlyDelinquent_AllFlaggedForAml()
    {
        var loan1 = CreateLoan("00000000001", LoanStatus.Active);
        var loan2 = CreateLoan("00000000002", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan1, loan2];

        var valuated1 = CreateValuatedLoan("00000000001",
            ltvRatio: 95m, isWithinLtvLimit: false, hasStaleValuation: true);
        var valuated2 = CreateValuatedLoan("00000000002",
            ltvRatio: 88m, isWithinLtvLimit: false, hasStaleValuation: true);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated1, valuated2]);

        Assert.Equal(2, result.NewlyDelinquentCount);
        Assert.Equal(2, result.FlaggedForAmlScreeningCount);
        Assert.True(result.HasAlerts);
    }

    // ===================================================================
    // AC-6: Elevated risk detection (LTV > 90%)
    // ===================================================================

    [Fact]
    public async Task RunAsync_LtvAbove90_FlaggedAsElevatedRisk()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 95m, isWithinLtvLimit: false, hasStaleValuation: false);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated]);

        Assert.Equal(1, result.ElevatedRiskCount);
    }

    [Fact]
    public async Task RunAsync_LtvBelow90_NotElevatedRisk()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 87m, isWithinLtvLimit: false, hasStaleValuation: false);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated]);

        Assert.Equal(0, result.ElevatedRiskCount);
    }

    [Fact]
    public async Task RunAsync_LtvMaxValue_NotElevatedRisk()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: decimal.MaxValue, isWithinLtvLimit: false, hasStaleValuation: true);

        var function = CreateFunction();
        var result = await function.RunAsync([valuated]);

        Assert.Equal(0, result.ElevatedRiskCount);
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
        Assert.Equal(0, result.NewlyDelinquentCount);
        Assert.False(result.HasAlerts);
    }

    // ===================================================================
    // AC-8: Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 95m, isWithinLtvLimit: false, hasStaleValuation: true);

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var function = CreateFunction();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => function.RunAsync([valuated], cts.Token));
    }

    // ===================================================================
    // AC-9: Loan updated in repository after transition
    // ===================================================================

    [Fact]
    public async Task RunAsync_TransitionedLoan_UpdatedInRepository()
    {
        var loan = CreateLoan("00000000001", LoanStatus.Active);
        _loanRepository.ActiveLoans = [loan];

        var valuated = CreateValuatedLoan("00000000001",
            ltvRatio: 95m, isWithinLtvLimit: false, hasStaleValuation: true);

        var function = CreateFunction();
        await function.RunAsync([valuated]);

        Assert.Single(_loanRepository.UpdatedLoans);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static Loan CreateLoan(string accountId, LoanStatus status) => new()
    {
        AccountId = accountId,
        ActiveStatus = status,
        CurrentBalance = 500_000m,
        CreditLimit = 600_000m,
        LoanType = LoanType.Mortgage,
        OriginationDate = new DateTime(2025, 1, 1),
        BorrowerName = "Test Borrower",
        DisclosureGroupId = "GRP001"
    };

    private static CollateralValuationResult.ValuatedLoan CreateValuatedLoan(
        string accountId, decimal ltvRatio, bool isWithinLtvLimit, bool hasStaleValuation) => new()
        {
            AccountId = accountId,
            CurrentBalance = 500_000m,
            TotalCollateralValue = 700_000m,
            LtvRatio = ltvRatio,
            IsWithinLtvLimit = isWithinLtvLimit,
            HasStaleValuation = hasStaleValuation
        };
}
