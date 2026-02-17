using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Lending;
using NordKredit.Functions.Batch.Lending;

namespace NordKredit.UnitTests.Batch.Lending;

/// <summary>
/// Unit tests for AmortizationProcessingFunction — daily interest accrual batch.
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Consumer Credit Directive Art. 10.
/// </summary>
public class AmortizationProcessingFunctionTests
{
    private readonly StubLoanRepository _loanRepository = new();

    private AmortizationProcessingFunction CreateFunction() => new(
        _loanRepository,
        NullLogger<AmortizationProcessingFunction>.Instance);

    // ===================================================================
    // AC-1: Interest is calculated for all active loans
    // ===================================================================

    [Fact]
    public async Task RunAsync_ActiveLoansWithBalance_CalculatesInterest()
    {
        var loan = CreateLoan("00000000001", 100_000m, LoanType.TermLoan);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(1, result.SuccessCount);
        Assert.Equal(0, result.SkippedCount);
        Assert.Equal(0, result.FailedCount);
        Assert.True(result.TotalInterestAccrued > 0m);
    }

    [Fact]
    public async Task RunAsync_TermLoan_UsesCorrectRate()
    {
        // 6% annual rate, Actual/360, 1 day: 100,000 * 0.06 / 360 * 1 = 16.67
        var loan = CreateLoan("00000000001", 100_000m, LoanType.TermLoan);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        var result = await function.RunAsync();

        var processed = Assert.Single(result.ProcessedLoans);
        Assert.Equal(16.67m, processed.InterestAmount);
        Assert.Equal(100_016.67m, processed.CurrentBalance);
    }

    [Fact]
    public async Task RunAsync_RevolvingCredit_UsesCorrectRate()
    {
        // 8.5% annual rate, Actual/360, 1 day: 50,000 * 0.085 / 360 * 1 = 11.81
        var loan = CreateLoan("00000000001", 50_000m, LoanType.RevolvingCredit);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        var result = await function.RunAsync();

        var processed = Assert.Single(result.ProcessedLoans);
        Assert.Equal(11.81m, processed.InterestAmount);
    }

    [Fact]
    public async Task RunAsync_Mortgage_UsesCorrectRate()
    {
        // 3.5% annual rate, Actual/360, 1 day: 1,000,000 * 0.035 / 360 * 1 = 97.22
        var loan = CreateLoan("00000000001", 1_000_000m, LoanType.Mortgage);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        var result = await function.RunAsync();

        var processed = Assert.Single(result.ProcessedLoans);
        Assert.Equal(97.22m, processed.InterestAmount);
    }

    // ===================================================================
    // AC-2: Loans with zero/negative balance are skipped
    // ===================================================================

    [Fact]
    public async Task RunAsync_ZeroBalance_Skipped()
    {
        var loan = CreateLoan("00000000001", 0m, LoanType.TermLoan);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(0, result.SuccessCount);
        Assert.Equal(1, result.SkippedCount);
        Assert.Equal(0m, result.TotalInterestAccrued);
        Assert.Empty(result.ProcessedLoans);
    }

    [Fact]
    public async Task RunAsync_NegativeBalance_Skipped()
    {
        var loan = CreateLoan("00000000001", -500m, LoanType.TermLoan);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.SkippedCount);
        Assert.Empty(result.ProcessedLoans);
    }

    // ===================================================================
    // AC-3: Multiple loans processed, totals aggregated
    // ===================================================================

    [Fact]
    public async Task RunAsync_MultipleLoans_AggregatesResults()
    {
        _loanRepository.ActiveLoans =
        [
            CreateLoan("00000000001", 100_000m, LoanType.TermLoan),
            CreateLoan("00000000002", 50_000m, LoanType.RevolvingCredit),
            CreateLoan("00000000003", 0m, LoanType.Mortgage),
        ];

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(3, result.TotalProcessed);
        Assert.Equal(2, result.SuccessCount);
        Assert.Equal(1, result.SkippedCount);
        Assert.Equal(2, result.ProcessedLoans.Count);
        Assert.Equal(16.67m + 11.81m, result.TotalInterestAccrued);
    }

    // ===================================================================
    // AC-4: Loan balance is updated in repository
    // ===================================================================

    [Fact]
    public async Task RunAsync_UpdatesLoanBalance()
    {
        var loan = CreateLoan("00000000001", 100_000m, LoanType.TermLoan);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        await function.RunAsync();

        Assert.Equal(100_016.67m, loan.CurrentBalance);
        Assert.Single(_loanRepository.UpdatedLoans);
    }

    // ===================================================================
    // AC-5: Individual loan failure doesn't halt batch
    // ===================================================================

    [Fact]
    public async Task RunAsync_IndividualLoanFails_ContinuesProcessing()
    {
        var good1 = CreateLoan("00000000001", 100_000m, LoanType.TermLoan);
        var bad = CreateLoan("00000000002", 50_000m, LoanType.TermLoan);
        var good2 = CreateLoan("00000000003", 75_000m, LoanType.TermLoan);
        _loanRepository.ActiveLoans = [good1, bad, good2];
        _loanRepository.FailOnUpdate = "00000000002";

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(3, result.TotalProcessed);
        Assert.Equal(2, result.SuccessCount);
        Assert.Equal(1, result.FailedCount);
        Assert.True(result.HasErrors);
    }

    // ===================================================================
    // AC-6: Empty loan list succeeds
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoActiveLoans_CompletesSuccessfully()
    {
        _loanRepository.ActiveLoans = [];

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(0, result.TotalProcessed);
        Assert.Equal(0, result.SuccessCount);
        Assert.Equal(0m, result.TotalInterestAccrued);
        Assert.False(result.HasErrors);
    }

    // ===================================================================
    // AC-7: All financial calculations use decimal
    // ===================================================================

    [Fact]
    public async Task RunAsync_InterestRoundedToTwoDecimalPlaces()
    {
        // 100,001 * 0.06 / 360 = 16.6668... → rounds to 16.67 (banker's rounding)
        var loan = CreateLoan("00000000001", 100_001m, LoanType.TermLoan);
        _loanRepository.ActiveLoans = [loan];

        var function = CreateFunction();
        var result = await function.RunAsync();

        var processed = Assert.Single(result.ProcessedLoans);
        Assert.Equal(16.67m, processed.InterestAmount);
    }

    // ===================================================================
    // AC-8: Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        _loanRepository.ActiveLoans = [CreateLoan("00000000001", 100_000m, LoanType.TermLoan)];

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var function = CreateFunction();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => function.RunAsync(cts.Token));
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static Loan CreateLoan(string accountId, decimal balance, LoanType loanType) => new()
    {
        AccountId = accountId,
        CurrentBalance = balance,
        LoanType = loanType,
        ActiveStatus = LoanStatus.Active,
        CreditLimit = 500_000m,
        OriginationDate = new DateTime(2025, 1, 1),
        BorrowerName = "Test Borrower",
        DisclosureGroupId = "GRP001"
    };
}

// ===================================================================
// Test doubles
// ===================================================================

internal sealed class StubLoanRepository : ILoanRepository
{
    public IReadOnlyList<Loan> ActiveLoans { get; set; } = [];
    public List<Loan> UpdatedLoans { get; } = [];
    public string? FailOnUpdate { get; set; }

    private readonly Dictionary<string, Loan> _loans = [];

    public Task<Loan?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
    {
        if (_loans.TryGetValue(accountId, out var loan))
        {
            return Task.FromResult<Loan?>(loan);
        }

        var active = ActiveLoans.FirstOrDefault(l => l.AccountId == accountId);
        return Task.FromResult(active);
    }

    public Task<IReadOnlyList<Loan>> GetPageAsync(int pageSize, string? afterAccountId = null, CancellationToken cancellationToken = default) =>
        Task.FromResult(ActiveLoans);

    public Task AddAsync(Loan loan, CancellationToken cancellationToken = default)
    {
        _loans[loan.AccountId] = loan;
        return Task.CompletedTask;
    }

    public Task UpdateAsync(Loan loan, CancellationToken cancellationToken = default)
    {
        if (FailOnUpdate == loan.AccountId)
        {
            throw new InvalidOperationException($"Simulated update failure for {loan.AccountId}");
        }

        UpdatedLoans.Add(loan);
        _loans[loan.AccountId] = loan;
        return Task.CompletedTask;
    }

    public Task<IReadOnlyList<Loan>> GetByStatusAsync(LoanStatus status, CancellationToken cancellationToken = default) =>
        Task.FromResult(ActiveLoans);
}
