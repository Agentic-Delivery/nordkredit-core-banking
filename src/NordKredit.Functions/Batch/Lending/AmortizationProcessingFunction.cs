using NordKredit.Domain.Lending;

namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Batch amortization schedule processing function.
/// Calculates daily interest for all active loans and applies it to balances.
/// COBOL source: Nightly interest/amortization batch (inferred from CVACT01Y.cpy).
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Consumer Credit Directive Art. 10, 19.
/// </summary>
public partial class AmortizationProcessingFunction : IAmortizationProcessingStep
{
    private readonly ILoanRepository _loanRepository;
    private readonly ILogger<AmortizationProcessingFunction> _logger;

    public AmortizationProcessingFunction(
        ILoanRepository loanRepository,
        ILogger<AmortizationProcessingFunction> logger)
    {
        _loanRepository = loanRepository;
        _logger = logger;
    }

    /// <summary>
    /// Processes daily interest accrual for all active loans.
    /// Skips loans with zero or negative balance.
    /// Uses Actual/360 day count convention and banker's rounding.
    /// </summary>
    public async Task<AmortizationProcessingResult> RunAsync(CancellationToken cancellationToken = default)
    {
        LogBatchStarted(_logger);

        var activeLoans = await _loanRepository.GetByStatusAsync(LoanStatus.Active, cancellationToken);
        var processedLoans = new List<AmortizationProcessingResult.ProcessedLoan>();
        var successCount = 0;
        var skippedCount = 0;
        var failedCount = 0;
        var totalInterest = 0m;

        foreach (var loan in activeLoans)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (loan.CurrentBalance <= 0m)
            {
                skippedCount++;
                continue;
            }

            try
            {
                // LND-BR-004: Calculate daily interest using Actual/360 convention
                var interest = InterestCalculationService.CalculateDailyInterest(
                    loan.CurrentBalance,
                    GetAnnualRate(loan),
                    DayCountConvention.Actual360,
                    daysInPeriod: 1);

                loan.CurrentBalance += interest;
                await _loanRepository.UpdateAsync(loan, cancellationToken);

                processedLoans.Add(new AmortizationProcessingResult.ProcessedLoan
                {
                    AccountId = loan.AccountId,
                    InterestAmount = interest,
                    CurrentBalance = loan.CurrentBalance,
                    IsSuccess = true
                });

                totalInterest += interest;
                successCount++;
            }
            catch (Exception ex) when (ex is not OperationCanceledException)
            {
                LogLoanProcessingFailed(_logger, loan.AccountId, ex.Message);
                processedLoans.Add(new AmortizationProcessingResult.ProcessedLoan
                {
                    AccountId = loan.AccountId,
                    InterestAmount = 0m,
                    CurrentBalance = loan.CurrentBalance,
                    IsSuccess = false,
                    FailureReason = ex.Message
                });
                failedCount++;
            }
        }

        LogBatchCompleted(_logger, activeLoans.Count, successCount, skippedCount, failedCount, totalInterest);

        return new AmortizationProcessingResult
        {
            ProcessedLoans = processedLoans,
            TotalProcessed = activeLoans.Count,
            SuccessCount = successCount,
            SkippedCount = skippedCount,
            FailedCount = failedCount,
            TotalInterestAccrued = totalInterest
        };
    }

    /// <summary>
    /// Returns the annual interest rate for a loan based on its disclosure group.
    /// In production, this would query an interest rate schedule table.
    /// Default: 8.5% for revolving credit, 6% for term loans, 3.5% for mortgages.
    /// </summary>
    internal static decimal GetAnnualRate(Loan loan) => loan.LoanType switch
    {
        LoanType.RevolvingCredit => 0.085m,
        LoanType.TermLoan => 0.06m,
        LoanType.Mortgage => 0.035m,
        _ => 0.06m,
    };

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Start of execution of AmortizationProcessingFunction (lending batch step 1)")]
    private static partial void LogBatchStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "End of execution of AmortizationProcessingFunction. TotalProcessed: {TotalProcessed}, Success: {Success}, Skipped: {Skipped}, Failed: {Failed}, TotalInterest: {TotalInterest:F2}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int success, int skipped, int failed, decimal totalInterest);

    [LoggerMessage(Level = LogLevel.Warning,
        Message = "Failed to process amortization for loan {AccountId}: {ErrorMessage}")]
    private static partial void LogLoanProcessingFailed(ILogger logger, string accountId, string errorMessage);
}
