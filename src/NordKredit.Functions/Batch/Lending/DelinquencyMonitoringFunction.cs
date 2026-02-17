using NordKredit.Domain.Lending;

namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Batch delinquency monitoring function.
/// Identifies loans with high LTV or other risk indicators and transitions them
/// to delinquent status. Flags accounts for AML screening.
/// COBOL source: Dedicated program not yet in repository (inferred from LND-BR-008).
/// Business rule: LND-BR-008 (delinquency management).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Inkassolagen 1974:182,
///              AML/KYC (delinquent accounts feed AML screening — regulatory requirement).
/// </summary>
public partial class DelinquencyMonitoringFunction : IDelinquencyMonitoringStep
{
    /// <summary>
    /// LTV threshold above which a loan is flagged as elevated risk.
    /// FSA FFFS 2014:5 Ch. 6: loans exceeding this require enhanced monitoring.
    /// </summary>
    internal const decimal ElevatedRiskLtvThreshold = 90m;

    private readonly ILoanRepository _loanRepository;
    private readonly ILogger<DelinquencyMonitoringFunction> _logger;

    public DelinquencyMonitoringFunction(
        ILoanRepository loanRepository,
        ILogger<DelinquencyMonitoringFunction> logger)
    {
        _loanRepository = loanRepository;
        _logger = logger;
    }

    /// <summary>
    /// Monitors loans for delinquency indicators.
    /// Loans exceeding LTV limits with stale valuations are transitioned to Delinquent.
    /// All newly delinquent loans are flagged for AML screening.
    /// </summary>
    public async Task<DelinquencyMonitoringResult> RunAsync(
        IReadOnlyList<CollateralValuationResult.ValuatedLoan> valuatedLoans,
        CancellationToken cancellationToken = default)
    {
        LogBatchStarted(_logger);

        var newlyDelinquentCount = 0;
        var flaggedForAmlCount = 0;
        var elevatedRiskCount = 0;

        foreach (var valuatedLoan in valuatedLoans)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var isElevatedRisk = valuatedLoan.LtvRatio is not decimal.MaxValue
                and > ElevatedRiskLtvThreshold;

            if (isElevatedRisk)
            {
                elevatedRiskCount++;
            }

            // LND-BR-008: Transition to Delinquent if LTV exceeds limit AND valuation is stale
            if (!valuatedLoan.IsWithinLtvLimit && valuatedLoan.HasStaleValuation)
            {
                var loan = await _loanRepository.GetByAccountIdAsync(
                    valuatedLoan.AccountId, cancellationToken);

                if (loan is not null && loan.ActiveStatus == LoanStatus.Active)
                {
                    var transition = loan.TransitionTo(LoanStatus.Delinquent);
                    if (transition.IsValid)
                    {
                        await _loanRepository.UpdateAsync(loan, cancellationToken);
                        newlyDelinquentCount++;

                        // AML/KYC: All newly delinquent loans flagged for AML screening
                        flaggedForAmlCount++;
                        LogLoanTransitioned(_logger, loan.AccountId, valuatedLoan.LtvRatio);
                    }
                }
            }
        }

        LogBatchCompleted(_logger, valuatedLoans.Count, newlyDelinquentCount, flaggedForAmlCount, elevatedRiskCount);

        return new DelinquencyMonitoringResult
        {
            TotalProcessed = valuatedLoans.Count,
            NewlyDelinquentCount = newlyDelinquentCount,
            FlaggedForAmlScreeningCount = flaggedForAmlCount,
            ElevatedRiskCount = elevatedRiskCount
        };
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Start of execution of DelinquencyMonitoringFunction (lending batch step 3)")]
    private static partial void LogBatchStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "End of execution of DelinquencyMonitoringFunction. TotalProcessed: {TotalProcessed}, NewlyDelinquent: {NewlyDelinquent}, FlaggedForAML: {FlaggedForAml}, ElevatedRisk: {ElevatedRisk}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int newlyDelinquent, int flaggedForAml, int elevatedRisk);

    [LoggerMessage(Level = LogLevel.Warning,
        Message = "Loan {AccountId} transitioned to Delinquent — LTV ratio: {LtvRatio:F2}%. Flagged for AML screening.")]
    private static partial void LogLoanTransitioned(ILogger logger, string accountId, decimal ltvRatio);
}
