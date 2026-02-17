using NordKredit.Domain.Lending;

namespace NordKredit.Functions.Batch.Lending;

/// <summary>
/// Batch collateral valuation update function.
/// Reviews collateral records for processed loans, flags stale valuations,
/// and calculates LTV ratios for regulatory reporting.
/// COBOL source: Dedicated program not yet in repository (inferred VSAM COLLATERAL file).
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8 (assets), CRR Art. 194-217 (credit risk mitigation).
/// </summary>
public partial class CollateralValuationFunction : ICollateralValuationStep
{
    /// <summary>
    /// Maximum age in days before a collateral valuation is considered stale.
    /// FSA FFFS 2014:5 Ch. 8: periodic revaluation required.
    /// </summary>
    internal const int StaleValuationThresholdDays = 365;

    /// <summary>
    /// Maximum acceptable LTV ratio (85%) — Finansinspektionen mortgage cap.
    /// </summary>
    internal const decimal MaxLtvRatio = 85m;

    private readonly ICollateralRepository _collateralRepository;
    private readonly TimeProvider _timeProvider;
    private readonly ILogger<CollateralValuationFunction> _logger;

    public CollateralValuationFunction(
        ICollateralRepository collateralRepository,
        TimeProvider timeProvider,
        ILogger<CollateralValuationFunction> logger)
    {
        _collateralRepository = collateralRepository;
        _timeProvider = timeProvider;
        _logger = logger;
    }

    /// <summary>
    /// Evaluates collateral for each processed loan.
    /// Calculates LTV ratio, flags stale valuations, checks regulatory limits.
    /// </summary>
    public async Task<CollateralValuationResult> RunAsync(
        IReadOnlyList<AmortizationProcessingResult.ProcessedLoan> processedLoans,
        CancellationToken cancellationToken = default)
    {
        LogBatchStarted(_logger);

        var valuatedLoans = new List<CollateralValuationResult.ValuatedLoan>();
        var withinLtvCount = 0;
        var exceedingLtvCount = 0;
        var staleCount = 0;
        var now = _timeProvider.GetUtcNow().UtcDateTime;

        foreach (var loan in processedLoans)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var collaterals = await _collateralRepository
                .GetByLoanAccountIdAsync(loan.AccountId, cancellationToken);

            var activeCollaterals = collaterals
                .Where(c => c.Status == CollateralStatus.Active)
                .ToList();

            var totalCollateralValue = activeCollaterals.Sum(c => c.Value);
            var hasStaleValuation = activeCollaterals
                .Any(c => (now - c.ValuationDate).TotalDays > StaleValuationThresholdDays);

            // LND-BR-006: Calculate LTV ratio
            var ltvRatio = totalCollateralValue > 0m
                ? loan.CurrentBalance / totalCollateralValue * 100m
                : (loan.CurrentBalance > 0m ? decimal.MaxValue : 0m);

            var isWithinLimit = ltvRatio <= MaxLtvRatio;

            if (isWithinLimit)
            {
                withinLtvCount++;
            }
            else
            {
                exceedingLtvCount++;
            }

            if (hasStaleValuation)
            {
                staleCount++;
            }

            valuatedLoans.Add(new CollateralValuationResult.ValuatedLoan
            {
                AccountId = loan.AccountId,
                CurrentBalance = loan.CurrentBalance,
                TotalCollateralValue = totalCollateralValue,
                LtvRatio = ltvRatio == decimal.MaxValue ? ltvRatio : Math.Round(ltvRatio, 2, MidpointRounding.ToEven),
                IsWithinLtvLimit = isWithinLimit,
                HasStaleValuation = hasStaleValuation
            });
        }

        LogBatchCompleted(_logger, processedLoans.Count, withinLtvCount, exceedingLtvCount, staleCount);

        return new CollateralValuationResult
        {
            ValuatedLoans = valuatedLoans,
            TotalProcessed = processedLoans.Count,
            WithinLtvLimitCount = withinLtvCount,
            ExceedingLtvLimitCount = exceedingLtvCount,
            StaleValuationCount = staleCount
        };
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Start of execution of CollateralValuationFunction (lending batch step 2)")]
    private static partial void LogBatchStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "End of execution of CollateralValuationFunction. TotalProcessed: {TotalProcessed}, WithinLTV: {WithinLtv}, ExceedingLTV: {ExceedingLtv}, StaleValuations: {StaleValuations}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int withinLtv, int exceedingLtv, int staleValuations);
}
