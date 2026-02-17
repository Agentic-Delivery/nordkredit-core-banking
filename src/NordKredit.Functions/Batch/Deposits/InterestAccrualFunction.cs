using NordKredit.Domain.Deposits;

namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Batch nightly interest accrual function — replaces COBOL interest calculation batch.
/// Reads all active deposit accounts, calculates daily interest using product rate schedules,
/// and accrues interest to each account.
/// COBOL source: Dedicated interest calculation batch program.
/// Business rule: DEP-BR-004 (interest calculation and accrual).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive.
/// </summary>
public partial class InterestAccrualFunction : IInterestAccrualStep
{
    private readonly IDepositAccountRepository _depositAccountRepository;
    private readonly ISavingsProductRepository _savingsProductRepository;
    private readonly ILogger<InterestAccrualFunction> _logger;

    public InterestAccrualFunction(
        IDepositAccountRepository depositAccountRepository,
        ISavingsProductRepository savingsProductRepository,
        ILogger<InterestAccrualFunction> logger)
    {
        _depositAccountRepository = depositAccountRepository;
        _savingsProductRepository = savingsProductRepository;
        _logger = logger;
    }

    public async Task<InterestAccrualResult> RunAsync(CancellationToken cancellationToken = default)
    {
        LogBatchStarted(_logger);

        var accounts = await _depositAccountRepository.GetActiveAccountsAsync(cancellationToken);
        var products = await _savingsProductRepository.GetAllAsync(cancellationToken);
        var productLookup = products.ToDictionary(p => p.ProductId);

        var accruedCount = 0;
        var skippedCount = 0;
        var failedCount = 0;
        var totalInterest = 0m;

        foreach (var account in accounts)
        {
            cancellationToken.ThrowIfCancellationRequested();

            if (account.CurrentBalance <= 0)
            {
                skippedCount++;
                continue;
            }

            if (!productLookup.TryGetValue(account.DisclosureGroupId, out var product))
            {
                LogMissingProduct(_logger, account.Id, account.DisclosureGroupId);
                failedCount++;
                continue;
            }

            var dailyInterest = InterestCalculation.CalculateDailyInterest(
                account.CurrentBalance, product);

            account.AccrueInterest(dailyInterest);
            await _depositAccountRepository.UpdateAsync(account, cancellationToken);

            totalInterest += dailyInterest;
            accruedCount++;
        }

        LogBatchCompleted(_logger, accounts.Count, accruedCount, skippedCount, failedCount, totalInterest);

        return new InterestAccrualResult
        {
            TotalProcessed = accounts.Count,
            AccruedCount = accruedCount,
            SkippedCount = skippedCount,
            FailedCount = failedCount,
            TotalInterestAccrued = totalInterest
        };
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Start of execution of InterestAccrualFunction (nightly interest calculation batch)")]
    private static partial void LogBatchStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Warning,
        Message = "Account {AccountId} has no matching product for disclosure group {DisclosureGroupId}")]
    private static partial void LogMissingProduct(ILogger logger, string accountId, string disclosureGroupId);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "End of execution of InterestAccrualFunction. TotalProcessed: {TotalProcessed}, Accrued: {Accrued}, Skipped: {Skipped}, Failed: {Failed}, TotalInterest: {TotalInterest}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int accrued, int skipped, int failed, decimal totalInterest);
}
