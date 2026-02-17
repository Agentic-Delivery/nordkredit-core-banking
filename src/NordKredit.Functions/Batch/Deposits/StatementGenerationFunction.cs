using NordKredit.Domain.Deposits;

namespace NordKredit.Functions.Batch.Deposits;

/// <summary>
/// Batch monthly statement generation function — replaces COBOL statement batch.
/// Reads all active deposit accounts, posts accrued interest to balance,
/// and generates monthly statements.
/// COBOL source: Dedicated statement generation batch program.
/// Business rule: DEP-BR-004 (interest posting), DEP-BR-001 (account data).
/// Regulations: FSA FFFS 2014:5 Ch. 7 (financial reporting), PSD2 Art. 57.
/// </summary>
public partial class StatementGenerationFunction : IStatementGenerationStep
{
    private readonly IDepositAccountRepository _depositAccountRepository;
    private readonly ILogger<StatementGenerationFunction> _logger;

    public StatementGenerationFunction(
        IDepositAccountRepository depositAccountRepository,
        ILogger<StatementGenerationFunction> logger)
    {
        _depositAccountRepository = depositAccountRepository;
        _logger = logger;
    }

    public async Task<StatementGenerationResult> RunAsync(CancellationToken cancellationToken = default)
    {
        LogBatchStarted(_logger);

        var accounts = await _depositAccountRepository.GetActiveAccountsAsync(cancellationToken);

        var statementsGenerated = 0;
        var skippedCount = 0;
        var interestPostedCount = 0;
        var totalInterestPosted = 0m;

        foreach (var account in accounts)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var hasActivity = account.CurrentCycleCredit != 0
                || account.CurrentCycleDebit != 0
                || account.AccruedInterest != 0;

            if (!hasActivity)
            {
                skippedCount++;
                continue;
            }

            // Post accrued interest to balance — DEP-BR-004
            if (account.AccruedInterest > 0)
            {
                var interestAmount = account.AccruedInterest;
                account.PostInterest();
                account.LastInterestPostingDate = DateTime.UtcNow;
                totalInterestPosted += interestAmount;
                interestPostedCount++;
            }

            await _depositAccountRepository.UpdateAsync(account, cancellationToken);
            statementsGenerated++;
        }

        LogBatchCompleted(_logger, accounts.Count, statementsGenerated, skippedCount,
            interestPostedCount, totalInterestPosted);

        return new StatementGenerationResult
        {
            TotalProcessed = accounts.Count,
            StatementsGenerated = statementsGenerated,
            SkippedCount = skippedCount,
            InterestPostedCount = interestPostedCount,
            TotalInterestPosted = totalInterestPosted
        };
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Start of execution of StatementGenerationFunction (monthly statement generation batch)")]
    private static partial void LogBatchStarted(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "End of execution of StatementGenerationFunction. TotalProcessed: {TotalProcessed}, Generated: {Generated}, Skipped: {Skipped}, InterestPosted: {InterestPosted}, TotalInterestPosted: {TotalInterestPosted}")]
    private static partial void LogBatchCompleted(ILogger logger, int totalProcessed, int generated, int skipped, int interestPosted, decimal totalInterestPosted);
}
