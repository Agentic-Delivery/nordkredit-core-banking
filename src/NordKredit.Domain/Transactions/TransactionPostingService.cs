using Microsoft.Extensions.Logging;

namespace NordKredit.Domain.Transactions;

/// <summary>
/// Batch transaction posting service — replaces CBTRN02C.cbl:424-579.
/// For each validated transaction: (1) upsert category balance, (2) update account balance
/// and cycle credit/debit, (3) write transaction record with processing timestamp.
/// All three operations are wrapped in a single database transaction (atomicity fix
/// over non-atomic COBOL behavior).
/// COBOL source: CBTRN02C.cbl:424-579 (posting), CBTRN02C.cbl:467-542 (TCATBAL upsert).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public partial class TransactionPostingService
{
    private readonly IAccountRepository _accountRepository;
    private readonly ITransactionRepository _transactionRepository;
    private readonly ITransactionCategoryBalanceRepository _categoryBalanceRepository;
    private readonly IDailyTransactionRepository _dailyTransactionRepository;
    private readonly IUnitOfWork _unitOfWork;
    private readonly ILogger<TransactionPostingService> _logger;

    public TransactionPostingService(
        IAccountRepository accountRepository,
        ITransactionRepository transactionRepository,
        ITransactionCategoryBalanceRepository categoryBalanceRepository,
        IDailyTransactionRepository dailyTransactionRepository,
        IUnitOfWork unitOfWork,
        ILogger<TransactionPostingService> logger)
    {
        _accountRepository = accountRepository;
        _transactionRepository = transactionRepository;
        _categoryBalanceRepository = categoryBalanceRepository;
        _dailyTransactionRepository = dailyTransactionRepository;
        _unitOfWork = unitOfWork;
        _logger = logger;
    }

    /// <summary>
    /// Posts all validated transactions — updates category balances, account balances,
    /// and writes transaction records atomically.
    /// COBOL: CBTRN02C.cbl main posting loop (lines 424-579).
    /// </summary>
    public async Task<TransactionPostingServiceResult> PostTransactionsAsync(
        IReadOnlyList<ValidatedTransaction> validatedTransactions,
        CancellationToken cancellationToken = default)
    {
        LogPostingStart(_logger, validatedTransactions.Count);

        var postedCount = 0;
        var skippedCount = 0;
        var failedCount = 0;

        foreach (var validated in validatedTransactions)
        {
            cancellationToken.ThrowIfCancellationRequested();

            // Skip invalid transactions (already rejected during validation)
            if (!validated.IsValid)
            {
                skippedCount++;
                LogTransactionSkipped(_logger, validated.VerifiedTransaction.Transaction.Id);
                continue;
            }

            try
            {
                var posted = await PostSingleTransactionAsync(validated, cancellationToken);
                if (posted)
                {
                    postedCount++;
                }
                else
                {
                    failedCount++;
                }
            }
            catch (OperationCanceledException)
            {
                throw;
            }
            catch (Exception ex)
            {
                failedCount++;
                LogTransactionFailed(_logger, validated.VerifiedTransaction.Transaction.Id, ex);
                throw;
            }
        }

        LogPostingComplete(_logger, postedCount, skippedCount, failedCount);

        return new TransactionPostingServiceResult
        {
            TotalProcessed = validatedTransactions.Count,
            PostedCount = postedCount,
            SkippedCount = skippedCount,
            FailedCount = failedCount
        };
    }

    /// <summary>
    /// Posts a single validated transaction atomically.
    /// Returns true if posted, false if the transaction could not be posted due to data issues.
    /// Throws on infrastructure failures (DB errors) — these abort the entire batch.
    /// COBOL: CBTRN02C.cbl:424-579 — three operations in a single DB transaction.
    /// </summary>
    private async Task<bool> PostSingleTransactionAsync(
        ValidatedTransaction validated,
        CancellationToken cancellationToken)
    {
        var dailyTransaction = validated.VerifiedTransaction.Transaction;
        var accountId = validated.VerifiedTransaction.AccountId!;

        await _unitOfWork.BeginTransactionAsync(cancellationToken);

        try
        {
            // Step 1: Lookup account
            var account = await _accountRepository.GetByIdAsync(accountId, cancellationToken);
            if (account is null)
            {
                await _unitOfWork.RollbackAsync(cancellationToken);
                LogAccountNotFound(_logger, accountId, dailyTransaction.Id);
                return false;
            }

            // Step 2: Upsert category balance
            // COBOL: CBTRN02C.cbl:467-542 — TCATBAL upsert
            var existingCatBal = await _categoryBalanceRepository.GetAsync(
                accountId, dailyTransaction.TypeCode, dailyTransaction.CategoryCode, cancellationToken);

            if (existingCatBal is not null)
            {
                existingCatBal.Balance += dailyTransaction.Amount;
                await _categoryBalanceRepository.UpsertAsync(existingCatBal, cancellationToken);
            }
            else
            {
                var newCatBal = new TransactionCategoryBalance
                {
                    AccountId = accountId,
                    TypeCode = dailyTransaction.TypeCode,
                    CategoryCode = dailyTransaction.CategoryCode,
                    Balance = dailyTransaction.Amount
                };
                await _categoryBalanceRepository.UpsertAsync(newCatBal, cancellationToken);
            }

            // Step 3: Update account balance and cycle credit/debit
            // COBOL: CBTRN02C.cbl:480-510
            // currentBalance += amount
            // if amount >= 0 → cycleCredit += amount (zero goes to credit per COBOL)
            // if amount < 0 → cycleDebit += abs(amount)
            account.CurrentBalance += dailyTransaction.Amount;

            if (dailyTransaction.Amount >= 0)
            {
                account.CurrentCycleCredit += dailyTransaction.Amount;
            }
            else
            {
                account.CurrentCycleDebit += Math.Abs(dailyTransaction.Amount);
            }

            await _accountRepository.UpdateAsync(account, cancellationToken);

            // Step 4: Write transaction record with processing timestamp
            // COBOL: CBTRN02C.cbl:520-540 — WRITE-TRANSACT-FILE
            var transaction = new Transaction
            {
                Id = dailyTransaction.Id,
                TypeCode = dailyTransaction.TypeCode,
                CategoryCode = dailyTransaction.CategoryCode,
                Source = dailyTransaction.Source,
                Description = dailyTransaction.Description,
                Amount = dailyTransaction.Amount,
                MerchantId = dailyTransaction.MerchantId,
                MerchantName = dailyTransaction.MerchantName,
                MerchantCity = dailyTransaction.MerchantCity,
                MerchantZip = dailyTransaction.MerchantZip,
                CardNumber = dailyTransaction.CardNumber,
                OriginationTimestamp = dailyTransaction.OriginationTimestamp,
                ProcessingTimestamp = DateTime.UtcNow
            };

            await _transactionRepository.AddAsync(transaction, cancellationToken);

            // Step 5: Mark daily transaction as processed
            await _dailyTransactionRepository.MarkAsProcessedAsync(dailyTransaction.Id, cancellationToken);

            await _unitOfWork.CommitAsync(cancellationToken);

            LogTransactionPosted(_logger, dailyTransaction.Id, accountId, dailyTransaction.Amount);
            return true;
        }
        catch (OperationCanceledException)
        {
            await _unitOfWork.RollbackAsync(cancellationToken);
            throw;
        }
        catch
        {
            await _unitOfWork.RollbackAsync(cancellationToken);
            throw;
        }
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of transaction posting. Processing {Count} validated transactions")]
    private static partial void LogPostingStart(ILogger logger, int count);

    [LoggerMessage(Level = LogLevel.Information, Message = "Transaction posting complete. Posted: {Posted}, Skipped: {Skipped}, Failed: {Failed}")]
    private static partial void LogPostingComplete(ILogger logger, int posted, int skipped, int failed);

    [LoggerMessage(Level = LogLevel.Information, Message = "Transaction {TransactionId} posted to account {AccountId}. Amount: {Amount}")]
    private static partial void LogTransactionPosted(ILogger logger, string transactionId, string accountId, decimal amount);

    [LoggerMessage(Level = LogLevel.Debug, Message = "Transaction {TransactionId} skipped — not valid")]
    private static partial void LogTransactionSkipped(ILogger logger, string transactionId);

    [LoggerMessage(Level = LogLevel.Error, Message = "Transaction {TransactionId} failed during posting")]
    private static partial void LogTransactionFailed(ILogger logger, string transactionId, Exception exception);

    [LoggerMessage(Level = LogLevel.Error, Message = "Account {AccountId} not found during posting of transaction {TransactionId}")]
    private static partial void LogAccountNotFound(ILogger logger, string accountId, string transactionId);
}
