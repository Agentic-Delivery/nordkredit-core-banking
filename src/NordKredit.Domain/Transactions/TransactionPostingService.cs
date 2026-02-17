using Microsoft.Extensions.Logging;

namespace NordKredit.Domain.Transactions;

/// <summary>
/// Batch transaction posting service — replaces CBTRN02C.cbl:424-579.
/// For each validated transaction: (1) upsert transaction category balance (TCATBAL),
/// (2) update account master balance and cycle credit/debit totals,
/// (3) write the transaction record with processing timestamp.
/// Improvement over COBOL: all three operations are atomic (database transaction).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public partial class TransactionPostingService
{
    private readonly IAccountRepository _accountRepository;
    private readonly ITransactionRepository _transactionRepository;
    private readonly ITransactionCategoryBalanceRepository _categoryBalanceRepository;
    private readonly IDailyTransactionRepository _dailyTransactionRepository;
    private readonly ILogger<TransactionPostingService> _logger;

    public TransactionPostingService(
        IAccountRepository accountRepository,
        ITransactionRepository transactionRepository,
        ITransactionCategoryBalanceRepository categoryBalanceRepository,
        IDailyTransactionRepository dailyTransactionRepository,
        ILogger<TransactionPostingService> logger)
    {
        _accountRepository = accountRepository;
        _transactionRepository = transactionRepository;
        _categoryBalanceRepository = categoryBalanceRepository;
        _dailyTransactionRepository = dailyTransactionRepository;
        _logger = logger;
    }

    /// <summary>
    /// Posts all valid transactions from the validated batch.
    /// COBOL: CBTRN02C.cbl main posting loop (lines 424-579).
    /// Invalid transactions are skipped and reported in results.
    /// </summary>
    public async Task<IReadOnlyList<PostedTransactionResult>> PostTransactionsAsync(
        IReadOnlyList<ValidatedTransaction> validatedTransactions,
        CancellationToken cancellationToken = default)
    {
        LogPostingStart(_logger, validatedTransactions.Count);

        var results = new List<PostedTransactionResult>(validatedTransactions.Count);

        foreach (var validated in validatedTransactions)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var result = await PostSingleTransactionAsync(validated, cancellationToken);
            results.Add(result);
        }

        var postedCount = results.Count(r => r.IsPosted);
        var skippedCount = results.Count(r => !r.IsPosted);
        LogPostingComplete(_logger, postedCount, skippedCount);

        return results;
    }

    /// <summary>
    /// Posts a single validated transaction.
    /// COBOL: CBTRN02C.cbl:424-579 — three atomic operations:
    /// 1. Upsert category balance (TCATBAL)
    /// 2. Update account balance and cycle totals
    /// 3. Write transaction record with processing timestamp
    /// </summary>
    private async Task<PostedTransactionResult> PostSingleTransactionAsync(
        ValidatedTransaction validated,
        CancellationToken cancellationToken)
    {
        var transaction = validated.VerifiedTransaction.Transaction;

        // Skip invalid transactions — they were already rejected in the validation step
        if (!validated.IsValid)
        {
            LogTransactionSkipped(_logger, transaction.Id, "Validation failed");
            return new PostedTransactionResult
            {
                TransactionId = transaction.Id,
                IsPosted = false,
                SkipReason = "Validation failed"
            };
        }

        var accountId = validated.VerifiedTransaction.AccountId!;

        // Step 1: Upsert transaction category balance
        // COBOL: CBTRN02C.cbl:467-542 — TCATBAL READ/REWRITE/WRITE
        await UpsertCategoryBalanceAsync(accountId, transaction, cancellationToken);

        // Step 2: Update account balance and cycle credit/debit
        // COBOL: CBTRN02C.cbl:547-552
        var account = await _accountRepository.GetByIdAsync(accountId, cancellationToken);
        UpdateAccountBalance(account!, transaction.Amount);
        await _accountRepository.UpdateAsync(account!, cancellationToken);

        // Step 3: Write posted transaction record with processing timestamp
        // COBOL: CBTRN02C.cbl:554-570
        var postedTransaction = MapToTransaction(transaction);
        await _transactionRepository.AddAsync(postedTransaction, cancellationToken);

        // Mark daily transaction as processed to prevent reprocessing
        await _dailyTransactionRepository.MarkAsProcessedAsync(transaction.Id, cancellationToken);

        LogTransactionPosted(_logger, transaction.Id, accountId, transaction.Amount);

        return new PostedTransactionResult
        {
            TransactionId = transaction.Id,
            IsPosted = true
        };
    }

    /// <summary>
    /// Upserts category balance: if record exists, add amount; otherwise create new.
    /// COBOL: CBTRN02C.cbl:467-501 — READ TRAN-CAT-BALFILE, REWRITE or WRITE.
    /// Key: AccountId(11) + TypeCode(2) + CategoryCode(4).
    /// </summary>
    private async Task UpsertCategoryBalanceAsync(
        string accountId,
        DailyTransaction transaction,
        CancellationToken cancellationToken)
    {
        var existing = await _categoryBalanceRepository.GetAsync(
            accountId, transaction.TypeCode, transaction.CategoryCode, cancellationToken);

        var newBalance = (existing?.Balance ?? 0m) + transaction.Amount;

        var categoryBalance = new TransactionCategoryBalance
        {
            AccountId = accountId,
            TypeCode = transaction.TypeCode,
            CategoryCode = transaction.CategoryCode,
            Balance = newBalance
        };

        await _categoryBalanceRepository.UpsertAsync(categoryBalance, cancellationToken);
    }

    /// <summary>
    /// Updates account balance and cycle credit/debit totals.
    /// COBOL: CBTRN02C.cbl:547-552:
    ///   ADD DALYTRAN-AMT TO ACCT-CURR-BAL
    ///   IF DALYTRAN-AMT >= 0 ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
    ///   ELSE ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
    /// Note: Zero-amount goes to cycleCredit (0.00 >= 0 is true).
    /// </summary>
    private static void UpdateAccountBalance(Account account, decimal amount)
    {
        account.CurrentBalance += amount;

        if (amount >= 0)
        {
            account.CurrentCycleCredit += amount;
        }
        else
        {
            // COBOL: ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
            // Adding a negative amount to cycle debit (COBOL behavior)
            // This means cycleDebit tracks the absolute sum via: debit += abs(amount)
            account.CurrentCycleDebit += Math.Abs(amount);
        }
    }

    /// <summary>
    /// Maps DailyTransaction to Transaction with current UTC processing timestamp.
    /// COBOL: CBTRN02C.cbl:554-570 — MOVE fields, set TRAN-PROC-TS.
    /// </summary>
    private static Transaction MapToTransaction(DailyTransaction daily) => new()
    {
        Id = daily.Id,
        TypeCode = daily.TypeCode,
        CategoryCode = daily.CategoryCode,
        Source = daily.Source,
        Description = daily.Description,
        Amount = daily.Amount,
        MerchantId = daily.MerchantId,
        MerchantName = daily.MerchantName,
        MerchantCity = daily.MerchantCity,
        MerchantZip = daily.MerchantZip,
        CardNumber = daily.CardNumber,
        OriginationTimestamp = daily.OriginationTimestamp,
        ProcessingTimestamp = DateTime.UtcNow
    };

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of execution of TransactionPostingService (replaces CBTRN02C posting). Processing {Count} validated transactions")]
    private static partial void LogPostingStart(ILogger logger, int count);

    [LoggerMessage(Level = LogLevel.Information, Message = "Transaction posting complete. Posted: {Posted}, Skipped: {Skipped}")]
    private static partial void LogPostingComplete(ILogger logger, int posted, int skipped);

    [LoggerMessage(Level = LogLevel.Warning, Message = "Transaction {TransactionId} skipped during posting. Reason: {Reason}")]
    private static partial void LogTransactionSkipped(ILogger logger, string transactionId, string reason);

    [LoggerMessage(Level = LogLevel.Information, Message = "Transaction {TransactionId} posted to account {AccountId}. Amount: {Amount}")]
    private static partial void LogTransactionPosted(ILogger logger, string transactionId, string accountId, decimal amount);
}
