using Microsoft.Extensions.Logging;

namespace NordKredit.Domain.Transactions;

/// <summary>
/// Batch credit limit and account expiration validation service — replaces CBTRN02C.cbl:370-422.
/// Validates verified transactions against credit limit and account expiration.
/// Transactions failing any check are rejected with coded reasons.
/// Improvement over COBOL: collects ALL validation failures, not just the last one.
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
/// </summary>
public partial class TransactionCreditValidationService
{
    private readonly IAccountRepository _accountRepository;
    private readonly IDailyRejectRepository _dailyRejectRepository;
    private readonly ILogger<TransactionCreditValidationService> _logger;

    public TransactionCreditValidationService(
        IAccountRepository accountRepository,
        IDailyRejectRepository dailyRejectRepository,
        ILogger<TransactionCreditValidationService> logger)
    {
        _accountRepository = accountRepository;
        _dailyRejectRepository = dailyRejectRepository;
        _logger = logger;
    }

    /// <summary>
    /// Validates all verified transactions against credit limit and account expiration.
    /// COBOL: CBTRN02C.cbl main validation loop (lines 370-422).
    /// </summary>
    public async Task<IReadOnlyList<ValidatedTransaction>> ValidateTransactionsAsync(
        IReadOnlyList<VerifiedTransaction> verifiedTransactions,
        CancellationToken cancellationToken = default)
    {
        LogValidationStart(_logger, verifiedTransactions.Count);

        var results = new List<ValidatedTransaction>(verifiedTransactions.Count);

        foreach (var verified in verifiedTransactions)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var result = await ValidateTransactionAsync(verified, cancellationToken);
            results.Add(result);
        }

        var validCount = results.Count(r => r.IsValid);
        var rejectedCount = results.Count(r => !r.IsValid);
        LogValidationComplete(_logger, validCount, rejectedCount);

        return results;
    }

    /// <summary>
    /// Validates a single verified transaction against credit limit and account expiration.
    /// COBOL: CBTRN02C.cbl:370-422.
    /// </summary>
    private async Task<ValidatedTransaction> ValidateTransactionAsync(
        VerifiedTransaction verified,
        CancellationToken cancellationToken)
    {
        var rejections = new List<DailyReject>();
        var transaction = verified.Transaction;

        // Step 1: If not verified (card not found), reject with code 100
        if (!verified.IsVerified && verified.AccountId is null)
        {
            var reject = new DailyReject
            {
                TransactionId = transaction.Id,
                CardNumber = transaction.CardNumber,
                AccountId = string.Empty,
                RejectCode = 100,
                RejectReason = "INVALID CARD NUMBER FOUND",
                TransactionAmount = transaction.Amount,
                RejectedAt = DateTime.UtcNow
            };
            rejections.Add(reject);
            await _dailyRejectRepository.AddAsync(reject, cancellationToken);

            LogTransactionRejected(_logger, transaction.Id, 100, "INVALID CARD NUMBER FOUND");

            return new ValidatedTransaction
            {
                VerifiedTransaction = verified,
                IsValid = false,
                Rejections = rejections
            };
        }

        // Step 2: If verified but account not found, reject with code 101
        if (!verified.IsVerified && verified.AccountId is not null)
        {
            var reject = new DailyReject
            {
                TransactionId = transaction.Id,
                CardNumber = transaction.CardNumber,
                AccountId = verified.AccountId,
                RejectCode = 101,
                RejectReason = "ACCOUNT RECORD NOT FOUND",
                TransactionAmount = transaction.Amount,
                RejectedAt = DateTime.UtcNow
            };
            rejections.Add(reject);
            await _dailyRejectRepository.AddAsync(reject, cancellationToken);

            LogTransactionRejected(_logger, transaction.Id, 101, "ACCOUNT RECORD NOT FOUND");

            return new ValidatedTransaction
            {
                VerifiedTransaction = verified,
                IsValid = false,
                Rejections = rejections
            };
        }

        // Step 3: Lookup account for credit limit and expiration checks
        var account = await _accountRepository.GetByIdAsync(verified.AccountId!, cancellationToken);

        if (account is null)
        {
            // Account was found during verification but missing now — treat as code 101
            var reject = new DailyReject
            {
                TransactionId = transaction.Id,
                CardNumber = transaction.CardNumber,
                AccountId = verified.AccountId!,
                RejectCode = 101,
                RejectReason = "ACCOUNT RECORD NOT FOUND",
                TransactionAmount = transaction.Amount,
                RejectedAt = DateTime.UtcNow
            };
            rejections.Add(reject);
            await _dailyRejectRepository.AddAsync(reject, cancellationToken);

            LogTransactionRejected(_logger, transaction.Id, 101, "ACCOUNT RECORD NOT FOUND");

            return new ValidatedTransaction
            {
                VerifiedTransaction = verified,
                IsValid = false,
                Rejections = rejections
            };
        }

        // Step 4: Credit limit check
        // COBOL: CBTRN02C.cbl:380-395
        // tempBalance = currentCycleCredit - currentCycleDebit + transactionAmount
        var tempBalance = account.CurrentCycleCredit - account.CurrentCycleDebit + transaction.Amount;

        if (account.CreditLimit < tempBalance)
        {
            var reject = new DailyReject
            {
                TransactionId = transaction.Id,
                CardNumber = transaction.CardNumber,
                AccountId = verified.AccountId!,
                RejectCode = 102,
                RejectReason = "OVERLIMIT TRANSACTION",
                TransactionAmount = transaction.Amount,
                RejectedAt = DateTime.UtcNow
            };
            rejections.Add(reject);
            await _dailyRejectRepository.AddAsync(reject, cancellationToken);

            LogTransactionRejected(_logger, transaction.Id, 102, "OVERLIMIT TRANSACTION");
        }

        // Step 5: Account expiration check
        // COBOL: CBTRN02C.cbl:400-410 — uses string comparison; we use proper DateTime
        // Null ExpirationDate means account never expires — skip expiration check
        if (account.ExpirationDate.HasValue
            && transaction.OriginationTimestamp.Date > account.ExpirationDate.Value.Date)
        {
            var reject = new DailyReject
            {
                TransactionId = transaction.Id,
                CardNumber = transaction.CardNumber,
                AccountId = verified.AccountId!,
                RejectCode = 103,
                RejectReason = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION",
                TransactionAmount = transaction.Amount,
                RejectedAt = DateTime.UtcNow
            };
            rejections.Add(reject);
            await _dailyRejectRepository.AddAsync(reject, cancellationToken);

            LogTransactionRejected(_logger, transaction.Id, 103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        }

        if (rejections.Count > 0)
        {
            return new ValidatedTransaction
            {
                VerifiedTransaction = verified,
                IsValid = false,
                Rejections = rejections
            };
        }

        LogTransactionValid(_logger, transaction.Id, verified.AccountId!);

        return new ValidatedTransaction
        {
            VerifiedTransaction = verified,
            IsValid = true,
            Rejections = rejections
        };
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of credit limit and expiration validation. Processing {Count} verified transactions")]
    private static partial void LogValidationStart(ILogger logger, int count);

    [LoggerMessage(Level = LogLevel.Information, Message = "Credit limit and expiration validation complete. Valid: {Valid}, Rejected: {Rejected}")]
    private static partial void LogValidationComplete(ILogger logger, int valid, int rejected);

    [LoggerMessage(Level = LogLevel.Warning, Message = "Transaction {TransactionId} rejected. Code: {RejectCode}, Reason: {RejectReason}")]
    private static partial void LogTransactionRejected(ILogger logger, string transactionId, int rejectCode, string rejectReason);

    [LoggerMessage(Level = LogLevel.Information, Message = "Transaction {TransactionId} passed validation for account {AccountId}")]
    private static partial void LogTransactionValid(ILogger logger, string transactionId, string accountId);
}
