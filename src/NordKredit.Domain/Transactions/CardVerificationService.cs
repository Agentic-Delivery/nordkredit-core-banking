using Microsoft.Extensions.Logging;

namespace NordKredit.Domain.Transactions;

/// <summary>
/// Batch card verification service — replaces CBTRN01C.cbl:154-250.
/// Reads daily transaction records, verifies each card number against the
/// cross-reference file, then confirms the linked account exists.
/// Verification only — no balance updates.
/// Regulations: PSD2 Art.97 (transaction authorization), FFFS 2014:5 Ch.4 §3
/// (operational risk), AML/KYC (source verification).
/// </summary>
public partial class CardVerificationService
{
    private readonly IDailyTransactionRepository _dailyTransactionRepository;
    private readonly ICardCrossReferenceRepository _cardCrossReferenceRepository;
    private readonly IAccountRepository _accountRepository;
    private readonly ILogger<CardVerificationService> _logger;

    public CardVerificationService(
        IDailyTransactionRepository dailyTransactionRepository,
        ICardCrossReferenceRepository cardCrossReferenceRepository,
        IAccountRepository accountRepository,
        ILogger<CardVerificationService> logger)
    {
        _dailyTransactionRepository = dailyTransactionRepository;
        _cardCrossReferenceRepository = cardCrossReferenceRepository;
        _accountRepository = accountRepository;
        _logger = logger;
    }

    /// <summary>
    /// Verifies all unprocessed daily transactions.
    /// COBOL: CBTRN01C.cbl main processing loop (lines 164-186).
    /// Reads each daily transaction, looks up the card number in the cross-reference
    /// file, and confirms the linked account exists. Returns a list of verification
    /// results for consumption by the next batch step (CBTRN02C posting).
    /// </summary>
    public async Task<IReadOnlyList<VerifiedTransaction>> VerifyDailyTransactionsAsync(
        CancellationToken cancellationToken = default)
    {
        // COBOL: 0000-DALYTRAN-OPEN — file open error causes ABEND 999.
        // In .NET, repository throws InvalidOperationException if source is unavailable.
        var transactions =
            await _dailyTransactionRepository.GetUnprocessedAsync(cancellationToken);

        LogVerificationStart(_logger, transactions.Count);

        var results = new List<VerifiedTransaction>(transactions.Count);

        // COBOL: PERFORM UNTIL END-OF-DAILY-TRANS-FILE = 'Y' (lines 164-186)
        foreach (var transaction in transactions)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var result = await VerifyTransactionAsync(transaction, cancellationToken);
            results.Add(result);
        }

        LogVerificationComplete(_logger, results.Count(r => r.IsVerified), results.Count(r => !r.IsVerified));

        return results;
    }

    /// <summary>
    /// Verifies a single daily transaction against the cross-reference and account files.
    /// COBOL: CBTRN01C.cbl lines 170-184.
    /// </summary>
    private async Task<VerifiedTransaction> VerifyTransactionAsync(
        DailyTransaction transaction,
        CancellationToken cancellationToken)
    {
        // COBOL: MOVE DALYTRAN-CARD-NUM TO XREF-CARD-NUM / PERFORM 2000-LOOKUP-XREF
        var xref = await _cardCrossReferenceRepository
            .GetByCardNumberAsync(transaction.CardNumber, cancellationToken);

        if (xref is null)
        {
            // COBOL: DISPLAY 'CARD NUMBER ' DALYTRAN-CARD-NUM
            //        ' COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-' DALYTRAN-ID
            LogCardVerificationFailed(_logger, transaction.Id, transaction.CardNumber);

            return new VerifiedTransaction
            {
                Transaction = transaction,
                IsVerified = false,
                FailureReason = "Card number could not be verified"
            };
        }

        // COBOL: MOVE XREF-ACCT-ID TO ACCT-ID / PERFORM 3000-READ-ACCOUNT
        LogCrossReferenceVerified(_logger, xref.CardNumber, xref.AccountId, xref.CustomerId);

        var account = await _accountRepository
            .GetByIdAsync(xref.AccountId, cancellationToken);

        if (account is null)
        {
            // COBOL: DISPLAY 'ACCOUNT ' ACCT-ID ' NOT FOUND'
            LogAccountNotFound(_logger, transaction.Id, xref.AccountId);

            return new VerifiedTransaction
            {
                Transaction = transaction,
                IsVerified = false,
                FailureReason = "Account not found",
                AccountId = xref.AccountId,
                CustomerId = xref.CustomerId
            };
        }

        return new VerifiedTransaction
        {
            Transaction = transaction,
            IsVerified = true,
            AccountId = xref.AccountId,
            CustomerId = xref.CustomerId
        };
    }

    [LoggerMessage(Level = LogLevel.Information, Message = "Start of card verification. Processing {Count} daily transactions")]
    private static partial void LogVerificationStart(ILogger logger, int count);

    [LoggerMessage(Level = LogLevel.Information, Message = "Card verification complete. Verified: {Verified}, Failed: {Failed}")]
    private static partial void LogVerificationComplete(ILogger logger, int verified, int failed);

    [LoggerMessage(Level = LogLevel.Warning, Message = "Card number could not be verified. Skipping transaction {TransactionId}. CardNumber: {CardNumber}")]
    private static partial void LogCardVerificationFailed(ILogger logger, string transactionId, string cardNumber);

    [LoggerMessage(Level = LogLevel.Information, Message = "Cross-reference verified. CardNumber: {CardNumber}, AccountId: {AccountId}, CustomerId: {CustomerId}")]
    private static partial void LogCrossReferenceVerified(ILogger logger, string cardNumber, string accountId, int customerId);

    [LoggerMessage(Level = LogLevel.Warning, Message = "Account not found. TransactionId: {TransactionId}, AccountId: {AccountId}")]
    private static partial void LogAccountNotFound(ILogger logger, string transactionId, string accountId);
}
