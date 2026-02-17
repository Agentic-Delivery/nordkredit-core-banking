using Microsoft.Extensions.Logging;

namespace NordKredit.Domain.Transactions;

/// <summary>
/// Generates the daily transaction detail report — replaces CBTRN03C.cbl:159-373.
/// Reads posted transactions for a date range, enriches with type/category descriptions
/// from lookup tables, groups by card number, and produces page totals (every N lines),
/// account totals (on card change), and a grand total.
/// COBOL source: CBTRN03C.cbl:159-373 (report generation).
/// Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility),
/// AML 2017:11 Para.3 (monitoring).
/// </summary>
public partial class TransactionDetailReportService
{
    private readonly ITransactionRepository _transactionRepository;
    private readonly ICardCrossReferenceRepository _xrefRepository;
    private readonly ITransactionTypeRepository _typeRepository;
    private readonly ITransactionCategoryRepository _categoryRepository;
    private readonly ILogger<TransactionDetailReportService> _logger;

    public TransactionDetailReportService(
        ITransactionRepository transactionRepository,
        ICardCrossReferenceRepository xrefRepository,
        ITransactionTypeRepository typeRepository,
        ITransactionCategoryRepository categoryRepository,
        ILogger<TransactionDetailReportService> logger)
    {
        _transactionRepository = transactionRepository;
        _xrefRepository = xrefRepository;
        _typeRepository = typeRepository;
        _categoryRepository = categoryRepository;
        _logger = logger;
    }

    /// <summary>
    /// Generates the daily transaction detail report for the specified date range.
    /// COBOL: CBTRN03C.cbl main loop (lines 159-373).
    /// </summary>
    /// <param name="startDate">Start of date range (inclusive).</param>
    /// <param name="endDate">End of date range (inclusive).</param>
    /// <param name="pageSize">Lines per page before page total (default 20, per COBOL WS-PAGE-SIZE).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    public async Task<TransactionReportResult> GenerateReportAsync(
        DateTime startDate,
        DateTime endDate,
        int pageSize = 20,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();

        LogReportStart(_logger, startDate, endDate, pageSize);

        var transactions = await _transactionRepository
            .GetByDateRangeAsync(startDate, endDate, cancellationToken);

        if (transactions.Count == 0)
        {
            LogReportEmpty(_logger);
            return new TransactionReportResult
            {
                TotalTransactions = 0,
                DetailLineCount = 0,
                PageCount = 0,
                AccountGroupCount = 0,
                GrandTotal = 0.00m,
                Lines = [],
                PageTotals = [],
                AccountTotals = []
            };
        }

        var lines = new List<TransactionReportLine>();
        var pageTotals = new List<decimal>();
        var accountTotals = new List<AccountTotal>();

        var pageLineCount = 0;
        var pageTotal = 0.00m;
        var accountTotal = 0.00m;
        var grandTotal = 0.00m;
        string? currentCardNumber = null;
        string? currentAccountId = null;
        var accountGroupCount = 0;

        foreach (var transaction in transactions)
        {
            cancellationToken.ThrowIfCancellationRequested();

            // Detect card number change — write account total for previous group
            // COBOL: CBTRN03C.cbl:158-217 — card change detection
            if (currentCardNumber is not null && transaction.CardNumber != currentCardNumber)
            {
                accountTotals.Add(new AccountTotal
                {
                    CardNumber = currentCardNumber,
                    AccountId = currentAccountId!,
                    Total = accountTotal
                });
                accountTotal = 0.00m;
            }

            // Detect page boundary — write page total when page is full
            // COBOL: CBTRN03C.cbl:274-290 — page size check
            if (pageLineCount >= pageSize && pageSize > 0)
            {
                pageTotals.Add(pageTotal);
                pageTotal = 0.00m;
                pageLineCount = 0;
            }

            // Track card number change
            if (currentCardNumber != transaction.CardNumber)
            {
                currentCardNumber = transaction.CardNumber;
                accountGroupCount++;
            }

            // Lookup card cross-reference — ABEND on failure
            // COBOL: CBTRN03C.cbl AF-1
            var xref = await _xrefRepository
                .GetByCardNumberAsync(transaction.CardNumber, cancellationToken);
            if (xref is null)
            {
                LogXrefLookupFailure(_logger, transaction.CardNumber, transaction.Id);
                throw new InvalidOperationException(
                    $"Card cross-reference not found for card {transaction.CardNumber} " +
                    $"(transaction {transaction.Id}). ABEND — data integrity issue.");
            }

            currentAccountId = xref.AccountId;

            // Lookup type description — ABEND on failure
            // COBOL: CBTRN03C.cbl AF-2
            var transactionType = await _typeRepository
                .GetByCodeAsync(transaction.TypeCode, cancellationToken);
            if (transactionType is null)
            {
                LogTypeLookupFailure(_logger, transaction.TypeCode, transaction.Id);
                throw new InvalidOperationException(
                    $"Transaction type '{transaction.TypeCode}' not found " +
                    $"(transaction {transaction.Id}). ABEND — data integrity issue.");
            }

            // Lookup category description — ABEND on failure
            // COBOL: CBTRN03C.cbl AF-3
            var category = await _categoryRepository
                .GetAsync(transaction.TypeCode, transaction.CategoryCode, cancellationToken);
            if (category is null)
            {
                LogCategoryLookupFailure(_logger, transaction.TypeCode,
                    transaction.CategoryCode, transaction.Id);
                throw new InvalidOperationException(
                    $"Transaction category '{transaction.TypeCode}'+'{transaction.CategoryCode}' not found " +
                    $"(transaction {transaction.Id}). ABEND — data integrity issue.");
            }

            // Build enriched report line
            // COBOL: CBTRN03C.cbl:361-370 — TRANSACTION-DETAIL-REPORT
            var line = new TransactionReportLine
            {
                TransactionId = transaction.Id,
                AccountId = xref.AccountId,
                TypeDescription = $"{transaction.TypeCode}-{transactionType.Description}",
                CategoryDescription = $"{transaction.CategoryCode:D4}-{category.Description}",
                Source = transaction.Source,
                Amount = transaction.Amount,
                CardNumber = transaction.CardNumber
            };

            lines.Add(line);

            // Accumulate totals
            // COBOL: CBTRN03C.cbl:287-289 — ADD TRAN-AMT TO WS-PAGE-TOTAL, WS-ACCOUNT-TOTAL
            pageTotal += transaction.Amount;
            accountTotal += transaction.Amount;
            grandTotal += transaction.Amount;
            pageLineCount++;
        }

        // Write final page total
        pageTotals.Add(pageTotal);

        // Write final account total (COBOL edge case: card-change detection doesn't fire at EOF)
        if (currentCardNumber is not null)
        {
            accountTotals.Add(new AccountTotal
            {
                CardNumber = currentCardNumber,
                AccountId = currentAccountId!,
                Total = accountTotal
            });
        }

        LogReportComplete(_logger, transactions.Count, lines.Count,
            pageTotals.Count, accountGroupCount, grandTotal);

        return new TransactionReportResult
        {
            TotalTransactions = transactions.Count,
            DetailLineCount = lines.Count,
            PageCount = pageTotals.Count,
            AccountGroupCount = accountGroupCount,
            GrandTotal = grandTotal,
            Lines = lines,
            PageTotals = pageTotals,
            AccountTotals = accountTotals
        };
    }

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Start of daily transaction report generation (replaces CBTRN03C). Date range: {StartDate:yyyy-MM-dd} to {EndDate:yyyy-MM-dd}, page size: {PageSize}")]
    private static partial void LogReportStart(ILogger logger, DateTime startDate, DateTime endDate, int pageSize);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "No transactions found in date range. Report will contain headers and zero grand total")]
    private static partial void LogReportEmpty(ILogger logger);

    [LoggerMessage(Level = LogLevel.Information,
        Message = "Daily transaction report complete. Transactions: {TransactionCount}, Lines: {LineCount}, Pages: {PageCount}, Account groups: {AccountGroupCount}, Grand total: {GrandTotal}")]
    private static partial void LogReportComplete(ILogger logger, int transactionCount, int lineCount, int pageCount, int accountGroupCount, decimal grandTotal);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Card cross-reference lookup failed for card {CardNumber} (transaction {TransactionId}). ABEND")]
    private static partial void LogXrefLookupFailure(ILogger logger, string cardNumber, string transactionId);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Transaction type lookup failed for type code '{TypeCode}' (transaction {TransactionId}). ABEND")]
    private static partial void LogTypeLookupFailure(ILogger logger, string typeCode, string transactionId);

    [LoggerMessage(Level = LogLevel.Error,
        Message = "Transaction category lookup failed for type '{TypeCode}' category '{CategoryCode}' (transaction {TransactionId}). ABEND")]
    private static partial void LogCategoryLookupFailure(ILogger logger, string typeCode, int categoryCode, string transactionId);
}
