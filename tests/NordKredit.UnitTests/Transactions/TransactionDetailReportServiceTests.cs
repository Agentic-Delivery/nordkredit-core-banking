using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionDetailReportService — daily transaction detail report generation.
/// COBOL source: CBTRN03C.cbl:159-373 (report generation).
/// Covers date filtering, type/category enrichment, card grouping with account totals,
/// page totals every 20 lines, and grand total consistency.
/// Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility),
/// AML 2017:11 Para.3 (monitoring).
/// </summary>
public class TransactionDetailReportServiceTests
{
    private readonly StubTransactionRepositoryForReport _transactionRepo = new();
    private readonly StubCardCrossReferenceRepositoryForReport _xrefRepo = new();
    private readonly StubTransactionTypeRepositoryForReport _typeRepo = new();
    private readonly StubTransactionCategoryRepositoryForReport _categoryRepo = new();
    private readonly ILogger<TransactionDetailReportService> _logger =
        NullLogger<TransactionDetailReportService>.Instance;
    private readonly TransactionDetailReportService _sut;

    public TransactionDetailReportServiceTests()
    {
        _sut = new TransactionDetailReportService(
            _transactionRepo,
            _xrefRepo,
            _typeRepo,
            _categoryRepo,
            _logger);
    }

    // ===================================================================
    // AC-1: Date range filtering — only transactions within range included
    // CBTRN03C.cbl:173-178 — date comparison using TRAN-PROC-TS(1:10)
    // ===================================================================

    [Fact]
    public async Task DateRangeFiltering_OnlyIncludesTransactionsInRange()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.AddRange(
            CreateTransaction("TXN001", "4000000000000001", "01", 1, 100.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)),
            CreateTransaction("TXN002", "4000000000000001", "01", 1, 200.00m,
                new DateTime(2025, 12, 31, 23, 59, 59, DateTimeKind.Utc)), // outside range
            CreateTransaction("TXN003", "4000000000000001", "01", 1, 300.00m,
                new DateTime(2026, 1, 31, 23, 59, 59, DateTimeKind.Utc)));

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(2, result.TotalTransactions);
        Assert.Equal(2, result.DetailLineCount);
        Assert.Equal(400.00m, result.GrandTotal);
    }

    // ===================================================================
    // AC-2: Date range uses processing timestamp, not origination
    // CBTRN03C.cbl:173 — TRAN-PROC-TS(1:10)
    // ===================================================================

    [Fact]
    public async Task DateRangeFiltering_UsesProcessingTimestamp()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        // Origination in Dec 2025, processing in Jan 2026 — should be included
        var txn = CreateTransaction("TXN001", "4000000000000001", "01", 1, 500.00m,
            new DateTime(2026, 1, 2, 10, 0, 0, DateTimeKind.Utc));
        txn.OriginationTimestamp = new DateTime(2025, 12, 31, 10, 0, 0, DateTimeKind.Utc);
        _transactionRepo.AddRange(txn);

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(1, result.TotalTransactions);
        Assert.Equal(500.00m, result.GrandTotal);
    }

    // ===================================================================
    // AC-3: Page totals every 20 lines
    // CBTRN03C.cbl:274-290 — WS-PAGE-SIZE PIC 9(03) COMP-3 VALUE 20
    // ===================================================================

    [Fact]
    public async Task PageTotals_AppearEvery20Lines()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        // Add 25 transactions
        for (var i = 1; i <= 25; i++)
        {
            _transactionRepo.AddRange(CreateTransaction(
                $"TXN{i:D3}", "4000000000000001", "01", 1, 100.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));
        }

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(25, result.TotalTransactions);
        // 2 pages: first page 20 lines, second page 5 lines
        Assert.Equal(2, result.PageTotals.Count);
        Assert.Equal(2000.00m, result.PageTotals[0]); // 20 * 100
        Assert.Equal(500.00m, result.PageTotals[1]);   // 5 * 100
    }

    // ===================================================================
    // AC-4: Configurable page size
    // ===================================================================

    [Fact]
    public async Task PageTotals_RespectsConfigurablePageSize()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        for (var i = 1; i <= 10; i++)
        {
            _transactionRepo.AddRange(CreateTransaction(
                $"TXN{i:D3}", "4000000000000001", "01", 1, 50.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));
        }

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31), pageSize: 5);

        Assert.Equal(2, result.PageTotals.Count);
        Assert.Equal(250.00m, result.PageTotals[0]); // 5 * 50
        Assert.Equal(250.00m, result.PageTotals[1]); // 5 * 50
    }

    // ===================================================================
    // AC-5: Account total on card number change
    // CBTRN03C.cbl:158-217 — card change detection
    // ===================================================================

    [Fact]
    public async Task AccountTotals_WrittenOnCardNumberChange()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");
        _xrefRepo.Add("4000000000000002", "00000000002");

        _transactionRepo.AddRange(
            CreateTransaction("TXN001", "4000000000000001", "01", 1, 500.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)),
            CreateTransaction("TXN002", "4000000000000001", "01", 1, 1000.00m,
                new DateTime(2026, 1, 15, 11, 0, 0, DateTimeKind.Utc)),
            CreateTransaction("TXN003", "4000000000000002", "01", 1, 800.00m,
                new DateTime(2026, 1, 15, 12, 0, 0, DateTimeKind.Utc)));

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(2, result.AccountTotals.Count);
        Assert.Equal("4000000000000001", result.AccountTotals[0].CardNumber);
        Assert.Equal("00000000001", result.AccountTotals[0].AccountId);
        Assert.Equal(1500.00m, result.AccountTotals[0].Total);
        Assert.Equal("4000000000000002", result.AccountTotals[1].CardNumber);
        Assert.Equal("00000000002", result.AccountTotals[1].AccountId);
        Assert.Equal(800.00m, result.AccountTotals[1].Total);
    }

    // ===================================================================
    // AC-6: Final account total at end of file
    // Edge case: COBOL card-change detection may not fire at EOF
    // ===================================================================

    [Fact]
    public async Task AccountTotals_FinalGroupWrittenAtEndOfFile()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.AddRange(
            CreateTransaction("TXN001", "4000000000000001", "01", 1, 300.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)),
            CreateTransaction("TXN002", "4000000000000001", "01", 1, 200.00m,
                new DateTime(2026, 1, 15, 11, 0, 0, DateTimeKind.Utc)));

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Single(result.AccountTotals);
        Assert.Equal(500.00m, result.AccountTotals[0].Total);
    }

    // ===================================================================
    // AC-7: Grand total equals sum of all page totals
    // CBTRN03C.cbl:318-322 — grand total write
    // ===================================================================

    [Fact]
    public async Task GrandTotal_EqualsSumOfAllPageTotals()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        for (var i = 1; i <= 25; i++)
        {
            _transactionRepo.AddRange(CreateTransaction(
                $"TXN{i:D3}", "4000000000000001", "01", 1, 100.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));
        }

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(result.GrandTotal, result.PageTotals.Sum());
    }

    // ===================================================================
    // AC-8: Type/category enrichment
    // CBTRN03C.cbl:361-370 — type/category description lookups
    // ===================================================================

    [Fact]
    public async Task Enrichment_TypeAndCategoryDescriptionsFormatted()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.AddRange(CreateTransaction(
            "TXN001", "4000000000000001", "01", 1, 100.00m,
            new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        var line = Assert.Single(result.Lines);
        Assert.Equal("01-Purchase", line.TypeDescription);
        Assert.Equal("0001-Groceries", line.CategoryDescription);
    }

    // ===================================================================
    // AC-9: Type lookup failure — ABEND (throws exception)
    // CBTRN03C.cbl AF-2: program ABENDs on missing type
    // ===================================================================

    [Fact]
    public async Task TypeLookupFailure_ThrowsException()
    {
        // Only set up category, not type — type "99" will fail lookup
        _categoryRepo.Add("99", 1, "SomeCategory");
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.AddRange(CreateTransaction(
            "TXN001", "4000000000000001", "99", 1, 100.00m,
            new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));

        await Assert.ThrowsAsync<InvalidOperationException>(
            () => _sut.GenerateReportAsync(
                new DateTime(2026, 1, 1), new DateTime(2026, 1, 31)));
    }

    // ===================================================================
    // AC-10: Category lookup failure — ABEND (throws exception)
    // CBTRN03C.cbl AF-3: program ABENDs on missing category
    // ===================================================================

    [Fact]
    public async Task CategoryLookupFailure_ThrowsException()
    {
        _typeRepo.Add("01", "Purchase");
        // No category setup — will fail lookup
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.AddRange(CreateTransaction(
            "TXN001", "4000000000000001", "01", 9999, 100.00m,
            new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));

        await Assert.ThrowsAsync<InvalidOperationException>(
            () => _sut.GenerateReportAsync(
                new DateTime(2026, 1, 1), new DateTime(2026, 1, 31)));
    }

    // ===================================================================
    // AC-11: Card XREF lookup failure — ABEND (throws exception)
    // CBTRN03C.cbl AF-1: program ABENDs on invalid card number
    // ===================================================================

    [Fact]
    public async Task CardXrefLookupFailure_ThrowsException()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        // No XREF setup — card lookup will fail

        _transactionRepo.AddRange(CreateTransaction(
            "TXN001", "4000000000000001", "01", 1, 100.00m,
            new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));

        await Assert.ThrowsAsync<InvalidOperationException>(
            () => _sut.GenerateReportAsync(
                new DateTime(2026, 1, 1), new DateTime(2026, 1, 31)));
    }

    // ===================================================================
    // AC-12: Empty result when no transactions in date range
    // AF-5: report with zero grand total
    // ===================================================================

    [Fact]
    public async Task EmptyDateRange_ReturnsZeroTotals()
    {
        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(0, result.TotalTransactions);
        Assert.Equal(0, result.DetailLineCount);
        Assert.Equal(0, result.PageCount);
        Assert.Equal(0, result.AccountGroupCount);
        Assert.Equal(0.00m, result.GrandTotal);
        Assert.Empty(result.Lines);
        Assert.Empty(result.PageTotals);
        Assert.Empty(result.AccountTotals);
    }

    // ===================================================================
    // AC-13: Report lines have correct field mapping
    // ===================================================================

    [Fact]
    public async Task ReportLines_HaveCorrectFieldMapping()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.AddRange(CreateTransaction(
            "TXN001", "4000000000000001", "01", 1, -125.50m,
            new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        var line = Assert.Single(result.Lines);
        Assert.Equal("TXN001", line.TransactionId);
        Assert.Equal("00000000001", line.AccountId);
        Assert.Equal("BATCH", line.Source);
        Assert.Equal(-125.50m, line.Amount);
        Assert.Equal("4000000000000001", line.CardNumber);
    }

    // ===================================================================
    // AC-14: Account group count matches distinct card groups
    // ===================================================================

    [Fact]
    public async Task AccountGroupCount_MatchesDistinctCardGroups()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");
        _xrefRepo.Add("4000000000000002", "00000000002");
        _xrefRepo.Add("4000000000000003", "00000000003");

        _transactionRepo.AddRange(
            CreateTransaction("TXN001", "4000000000000001", "01", 1, 100.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)),
            CreateTransaction("TXN002", "4000000000000002", "01", 1, 200.00m,
                new DateTime(2026, 1, 15, 11, 0, 0, DateTimeKind.Utc)),
            CreateTransaction("TXN003", "4000000000000003", "01", 1, 300.00m,
                new DateTime(2026, 1, 15, 12, 0, 0, DateTimeKind.Utc)));

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(3, result.AccountGroupCount);
    }

    // ===================================================================
    // AC-15: Cancellation support
    // ===================================================================

    [Fact]
    public async Task CancellationRequested_ThrowsOperationCanceledException()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.AddRange(CreateTransaction(
            "TXN001", "4000000000000001", "01", 1, 100.00m,
            new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => _sut.GenerateReportAsync(
                new DateTime(2026, 1, 1), new DateTime(2026, 1, 31),
                cancellationToken: cts.Token));
    }

    // ===================================================================
    // AC-16: Page count is correct
    // ===================================================================

    [Fact]
    public async Task PageCount_MatchesNumberOfPages()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        // 45 transactions = 3 pages (20 + 20 + 5)
        for (var i = 1; i <= 45; i++)
        {
            _transactionRepo.AddRange(CreateTransaction(
                $"TXN{i:D3}", "4000000000000001", "01", 1, 10.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));
        }

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(3, result.PageCount);
        Assert.Equal(3, result.PageTotals.Count);
        Assert.Equal(200.00m, result.PageTotals[0]); // 20 * 10
        Assert.Equal(200.00m, result.PageTotals[1]); // 20 * 10
        Assert.Equal(50.00m, result.PageTotals[2]);   // 5 * 10
    }

    // ===================================================================
    // AC-17: Exactly 20 transactions — one full page, no partial
    // ===================================================================

    [Fact]
    public async Task ExactlyOnePage_ProducesOnePageTotal()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        for (var i = 1; i <= 20; i++)
        {
            _transactionRepo.AddRange(CreateTransaction(
                $"TXN{i:D3}", "4000000000000001", "01", 1, 10.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));
        }

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(1, result.PageCount);
        Assert.Single(result.PageTotals);
        Assert.Equal(200.00m, result.PageTotals[0]);
    }

    // ===================================================================
    // AC-18: Mixed card groups across page boundaries
    // ===================================================================

    [Fact]
    public async Task MixedCardGroups_AccountTotalsCorrectAcrossPageBoundaries()
    {
        SetupLookups("01", "Purchase", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");
        _xrefRepo.Add("4000000000000002", "00000000002");

        // 15 transactions for card 1, then 10 for card 2
        // Card 1 transactions on first page, card change at line 16
        for (var i = 1; i <= 15; i++)
        {
            _transactionRepo.AddRange(CreateTransaction(
                $"TXN{i:D3}", "4000000000000001", "01", 1, 100.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));
        }

        for (var i = 16; i <= 25; i++)
        {
            _transactionRepo.AddRange(CreateTransaction(
                $"TXN{i:D3}", "4000000000000002", "01", 1, 50.00m,
                new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc)));
        }

        var result = await _sut.GenerateReportAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(2, result.AccountTotals.Count);
        Assert.Equal(1500.00m, result.AccountTotals[0].Total); // 15 * 100
        Assert.Equal(500.00m, result.AccountTotals[1].Total);  // 10 * 50
        Assert.Equal(2000.00m, result.GrandTotal);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private void SetupLookups(string typeCode, string typeDesc, int catCode, string catDesc)
    {
        _typeRepo.Add(typeCode, typeDesc);
        _categoryRepo.Add(typeCode, catCode, catDesc);
    }

    private static Transaction CreateTransaction(
        string id, string cardNumber, string typeCode, int categoryCode,
        decimal amount, DateTime processingTimestamp) => new()
        {
            Id = id,
            CardNumber = cardNumber,
            TypeCode = typeCode,
            CategoryCode = categoryCode,
            Source = "BATCH",
            Description = "Test transaction",
            Amount = amount,
            MerchantId = 1,
            MerchantName = "Test Merchant",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            OriginationTimestamp = new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc),
            ProcessingTimestamp = processingTimestamp
        };
}

// ===================================================================
// Test doubles for report tests
// ===================================================================

internal sealed class StubTransactionRepositoryForReport : ITransactionRepository
{
    private readonly List<Transaction> _transactions = [];

    public void AddRange(params Transaction[] transactions) =>
        _transactions.AddRange(transactions);

    public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default)
        => Task.FromResult(_transactions.FirstOrDefault(t => t.Id == transactionId));

    public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(
        string accountId, int pageSize, string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>([]);

    public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
    {
        _transactions.Add(transaction);
        return Task.CompletedTask;
    }

    public Task<IReadOnlyList<Transaction>> GetPageAsync(
        int pageSize, string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>([]);

    public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default)
        => Task.FromResult<Transaction?>(null);

    public Task<IReadOnlyList<Transaction>> GetByDateRangeAsync(
        DateTime startDate, DateTime endDate, CancellationToken cancellationToken = default)
    {
        IReadOnlyList<Transaction> result = [.. _transactions
            .Where(t => t.ProcessingTimestamp.Date >= startDate.Date
                     && t.ProcessingTimestamp.Date <= endDate.Date)
            .OrderBy(t => t.CardNumber)
            .ThenBy(t => t.Id)];
        return Task.FromResult(result);
    }
}

internal sealed class StubCardCrossReferenceRepositoryForReport : ICardCrossReferenceRepository
{
    private readonly Dictionary<string, CardCrossReference> _xrefs = [];

    public void Add(string cardNumber, string accountId)
        => _xrefs[cardNumber] = new CardCrossReference
        {
            CardNumber = cardNumber,
            AccountId = accountId,
            CustomerId = 100000001
        };

    public Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default)
        => Task.FromResult(_xrefs.GetValueOrDefault(cardNumber));

    public Task<CardCrossReference?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_xrefs.Values.FirstOrDefault(x => x.AccountId == accountId));
}

internal sealed class StubTransactionTypeRepositoryForReport : ITransactionTypeRepository
{
    private readonly Dictionary<string, TransactionType> _types = [];

    public void Add(string typeCode, string description)
        => _types[typeCode] = new TransactionType { TypeCode = typeCode, Description = description };

    public Task<TransactionType?> GetByCodeAsync(string typeCode, CancellationToken cancellationToken = default)
        => Task.FromResult(_types.GetValueOrDefault(typeCode));
}

internal sealed class StubTransactionCategoryRepositoryForReport : ITransactionCategoryRepository
{
    private readonly Dictionary<string, TransactionCategory> _categories = [];

    public void Add(string typeCode, int categoryCode, string description)
        => _categories[$"{typeCode}|{categoryCode}"] = new TransactionCategory
        {
            TypeCode = typeCode,
            CategoryCode = categoryCode,
            Description = description
        };

    public Task<TransactionCategory?> GetAsync(string typeCode, int categoryCode, CancellationToken cancellationToken = default)
        => Task.FromResult(_categories.GetValueOrDefault($"{typeCode}|{categoryCode}"));
}
