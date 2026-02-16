using System.Globalization;
using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionListService.
/// COBOL source: COTRN00C.cbl:94-328 (CICS transaction CT00 — paginated transaction list).
/// Regulations: FFFS 2014:5 Ch.8 (operational info systems), PSD2 Art.94 (transaction history access).
/// Covers: keyset pagination, forward browsing, Transaction ID filtering, empty table, page boundaries.
/// </summary>
public class TransactionListServiceTests
{
    private readonly StubTransactionListRepository _repo = new();
    private readonly TransactionListService _sut;

    public TransactionListServiceTests()
    {
        _sut = new TransactionListService(_repo);
    }

    // ===================================================================
    // First page — no cursor (COTRN00C.cbl:279-303)
    // ===================================================================

    [Fact]
    public async Task GetTransactions_25Records_NoCursor_ReturnsFirst10()
    {
        // GIVEN the TRANSACT table contains 25 transactions
        _repo.SeedTransactions(25);

        // WHEN GET /api/transactions is called with no cursor
        var result = await _sut.GetTransactionsAsync();

        // THEN the first 10 transactions are returned
        Assert.Equal(10, result.Transactions.Count);
        Assert.Equal("0000000000000001", result.Transactions[0].TransactionId);
        Assert.Equal("0000000000000010", result.Transactions[9].TransactionId);
    }

    [Fact]
    public async Task GetTransactions_25Records_NoCursor_ReturnsHasNextPageTrue()
    {
        // GIVEN the TRANSACT table contains 25 transactions
        _repo.SeedTransactions(25);

        // WHEN GET /api/transactions is called with no cursor
        var result = await _sut.GetTransactionsAsync();

        // THEN hasNextPage is true
        Assert.True(result.HasNextPage);
    }

    [Fact]
    public async Task GetTransactions_25Records_NoCursor_ReturnsNextCursor()
    {
        // GIVEN the TRANSACT table contains 25 transactions
        _repo.SeedTransactions(25);

        // WHEN GET /api/transactions is called with no cursor
        var result = await _sut.GetTransactionsAsync();

        // THEN nextCursor points to last item on page
        Assert.Equal("0000000000000010", result.NextCursor);
    }

    [Fact]
    public async Task GetTransactions_ReturnsTransactionIdDateDescriptionAmount()
    {
        _repo.SeedTransactions(1);

        var result = await _sut.GetTransactionsAsync();

        Assert.Single(result.Transactions);
        var item = result.Transactions[0];
        Assert.Equal("0000000000000001", item.TransactionId);
        Assert.Equal("Transaction 1", item.Description);
        Assert.Equal(100.50m, item.Amount);
        Assert.Equal(new DateTime(2026, 1, 15), item.Date);
    }

    // ===================================================================
    // Cursor-based paging (COTRN00C.cbl:279-328, PF8 handler)
    // ===================================================================

    [Fact]
    public async Task GetTransactions_WithCursor_ReturnsNextPage()
    {
        // GIVEN the user has a nextCursor from a previous response
        _repo.SeedTransactions(25);

        // WHEN GET /api/transactions?cursor={nextCursor} is called
        var result = await _sut.GetTransactionsAsync(cursor: "0000000000000010");

        // THEN the next 10 transactions are returned
        Assert.Equal(10, result.Transactions.Count);
        Assert.Equal("0000000000000011", result.Transactions[0].TransactionId);
        Assert.Equal("0000000000000020", result.Transactions[9].TransactionId);
    }

    [Fact]
    public async Task GetTransactions_LastPage_ReturnsHasNextPageFalse()
    {
        // GIVEN we're on the last page
        _repo.SeedTransactions(25);

        // WHEN requesting the third page (only 5 records left)
        var result = await _sut.GetTransactionsAsync(cursor: "0000000000000020");

        // THEN hasNextPage is false
        Assert.False(result.HasNextPage);
        Assert.Equal(5, result.Transactions.Count);
        Assert.Null(result.NextCursor);
    }

    // ===================================================================
    // Transaction ID filter (COTRN00C.cbl:206-213)
    // ===================================================================

    [Fact]
    public async Task GetTransactions_WithFromTransactionId_StartsFromThatId()
    {
        // GIVEN a numeric transactionId filter
        _repo.SeedTransactions(25);

        // WHEN GET /api/transactions?fromTransactionId={id} is called
        var result = await _sut.GetTransactionsAsync(
            fromTransactionId: "0000000000000015");

        // THEN transactions starting after that ID are returned
        Assert.Equal(10, result.Transactions.Count);
        Assert.Equal("0000000000000016", result.Transactions[0].TransactionId);
    }

    [Fact]
    public async Task GetTransactions_WithFromTransactionId_OverridesCursor()
    {
        // fromTransactionId takes precedence over cursor
        _repo.SeedTransactions(25);

        var result = await _sut.GetTransactionsAsync(
            cursor: "0000000000000001",
            fromTransactionId: "0000000000000015");

        Assert.Equal("0000000000000016", result.Transactions[0].TransactionId);
    }

    // ===================================================================
    // Empty table (edge case)
    // ===================================================================

    [Fact]
    public async Task GetTransactions_EmptyTable_ReturnsEmptyArray()
    {
        // GIVEN the TRANSACT table is empty
        // (no seeding)

        // WHEN GET /api/transactions is called
        var result = await _sut.GetTransactionsAsync();

        // THEN empty array with hasNextPage: false
        Assert.Empty(result.Transactions);
        Assert.False(result.HasNextPage);
        Assert.Null(result.NextCursor);
    }

    // ===================================================================
    // Exactly 10 records (edge case — COTRN00C.cbl:305-313)
    // ===================================================================

    [Fact]
    public async Task GetTransactions_Exactly10Records_HasNextPageFalse()
    {
        // COBOL reads 11th record to detect next page
        // With exactly 10, the 11th read fails → next-page = NO
        _repo.SeedTransactions(10);

        var result = await _sut.GetTransactionsAsync();

        Assert.Equal(10, result.Transactions.Count);
        Assert.False(result.HasNextPage);
        Assert.Null(result.NextCursor);
    }

    [Fact]
    public async Task GetTransactions_11Records_HasNextPageTrue()
    {
        // With 11 records, the 11th read succeeds → next-page = YES
        _repo.SeedTransactions(11);

        var result = await _sut.GetTransactionsAsync();

        Assert.Equal(10, result.Transactions.Count);
        Assert.True(result.HasNextPage);
        Assert.Equal("0000000000000010", result.NextCursor);
    }

    // ===================================================================
    // Cursor beyond last record (edge case)
    // ===================================================================

    [Fact]
    public async Task GetTransactions_CursorBeyondLastRecord_ReturnsEmpty()
    {
        _repo.SeedTransactions(5);

        var result = await _sut.GetTransactionsAsync(cursor: "9999999999999999");

        Assert.Empty(result.Transactions);
        Assert.False(result.HasNextPage);
    }
}

/// <summary>
/// In-memory test double for ITransactionRepository supporting GetPageAsync.
/// </summary>
internal sealed class StubTransactionListRepository : ITransactionRepository
{
    private readonly List<Transaction> _transactions = [];

    public void SeedTransactions(int count)
    {
        for (var i = 1; i <= count; i++)
        {
            _transactions.Add(new Transaction
            {
                Id = i.ToString("D16", CultureInfo.InvariantCulture),
                TypeCode = "01",
                CategoryCode = 1001,
                Source = "ONLINE",
                Description = $"Transaction {i}",
                Amount = 100.50m,
                MerchantId = 1,
                MerchantName = "Test Merchant",
                MerchantCity = "Stockholm",
                MerchantZip = "11122",
                CardNumber = "4000123456789010",
                OriginationTimestamp = new DateTime(2026, 1, 15),
                ProcessingTimestamp = new DateTime(2026, 1, 16)
            });
        }
    }

    public Task<IReadOnlyList<Transaction>> GetPageAsync(
        int pageSize,
        string? cursor = null,
        CancellationToken cancellationToken = default)
    {
        IEnumerable<Transaction> query = _transactions.OrderBy(t => t.Id);

        if (!string.IsNullOrEmpty(cursor))
        {
            query = query.Where(t => string.Compare(t.Id, cursor, StringComparison.Ordinal) > 0);
        }

        var result = query.Take(pageSize).ToList();
        return Task.FromResult<IReadOnlyList<Transaction>>(result);
    }

    public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default)
        => Task.FromResult(_transactions.FirstOrDefault(t => t.Id == transactionId));

    public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(
        string accountId, int pageSize, string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>(_transactions.AsReadOnly());

    public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
    {
        _transactions.Add(transaction);
        return Task.CompletedTask;
    }

    public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(_transactions.OrderByDescending(t => t.Id).FirstOrDefault());
}
