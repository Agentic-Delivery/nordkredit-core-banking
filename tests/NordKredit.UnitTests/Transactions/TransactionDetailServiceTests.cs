using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionDetailService.
/// COBOL source: COTRN01C.cbl:85-296 (CICS transaction CT01 — transaction detail view).
/// Regulations: FFFS 2014:5 Ch.8, PSD2 Art.94, GDPR Art.15 (right of access).
/// </summary>
public class TransactionDetailServiceTests
{
    private readonly StubDetailRepository _repo = new();
    private readonly TransactionDetailService _service;

    public TransactionDetailServiceTests()
    {
        _service = new TransactionDetailService(_repo);
    }

    private static Transaction CreateTestTransaction(string id = "0000000000000042") => new()
    {
        Id = id,
        TypeCode = "SA",
        CategoryCode = 5010,
        Source = "ONLINE",
        Description = "Monthly rent payment",
        Amount = 12500.00m,
        MerchantId = 123456789,
        MerchantName = "Stockholm Housing AB",
        MerchantCity = "Stockholm",
        MerchantZip = "11122",
        CardNumber = "4000123456780042",
        OriginationTimestamp = new DateTime(2026, 1, 15, 10, 30, 0),
        ProcessingTimestamp = new DateTime(2026, 1, 15, 14, 45, 0)
    };

    // ===================================================================
    // Happy path — transaction found (COTRN01C.cbl:261-280)
    // ===================================================================

    [Fact]
    public async Task GetByIdAsync_ExistingTransaction_ReturnsDetail()
    {
        // GIVEN transaction "0000000000000042" exists
        _repo.Add(CreateTestTransaction());

        // WHEN the detail is requested
        var result = await _service.GetByIdAsync("0000000000000042", CancellationToken.None);

        // THEN a detail response is returned
        Assert.NotNull(result);
    }

    [Fact]
    public async Task GetByIdAsync_ExistingTransaction_ReturnsAllFields()
    {
        // GIVEN transaction "0000000000000042" exists
        _repo.Add(CreateTestTransaction());

        // WHEN the detail is requested
        var result = await _service.GetByIdAsync("0000000000000042", CancellationToken.None);

        // THEN all 14 fields are populated
        Assert.NotNull(result);
        Assert.Equal("0000000000000042", result.TransactionId);
        Assert.Equal("SA", result.TypeCode);
        Assert.Equal(5010, result.CategoryCode);
        Assert.Equal("ONLINE", result.Source);
        Assert.Equal("Monthly rent payment", result.Description);
        Assert.Equal(12500.00m, result.Amount);
        Assert.Equal(123456789, result.MerchantId);
        Assert.Equal("Stockholm Housing AB", result.MerchantName);
        Assert.Equal("Stockholm", result.MerchantCity);
        Assert.Equal("11122", result.MerchantZip);
        Assert.Equal(new DateTime(2026, 1, 15, 10, 30, 0), result.OriginationTimestamp);
        Assert.Equal(new DateTime(2026, 1, 15, 14, 45, 0), result.ProcessingTimestamp);
    }

    // ===================================================================
    // Card number masking — PCI-DSS compliance
    // ===================================================================

    [Fact]
    public async Task GetByIdAsync_ExistingTransaction_MasksCardNumber()
    {
        // GIVEN transaction with card number "4000123456780042"
        _repo.Add(CreateTestTransaction());

        // WHEN the detail is requested
        var result = await _service.GetByIdAsync("0000000000000042", CancellationToken.None);

        // THEN card number shows only last 4 digits
        Assert.NotNull(result);
        Assert.Equal("************0042", result.CardNumber);
    }

    [Fact]
    public async Task GetByIdAsync_ShortCardNumber_MasksAvailableDigits()
    {
        // GIVEN transaction with a short card number (edge case)
        var transaction = CreateTestTransaction();
        transaction.CardNumber = "1234";
        _repo.Add(transaction);

        var result = await _service.GetByIdAsync("0000000000000042", CancellationToken.None);

        Assert.NotNull(result);
        Assert.Equal("1234", result.CardNumber);
    }

    [Fact]
    public async Task GetByIdAsync_EmptyCardNumber_ReturnsEmpty()
    {
        // GIVEN transaction with empty card number
        var transaction = CreateTestTransaction();
        transaction.CardNumber = "";
        _repo.Add(transaction);

        var result = await _service.GetByIdAsync("0000000000000042", CancellationToken.None);

        Assert.NotNull(result);
        Assert.Equal("", result.CardNumber);
    }

    // ===================================================================
    // Not found — COTRN01C.cbl:286-292 "Transaction ID NOT found"
    // ===================================================================

    [Fact]
    public async Task GetByIdAsync_NonExistentTransaction_ReturnsNull()
    {
        // GIVEN no transaction with ID "9999999999999999"
        // WHEN the detail is requested
        var result = await _service.GetByIdAsync("9999999999999999", CancellationToken.None);

        // THEN null is returned
        Assert.Null(result);
    }
}

/// <summary>
/// In-memory test double for ITransactionRepository — detail view tests.
/// </summary>
internal sealed class StubDetailRepository : ITransactionRepository
{
    private readonly List<Transaction> _transactions = [];

    public void Add(Transaction transaction) => _transactions.Add(transaction);

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
}
