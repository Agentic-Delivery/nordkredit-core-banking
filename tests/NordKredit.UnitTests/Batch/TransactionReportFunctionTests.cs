using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;
using NordKredit.Functions.Batch;

namespace NordKredit.UnitTests.Batch;

/// <summary>
/// Unit tests for TransactionReportFunction — daily transaction report batch function.
/// COBOL source: CBTRN03C.cbl:159-373 (report generation).
/// Verifies the function delegates to TransactionDetailReportService and maps results.
/// Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility),
/// AML 2017:11 Para.3 (monitoring).
/// </summary>
public class TransactionReportFunctionTests
{
    private readonly StubTransactionRepositoryForReportFunction _transactionRepo = new();
    private readonly StubCardXrefRepoForReportFunction _xrefRepo = new();
    private readonly StubTypeRepoForReportFunction _typeRepo = new();
    private readonly StubCategoryRepoForReportFunction _categoryRepo = new();
    private readonly ILogger<TransactionDetailReportService> _serviceLogger =
        NullLogger<TransactionDetailReportService>.Instance;
    private readonly ILogger<TransactionReportFunction> _functionLogger =
        NullLogger<TransactionReportFunction>.Instance;
    private readonly TransactionReportFunction _sut;

    public TransactionReportFunctionTests()
    {
        var service = new TransactionDetailReportService(
            _transactionRepo, _xrefRepo, _typeRepo, _categoryRepo, _serviceLogger);
        _sut = new TransactionReportFunction(service, _functionLogger);
    }

    [Fact]
    public async Task RunAsync_DelegatesToServiceAndReturnsResult()
    {
        _typeRepo.Add("01", "Purchase");
        _categoryRepo.Add("01", 1, "Groceries");
        _xrefRepo.Add("4000000000000001", "00000000001");

        _transactionRepo.Add(new Transaction
        {
            Id = "TXN001",
            CardNumber = "4000000000000001",
            TypeCode = "01",
            CategoryCode = 1,
            Source = "BATCH",
            Description = "Test",
            Amount = 500.00m,
            MerchantId = 1,
            MerchantName = "Test",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            OriginationTimestamp = new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc),
            ProcessingTimestamp = new DateTime(2026, 1, 15, 12, 0, 0, DateTimeKind.Utc)
        });

        var result = await _sut.RunAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(1, result.TotalTransactions);
        Assert.Equal(1, result.DetailLineCount);
        Assert.Equal(500.00m, result.GrandTotal);
    }

    [Fact]
    public async Task RunAsync_EmptyDateRange_ReturnsZero()
    {
        var result = await _sut.RunAsync(
            new DateTime(2026, 1, 1), new DateTime(2026, 1, 31));

        Assert.Equal(0, result.TotalTransactions);
        Assert.Equal(0.00m, result.GrandTotal);
    }

    [Fact]
    public async Task RunAsync_ReportResult_HasErrorsOnLookupFailure()
    {
        // No type lookup set up — will cause ABEND
        _xrefRepo.Add("4000000000000001", "00000000001");
        _transactionRepo.Add(new Transaction
        {
            Id = "TXN001",
            CardNumber = "4000000000000001",
            TypeCode = "99",
            CategoryCode = 1,
            Source = "BATCH",
            Description = "Test",
            Amount = 100.00m,
            MerchantId = 1,
            MerchantName = "Test",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            OriginationTimestamp = new DateTime(2026, 1, 15, 10, 0, 0, DateTimeKind.Utc),
            ProcessingTimestamp = new DateTime(2026, 1, 15, 12, 0, 0, DateTimeKind.Utc)
        });

        // ABEND behavior — the exception should propagate
        await Assert.ThrowsAsync<InvalidOperationException>(
            () => _sut.RunAsync(new DateTime(2026, 1, 1), new DateTime(2026, 1, 31)));
    }
}

// ===================================================================
// Test doubles for function tests
// ===================================================================

internal sealed class StubTransactionRepositoryForReportFunction : ITransactionRepository
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

internal sealed class StubCardXrefRepoForReportFunction : ICardCrossReferenceRepository
{
    private readonly Dictionary<string, CardCrossReference> _xrefs = [];

    public void Add(string cardNumber, string accountId)
        => _xrefs[cardNumber] = new CardCrossReference
        { CardNumber = cardNumber, AccountId = accountId, CustomerId = 100000001 };

    public Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default)
        => Task.FromResult(_xrefs.GetValueOrDefault(cardNumber));

    public Task<CardCrossReference?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_xrefs.Values.FirstOrDefault(x => x.AccountId == accountId));
}

internal sealed class StubTypeRepoForReportFunction : ITransactionTypeRepository
{
    private readonly Dictionary<string, TransactionType> _types = [];

    public void Add(string typeCode, string description)
        => _types[typeCode] = new TransactionType { TypeCode = typeCode, Description = description };

    public Task<TransactionType?> GetByCodeAsync(string typeCode, CancellationToken cancellationToken = default)
        => Task.FromResult(_types.GetValueOrDefault(typeCode));
}

internal sealed class StubCategoryRepoForReportFunction : ITransactionCategoryRepository
{
    private readonly Dictionary<string, TransactionCategory> _categories = [];

    public void Add(string typeCode, int categoryCode, string description)
        => _categories[$"{typeCode}|{categoryCode}"] = new TransactionCategory
        { TypeCode = typeCode, CategoryCode = categoryCode, Description = description };

    public Task<TransactionCategory?> GetAsync(string typeCode, int categoryCode, CancellationToken cancellationToken = default)
        => Task.FromResult(_categories.GetValueOrDefault($"{typeCode}|{categoryCode}"));
}
