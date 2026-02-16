using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;
using NordKredit.Functions.Batch;

namespace NordKredit.UnitTests.Batch;

/// <summary>
/// Unit tests for CardVerificationFunction — batch Azure Function wrapper.
/// COBOL source: CBTRN01C.cbl:154-250.
/// Verifies function-level orchestration, structured logging, and error handling.
/// Regulations: PSD2 Art.97, FFFS 2014:5 Ch.4 §3, AML/KYC.
/// </summary>
public class CardVerificationFunctionTests
{
    private readonly StubDailyTransactionRepository _dailyTransRepo = new();
    private readonly StubCardCrossReferenceRepository _crossRefRepo = new();
    private readonly StubAccountRepository _accountRepo = new();
    private readonly ILogger<CardVerificationService> _serviceLogger = NullLogger<CardVerificationService>.Instance;
    private readonly ILogger<CardVerificationFunction> _functionLogger = NullLogger<CardVerificationFunction>.Instance;

    private CardVerificationFunction CreateFunction()
    {
        var service = new CardVerificationService(
            _dailyTransRepo,
            _crossRefRepo,
            _accountRepo,
            _serviceLogger);
        return new CardVerificationFunction(service, _functionLogger);
    }

    // ===================================================================
    // AC-1: Valid card + existing account → verified
    // CBTRN01C.cbl:173-179
    // ===================================================================

    [Fact]
    public async Task RunAsync_ValidCardAndAccount_ReturnsVerifiedResult()
    {
        _dailyTransRepo.Add(CreateDailyTransaction("TXN001", "4000000000000001"));
        _crossRefRepo.AddByCardNumber("4000000000000001", new CardCrossReference
        {
            CardNumber = "4000000000000001",
            AccountId = "00000000001",
            CustomerId = 100000001
        });
        _accountRepo.Add(new Account { Id = "00000000001", ActiveStatus = "A" });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(1, result.VerifiedCount);
        Assert.Equal(0, result.FailedCount);
        Assert.Single(result.Results);
        Assert.True(result.Results[0].IsVerified);
    }

    // ===================================================================
    // AC-2: Card not in XREF → failed with message
    // CBTRN01C.cbl:180-184
    // ===================================================================

    [Fact]
    public async Task RunAsync_CardNotInXref_ReturnsFailedResult()
    {
        _dailyTransRepo.Add(CreateDailyTransaction("TXN002", "9999999999999999"));

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(0, result.VerifiedCount);
        Assert.Equal(1, result.FailedCount);
        Assert.Equal("Card number could not be verified", result.Results[0].FailureReason);
    }

    // ===================================================================
    // AC-3: Valid card but account not found → failed
    // CBTRN01C.cbl:177-178
    // ===================================================================

    [Fact]
    public async Task RunAsync_ValidCardAccountNotFound_ReturnsFailedResult()
    {
        _dailyTransRepo.Add(CreateDailyTransaction("TXN003", "4000000000000002"));
        _crossRefRepo.AddByCardNumber("4000000000000002", new CardCrossReference
        {
            CardNumber = "4000000000000002",
            AccountId = "00000000099",
            CustomerId = 100000002
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(1, result.TotalProcessed);
        Assert.Equal(0, result.VerifiedCount);
        Assert.Equal(1, result.FailedCount);
        Assert.Equal("Account not found", result.Results[0].FailureReason);
    }

    // ===================================================================
    // AC-4: Data source unavailable → throws exception
    // CBTRN01C.cbl:252-267 — file open error causes ABEND 999
    // ===================================================================

    [Fact]
    public async Task RunAsync_DataSourceUnavailable_ThrowsInvalidOperationException()
    {
        _dailyTransRepo.ThrowOnRead = true;

        var function = CreateFunction();

        await Assert.ThrowsAsync<InvalidOperationException>(
            () => function.RunAsync());
    }

    // ===================================================================
    // Empty batch — no transactions to process
    // CBTRN01C.cbl:164 — loop exits immediately at EOF
    // ===================================================================

    [Fact]
    public async Task RunAsync_NoTransactions_ReturnsEmptyResult()
    {
        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(0, result.TotalProcessed);
        Assert.Equal(0, result.VerifiedCount);
        Assert.Equal(0, result.FailedCount);
        Assert.Empty(result.Results);
    }

    // ===================================================================
    // Mixed outcomes — multiple transactions
    // CBTRN01C.cbl:164-186
    // ===================================================================

    [Fact]
    public async Task RunAsync_MixedTransactions_ReturnsCorrectCounts()
    {
        // Transaction 1: Valid card + account
        _dailyTransRepo.Add(CreateDailyTransaction("TXN010", "4000000000000001"));
        _crossRefRepo.AddByCardNumber("4000000000000001", new CardCrossReference
        {
            CardNumber = "4000000000000001",
            AccountId = "00000000001",
            CustomerId = 100000001
        });
        _accountRepo.Add(new Account { Id = "00000000001", ActiveStatus = "A" });

        // Transaction 2: Card not in XREF
        _dailyTransRepo.Add(CreateDailyTransaction("TXN011", "9999999999999999"));

        // Transaction 3: Valid card, missing account
        _dailyTransRepo.Add(CreateDailyTransaction("TXN012", "4000000000000003"));
        _crossRefRepo.AddByCardNumber("4000000000000003", new CardCrossReference
        {
            CardNumber = "4000000000000003",
            AccountId = "00000000099",
            CustomerId = 100000003
        });

        var function = CreateFunction();
        var result = await function.RunAsync();

        Assert.Equal(3, result.TotalProcessed);
        Assert.Equal(1, result.VerifiedCount);
        Assert.Equal(2, result.FailedCount);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task RunAsync_CancellationRequested_ThrowsOperationCanceledException()
    {
        _dailyTransRepo.Add(CreateDailyTransaction("TXN020", "4000000000000001"));
        _crossRefRepo.AddByCardNumber("4000000000000001", new CardCrossReference
        {
            CardNumber = "4000000000000001",
            AccountId = "00000000001",
            CustomerId = 100000001
        });
        _accountRepo.Add(new Account { Id = "00000000001", ActiveStatus = "A" });

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        var function = CreateFunction();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => function.RunAsync(cts.Token));
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static DailyTransaction CreateDailyTransaction(string id, string cardNumber) => new()
    {
        Id = id,
        CardNumber = cardNumber,
        TypeCode = "01",
        CategoryCode = 1001,
        Source = "BATCH",
        Description = "Test daily transaction",
        Amount = 100.00m,
        MerchantId = 1,
        MerchantName = "Test Merchant",
        MerchantCity = "Stockholm",
        MerchantZip = "11122",
        OriginationTimestamp = new DateTime(2026, 1, 15, 10, 0, 0),
        ProcessingTimestamp = new DateTime(2026, 1, 16, 6, 0, 0)
    };
}

// ===================================================================
// Test doubles (reused from CardVerificationServiceTests pattern)
// ===================================================================

internal sealed class StubDailyTransactionRepository : IDailyTransactionRepository
{
    private readonly List<DailyTransaction> _transactions = [];

    public bool ThrowOnRead { get; set; }

    public void Add(DailyTransaction transaction) => _transactions.Add(transaction);

    public Task<IReadOnlyList<DailyTransaction>> GetUnprocessedAsync(CancellationToken cancellationToken = default)
    {
        if (ThrowOnRead)
        {
            throw new InvalidOperationException("Daily transaction source is unavailable");
        }

        return Task.FromResult<IReadOnlyList<DailyTransaction>>(_transactions.AsReadOnly());
    }

    public Task AddAsync(DailyTransaction dailyTransaction, CancellationToken cancellationToken = default)
    {
        _transactions.Add(dailyTransaction);
        return Task.CompletedTask;
    }

    public Task MarkAsProcessedAsync(string transactionId, CancellationToken cancellationToken = default)
        => Task.CompletedTask;
}

internal sealed class StubCardCrossReferenceRepository : ICardCrossReferenceRepository
{
    private readonly Dictionary<string, CardCrossReference> _byCardNumber = [];
    private readonly Dictionary<string, CardCrossReference> _byAccountId = [];

    public void AddByCardNumber(string cardNumber, CardCrossReference xref)
        => _byCardNumber[cardNumber] = xref;

    public void AddByAccountId(string accountId, CardCrossReference xref)
        => _byAccountId[accountId] = xref;

    public Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default)
        => Task.FromResult(_byCardNumber.GetValueOrDefault(cardNumber));

    public Task<CardCrossReference?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_byAccountId.GetValueOrDefault(accountId));
}

internal sealed class StubAccountRepository : IAccountRepository
{
    private readonly Dictionary<string, Account> _accounts = [];

    public void Add(Account account) => _accounts[account.Id] = account;

    public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_accounts.GetValueOrDefault(accountId));

    public Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
        => Task.CompletedTask;
}
