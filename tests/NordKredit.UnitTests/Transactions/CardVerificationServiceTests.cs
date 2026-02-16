using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for CardVerificationService — batch card verification.
/// COBOL source: CBTRN01C.cbl:154-250.
/// Covers card cross-reference lookup, account existence verification,
/// error handling, and structured logging.
/// Regulations: PSD2 Art.97, FFFS 2014:5 Ch.4 §3, AML/KYC.
/// </summary>
public class CardVerificationServiceTests
{
    private readonly StubDailyTransactionRepository _dailyTransRepo = new();
    private readonly StubCardCrossReferenceRepository _crossRefRepo = new();
    private readonly StubAccountRepository _accountRepo = new();
    private readonly ILogger<CardVerificationService> _logger = NullLogger<CardVerificationService>.Instance;
    private readonly CardVerificationService _sut;

    public CardVerificationServiceTests()
    {
        _sut = new CardVerificationService(
            _dailyTransRepo,
            _crossRefRepo,
            _accountRepo,
            _logger);
    }

    // ===================================================================
    // Scenario 1: Valid card with existing account (AC-1)
    // CBTRN01C.cbl:173-179 — XREF lookup succeeds, account read succeeds
    // ===================================================================

    [Fact]
    public async Task ValidCard_ExistingAccount_ReturnsVerified()
    {
        var transaction = CreateDailyTransaction("TXN001", "4000000000000001");
        _dailyTransRepo.Add(transaction);
        _crossRefRepo.AddByCardNumber("4000000000000001", new CardCrossReference
        {
            CardNumber = "4000000000000001",
            AccountId = "00000000001",
            CustomerId = 100000001
        });
        _accountRepo.Add(new Account { Id = "00000000001", ActiveStatus = "A" });

        var results = await _sut.VerifyDailyTransactionsAsync();

        var result = Assert.Single(results);
        Assert.True(result.IsVerified);
        Assert.Null(result.FailureReason);
        Assert.Equal("00000000001", result.AccountId);
        Assert.Equal(100000001, result.CustomerId);
        Assert.Same(transaction, result.Transaction);
    }

    // ===================================================================
    // Scenario 2: Card not in cross-reference (AC-2)
    // CBTRN01C.cbl:180-184 — XREF lookup fails
    // ===================================================================

    [Fact]
    public async Task CardNotInXref_ReturnsFailedWithCardVerificationMessage()
    {
        var transaction = CreateDailyTransaction("TXN002", "9999999999999999");
        _dailyTransRepo.Add(transaction);
        // No XREF entry for this card

        var results = await _sut.VerifyDailyTransactionsAsync();

        var result = Assert.Single(results);
        Assert.False(result.IsVerified);
        Assert.Equal("Card number could not be verified", result.FailureReason);
        Assert.Null(result.AccountId);
        Assert.Null(result.CustomerId);
    }

    // ===================================================================
    // Scenario 3: Valid card but account not found (AC-3)
    // CBTRN01C.cbl:177-178 — XREF succeeds but account read fails
    // ===================================================================

    [Fact]
    public async Task ValidCard_AccountNotFound_ReturnsFailedWithAccountMessage()
    {
        var transaction = CreateDailyTransaction("TXN003", "4000000000000002");
        _dailyTransRepo.Add(transaction);
        _crossRefRepo.AddByCardNumber("4000000000000002", new CardCrossReference
        {
            CardNumber = "4000000000000002",
            AccountId = "00000000099",
            CustomerId = 100000002
        });
        // No account for "00000000099"

        var results = await _sut.VerifyDailyTransactionsAsync();

        var result = Assert.Single(results);
        Assert.False(result.IsVerified);
        Assert.Equal("Account not found", result.FailureReason);
    }

    // ===================================================================
    // Scenario 4: No daily transactions — empty input
    // CBTRN01C.cbl:164 — loop exits immediately at EOF
    // ===================================================================

    [Fact]
    public async Task NoDailyTransactions_ReturnsEmptyList()
    {
        var results = await _sut.VerifyDailyTransactionsAsync();

        Assert.Empty(results);
    }

    // ===================================================================
    // Multiple transactions — mixed verification outcomes
    // CBTRN01C.cbl:164-186 — loop processes all records sequentially
    // ===================================================================

    [Fact]
    public async Task MultipleTransactions_ProcessesAllSequentially()
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

        var results = await _sut.VerifyDailyTransactionsAsync();

        Assert.Equal(3, results.Count);

        Assert.True(results[0].IsVerified);
        Assert.Equal("TXN010", results[0].Transaction.Id);

        Assert.False(results[1].IsVerified);
        Assert.Equal("Card number could not be verified", results[1].FailureReason);
        Assert.Equal("TXN011", results[1].Transaction.Id);

        Assert.False(results[2].IsVerified);
        Assert.Equal("Account not found", results[2].FailureReason);
        Assert.Equal("TXN012", results[2].Transaction.Id);
    }

    // ===================================================================
    // Scenario 4 (issue AC): Data source unavailable — throws exception
    // CBTRN01C.cbl:252-267 — file open error causes ABEND
    // ===================================================================

    [Fact]
    public async Task DailyTransactionSourceUnavailable_ThrowsException()
    {
        _dailyTransRepo.ThrowOnRead = true;

        await Assert.ThrowsAsync<InvalidOperationException>(
            () => _sut.VerifyDailyTransactionsAsync());
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task CancellationRequested_ThrowsOperationCanceledException()
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

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => _sut.VerifyDailyTransactionsAsync(cts.Token));
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
// Test doubles
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

internal sealed class StubAccountRepository : IAccountRepository
{
    private readonly Dictionary<string, Account> _accounts = [];

    public void Add(Account account) => _accounts[account.Id] = account;

    public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_accounts.GetValueOrDefault(accountId));

    public Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
        => Task.CompletedTask;
}
