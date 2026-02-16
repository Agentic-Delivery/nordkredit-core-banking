using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionCreditValidationService — batch credit limit and expiration validation.
/// COBOL source: CBTRN02C.cbl:370-422.
/// Covers credit limit checks, account expiration, rejection code generation,
/// and the improvement over COBOL's "last wins" behavior (collecting all failures).
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
/// </summary>
public class TransactionCreditValidationServiceTests
{
    private readonly StubAccountRepository2 _accountRepo = new();
    private readonly StubDailyRejectRepository _rejectRepo = new();
    private readonly ILogger<TransactionCreditValidationService> _logger =
        NullLogger<TransactionCreditValidationService>.Instance;
    private readonly TransactionCreditValidationService _sut;

    public TransactionCreditValidationServiceTests()
    {
        _sut = new TransactionCreditValidationService(
            _accountRepo,
            _rejectRepo,
            _logger);
    }

    // ===================================================================
    // AC-1: Credit limit passes — tempBalance within limit
    // CBTRN02C.cbl:380-395 — credit limit check
    // Credit limit 10000, credits 5000, debits 3000, amount 1500
    // tempBalance = 5000 - 3000 + 1500 = 3500 → PASSES (10000 >= 3500)
    // ===================================================================

    [Fact]
    public async Task CreditLimitPasses_WithinLimit_ReturnsValid()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 5000.00m,
            CurrentCycleDebit = 3000.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account);

        var verified = CreateVerifiedTransaction("TXN001", "4000000000000001",
            "00000000001", 100000001, 1500.00m, new DateTime(2026, 1, 15));

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.True(result.IsValid);
        Assert.Empty(result.Rejections);
    }

    // ===================================================================
    // AC-2: Credit limit exceeded — REJECTED code 102 "OVERLIMIT TRANSACTION"
    // Credit limit 5000, credits 4000, debits 200, amount 1500
    // tempBalance = 4000 - 200 + 1500 = 5300 → REJECTED (5000 < 5300)
    // ===================================================================

    [Fact]
    public async Task CreditLimitExceeded_ReturnsRejectedWithCode102()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 4000.00m,
            CurrentCycleDebit = 200.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account);

        var verified = CreateVerifiedTransaction("TXN002", "4000000000000001",
            "00000000001", 100000001, 1500.00m, new DateTime(2026, 1, 15));

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.False(result.IsValid);
        var rejection = Assert.Single(result.Rejections);
        Assert.Equal(102, rejection.RejectCode);
        Assert.Equal("OVERLIMIT TRANSACTION", rejection.RejectReason);
        Assert.Equal("TXN002", rejection.TransactionId);
        Assert.Equal(1500.00m, rejection.TransactionAmount);
    }

    // ===================================================================
    // AC-3: Account expired — REJECTED code 103
    // Account expiration "2025-12-31", origination "2026-01-15"
    // ===================================================================

    [Fact]
    public async Task AccountExpired_ReturnsRejectedWithCode103()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 1000.00m,
            CurrentCycleDebit = 500.00m,
            ExpirationDate = new DateTime(2025, 12, 31)
        };
        _accountRepo.Add(account);

        var verified = CreateVerifiedTransaction("TXN003", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.False(result.IsValid);
        var rejection = Assert.Single(result.Rejections);
        Assert.Equal(103, rejection.RejectCode);
        Assert.Equal("TRANSACTION RECEIVED AFTER ACCT EXPIRATION", rejection.RejectReason);
    }

    // ===================================================================
    // AC-4: Card number not in XREF — REJECTED code 100, account lookup skipped
    // ===================================================================

    [Fact]
    public async Task CardNotInXref_ReturnsRejectedWithCode100()
    {
        var transaction = CreateDailyTransaction("TXN004", "9999999999999999", 500.00m, new DateTime(2026, 1, 15));
        var verified = new VerifiedTransaction
        {
            Transaction = transaction,
            IsVerified = false,
            FailureReason = "Card number could not be verified",
            AccountId = null,
            CustomerId = null
        };

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.False(result.IsValid);
        var rejection = Assert.Single(result.Rejections);
        Assert.Equal(100, rejection.RejectCode);
        Assert.Equal("INVALID CARD NUMBER FOUND", rejection.RejectReason);
        Assert.Equal(string.Empty, rejection.AccountId);
    }

    // ===================================================================
    // AC-5: Valid card but account missing — REJECTED code 101
    // ===================================================================

    [Fact]
    public async Task ValidCardAccountMissing_ReturnsRejectedWithCode101()
    {
        var transaction = CreateDailyTransaction("TXN005", "4000000000000002", 500.00m, new DateTime(2026, 1, 15));
        var verified = new VerifiedTransaction
        {
            Transaction = transaction,
            IsVerified = false,
            FailureReason = "Account not found",
            AccountId = "00000000099",
            CustomerId = 100000002
        };

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.False(result.IsValid);
        var rejection = Assert.Single(result.Rejections);
        Assert.Equal(101, rejection.RejectCode);
        Assert.Equal("ACCOUNT RECORD NOT FOUND", rejection.RejectReason);
        Assert.Equal("00000000099", rejection.AccountId);
    }

    // ===================================================================
    // AC-6: Negative amount (credit/payment) reduces temp balance
    // Credits 4800, debits 100, amount -500, limit 5000
    // tempBalance = 4800 - 100 + (-500) = 4200 → PASSES
    // ===================================================================

    [Fact]
    public async Task NegativeAmount_ReducesTempBalance_Passes()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 4800.00m,
            CurrentCycleDebit = 100.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account);

        var verified = CreateVerifiedTransaction("TXN006", "4000000000000001",
            "00000000001", 100000001, -500.00m, new DateTime(2026, 1, 15));

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.True(result.IsValid);
        Assert.Empty(result.Rejections);
    }

    // ===================================================================
    // AC-7: Both credit limit AND expiration fail — ALL failures collected
    // Improvement over COBOL's "last wins" behavior
    // ===================================================================

    [Fact]
    public async Task BothCreditLimitAndExpirationFail_CollectsAllRejections()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 4000.00m,
            CurrentCycleDebit = 200.00m,
            ExpirationDate = new DateTime(2025, 12, 31)
        };
        _accountRepo.Add(account);

        // Amount 1500 → tempBalance = 4000 - 200 + 1500 = 5300 > 5000 → OVERLIMIT
        // Origination 2026-01-15 > Expiration 2025-12-31 → EXPIRED
        var verified = CreateVerifiedTransaction("TXN007", "4000000000000001",
            "00000000001", 100000001, 1500.00m, new DateTime(2026, 1, 15));

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.False(result.IsValid);
        Assert.Equal(2, result.Rejections.Count);

        var codes = result.Rejections.Select(r => r.RejectCode).OrderBy(c => c).ToList();
        Assert.Equal(102, codes[0]);
        Assert.Equal(103, codes[1]);
    }

    // ===================================================================
    // Boundary: Exact credit limit — tempBalance equals limit → PASSES
    // ===================================================================

    [Fact]
    public async Task TempBalanceExactlyAtLimit_Passes()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 3000.00m,
            CurrentCycleDebit = 1000.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account);

        // tempBalance = 3000 - 1000 + 3000 = 5000.00 == CreditLimit → PASSES
        var verified = CreateVerifiedTransaction("TXN008", "4000000000000001",
            "00000000001", 100000001, 3000.00m, new DateTime(2026, 1, 15));

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.True(result.IsValid);
        Assert.Empty(result.Rejections);
    }

    // ===================================================================
    // Boundary: Expiration date matches origination date → PASSES
    // ===================================================================

    [Fact]
    public async Task ExpirationDateEqualsOriginationDate_Passes()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 1000.00m,
            CurrentCycleDebit = 500.00m,
            ExpirationDate = new DateTime(2026, 1, 15)
        };
        _accountRepo.Add(account);

        var verified = CreateVerifiedTransaction("TXN009", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.True(result.IsValid);
        Assert.Empty(result.Rejections);
    }

    // ===================================================================
    // Empty input — no transactions to validate
    // ===================================================================

    [Fact]
    public async Task EmptyInput_ReturnsEmptyList()
    {
        var results = await _sut.ValidateTransactionsAsync([]);

        Assert.Empty(results);
    }

    // ===================================================================
    // Multiple transactions — mixed outcomes
    // ===================================================================

    [Fact]
    public async Task MultipleTransactions_ProcessesAllSequentially()
    {
        var account1 = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 10000.00m,
            CurrentCycleCredit = 1000.00m,
            CurrentCycleDebit = 500.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account1);

        var account2 = new Account
        {
            Id = "00000000002",
            ActiveStatus = "A",
            CreditLimit = 500.00m,
            CurrentCycleCredit = 400.00m,
            CurrentCycleDebit = 100.00m,
            ExpirationDate = new DateTime(2027, 12, 31)
        };
        _accountRepo.Add(account2);

        // Transaction 1: Valid (within limit, not expired)
        var v1 = CreateVerifiedTransaction("TXN010", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));

        // Transaction 2: Overlimit (tempBalance = 400-100+300 = 600 > 500)
        var v2 = CreateVerifiedTransaction("TXN011", "4000000000000002",
            "00000000002", 100000002, 300.00m, new DateTime(2026, 1, 15));

        // Transaction 3: Invalid card
        var txn3 = CreateDailyTransaction("TXN012", "9999999999999999", 100.00m, new DateTime(2026, 1, 15));
        var v3 = new VerifiedTransaction
        {
            Transaction = txn3,
            IsVerified = false,
            FailureReason = "Card number could not be verified",
            AccountId = null,
            CustomerId = null
        };

        var results = await _sut.ValidateTransactionsAsync([v1, v2, v3]);

        Assert.Equal(3, results.Count);

        Assert.True(results[0].IsValid);
        Assert.Empty(results[0].Rejections);

        Assert.False(results[1].IsValid);
        Assert.Equal(102, results[1].Rejections[0].RejectCode);

        Assert.False(results[2].IsValid);
        Assert.Equal(100, results[2].Rejections[0].RejectCode);
    }

    // ===================================================================
    // Rejections are persisted to repository
    // ===================================================================

    [Fact]
    public async Task Rejections_ArePersistedToRepository()
    {
        var account = new Account
        {
            Id = "00000000001",
            ActiveStatus = "A",
            CreditLimit = 5000.00m,
            CurrentCycleCredit = 4000.00m,
            CurrentCycleDebit = 200.00m,
            ExpirationDate = new DateTime(2025, 12, 31)
        };
        _accountRepo.Add(account);

        // Both overlimit and expired
        var verified = CreateVerifiedTransaction("TXN013", "4000000000000001",
            "00000000001", 100000001, 1500.00m, new DateTime(2026, 1, 15));

        await _sut.ValidateTransactionsAsync([verified]);

        Assert.Equal(2, _rejectRepo.Rejects.Count);
        Assert.Contains(_rejectRepo.Rejects, r => r.RejectCode == 102);
        Assert.Contains(_rejectRepo.Rejects, r => r.RejectCode == 103);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task CancellationRequested_ThrowsOperationCanceledException()
    {
        var verified = CreateVerifiedTransaction("TXN014", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => _sut.ValidateTransactionsAsync([verified], cts.Token));
    }

    // ===================================================================
    // Verified transaction with account deleted between steps → code 101
    // ===================================================================

    [Fact]
    public async Task AccountDeletedBetweenSteps_ReturnsRejectedWithCode101()
    {
        // Account was found during card verification but no longer exists
        var verified = CreateVerifiedTransaction("TXN015", "4000000000000001",
            "00000000001", 100000001, 100.00m, new DateTime(2026, 1, 15));
        // Don't add account to repo → simulates deletion between steps

        var results = await _sut.ValidateTransactionsAsync([verified]);

        var result = Assert.Single(results);
        Assert.False(result.IsValid);
        var rejection = Assert.Single(result.Rejections);
        Assert.Equal(101, rejection.RejectCode);
        Assert.Equal("ACCOUNT RECORD NOT FOUND", rejection.RejectReason);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static VerifiedTransaction CreateVerifiedTransaction(
        string id, string cardNumber, string accountId, int customerId,
        decimal amount, DateTime originationDate)
    {
        var transaction = CreateDailyTransaction(id, cardNumber, amount, originationDate);
        return new VerifiedTransaction
        {
            Transaction = transaction,
            IsVerified = true,
            AccountId = accountId,
            CustomerId = customerId
        };
    }

    private static DailyTransaction CreateDailyTransaction(
        string id, string cardNumber, decimal amount, DateTime originationDate) => new()
        {
            Id = id,
            CardNumber = cardNumber,
            TypeCode = "01",
            CategoryCode = 1001,
            Source = "BATCH",
            Description = "Test daily transaction",
            Amount = amount,
            MerchantId = 1,
            MerchantName = "Test Merchant",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            OriginationTimestamp = originationDate,
            ProcessingTimestamp = originationDate.AddDays(1)
        };
}

// ===================================================================
// Test doubles
// ===================================================================

internal sealed class StubAccountRepository2 : IAccountRepository
{
    private readonly Dictionary<string, Account> _accounts = [];

    public void Add(Account account) => _accounts[account.Id] = account;

    public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_accounts.GetValueOrDefault(accountId));

    public Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
        => Task.CompletedTask;
}

internal sealed class StubDailyRejectRepository : IDailyRejectRepository
{
    public List<DailyReject> Rejects { get; } = [];

    public Task AddAsync(DailyReject reject, CancellationToken cancellationToken = default)
    {
        Rejects.Add(reject);
        return Task.CompletedTask;
    }
}
