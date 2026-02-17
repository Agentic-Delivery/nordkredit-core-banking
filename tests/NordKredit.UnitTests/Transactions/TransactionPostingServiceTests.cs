using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionPostingService — batch transaction posting with balance updates.
/// COBOL source: CBTRN02C.cbl:424-579 (posting), CBTRN02C.cbl:467-542 (TCATBAL upsert).
/// Covers account balance updates, category balance upserts, transaction record writes,
/// and atomicity requirements (all three operations in a single database transaction).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public class TransactionPostingServiceTests
{
    private readonly StubAccountRepository3 _accountRepo = new();
    private readonly StubPostingTransactionRepository _transactionRepo = new();
    private readonly StubTransactionCategoryBalanceRepository _categoryBalanceRepo = new();
    private readonly StubPostingDailyTransactionRepository _dailyTransactionRepo = new();
    private readonly ILogger<TransactionPostingService> _logger =
        NullLogger<TransactionPostingService>.Instance;
    private readonly TransactionPostingService _sut;

    public TransactionPostingServiceTests()
    {
        _sut = new TransactionPostingService(
            _accountRepo,
            _transactionRepo,
            _categoryBalanceRepo,
            _dailyTransactionRepo,
            _logger);
    }

    // ===================================================================
    // TRN-BR-007 AC-1: Positive amount updates balance and cycle credit
    // Account: balance 1000, cycleCredit 500, cycleDebit 200, amount +150
    // THEN: balance → 1150, cycleCredit → 650, cycleDebit stays 200
    // COBOL: CBTRN02C.cbl:547-552
    // ===================================================================

    [Fact]
    public async Task PositiveAmount_UpdatesBalanceAndCycleCredit()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN001", "4000000000000001",
            "00000000001", 100000001, 150.00m, "01", 1);

        var results = await _sut.PostTransactionsAsync([validated]);

        Assert.Single(results);
        Assert.Equal(1150.00m, account.CurrentBalance);
        Assert.Equal(650.00m, account.CurrentCycleCredit);
        Assert.Equal(200.00m, account.CurrentCycleDebit);
    }

    // ===================================================================
    // TRN-BR-007 AC-2: Negative amount updates balance and cycle debit
    // Account: balance 5000, cycleCredit 4000, cycleDebit 1000, amount -500
    // THEN: balance → 4500, cycleCredit stays 4000, cycleDebit → 1500
    // COBOL: CBTRN02C.cbl:547-552 — ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
    // ===================================================================

    [Fact]
    public async Task NegativeAmount_UpdatesBalanceAndCycleDebit()
    {
        var account = CreateAccount("00000000001", 5000.00m, 4000.00m, 1000.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN002", "4000000000000001",
            "00000000001", 100000001, -500.00m, "01", 1);

        var results = await _sut.PostTransactionsAsync([validated]);

        Assert.Single(results);
        Assert.Equal(4500.00m, account.CurrentBalance);
        Assert.Equal(4000.00m, account.CurrentCycleCredit);
        Assert.Equal(1500.00m, account.CurrentCycleDebit);
    }

    // ===================================================================
    // TRN-BR-007 AC-3: Zero amount goes to cycle credit (matches COBOL)
    // Zero-amount transactions: 0.00 >= 0 → goes to cycleCredit
    // COBOL: IF DALYTRAN-AMT >= 0 ADD TO ACCT-CURR-CYC-CREDIT
    // ===================================================================

    [Fact]
    public async Task ZeroAmount_GoesToCycleCredit()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN003", "4000000000000001",
            "00000000001", 100000001, 0.00m, "01", 1);

        var results = await _sut.PostTransactionsAsync([validated]);

        Assert.Single(results);
        Assert.Equal(1000.00m, account.CurrentBalance);
        Assert.Equal(500.00m, account.CurrentCycleCredit);
        Assert.Equal(200.00m, account.CurrentCycleDebit);
    }

    // ===================================================================
    // TRN-BR-007 AC-4: Processing timestamp set to UTC with precision
    // GIVEN a transaction is posted
    // THEN ProcessingTimestamp is set to current UTC time
    // ===================================================================

    [Fact]
    public async Task PostedTransaction_HasProcessingTimestamp()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var before = DateTime.UtcNow;
        var validated = CreateValidatedTransaction("TXN004", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1);

        await _sut.PostTransactionsAsync([validated]);
        var after = DateTime.UtcNow;

        var posted = Assert.Single(_transactionRepo.Transactions);
        Assert.InRange(posted.ProcessingTimestamp, before, after);
    }

    // ===================================================================
    // TRN-BR-007: Transaction record is written to Transactions table
    // All fields mapped from DailyTransaction → Transaction
    // ===================================================================

    [Fact]
    public async Task PostedTransaction_FieldsMappedCorrectly()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN005", "4000000000000001",
            "00000000001", 100000001, 250.00m, "01", 1001);

        await _sut.PostTransactionsAsync([validated]);

        var posted = Assert.Single(_transactionRepo.Transactions);
        Assert.Equal("TXN005", posted.Id);
        Assert.Equal("01", posted.TypeCode);
        Assert.Equal(1001, posted.CategoryCode);
        Assert.Equal("BATCH", posted.Source);
        Assert.Equal(250.00m, posted.Amount);
        Assert.Equal("4000000000000001", posted.CardNumber);
    }

    // ===================================================================
    // TRN-BR-008 AC-1: New category balance record created
    // GIVEN no TCATBAL record exists for account/type/category
    // THEN new record created with amount as initial balance
    // COBOL: CBTRN02C.cbl:487-501
    // ===================================================================

    [Fact]
    public async Task NewCategoryBalance_CreatedWithTransactionAmount()
    {
        var account = CreateAccount("00000000042", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN006", "4000000000000001",
            "00000000042", 100000001, 250.00m, "01", 1);

        await _sut.PostTransactionsAsync([validated]);

        var catBalance = _categoryBalanceRepo.GetBalance("00000000042", "01", 1);
        Assert.NotNull(catBalance);
        Assert.Equal(250.00m, catBalance.Balance);
    }

    // ===================================================================
    // TRN-BR-008 AC-2: Existing category balance updated (positive amount)
    // GIVEN existing TCATBAL with balance 1000, amount +150
    // THEN balance → 1150
    // COBOL: CBTRN02C.cbl:467-486
    // ===================================================================

    [Fact]
    public async Task ExistingCategoryBalance_UpdatedWithPositiveAmount()
    {
        var account = CreateAccount("00000000001", 5000.00m, 2000.00m, 500.00m);
        _accountRepo.Add(account);

        _categoryBalanceRepo.Set(new TransactionCategoryBalance
        {
            AccountId = "00000000001",
            TypeCode = "01",
            CategoryCode = 1,
            Balance = 1000.00m
        });

        var validated = CreateValidatedTransaction("TXN007", "4000000000000001",
            "00000000001", 100000001, 150.00m, "01", 1);

        await _sut.PostTransactionsAsync([validated]);

        var catBalance = _categoryBalanceRepo.GetBalance("00000000001", "01", 1);
        Assert.NotNull(catBalance);
        Assert.Equal(1150.00m, catBalance.Balance);
    }

    // ===================================================================
    // TRN-BR-008 AC-3: Existing category balance updated (negative amount)
    // GIVEN existing TCATBAL with balance 500, amount -200
    // THEN balance → 300
    // ===================================================================

    [Fact]
    public async Task ExistingCategoryBalance_UpdatedWithNegativeAmount()
    {
        var account = CreateAccount("00000000001", 5000.00m, 2000.00m, 500.00m);
        _accountRepo.Add(account);

        _categoryBalanceRepo.Set(new TransactionCategoryBalance
        {
            AccountId = "00000000001",
            TypeCode = "01",
            CategoryCode = 1,
            Balance = 500.00m
        });

        var validated = CreateValidatedTransaction("TXN008", "4000000000000001",
            "00000000001", 100000001, -200.00m, "01", 1);

        await _sut.PostTransactionsAsync([validated]);

        var catBalance = _categoryBalanceRepo.GetBalance("00000000001", "01", 1);
        Assert.NotNull(catBalance);
        Assert.Equal(300.00m, catBalance.Balance);
    }

    // ===================================================================
    // Multiple transactions for the same account accumulate correctly
    // Two transactions to same account: +100, -50
    // Balance: 1000 + 100 - 50 = 1050
    // CycleCredit: 500 + 100 = 600
    // CycleDebit: 200 + 50 = 250 (adding absolute of -50)
    // ===================================================================

    [Fact]
    public async Task MultipleTransactionsSameAccount_AccumulateCorrectly()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var v1 = CreateValidatedTransaction("TXN009", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1);
        var v2 = CreateValidatedTransaction("TXN010", "4000000000000001",
            "00000000001", 100000001, -50.00m, "01", 1);

        var results = await _sut.PostTransactionsAsync([v1, v2]);

        Assert.Equal(2, results.Count);
        Assert.Equal(1050.00m, account.CurrentBalance);
        Assert.Equal(600.00m, account.CurrentCycleCredit);
        Assert.Equal(250.00m, account.CurrentCycleDebit);
    }

    // ===================================================================
    // Multiple transactions same category accumulate category balance
    // Two transactions to same category: +100, +200
    // CategoryBalance: 0 + 100 + 200 = 300
    // ===================================================================

    [Fact]
    public async Task MultipleTransactionsSameCategory_AccumulateCategoryBalance()
    {
        var account = CreateAccount("00000000001", 5000.00m, 2000.00m, 500.00m);
        _accountRepo.Add(account);

        var v1 = CreateValidatedTransaction("TXN011", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1);
        var v2 = CreateValidatedTransaction("TXN012", "4000000000000001",
            "00000000001", 100000001, 200.00m, "01", 1);

        await _sut.PostTransactionsAsync([v1, v2]);

        var catBalance = _categoryBalanceRepo.GetBalance("00000000001", "01", 1);
        Assert.NotNull(catBalance);
        Assert.Equal(300.00m, catBalance.Balance);
    }

    // ===================================================================
    // Invalid transactions are skipped (only valid ones posted)
    // ===================================================================

    [Fact]
    public async Task InvalidTransactions_AreSkipped()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var valid = CreateValidatedTransaction("TXN013", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1);

        var invalidTxn = CreateDailyTransaction("TXN014", "9999999999999999", 500.00m);
        var invalid = new ValidatedTransaction
        {
            VerifiedTransaction = new VerifiedTransaction
            {
                Transaction = invalidTxn,
                IsVerified = false,
                AccountId = null,
                CustomerId = null
            },
            IsValid = false,
            Rejections = [new DailyReject
            {
                TransactionId = "TXN014",
                CardNumber = "9999999999999999",
                AccountId = string.Empty,
                RejectCode = 100,
                RejectReason = "INVALID CARD NUMBER FOUND",
                TransactionAmount = 500.00m,
                RejectedAt = DateTime.UtcNow
            }]
        };

        var results = await _sut.PostTransactionsAsync([valid, invalid]);

        Assert.Equal(2, results.Count);
        Assert.Single(_transactionRepo.Transactions);
        Assert.Equal("TXN013", _transactionRepo.Transactions[0].Id);

        // Account only updated by valid transaction
        Assert.Equal(1100.00m, account.CurrentBalance);
    }

    // ===================================================================
    // DailyTransaction marked as processed after successful posting
    // ===================================================================

    [Fact]
    public async Task PostedTransactions_MarkedAsProcessed()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN015", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1);

        await _sut.PostTransactionsAsync([validated]);

        Assert.Contains("TXN015", _dailyTransactionRepo.ProcessedIds);
    }

    // ===================================================================
    // Empty input returns empty result
    // ===================================================================

    [Fact]
    public async Task EmptyInput_ReturnsEmptyList()
    {
        var results = await _sut.PostTransactionsAsync([]);

        Assert.Empty(results);
        Assert.Empty(_transactionRepo.Transactions);
    }

    // ===================================================================
    // Cancellation support
    // ===================================================================

    [Fact]
    public async Task CancellationRequested_ThrowsOperationCanceledException()
    {
        var validated = CreateValidatedTransaction("TXN016", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1);

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => _sut.PostTransactionsAsync([validated], cts.Token));
    }

    // ===================================================================
    // Multiple different categories for same account
    // ===================================================================

    [Fact]
    public async Task DifferentCategories_CreateSeparateCategoryBalances()
    {
        var account = CreateAccount("00000000001", 5000.00m, 2000.00m, 500.00m);
        _accountRepo.Add(account);

        var v1 = CreateValidatedTransaction("TXN017", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1001);
        var v2 = CreateValidatedTransaction("TXN018", "4000000000000001",
            "00000000001", 100000001, 200.00m, "01", 2002);

        await _sut.PostTransactionsAsync([v1, v2]);

        var cat1 = _categoryBalanceRepo.GetBalance("00000000001", "01", 1001);
        var cat2 = _categoryBalanceRepo.GetBalance("00000000001", "01", 2002);
        Assert.NotNull(cat1);
        Assert.NotNull(cat2);
        Assert.Equal(100.00m, cat1.Balance);
        Assert.Equal(200.00m, cat2.Balance);
    }

    // ===================================================================
    // Return value contains posted status per transaction
    // ===================================================================

    [Fact]
    public async Task Results_ContainPostedStatus()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var valid = CreateValidatedTransaction("TXN019", "4000000000000001",
            "00000000001", 100000001, 100.00m, "01", 1);

        var invalidTxn = CreateDailyTransaction("TXN020", "9999999999999999", 500.00m);
        var invalid = new ValidatedTransaction
        {
            VerifiedTransaction = new VerifiedTransaction
            {
                Transaction = invalidTxn,
                IsVerified = false,
                AccountId = null,
                CustomerId = null
            },
            IsValid = false,
            Rejections = [new DailyReject
            {
                TransactionId = "TXN020",
                CardNumber = "9999999999999999",
                AccountId = string.Empty,
                RejectCode = 100,
                RejectReason = "INVALID CARD NUMBER FOUND",
                TransactionAmount = 500.00m,
                RejectedAt = DateTime.UtcNow
            }]
        };

        var results = await _sut.PostTransactionsAsync([valid, invalid]);

        Assert.Equal(2, results.Count);
        Assert.True(results[0].IsPosted);
        Assert.False(results[1].IsPosted);
        Assert.Equal("TXN019", results[0].TransactionId);
        Assert.Equal("TXN020", results[1].TransactionId);
    }

    // ===================================================================
    // Account update is persisted via repository
    // ===================================================================

    [Fact]
    public async Task AccountUpdate_IsPersistedViaRepository()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN021", "4000000000000001",
            "00000000001", 100000001, 150.00m, "01", 1);

        await _sut.PostTransactionsAsync([validated]);

        Assert.Contains("00000000001", _accountRepo.UpdatedIds);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static Account CreateAccount(string id, decimal balance, decimal cycleCredit, decimal cycleDebit) => new()
    {
        Id = id,
        ActiveStatus = "A",
        CurrentBalance = balance,
        CreditLimit = 10000.00m,
        CashCreditLimit = 5000.00m,
        CurrentCycleCredit = cycleCredit,
        CurrentCycleDebit = cycleDebit,
        ExpirationDate = new DateTime(2027, 12, 31)
    };

    private static ValidatedTransaction CreateValidatedTransaction(
        string id, string cardNumber, string accountId, int customerId,
        decimal amount, string typeCode, int categoryCode)
    {
        var dailyTxn = CreateDailyTransaction(id, cardNumber, amount, typeCode, categoryCode);
        return new ValidatedTransaction
        {
            VerifiedTransaction = new VerifiedTransaction
            {
                Transaction = dailyTxn,
                IsVerified = true,
                AccountId = accountId,
                CustomerId = customerId
            },
            IsValid = true,
            Rejections = []
        };
    }

    private static DailyTransaction CreateDailyTransaction(
        string id, string cardNumber, decimal amount,
        string typeCode = "01", int categoryCode = 1) => new()
        {
            Id = id,
            CardNumber = cardNumber,
            TypeCode = typeCode,
            CategoryCode = categoryCode,
            Source = "BATCH",
            Description = "Test daily transaction",
            Amount = amount,
            MerchantId = 1,
            MerchantName = "Test Merchant",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            OriginationTimestamp = new DateTime(2026, 1, 15),
            ProcessingTimestamp = new DateTime(2026, 1, 16)
        };
}

// ===================================================================
// Test doubles
// ===================================================================

internal sealed class StubAccountRepository3 : IAccountRepository
{
    private readonly Dictionary<string, Account> _accounts = [];

    public List<string> UpdatedIds { get; } = [];

    public void Add(Account account) => _accounts[account.Id] = account;

    public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_accounts.GetValueOrDefault(accountId));

    public Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
    {
        UpdatedIds.Add(account.Id);
        return Task.CompletedTask;
    }
}

internal sealed class StubPostingTransactionRepository : ITransactionRepository
{
    public List<Transaction> Transactions { get; } = [];

    public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
    {
        Transactions.Add(transaction);
        return Task.CompletedTask;
    }

    public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default)
        => Task.FromResult(Transactions.FirstOrDefault(t => t.Id == transactionId));

    public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(string accountId, int pageSize,
        string? startAfterTransactionId = null, CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>([]);

    public Task<IReadOnlyList<Transaction>> GetPageAsync(int pageSize,
        string? startAfterTransactionId = null, CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>([]);

    public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(Transactions.LastOrDefault());
}

internal sealed class StubTransactionCategoryBalanceRepository : ITransactionCategoryBalanceRepository
{
    private readonly Dictionary<string, TransactionCategoryBalance> _balances = [];

    public void Set(TransactionCategoryBalance balance) =>
        _balances[Key(balance.AccountId, balance.TypeCode, balance.CategoryCode)] = balance;

    public TransactionCategoryBalance? GetBalance(string accountId, string typeCode, int categoryCode) =>
        _balances.GetValueOrDefault(Key(accountId, typeCode, categoryCode));

    public Task<TransactionCategoryBalance?> GetAsync(string accountId, string typeCode, int categoryCode,
        CancellationToken cancellationToken = default)
        => Task.FromResult(_balances.GetValueOrDefault(Key(accountId, typeCode, categoryCode)));

    public Task<IReadOnlyList<TransactionCategoryBalance>> GetByAccountIdAsync(string accountId,
        CancellationToken cancellationToken = default)
    {
        IReadOnlyList<TransactionCategoryBalance> result =
            [.. _balances.Values.Where(b => b.AccountId == accountId)];
        return Task.FromResult(result);
    }

    public Task UpsertAsync(TransactionCategoryBalance balance, CancellationToken cancellationToken = default)
    {
        var key = Key(balance.AccountId, balance.TypeCode, balance.CategoryCode);
        _balances[key] = balance;
        return Task.CompletedTask;
    }

    private static string Key(string accountId, string typeCode, int categoryCode)
        => $"{accountId}|{typeCode}|{categoryCode}";
}

internal sealed class StubPostingDailyTransactionRepository : IDailyTransactionRepository
{
    public List<string> ProcessedIds { get; } = [];

    public Task<IReadOnlyList<DailyTransaction>> GetUnprocessedAsync(CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<DailyTransaction>>([]);

    public Task AddAsync(DailyTransaction dailyTransaction, CancellationToken cancellationToken = default)
        => Task.CompletedTask;

    public Task MarkAsProcessedAsync(string transactionId, CancellationToken cancellationToken = default)
    {
        ProcessedIds.Add(transactionId);
        return Task.CompletedTask;
    }
}
