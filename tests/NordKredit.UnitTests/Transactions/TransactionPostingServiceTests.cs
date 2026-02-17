using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionPostingService — batch transaction posting with
/// balance update and category balance upsert.
/// COBOL source: CBTRN02C.cbl:424-579 (posting), CBTRN02C.cbl:467-542 (TCATBAL upsert).
/// Covers account balance update, cycle credit/debit, category balance upsert,
/// transaction record creation, and atomicity.
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// PSD2 Art.94 (retention).
/// </summary>
public class TransactionPostingServiceTests
{
    private readonly StubAccountRepositoryForPosting _accountRepo = new();
    private readonly StubTransactionRepositoryForPosting _transactionRepo = new();
    private readonly StubTransactionCategoryBalanceRepositoryForPosting _categoryBalanceRepo = new();
    private readonly StubDailyTransactionRepositoryForPosting _dailyTransactionRepo = new();
    private readonly StubUnitOfWork _unitOfWork = new();
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
            _unitOfWork,
            _logger);
    }

    // ===================================================================
    // AC-1: Positive amount — balance increases, cycleCredit increases
    // CBTRN02C.cbl:424-479 — PROCESS-TRANSACTION posting
    // Account balance 1000, cycleCredit 500, cycleDebit 200, amount +150
    // THEN balance 1150, cycleCredit 650, cycleDebit stays 200
    // ===================================================================

    [Fact]
    public async Task PositiveAmount_UpdatesBalanceAndCycleCredit()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN001", "4000000000000001",
            "00000000001", "01", 1, 150.00m);

        var results = await _sut.PostTransactionsAsync([validated]);

        Assert.Equal(1, results.PostedCount);
        Assert.Equal(1150.00m, account.CurrentBalance);
        Assert.Equal(650.00m, account.CurrentCycleCredit);
        Assert.Equal(200.00m, account.CurrentCycleDebit);
    }

    // ===================================================================
    // AC-2: Negative amount — balance decreases, cycleDebit increases (abs)
    // Account balance 5000, cycleCredit 4000, cycleDebit 1000, amount -500
    // THEN balance 4500, cycleCredit stays 4000, cycleDebit 1500
    // ===================================================================

    [Fact]
    public async Task NegativeAmount_UpdatesBalanceAndCycleDebit()
    {
        var account = CreateAccount("00000000001", 5000.00m, 4000.00m, 1000.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN002", "4000000000000001",
            "00000000001", "01", 1, -500.00m);

        var results = await _sut.PostTransactionsAsync([validated]);

        Assert.Equal(1, results.PostedCount);
        Assert.Equal(4500.00m, account.CurrentBalance);
        Assert.Equal(4000.00m, account.CurrentCycleCredit);
        Assert.Equal(1500.00m, account.CurrentCycleDebit);
    }

    // ===================================================================
    // AC-3: Processing timestamp set to UTC with precision
    // ===================================================================

    [Fact]
    public async Task PostedTransaction_HasUtcProcessingTimestamp()
    {
        var account = CreateAccount("00000000001", 1000.00m, 0m, 0m);
        _accountRepo.Add(account);

        var before = DateTime.UtcNow;
        var validated = CreateValidatedTransaction("TXN003", "4000000000000001",
            "00000000001", "01", 1, 100.00m);

        await _sut.PostTransactionsAsync([validated]);
        var after = DateTime.UtcNow;

        var posted = Assert.Single(_transactionRepo.Transactions);
        Assert.Equal(DateTimeKind.Utc, posted.ProcessingTimestamp.Kind);
        Assert.True(posted.ProcessingTimestamp >= before);
        Assert.True(posted.ProcessingTimestamp <= after);
    }

    // ===================================================================
    // AC-4: Category balance upsert — new record created
    // No TCATBAL for account "00000000042" / type "01" / category 1, amount +250
    // THEN new record with balance 250
    // ===================================================================

    [Fact]
    public async Task CategoryBalance_NewRecord_CreatedWithCorrectBalance()
    {
        var account = CreateAccount("00000000042", 0m, 0m, 0m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN004", "4000000000000042",
            "00000000042", "01", 1, 250.00m);

        await _sut.PostTransactionsAsync([validated]);

        var catBal = _categoryBalanceRepo.GetBalance("00000000042", "01", 1);
        Assert.NotNull(catBal);
        Assert.Equal(250.00m, catBal.Balance);
    }

    // ===================================================================
    // AC-5: Category balance upsert — existing record updated (positive)
    // Existing TCATBAL balance 1000, amount +150 → balance 1150
    // ===================================================================

    [Fact]
    public async Task CategoryBalance_ExistingRecord_UpdatedWithPositiveAmount()
    {
        var account = CreateAccount("00000000001", 5000.00m, 0m, 0m);
        _accountRepo.Add(account);

        _categoryBalanceRepo.Add(new TransactionCategoryBalance
        {
            AccountId = "00000000001",
            TypeCode = "01",
            CategoryCode = 1,
            Balance = 1000.00m
        });

        var validated = CreateValidatedTransaction("TXN005", "4000000000000001",
            "00000000001", "01", 1, 150.00m);

        await _sut.PostTransactionsAsync([validated]);

        var catBal = _categoryBalanceRepo.GetBalance("00000000001", "01", 1);
        Assert.NotNull(catBal);
        Assert.Equal(1150.00m, catBal.Balance);
    }

    // ===================================================================
    // AC-6: Category balance upsert — existing record updated (negative/credit)
    // Existing TCATBAL balance 500, amount -200 → balance 300
    // ===================================================================

    [Fact]
    public async Task CategoryBalance_ExistingRecord_UpdatedWithNegativeAmount()
    {
        var account = CreateAccount("00000000001", 5000.00m, 0m, 0m);
        _accountRepo.Add(account);

        _categoryBalanceRepo.Add(new TransactionCategoryBalance
        {
            AccountId = "00000000001",
            TypeCode = "01",
            CategoryCode = 1,
            Balance = 500.00m
        });

        var validated = CreateValidatedTransaction("TXN006", "4000000000000001",
            "00000000001", "01", 1, -200.00m);

        await _sut.PostTransactionsAsync([validated]);

        var catBal = _categoryBalanceRepo.GetBalance("00000000001", "01", 1);
        Assert.NotNull(catBal);
        Assert.Equal(300.00m, catBal.Balance);
    }

    // ===================================================================
    // AC-7: Transaction record written with correct field mapping
    // ===================================================================

    [Fact]
    public async Task PostedTransaction_HasCorrectFieldMapping()
    {
        var account = CreateAccount("00000000001", 1000.00m, 0m, 0m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN007", "4000000000000001",
            "00000000001", "01", 1001, 500.00m);

        await _sut.PostTransactionsAsync([validated]);

        var posted = Assert.Single(_transactionRepo.Transactions);
        Assert.Equal("TXN007", posted.Id);
        Assert.Equal("01", posted.TypeCode);
        Assert.Equal(1001, posted.CategoryCode);
        Assert.Equal(500.00m, posted.Amount);
        Assert.Equal("4000000000000001", posted.CardNumber);
    }

    // ===================================================================
    // AC-8: Zero amount — 0.00 >= 0 → goes to cycleCredit (matches COBOL)
    // ===================================================================

    [Fact]
    public async Task ZeroAmount_GoesToCycleCredit()
    {
        var account = CreateAccount("00000000001", 1000.00m, 500.00m, 200.00m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN008", "4000000000000001",
            "00000000001", "01", 1, 0.00m);

        await _sut.PostTransactionsAsync([validated]);

        Assert.Equal(1000.00m, account.CurrentBalance);
        Assert.Equal(500.00m, account.CurrentCycleCredit);
        Assert.Equal(200.00m, account.CurrentCycleDebit);
    }

    // ===================================================================
    // AC-9: Atomicity — UnitOfWork transaction used
    // ===================================================================

    [Fact]
    public async Task Posting_UsesUnitOfWorkTransaction()
    {
        var account = CreateAccount("00000000001", 1000.00m, 0m, 0m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN009", "4000000000000001",
            "00000000001", "01", 1, 100.00m);

        await _sut.PostTransactionsAsync([validated]);

        Assert.True(_unitOfWork.BeginCalled);
        Assert.True(_unitOfWork.CommitCalled);
        Assert.False(_unitOfWork.RollbackCalled);
    }

    // ===================================================================
    // AC-10: Atomicity — rollback on failure
    // ===================================================================

    [Fact]
    public async Task Posting_RollsBackOnTransactionWriteFailure()
    {
        var account = CreateAccount("00000000001", 1000.00m, 0m, 0m);
        _accountRepo.Add(account);

        _transactionRepo.FailOnAdd = true;

        var validated = CreateValidatedTransaction("TXN010", "4000000000000001",
            "00000000001", "01", 1, 100.00m);

        await Assert.ThrowsAsync<InvalidOperationException>(
            () => _sut.PostTransactionsAsync([validated]));

        Assert.True(_unitOfWork.BeginCalled);
        Assert.True(_unitOfWork.RollbackCalled);
        Assert.False(_unitOfWork.CommitCalled);
    }

    // ===================================================================
    // AC-11: Invalid transactions are skipped
    // ===================================================================

    [Fact]
    public async Task InvalidTransactions_AreSkipped()
    {
        var validated = CreateInvalidTransaction("TXN011", "4000000000000001");

        var results = await _sut.PostTransactionsAsync([validated]);

        Assert.Equal(0, results.PostedCount);
        Assert.Equal(1, results.SkippedCount);
        Assert.Empty(_transactionRepo.Transactions);
    }

    // ===================================================================
    // AC-12: Multiple transactions — all posted sequentially
    // ===================================================================

    [Fact]
    public async Task MultipleTransactions_AllPostedSequentially()
    {
        var account1 = CreateAccount("00000000001", 1000.00m, 100.00m, 50.00m);
        _accountRepo.Add(account1);

        var account2 = CreateAccount("00000000002", 2000.00m, 200.00m, 100.00m);
        _accountRepo.Add(account2);

        var v1 = CreateValidatedTransaction("TXN012", "4000000000000001",
            "00000000001", "01", 1, 150.00m);
        var v2 = CreateValidatedTransaction("TXN013", "4000000000000002",
            "00000000002", "01", 1, -300.00m);

        var results = await _sut.PostTransactionsAsync([v1, v2]);

        Assert.Equal(2, results.PostedCount);
        Assert.Equal(2, _transactionRepo.Transactions.Count);

        // Account 1: balance 1150, cycleCredit 250
        Assert.Equal(1150.00m, account1.CurrentBalance);
        Assert.Equal(250.00m, account1.CurrentCycleCredit);

        // Account 2: balance 1700, cycleDebit 400
        Assert.Equal(1700.00m, account2.CurrentBalance);
        Assert.Equal(400.00m, account2.CurrentCycleDebit);
    }

    // ===================================================================
    // AC-13: Empty input
    // ===================================================================

    [Fact]
    public async Task EmptyInput_ReturnsZeroCounts()
    {
        var results = await _sut.PostTransactionsAsync([]);

        Assert.Equal(0, results.PostedCount);
        Assert.Equal(0, results.SkippedCount);
        Assert.Equal(0, results.FailedCount);
    }

    // ===================================================================
    // AC-14: Daily transaction marked as processed
    // ===================================================================

    [Fact]
    public async Task PostedTransaction_DailyTransactionMarkedAsProcessed()
    {
        var account = CreateAccount("00000000001", 1000.00m, 0m, 0m);
        _accountRepo.Add(account);

        var validated = CreateValidatedTransaction("TXN014", "4000000000000001",
            "00000000001", "01", 1, 100.00m);

        await _sut.PostTransactionsAsync([validated]);

        Assert.Contains("TXN014", _dailyTransactionRepo.ProcessedIds);
    }

    // ===================================================================
    // AC-15: Cancellation support
    // ===================================================================

    [Fact]
    public async Task CancellationRequested_ThrowsOperationCanceledException()
    {
        var validated = CreateValidatedTransaction("TXN015", "4000000000000001",
            "00000000001", "01", 1, 100.00m);

        using var cts = new CancellationTokenSource();
        cts.Cancel();

        await Assert.ThrowsAsync<OperationCanceledException>(
            () => _sut.PostTransactionsAsync([validated], cts.Token));
    }

    // ===================================================================
    // AC-16: Account not found during posting — transaction fails
    // ===================================================================

    [Fact]
    public async Task AccountNotFoundDuringPosting_TransactionFails()
    {
        // Don't add account to repo — simulates account not found
        var validated = CreateValidatedTransaction("TXN016", "4000000000000001",
            "00000000001", "01", 1, 100.00m);

        var results = await _sut.PostTransactionsAsync([validated]);

        Assert.Equal(0, results.PostedCount);
        Assert.Equal(1, results.FailedCount);
        Assert.Empty(_transactionRepo.Transactions);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static Account CreateAccount(
        string id, decimal balance, decimal cycleCredit, decimal cycleDebit) => new()
        {
            Id = id,
            ActiveStatus = "A",
            CurrentBalance = balance,
            CreditLimit = 99999.00m,
            CashCreditLimit = 99999.00m,
            CurrentCycleCredit = cycleCredit,
            CurrentCycleDebit = cycleDebit,
            ExpirationDate = new DateTime(2027, 12, 31)
        };

    private static ValidatedTransaction CreateValidatedTransaction(
        string id, string cardNumber, string accountId,
        string typeCode, int categoryCode, decimal amount)
    {
        var transaction = new DailyTransaction
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
            OriginationTimestamp = new DateTime(2026, 1, 15, 10, 30, 0, DateTimeKind.Utc),
            ProcessingTimestamp = DateTime.MinValue
        };

        var verified = new VerifiedTransaction
        {
            Transaction = transaction,
            IsVerified = true,
            AccountId = accountId,
            CustomerId = 100000001
        };

        return new ValidatedTransaction
        {
            VerifiedTransaction = verified,
            IsValid = true,
            Rejections = []
        };
    }

    private static ValidatedTransaction CreateInvalidTransaction(string id, string cardNumber)
    {
        var transaction = new DailyTransaction
        {
            Id = id,
            CardNumber = cardNumber,
            TypeCode = "01",
            CategoryCode = 1,
            Source = "BATCH",
            Description = "Invalid transaction",
            Amount = 100.00m,
            MerchantId = 1,
            MerchantName = "Test",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            OriginationTimestamp = new DateTime(2026, 1, 15, 10, 30, 0, DateTimeKind.Utc),
            ProcessingTimestamp = DateTime.MinValue
        };

        var verified = new VerifiedTransaction
        {
            Transaction = transaction,
            IsVerified = false,
            FailureReason = "Card not found",
            AccountId = null,
            CustomerId = null
        };

        return new ValidatedTransaction
        {
            VerifiedTransaction = verified,
            IsValid = false,
            Rejections = [new DailyReject
            {
                TransactionId = id,
                CardNumber = cardNumber,
                AccountId = string.Empty,
                RejectCode = 100,
                RejectReason = "INVALID CARD NUMBER FOUND",
                TransactionAmount = 100.00m,
                RejectedAt = DateTime.UtcNow
            }]
        };
    }
}

// ===================================================================
// Test doubles for posting tests
// ===================================================================

internal sealed class StubAccountRepositoryForPosting : IAccountRepository
{
    private readonly Dictionary<string, Account> _accounts = [];

    public void Add(Account account) => _accounts[account.Id] = account;

    public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_accounts.GetValueOrDefault(accountId));

    public Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
    {
        _accounts[account.Id] = account;
        return Task.CompletedTask;
    }
}

internal sealed class StubTransactionRepositoryForPosting : ITransactionRepository
{
    public List<Transaction> Transactions { get; } = [];
    public bool FailOnAdd { get; set; }

    public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default)
        => Task.FromResult(Transactions.FirstOrDefault(t => t.Id == transactionId));

    public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(
        string accountId, int pageSize, string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>([]);

    public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
    {
        if (FailOnAdd)
        {
            throw new InvalidOperationException("Simulated TRANSACT write failure");
        }

        Transactions.Add(transaction);
        return Task.CompletedTask;
    }

    public Task<IReadOnlyList<Transaction>> GetPageAsync(
        int pageSize, string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>([]);

    public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default)
        => Task.FromResult<Transaction?>(null);
}

internal sealed class StubTransactionCategoryBalanceRepositoryForPosting : ITransactionCategoryBalanceRepository
{
    private readonly Dictionary<string, TransactionCategoryBalance> _balances = [];

    public void Add(TransactionCategoryBalance balance)
        => _balances[Key(balance.AccountId, balance.TypeCode, balance.CategoryCode)] = balance;

    public TransactionCategoryBalance? GetBalance(string accountId, string typeCode, int categoryCode)
        => _balances.GetValueOrDefault(Key(accountId, typeCode, categoryCode));

    public Task<TransactionCategoryBalance?> GetAsync(
        string accountId, string typeCode, int categoryCode,
        CancellationToken cancellationToken = default)
        => Task.FromResult(_balances.GetValueOrDefault(Key(accountId, typeCode, categoryCode)));

    public Task<IReadOnlyList<TransactionCategoryBalance>> GetByAccountIdAsync(
        string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<TransactionCategoryBalance>>(
            [.. _balances.Values.Where(b => b.AccountId == accountId)]);

    public Task UpsertAsync(TransactionCategoryBalance balance, CancellationToken cancellationToken = default)
    {
        _balances[Key(balance.AccountId, balance.TypeCode, balance.CategoryCode)] = balance;
        return Task.CompletedTask;
    }

    private static string Key(string accountId, string typeCode, int categoryCode)
        => $"{accountId}|{typeCode}|{categoryCode}";
}

internal sealed class StubDailyTransactionRepositoryForPosting : IDailyTransactionRepository
{
    public List<string> ProcessedIds { get; } = [];

    public Task<IReadOnlyList<DailyTransaction>> GetUnprocessedAsync(
        CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<DailyTransaction>>([]);

    public Task AddAsync(DailyTransaction dailyTransaction, CancellationToken cancellationToken = default)
        => Task.CompletedTask;

    public Task MarkAsProcessedAsync(string transactionId, CancellationToken cancellationToken = default)
    {
        ProcessedIds.Add(transactionId);
        return Task.CompletedTask;
    }
}

internal sealed class StubUnitOfWork : IUnitOfWork
{
    public bool BeginCalled { get; private set; }
    public bool CommitCalled { get; private set; }
    public bool RollbackCalled { get; private set; }

    public Task BeginTransactionAsync(CancellationToken cancellationToken = default)
    {
        BeginCalled = true;
        return Task.CompletedTask;
    }

    public Task CommitAsync(CancellationToken cancellationToken = default)
    {
        CommitCalled = true;
        return Task.CompletedTask;
    }

    public Task RollbackAsync(CancellationToken cancellationToken = default)
    {
        RollbackCalled = true;
        return Task.CompletedTask;
    }
}
