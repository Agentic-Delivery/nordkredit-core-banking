using System.Globalization;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Transactions;

/// <summary>
/// Step definitions for daily batch transaction posting BDD scenarios (TRN-BR-005).
/// COBOL source: CBTRN02C.cbl:193-579 (daily batch transaction posting step 2 of 3).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
///              PSD2 Art.94 (retention).
/// </summary>
[Binding]
[Scope(Feature = "Daily batch transaction posting with balance updates")]
public sealed class TransactionPostingStepDefinitions
{
    private readonly StubAccountRepository _accountRepo = new();
    private readonly StubTransactionRepository _transactionRepo = new();
    private readonly StubCategoryBalanceRepository _categoryBalanceRepo = new();
    private readonly StubDailyTransactionRepository _dailyRepo = new();
    private readonly StubUnitOfWork _unitOfWork = new();
    private readonly List<ValidatedTransaction> _validatedTransactions = [];
    private TransactionPostingService _service = null!;
    private TransactionPostingServiceResult _result = null!;

    [BeforeScenario]
    public void SetUp() =>
        _service = new TransactionPostingService(
            _accountRepo,
            _transactionRepo,
            _categoryBalanceRepo,
            _dailyRepo,
            _unitOfWork,
            NullLogger<TransactionPostingService>.Instance);

    [Given(@"a verified transaction exists")]
    public void GivenAVerifiedTransactionExists(Table table)
    {
        var row = table.Rows[0];
        var accountId = row["AccountId"];
        var dailyTransaction = new DailyTransaction
        {
            Id = row["Id"],
            CardNumber = row["CardNumber"],
            TypeCode = row["TypeCode"],
            CategoryCode = int.Parse(row["CategoryCode"], CultureInfo.InvariantCulture),
            Amount = decimal.Parse(row["Amount"], CultureInfo.InvariantCulture),
            Description = "TEST",
            Source = "BATCH",
            MerchantName = "TEST",
            MerchantCity = "STOCKHOLM",
            MerchantZip = "11120",
            OriginationTimestamp = DateTime.UtcNow,
            ProcessingTimestamp = DateTime.UtcNow
        };

        _validatedTransactions.Add(new ValidatedTransaction
        {
            VerifiedTransaction = new VerifiedTransaction
            {
                Transaction = dailyTransaction,
                IsVerified = true,
                AccountId = accountId,
                CustomerId = 1001
            },
            IsValid = true,
            Rejections = []
        });
    }

    [Given(@"an invalid validated transaction exists")]
    public void GivenAnInvalidValidatedTransactionExists(Table table)
    {
        var row = table.Rows[0];
        var dailyTransaction = new DailyTransaction
        {
            Id = row["Id"],
            CardNumber = row["CardNumber"],
            TypeCode = row["TypeCode"],
            CategoryCode = int.Parse(row["CategoryCode"], CultureInfo.InvariantCulture),
            Amount = decimal.Parse(row["Amount"], CultureInfo.InvariantCulture),
            Description = "TEST",
            Source = "BATCH",
            MerchantName = "TEST",
            MerchantCity = "STOCKHOLM",
            MerchantZip = "11120",
            OriginationTimestamp = DateTime.UtcNow,
            ProcessingTimestamp = DateTime.UtcNow
        };

        _validatedTransactions.Add(new ValidatedTransaction
        {
            VerifiedTransaction = new VerifiedTransaction
            {
                Transaction = dailyTransaction,
                IsVerified = false,
                AccountId = row["AccountId"]
            },
            IsValid = false,
            Rejections = [new DailyReject
            {
                TransactionId = row["Id"],
                CardNumber = row["CardNumber"],
                RejectCode = 1,
                RejectReason = "Validation failed"
            }]
        });
    }

    [Given(@"the account ""(.*)"" has balance (.+) and cycle credit (.+) and cycle debit (.+)")]
    public void GivenTheAccountHasBalanceAndCycleCreditAndCycleDebit(
        string accountId, decimal balance, decimal cycleCredit, decimal cycleDebit)
    {
        _accountRepo.Add(new Account
        {
            Id = accountId,
            CurrentBalance = balance,
            CurrentCycleCredit = cycleCredit,
            CurrentCycleDebit = cycleDebit,
            ActiveStatus = "Y"
        });
    }

    [Given(@"no category balance exists for account ""(.*)"" type ""(.*)"" category (\d+)")]
    public static void GivenNoCategoryBalanceExistsForAccountTypeCategory(string _1, string _2, int _3)
    {
        // No-op: category balance repo is empty by default
    }

    [Given(@"the category balance for account ""(.*)"" type ""(.*)"" category (\d+) is (.+)")]
    public void GivenTheCategoryBalanceForAccountTypeCategoryIs(
        string accountId, string typeCode, int categoryCode, decimal balance)
    {
        _categoryBalanceRepo.Add(new TransactionCategoryBalance
        {
            AccountId = accountId,
            TypeCode = typeCode,
            CategoryCode = categoryCode,
            Balance = balance
        });
    }

    [When(@"I post the validated transactions")]
    public async Task WhenIPostTheValidatedTransactions() =>
        _result = await _service.PostTransactionsAsync(_validatedTransactions);

    [Then(@"the posting result shows (\d+) posted and (\d+) skipped and (\d+) failed")]
    public void ThenThePostingResultShows(int posted, int skipped, int failed)
    {
        Assert.Equal(posted, _result.PostedCount);
        Assert.Equal(skipped, _result.SkippedCount);
        Assert.Equal(failed, _result.FailedCount);
    }

    [Then(@"the account ""(.*)"" has balance (.+)")]
    public void ThenTheAccountHasBalance(string accountId, decimal expectedBalance)
    {
        var account = _accountRepo.GetByIdAsync(accountId).Result;
        Assert.NotNull(account);
        Assert.Equal(expectedBalance, account.CurrentBalance);
    }

    [Then(@"the account ""(.*)"" has cycle credit (.+)")]
    public void ThenTheAccountHasCycleCredit(string accountId, decimal expectedCredit)
    {
        var account = _accountRepo.GetByIdAsync(accountId).Result;
        Assert.NotNull(account);
        Assert.Equal(expectedCredit, account.CurrentCycleCredit);
    }

    [Then(@"the account ""(.*)"" has cycle debit (.+)")]
    public void ThenTheAccountHasCycleDebit(string accountId, decimal expectedDebit)
    {
        var account = _accountRepo.GetByIdAsync(accountId).Result;
        Assert.NotNull(account);
        Assert.Equal(expectedDebit, account.CurrentCycleDebit);
    }

    [Then(@"the category balance for account ""(.*)"" type ""(.*)"" category (\d+) is (.+)")]
    public void ThenTheCategoryBalanceForAccountTypeCategoryIs(
        string accountId, string typeCode, int categoryCode, decimal expectedBalance)
    {
        var catBal = _categoryBalanceRepo.GetAsync(accountId, typeCode, categoryCode).Result;
        Assert.NotNull(catBal);
        Assert.Equal(expectedBalance, catBal.Balance);
    }

    internal sealed class StubAccountRepository : IAccountRepository
    {
        private readonly List<Account> _accounts = [];

        public void Add(Account account) => _accounts.Add(account);

        public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_accounts.FirstOrDefault(a => a.Id == accountId));

        public Task UpdateAsync(Account account, CancellationToken cancellationToken = default)
        {
            var existing = _accounts.FindIndex(a => a.Id == account.Id);
            if (existing >= 0)
            {
                _accounts[existing] = account;
            }

            return Task.CompletedTask;
        }
    }

    internal sealed class StubTransactionRepository : ITransactionRepository
    {
        private readonly List<Transaction> _transactions = [];

        public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_transactions.FirstOrDefault(t => t.Id == transactionId));

        public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(string accountId, int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);

        public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
        {
            _transactions.Add(transaction);
            return Task.CompletedTask;
        }

        public Task<IReadOnlyList<Transaction>> GetPageAsync(int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);

        public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<Transaction?>(null);

        public Task<IReadOnlyList<Transaction>> GetByDateRangeAsync(DateTime startDate, DateTime endDate, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);
    }

    internal sealed class StubCategoryBalanceRepository : ITransactionCategoryBalanceRepository
    {
        private readonly List<TransactionCategoryBalance> _balances = [];

        public void Add(TransactionCategoryBalance balance) => _balances.Add(balance);

        public Task<TransactionCategoryBalance?> GetAsync(string accountId, string typeCode, int categoryCode, CancellationToken cancellationToken = default) =>
            Task.FromResult(_balances.FirstOrDefault(b =>
                b.AccountId == accountId && b.TypeCode == typeCode && b.CategoryCode == categoryCode));

        public Task<IReadOnlyList<TransactionCategoryBalance>> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default)
        {
            IReadOnlyList<TransactionCategoryBalance> result = [.. _balances.Where(b => b.AccountId == accountId)];
            return Task.FromResult(result);
        }

        public Task UpsertAsync(TransactionCategoryBalance balance, CancellationToken cancellationToken = default)
        {
            var existing = _balances.FindIndex(b =>
                b.AccountId == balance.AccountId && b.TypeCode == balance.TypeCode && b.CategoryCode == balance.CategoryCode);

            if (existing >= 0)
            {
                _balances[existing] = balance;
            }
            else
            {
                _balances.Add(balance);
            }

            return Task.CompletedTask;
        }
    }

    internal sealed class StubDailyTransactionRepository : IDailyTransactionRepository
    {
        public Task<IReadOnlyList<DailyTransaction>> GetUnprocessedAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<DailyTransaction>>([]);

        public Task AddAsync(DailyTransaction dailyTransaction, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;

        public Task MarkAsProcessedAsync(string transactionId, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }

    internal sealed class StubUnitOfWork : IUnitOfWork
    {
        public Task BeginTransactionAsync(CancellationToken cancellationToken = default) =>
            Task.CompletedTask;

        public Task CommitAsync(CancellationToken cancellationToken = default) =>
            Task.CompletedTask;

        public Task RollbackAsync(CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }
}
