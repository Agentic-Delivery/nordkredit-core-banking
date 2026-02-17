using System.Globalization;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Transactions;

/// <summary>
/// Step definitions for daily batch card verification BDD scenarios (TRN-BR-004).
/// COBOL source: CBTRN01C.cbl:154-251 (daily batch card verification step 1 of 3).
/// Regulations: PSD2 Art.97 (transaction authorization), FFFS 2014:5 Ch.4 S3
///              (operational risk), AML/KYC (source verification).
/// </summary>
[Binding]
[Scope(Feature = "Daily batch card and account verification")]
public sealed class TransactionVerificationStepDefinitions
{
    private readonly StubDailyTransactionRepository _dailyRepo = new();
    private readonly StubCardCrossReferenceRepository _xrefRepo = new();
    private readonly StubAccountRepository _accountRepo = new();
    private CardVerificationService _service = null!;
    private IReadOnlyList<VerifiedTransaction> _results = [];

    [BeforeScenario]
    public void SetUp() =>
        _service = new CardVerificationService(
            _dailyRepo,
            _xrefRepo,
            _accountRepo,
            NullLogger<CardVerificationService>.Instance);

    [Given(@"the daily transaction repository contains")]
    public void GivenTheDailyTransactionRepositoryContains(Table table)
    {
        foreach (var row in table.Rows)
        {
            _dailyRepo.Add(new DailyTransaction
            {
                Id = row["Id"],
                CardNumber = row["CardNumber"],
                TypeCode = row["TypeCode"],
                CategoryCode = int.Parse(row["CategoryCode"], CultureInfo.InvariantCulture),
                Amount = decimal.Parse(row["Amount"], CultureInfo.InvariantCulture),
                Description = row["Description"],
                Source = "BATCH",
                MerchantName = "TEST",
                MerchantCity = "STOCKHOLM",
                MerchantZip = "11120",
                OriginationTimestamp = DateTime.UtcNow,
                ProcessingTimestamp = DateTime.UtcNow
            });
        }
    }

    [Given(@"the daily transaction repository is empty")]
    public static void GivenTheDailyTransactionRepositoryIsEmpty() { }

    [Given(@"the card cross-reference file contains")]
    public void GivenTheCardCrossReferenceFileContains(Table table)
    {
        foreach (var row in table.Rows)
        {
            _xrefRepo.Add(new CardCrossReference
            {
                CardNumber = row["CardNumber"],
                AccountId = row["AccountId"],
                CustomerId = int.Parse(row["CustomerId"], CultureInfo.InvariantCulture)
            });
        }
    }

    [Given(@"the card cross-reference file is empty")]
    public static void GivenTheCardCrossReferenceFileIsEmpty() { }

    [Given(@"the account repository contains")]
    public void GivenTheAccountRepositoryContains(Table table)
    {
        foreach (var row in table.Rows)
        {
            _accountRepo.Add(new Account
            {
                Id = row["Id"],
                CurrentBalance = decimal.Parse(row["CurrentBalance"], CultureInfo.InvariantCulture),
                ActiveStatus = "Y"
            });
        }
    }

    [Given(@"the account repository is empty")]
    public static void GivenTheAccountRepositoryIsEmpty() { }

    [When(@"I run the daily card verification")]
    public async Task WhenIRunTheDailyCardVerification() =>
        _results = await _service.VerifyDailyTransactionsAsync();

    [Then(@"all (\d+) transactions? (?:are|is) verified")]
    public void ThenAllNTransactionsAreVerified(int expectedCount)
    {
        Assert.Equal(expectedCount, _results.Count(r => r.IsVerified));
        Assert.Equal(expectedCount, _results.Count);
    }

    [Then(@"(\d+) transactions? (?:are|is) verified")]
    public void ThenNTransactionsAreVerified(int expectedCount) =>
        Assert.Equal(expectedCount, _results.Count(r => r.IsVerified));

    [Then(@"(\d+) transactions? failed verification")]
    public void ThenNTransactionsFailedVerification(int expectedCount) =>
        Assert.Equal(expectedCount, _results.Count(r => !r.IsVerified));

    [Then(@"the verified transaction for ""(.*)"" has account ID ""(.*)""")]
    public void ThenTheVerifiedTransactionForHasAccountId(string transactionId, string expectedAccountId)
    {
        var result = _results.First(r => r.Transaction.Id == transactionId);
        Assert.True(result.IsVerified);
        Assert.Equal(expectedAccountId, result.AccountId);
    }

    [Then(@"the verification failure reason for ""(.*)"" is ""(.*)""")]
    public void ThenTheVerificationFailureReasonForIs(string transactionId, string expectedReason)
    {
        var result = _results.First(r => r.Transaction.Id == transactionId);
        Assert.False(result.IsVerified);
        Assert.Equal(expectedReason, result.FailureReason);
    }

    internal sealed class StubDailyTransactionRepository : IDailyTransactionRepository
    {
        private readonly List<DailyTransaction> _transactions = [];

        public void Add(DailyTransaction transaction) => _transactions.Add(transaction);

        public Task<IReadOnlyList<DailyTransaction>> GetUnprocessedAsync(CancellationToken cancellationToken = default)
        {
            IReadOnlyList<DailyTransaction> result = [.. _transactions];
            return Task.FromResult(result);
        }

        public Task AddAsync(DailyTransaction dailyTransaction, CancellationToken cancellationToken = default)
        {
            _transactions.Add(dailyTransaction);
            return Task.CompletedTask;
        }

        public Task MarkAsProcessedAsync(string transactionId, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }

    internal sealed class StubCardCrossReferenceRepository : ICardCrossReferenceRepository
    {
        private readonly List<CardCrossReference> _xrefs = [];

        public void Add(CardCrossReference xref) => _xrefs.Add(xref);

        public Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default) =>
            Task.FromResult(_xrefs.FirstOrDefault(x => x.CardNumber == cardNumber));

        public Task<CardCrossReference?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_xrefs.FirstOrDefault(x => x.AccountId == accountId));
    }

    internal sealed class StubAccountRepository : IAccountRepository
    {
        private readonly List<Account> _accounts = [];

        public void Add(Account account) => _accounts.Add(account);

        public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_accounts.FirstOrDefault(a => a.Id == accountId));

        public Task UpdateAsync(Account account, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }
}
