using System.Globalization;
using Microsoft.Extensions.Logging.Abstractions;
using NordKredit.Domain.Transactions;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Transactions;

/// <summary>
/// Step definitions for daily transaction detail report BDD scenarios (TRN-BR-006).
/// COBOL source: CBTRN03C.cbl:158-374 (daily transaction detail report step 3 of 3).
/// Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility),
///              AML 2017:11 Para.3 (monitoring).
/// </summary>
[Binding]
[Scope(Feature = "Daily transaction detail report generation")]
public sealed class TransactionReportingStepDefinitions
{
    private readonly StubTransactionRepository _transactionRepo = new();
    private readonly StubCardCrossReferenceRepository _xrefRepo = new();
    private readonly StubTransactionTypeRepository _typeRepo = new();
    private readonly StubTransactionCategoryRepository _categoryRepo = new();
    private TransactionDetailReportService _service = null!;
    private TransactionReportResult? _result;
    private Exception? _exception;

    [BeforeScenario]
    public void SetUp() =>
        _service = new TransactionDetailReportService(
            _transactionRepo,
            _xrefRepo,
            _typeRepo,
            _categoryRepo,
            NullLogger<TransactionDetailReportService>.Instance);

    [Given(@"the transaction type lookup contains")]
    public void GivenTheTransactionTypeLookupContains(Table table)
    {
        foreach (var row in table.Rows)
        {
            _typeRepo.Add(new TransactionType
            {
                TypeCode = row["TypeCode"],
                Description = row["Description"]
            });
        }
    }

    [Given(@"the transaction category lookup contains")]
    public void GivenTheTransactionCategoryLookupContains(Table table)
    {
        foreach (var row in table.Rows)
        {
            _categoryRepo.Add(new TransactionCategory
            {
                TypeCode = row["TypeCode"],
                CategoryCode = int.Parse(row["CategoryCode"], CultureInfo.InvariantCulture),
                Description = row["Description"]
            });
        }
    }

    [Given(@"the posted transaction repository contains")]
    public void GivenThePostedTransactionRepositoryContains(Table table)
    {
        foreach (var row in table.Rows)
        {
            _transactionRepo.AddTransaction(new Transaction
            {
                Id = row["Id"],
                CardNumber = row["CardNumber"],
                TypeCode = row["TypeCode"],
                CategoryCode = int.Parse(row["CategoryCode"], CultureInfo.InvariantCulture),
                Amount = decimal.Parse(row["Amount"], CultureInfo.InvariantCulture),
                Description = "TEST",
                Source = "BATCH",
                OriginationTimestamp = DateTime.ParseExact(
                    row["OriginationTimestamp"], "yyyy-MM-dd", CultureInfo.InvariantCulture),
                ProcessingTimestamp = DateTime.UtcNow,
                MerchantName = "TEST",
                MerchantCity = "STOCKHOLM",
                MerchantZip = "11120"
            });
        }
    }

    [Given(@"the posted transaction repository contains (\d+) transactions for card ""(.*)"" with amount (.+) each on ""(.*)""")]
    public void GivenThePostedTransactionRepositoryContainsNTransactionsForCardWithAmountEachOn(
        int count, string cardNumber, decimal amount, string date)
    {
        var timestamp = DateTime.ParseExact(date, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        for (var i = 1; i <= count; i++)
        {
            _transactionRepo.AddTransaction(new Transaction
            {
                Id = $"TRN{i:D8}",
                CardNumber = cardNumber,
                TypeCode = "SA",
                CategoryCode = 5010,
                Amount = amount,
                Description = $"BULK TXN {i}",
                Source = "BATCH",
                OriginationTimestamp = timestamp,
                ProcessingTimestamp = DateTime.UtcNow,
                MerchantName = "TEST",
                MerchantCity = "STOCKHOLM",
                MerchantZip = "11120"
            });
        }
    }

    [Given(@"the card cross-reference lookup contains")]
    public void GivenTheCardCrossReferenceLookupContains(Table table)
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

    [When(@"I generate the daily report for date range ""(.*)"" to ""(.*)""")]
    public async Task WhenIGenerateTheDailyReportForDateRange(string startDate, string endDate)
    {
        var start = DateTime.ParseExact(startDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        var end = DateTime.ParseExact(endDate, "yyyy-MM-dd", CultureInfo.InvariantCulture)
            .AddDays(1).AddTicks(-1);
        try
        {
            _result = await _service.GenerateReportAsync(start, end);
        }
        catch (InvalidOperationException ex)
        {
            _exception = ex;
        }
    }

    [When(@"I generate the daily report for date range ""(.*)"" to ""(.*)"" with page size (\d+)")]
    public async Task WhenIGenerateTheDailyReportForDateRangeWithPageSize(
        string startDate, string endDate, int pageSize)
    {
        var start = DateTime.ParseExact(startDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);
        var end = DateTime.ParseExact(endDate, "yyyy-MM-dd", CultureInfo.InvariantCulture)
            .AddDays(1).AddTicks(-1);
        try
        {
            _result = await _service.GenerateReportAsync(start, end, pageSize);
        }
        catch (InvalidOperationException ex)
        {
            _exception = ex;
        }
    }

    [Then(@"the report contains (\d+) detail lines?")]
    public void ThenTheReportContainsNDetailLines(int expectedCount)
    {
        Assert.NotNull(_result);
        Assert.Equal(expectedCount, _result.DetailLineCount);
    }

    [Then(@"the report grand total is (.+)")]
    public void ThenTheReportGrandTotalIs(decimal expectedTotal)
    {
        Assert.NotNull(_result);
        Assert.Equal(expectedTotal, _result.GrandTotal);
    }

    [Then(@"the report has (\d+) account groups?")]
    public void ThenTheReportHasNAccountGroups(int expectedCount)
    {
        Assert.NotNull(_result);
        Assert.Equal(expectedCount, _result.AccountGroupCount);
    }

    [Then(@"the account total for card ""(.*)"" is (.+)")]
    public void ThenTheAccountTotalForCardIs(string cardNumber, decimal expectedTotal)
    {
        Assert.NotNull(_result);
        var accountTotal = _result.AccountTotals.First(a => a.CardNumber == cardNumber);
        Assert.Equal(expectedTotal, accountTotal.Total);
    }

    [Then(@"the report has (\d+) page totals?")]
    public void ThenTheReportHasNPageTotals(int expectedCount)
    {
        Assert.NotNull(_result);
        Assert.Equal(expectedCount, _result.PageTotals.Count);
    }

    [Then(@"page total (\d+) is (.+)")]
    public void ThenPageTotalNIs(int pageIndex, decimal expectedTotal)
    {
        Assert.NotNull(_result);
        Assert.Equal(expectedTotal, _result.PageTotals[pageIndex - 1]);
    }

    [Then(@"report line (\d+) has type description ""(.*)""")]
    public void ThenReportLineNHasTypeDescription(int lineIndex, string expectedDescription)
    {
        Assert.NotNull(_result);
        Assert.Equal(expectedDescription, _result.Lines[lineIndex - 1].TypeDescription);
    }

    [Then(@"report line (\d+) has category description ""(.*)""")]
    public void ThenReportLineNHasCategoryDescription(int lineIndex, string expectedDescription)
    {
        Assert.NotNull(_result);
        Assert.Equal(expectedDescription, _result.Lines[lineIndex - 1].CategoryDescription);
    }

    [Then(@"the report generation fails with a data integrity error")]
    public void ThenTheReportGenerationFailsWithADataIntegrityError()
    {
        Assert.NotNull(_exception);
        Assert.Contains("ABEND", _exception.Message);
    }

    internal sealed class StubTransactionRepository : ITransactionRepository
    {
        private readonly List<Transaction> _transactions = [];

        public void AddTransaction(Transaction transaction) => _transactions.Add(transaction);

        public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_transactions.FirstOrDefault(t => t.Id == transactionId));

        public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(string accountId, int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);

        public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;

        public Task<IReadOnlyList<Transaction>> GetPageAsync(int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);

        public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<Transaction?>(null);

        public Task<IReadOnlyList<Transaction>> GetByDateRangeAsync(DateTime startDate, DateTime endDate, CancellationToken cancellationToken = default)
        {
            IReadOnlyList<Transaction> result = [.. _transactions
                .Where(t => t.OriginationTimestamp >= startDate && t.OriginationTimestamp <= endDate)
                .OrderBy(t => t.CardNumber)
                .ThenBy(t => t.Id)];
            return Task.FromResult(result);
        }
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

    internal sealed class StubTransactionTypeRepository : ITransactionTypeRepository
    {
        private readonly List<TransactionType> _types = [];

        public void Add(TransactionType type) => _types.Add(type);

        public Task<TransactionType?> GetByCodeAsync(string typeCode, CancellationToken cancellationToken = default) =>
            Task.FromResult(_types.FirstOrDefault(t => t.TypeCode == typeCode));
    }

    internal sealed class StubTransactionCategoryRepository : ITransactionCategoryRepository
    {
        private readonly List<TransactionCategory> _categories = [];

        public void Add(TransactionCategory category) => _categories.Add(category);

        public Task<TransactionCategory?> GetAsync(string typeCode, int categoryCode, CancellationToken cancellationToken = default) =>
            Task.FromResult(_categories.FirstOrDefault(c =>
                c.TypeCode == typeCode && c.CategoryCode == categoryCode));
    }
}
