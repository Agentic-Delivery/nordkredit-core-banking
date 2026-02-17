using System.Globalization;
using NordKredit.Domain.Transactions;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Transactions;

/// <summary>
/// Step definitions for transaction detail BDD scenarios (TRN-BR-002).
/// COBOL source: COTRN01C.cbl:85-296 (CICS transaction CT01 — single transaction read).
/// Regulations: FFFS 2014:5 Ch.8, PSD2 Art.94, GDPR Art.15 (right of access).
/// </summary>
[Binding]
[Scope(Feature = "Transaction detail view and lookup")]
public sealed class TransactionDetailStepDefinitions
{
    private readonly StubTransactionRepository _transactionRepo = new();
    private TransactionDetailService _service = null!;
    private TransactionDetailResponse? _response;

    [BeforeScenario]
    public void SetUp() =>
        _service = new TransactionDetailService(_transactionRepo);

    [Given(@"the transaction repository contains a transaction with")]
    public void GivenTheTransactionRepositoryContainsATransactionWith(Table table)
    {
        var row = table.Rows[0];
        _transactionRepo.AddTransaction(new Transaction
        {
            Id = row["Id"],
            CardNumber = row["CardNumber"],
            TypeCode = row["TypeCode"],
            CategoryCode = int.Parse(row["CategoryCode"], CultureInfo.InvariantCulture),
            Source = row["Source"],
            Amount = decimal.Parse(row["Amount"], CultureInfo.InvariantCulture),
            Description = row["Description"],
            OriginationTimestamp = DateTime.ParseExact(
                row["OriginationTimestamp"], "yyyy-MM-dd", CultureInfo.InvariantCulture),
            ProcessingTimestamp = DateTime.ParseExact(
                row["ProcessingTimestamp"], "yyyy-MM-dd", CultureInfo.InvariantCulture),
            MerchantId = int.Parse(row["MerchantId"], CultureInfo.InvariantCulture),
            MerchantName = row["MerchantName"],
            MerchantCity = row["MerchantCity"],
            MerchantZip = row["MerchantZip"]
        });
    }

    [When(@"I request transaction detail for ID ""(.*)""")]
    public async Task WhenIRequestTransactionDetailForId(string transactionId) =>
        _response = await _service.GetByIdAsync(transactionId);

    [Then(@"the transaction detail response contains transaction ID ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsTransactionId(string expectedId)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedId, _response.TransactionId);
    }

    [Then(@"the transaction detail response is null")]
    public void ThenTheTransactionDetailResponseIsNull() =>
        Assert.Null(_response);

    [Then(@"the transaction detail response contains masked card number ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsMaskedCardNumber(string expectedMasked)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedMasked, _response.CardNumber);
    }

    [Then(@"the transaction detail response contains type code ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsTypeCode(string expectedTypeCode)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedTypeCode, _response.TypeCode);
    }

    [Then(@"the transaction detail response contains category code (\d+)")]
    public void ThenTheTransactionDetailResponseContainsCategoryCode(int expectedCategoryCode)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedCategoryCode, _response.CategoryCode);
    }

    [Then(@"the transaction detail response contains source ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsSource(string expectedSource)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedSource, _response.Source);
    }

    [Then(@"the transaction detail response contains amount (.+)")]
    public void ThenTheTransactionDetailResponseContainsAmount(decimal expectedAmount)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedAmount, _response.Amount);
    }

    [Then(@"the transaction detail response contains description ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsDescription(string expectedDescription)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedDescription, _response.Description);
    }

    [Then(@"the transaction detail response contains merchant name ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsMerchantName(string expectedName)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedName, _response.MerchantName);
    }

    [Then(@"the transaction detail response contains merchant city ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsMerchantCity(string expectedCity)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedCity, _response.MerchantCity);
    }

    [Then(@"the transaction detail response contains merchant zip ""(.*)""")]
    public void ThenTheTransactionDetailResponseContainsMerchantZip(string expectedZip)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedZip, _response.MerchantZip);
    }

    [Then(@"the transaction detail response contains merchant ID (\d+)")]
    public void ThenTheTransactionDetailResponseContainsMerchantId(int expectedId)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedId, _response.MerchantId);
    }

    /// <summary>
    /// In-memory stub repository for transaction detail BDD scenarios.
    /// </summary>
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

        public Task<IReadOnlyList<Transaction>> GetByDateRangeAsync(DateTime startDate, DateTime endDate, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);
    }
}
