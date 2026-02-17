using System.Globalization;
using NordKredit.Domain.Transactions;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Transactions;

/// <summary>
/// Step definitions for transaction list BDD scenarios (TRN-BR-001).
/// COBOL source: COTRN00C.cbl:94-328 (CICS transaction CT00 — paginated transaction list).
/// Regulations: FFFS 2014:5 Ch.8 (operational info systems), PSD2 Art.94 (transaction history access).
/// </summary>
[Binding]
[Scope(Feature = "Transaction list display with keyset pagination")]
public sealed class TransactionListStepDefinitions
{
    private readonly StubTransactionRepository _transactionRepo = new();
    private TransactionListService _service = null!;
    private TransactionListResponse _response = null!;

    [BeforeScenario]
    public void SetUp() =>
        _service = new TransactionListService(_transactionRepo);

    [Given(@"the transaction repository contains the following transactions")]
    public void GivenTheTransactionRepositoryContainsTheFollowingTransactions(Table table)
    {
        foreach (var row in table.Rows)
        {
            _transactionRepo.AddTransaction(new Transaction
            {
                Id = row["Id"],
                Description = row["Description"],
                Amount = decimal.Parse(row["Amount"], CultureInfo.InvariantCulture),
                OriginationTimestamp = DateTime.ParseExact(
                    row["OriginationTimestamp"], "yyyy-MM-dd", CultureInfo.InvariantCulture),
                TypeCode = "SA",
                CategoryCode = 5010,
                Source = "ONLINE",
                CardNumber = "4000123456789012",
                MerchantName = "TEST",
                MerchantCity = "STOCKHOLM",
                MerchantZip = "11120"
            });
        }
    }

    [Given(@"the transaction repository is empty")]
    public void GivenTheTransactionRepositoryIsEmpty() =>
        _transactionRepo.Clear();

    [When(@"I request the first page of transactions")]
    public async Task WhenIRequestTheFirstPageOfTransactions() =>
        _response = await _service.GetTransactionsAsync();

    [When(@"I request the next page using the last transaction ID as cursor")]
    public async Task WhenIRequestTheNextPageUsingTheLastTransactionIdAsCursor() =>
        _response = await _service.GetTransactionsAsync(cursor: _response.NextCursor);

    [When(@"I request transactions starting from transaction ID ""(.*)""")]
    public async Task WhenIRequestTransactionsStartingFromTransactionId(string transactionId) =>
        _response = await _service.GetTransactionsAsync(fromTransactionId: transactionId);

    [Then(@"the transaction list contains (\d+) transactions?")]
    public void ThenTheTransactionListContainsNTransactions(int expectedCount) =>
        Assert.Equal(expectedCount, _response.Transactions.Count);

    [Then(@"the transaction list contains exactly (\d+) transactions")]
    public void ThenTheTransactionListContainsExactlyNTransactions(int expectedCount) =>
        Assert.Equal(expectedCount, _response.Transactions.Count);

    [Then(@"the transaction list indicates a next page exists")]
    public void ThenTheTransactionListIndicatesANextPageExists() =>
        Assert.True(_response.HasNextPage);

    [Then(@"the transaction list indicates no next page exists")]
    public void ThenTheTransactionListIndicatesNoNextPageExists() =>
        Assert.False(_response.HasNextPage);

    [Then(@"the first transaction ID is ""(.*)""")]
    public void ThenTheFirstTransactionIdIs(string expectedId) =>
        Assert.Equal(expectedId, _response.Transactions[0].TransactionId);

    /// <summary>
    /// In-memory stub repository for transaction list BDD scenarios.
    /// Matches COBOL VSAM TRANSACT-FILE keyset read behavior.
    /// </summary>
    internal sealed class StubTransactionRepository : ITransactionRepository
    {
        private readonly List<Transaction> _transactions = [];

        public void AddTransaction(Transaction transaction) => _transactions.Add(transaction);

        public void Clear() => _transactions.Clear();

        public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_transactions.FirstOrDefault(t => t.Id == transactionId));

        public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(string accountId, int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);

        public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
        {
            _transactions.Add(transaction);
            return Task.CompletedTask;
        }

        public Task<IReadOnlyList<Transaction>> GetPageAsync(int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default)
        {
            var query = _transactions.OrderBy(t => t.Id).AsEnumerable();
            if (startAfterTransactionId is not null)
            {
                query = query.Where(t => string.Compare(t.Id, startAfterTransactionId, StringComparison.Ordinal) > 0);
            }

            IReadOnlyList<Transaction> result = [.. query.Take(pageSize)];
            return Task.FromResult(result);
        }

        public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult(_transactions.OrderByDescending(t => t.Id).FirstOrDefault());

        public Task<IReadOnlyList<Transaction>> GetByDateRangeAsync(DateTime startDate, DateTime endDate, CancellationToken cancellationToken = default)
        {
            IReadOnlyList<Transaction> result = [.. _transactions
                .Where(t => t.OriginationTimestamp >= startDate && t.OriginationTimestamp <= endDate)
                .OrderBy(t => t.CardNumber)
                .ThenBy(t => t.Id)];
            return Task.FromResult(result);
        }
    }
}
