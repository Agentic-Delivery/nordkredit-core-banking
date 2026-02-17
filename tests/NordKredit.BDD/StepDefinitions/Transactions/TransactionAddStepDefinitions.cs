using System.Globalization;
using NordKredit.Domain.Transactions;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Transactions;

/// <summary>
/// Step definitions for transaction add BDD scenarios (TRN-BR-003).
/// COBOL source: COTRN02C.cbl:106-784 (CICS transaction CT02 — add transaction).
/// Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention).
/// </summary>
[Binding]
[Scope(Feature = "Transaction add with validation")]
public sealed class TransactionAddStepDefinitions
{
    private readonly StubCardCrossReferenceRepository _xrefRepo = new();
    private readonly StubTransactionRepository _transactionRepo = new();
    private readonly StubTransactionIdGenerator _idGenerator = new();
    private TransactionValidationService _validationService = null!;
    private TransactionAddService _service = null!;
    private TransactionAddResult _result = null!;
    private Transaction? _lastTransaction;

    [BeforeScenario]
    public void SetUp()
    {
        _validationService = new TransactionValidationService(_xrefRepo);
        _service = new TransactionAddService(_validationService, _idGenerator, _transactionRepo);
    }

    [Given(@"the card cross-reference contains")]
    public void GivenTheCardCrossReferenceContains(Table table)
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

    [Given(@"the transaction ID generator will return ""(.*)""")]
    public void GivenTheTransactionIdGeneratorWillReturn(string transactionId) =>
        _idGenerator.NextId = transactionId;

    [Given(@"the transaction repository already contains transaction ""(.*)""")]
    public void GivenTheTransactionRepositoryAlreadyContainsTransaction(string _) =>
        _transactionRepo.ThrowDuplicateOnAdd = true;

    [Given(@"the transaction repository contains a last transaction with ID ""(.*)""")]
    public void GivenTheTransactionRepositoryContainsALastTransactionWithId(string transactionId)
    {
        _transactionRepo.AddTransaction(new Transaction
        {
            Id = transactionId,
            TypeCode = "SA",
            CategoryCode = 5010,
            Source = "ONLINE",
            Description = "LAST TRANSACTION",
            Amount = 99.99m,
            CardNumber = "4000123456789012",
            OriginationTimestamp = DateTime.UtcNow,
            ProcessingTimestamp = DateTime.UtcNow,
            MerchantName = "TEST",
            MerchantCity = "STOCKHOLM",
            MerchantZip = "11120"
        });
    }

    [When(@"I submit a transaction add request with")]
    public async Task WhenISubmitATransactionAddRequestWith(Table table)
    {
        var row = table.Rows[0];
        var request = new TransactionAddRequest
        {
            CardNumber = row["CardNumber"],
            TypeCode = row["TypeCode"],
            CategoryCode = row["CategoryCode"],
            Source = row["Source"],
            Description = row["Description"],
            Amount = row["Amount"],
            OriginationDate = row["OriginationDate"],
            ProcessingDate = row["ProcessingDate"],
            MerchantId = row["MerchantId"],
            MerchantName = row["MerchantName"],
            MerchantCity = row["MerchantCity"],
            MerchantZip = row["MerchantZip"],
            Confirm = row["Confirm"]
        };

        _result = await _service.AddTransactionAsync(request);
    }

    [When(@"I request the last transaction")]
    public async Task WhenIRequestTheLastTransaction() =>
        _lastTransaction = await _service.GetLastTransactionAsync();

    [Then(@"the transaction add result is success")]
    public void ThenTheTransactionAddResultIsSuccess() =>
        Assert.True(_result.IsSuccess);

    [Then(@"the transaction add result is validation error")]
    public void ThenTheTransactionAddResultIsValidationError()
    {
        Assert.False(_result.IsSuccess);
        Assert.False(_result.ConfirmationRequired);
    }

    [Then(@"the transaction add result is confirmation required")]
    public void ThenTheTransactionAddResultIsConfirmationRequired() =>
        Assert.True(_result.ConfirmationRequired);

    [Then(@"the transaction add result is duplicate key")]
    public void ThenTheTransactionAddResultIsDuplicateKey()
    {
        Assert.False(_result.IsSuccess);
        Assert.Equal("Tran ID already exist...", _result.Message);
    }

    [Then(@"the assigned transaction ID is ""(.*)""")]
    public void ThenTheAssignedTransactionIdIs(string expectedId) =>
        Assert.Equal(expectedId, _result.TransactionId);

    [Then(@"the last transaction ID is ""(.*)""")]
    public void ThenTheLastTransactionIdIs(string expectedId)
    {
        Assert.NotNull(_lastTransaction);
        Assert.Equal(expectedId, _lastTransaction.Id);
    }

    /// <summary>
    /// In-memory stub for card cross-reference repository.
    /// </summary>
    internal sealed class StubCardCrossReferenceRepository : ICardCrossReferenceRepository
    {
        private readonly List<CardCrossReference> _xrefs = [];

        public void Add(CardCrossReference xref) => _xrefs.Add(xref);

        public Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default) =>
            Task.FromResult(_xrefs.FirstOrDefault(x => x.CardNumber == cardNumber));

        public Task<CardCrossReference?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_xrefs.FirstOrDefault(x => x.AccountId == accountId));
    }

    /// <summary>
    /// In-memory stub for transaction repository.
    /// </summary>
    internal sealed class StubTransactionRepository : ITransactionRepository
    {
        private readonly List<Transaction> _transactions = [];

        public bool ThrowDuplicateOnAdd { get; set; }

        public void AddTransaction(Transaction transaction) => _transactions.Add(transaction);

        public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_transactions.FirstOrDefault(t => t.Id == transactionId));

        public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(string accountId, int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);

        public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
        {
            if (ThrowDuplicateOnAdd)
            {
                throw new DuplicateTransactionException(transaction.Id);
            }

            _transactions.Add(transaction);
            return Task.CompletedTask;
        }

        public Task<IReadOnlyList<Transaction>> GetPageAsync(int pageSize, string? startAfterTransactionId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);

        public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult(_transactions.OrderByDescending(t => t.Id).FirstOrDefault());

        public Task<IReadOnlyList<Transaction>> GetByDateRangeAsync(DateTime startDate, DateTime endDate, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<Transaction>>([]);
    }

    /// <summary>
    /// Stub transaction ID generator returning a predefined ID.
    /// </summary>
    internal sealed class StubTransactionIdGenerator : ITransactionIdGenerator
    {
        public string NextId { get; set; } = "TRN00000001";

        public Task<string> GenerateNextIdAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult(NextId);
    }
}
