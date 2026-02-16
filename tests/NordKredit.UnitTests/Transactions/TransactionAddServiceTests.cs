using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionAddService.
/// COBOL source: COTRN02C.cbl:442-749 (ADD-TRANSACTION, WRITE-TRANSACT-FILE).
/// Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention).
/// Covers: ID generation, field mapping, amount conversion, persistence, error handling,
/// copy-last-transaction convenience, and confirmation flow.
/// </summary>
public class TransactionAddServiceTests
{
    private readonly StubCardCrossReferenceRepository _crossRefRepo = new();
    private readonly StubTransactionRepository _transactionRepo = new();
    private readonly StubTransactionIdGenerator _idGenerator = new();
    private readonly TransactionAddService _sut;

    public TransactionAddServiceTests()
    {
        var validationService = new TransactionValidationService(_crossRefRepo);
        _sut = new TransactionAddService(validationService, _idGenerator, _transactionRepo);
    }

    private static TransactionAddRequest CreateValidRequest() => new()
    {
        AccountId = "00000000001",
        CardNumber = "",
        TypeCode = "01",
        CategoryCode = "1001",
        Source = "ONLINE",
        Description = "Test transaction",
        Amount = "+00000100.50",
        OriginationDate = "2026-01-15",
        ProcessingDate = "2026-01-16",
        MerchantId = "000000001",
        MerchantName = "Test Merchant",
        MerchantCity = "Stockholm",
        MerchantZip = "11122",
        Confirm = "Y"
    };

    private void SetupValidCrossReference()
    {
        _crossRefRepo.AddByAccountId("00000000001", new CardCrossReference
        {
            AccountId = "00000000001",
            CardNumber = "4000123456789010",
            CustomerId = 100000001
        });
    }

    // ===================================================================
    // Successful Transaction Creation (COTRN02C.cbl:723-730)
    // ===================================================================

    [Fact]
    public async Task AddTransaction_ValidRequest_Returns201WithTransactionId()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();

        var result = await _sut.AddTransactionAsync(request);

        Assert.True(result.IsSuccess);
        Assert.Equal("0000000000000001", result.TransactionId);
        Assert.Contains("Transaction added successfully", result.Message);
        Assert.Contains("0000000000000001", result.Message);
    }

    [Fact]
    public async Task AddTransaction_ValidRequest_PersistsTransaction()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000042";
        var request = CreateValidRequest();

        await _sut.AddTransactionAsync(request);

        var persisted = _transactionRepo.GetById("0000000000000042");
        Assert.NotNull(persisted);
        Assert.Equal("0000000000000042", persisted.Id);
    }

    // ===================================================================
    // Transaction ID Generation (COTRN02C.cbl:442-449)
    // ===================================================================

    [Fact]
    public async Task AddTransaction_EmptyTable_StartsFrom0000000000000001()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();

        var result = await _sut.AddTransactionAsync(request);

        Assert.True(result.IsSuccess);
        Assert.Equal("0000000000000001", result.TransactionId);
    }

    [Fact]
    public async Task AddTransaction_TransactionIdIs16CharZeroPadded()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000100";
        var request = CreateValidRequest();

        var result = await _sut.AddTransactionAsync(request);

        Assert.True(result.IsSuccess);
        Assert.NotNull(result.TransactionId);
        Assert.Equal(16, result.TransactionId.Length);
        Assert.True(result.TransactionId.All(char.IsAsciiDigit));
    }

    // ===================================================================
    // Amount Conversion (COTRN02C.cbl:456 — FUNCTION NUMVAL-C)
    // ===================================================================

    [Fact]
    public async Task AddTransaction_PositiveAmount_StoredAsDecimal()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();
        request.Amount = "+00000100.50";

        await _sut.AddTransactionAsync(request);

        var persisted = _transactionRepo.GetById("0000000000000001");
        Assert.NotNull(persisted);
        Assert.Equal(100.50m, persisted.Amount);
    }

    [Fact]
    public async Task AddTransaction_NegativeAmount_StoredAsNegativeDecimal()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();
        request.Amount = "-00000050.25";

        await _sut.AddTransactionAsync(request);

        var persisted = _transactionRepo.GetById("0000000000000001");
        Assert.NotNull(persisted);
        Assert.Equal(-50.25m, persisted.Amount);
    }

    [Fact]
    public async Task AddTransaction_ZeroAmount_StoredAsZero()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();
        request.Amount = "+00000000.00";

        await _sut.AddTransactionAsync(request);

        var persisted = _transactionRepo.GetById("0000000000000001");
        Assert.NotNull(persisted);
        Assert.Equal(0.00m, persisted.Amount);
    }

    [Fact]
    public async Task AddTransaction_MaxAmount_StoredWithExactPrecision()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();
        request.Amount = "+99999999.99";

        await _sut.AddTransactionAsync(request);

        var persisted = _transactionRepo.GetById("0000000000000001");
        Assert.NotNull(persisted);
        Assert.Equal(99999999.99m, persisted.Amount);
    }

    // ===================================================================
    // Field Mapping (COTRN02C.cbl:450-458)
    // ===================================================================

    [Fact]
    public async Task AddTransaction_MapsAllFieldsCorrectly()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();

        await _sut.AddTransactionAsync(request);

        var persisted = _transactionRepo.GetById("0000000000000001");
        Assert.NotNull(persisted);
        Assert.Equal("01", persisted.TypeCode);
        Assert.Equal(1001, persisted.CategoryCode);
        Assert.Equal("ONLINE", persisted.Source);
        Assert.Equal("Test transaction", persisted.Description);
        Assert.Equal(100.50m, persisted.Amount);
        Assert.Equal(1, persisted.MerchantId);
        Assert.Equal("Test Merchant", persisted.MerchantName);
        Assert.Equal("Stockholm", persisted.MerchantCity);
        Assert.Equal("11122", persisted.MerchantZip);
        Assert.Equal("4000123456789010", persisted.CardNumber);
        Assert.Equal(new DateTime(2026, 1, 15), persisted.OriginationTimestamp);
        Assert.Equal(new DateTime(2026, 1, 16), persisted.ProcessingTimestamp);
    }

    [Fact]
    public async Task AddTransaction_UsesResolvedCardNumber_NotInputCardNumber()
    {
        // Account ID provided — resolved card number from cross-ref should be used
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        var request = CreateValidRequest();
        request.AccountId = "00000000001";
        request.CardNumber = "";

        await _sut.AddTransactionAsync(request);

        var persisted = _transactionRepo.GetById("0000000000000001");
        Assert.NotNull(persisted);
        Assert.Equal("4000123456789010", persisted.CardNumber);
    }

    // ===================================================================
    // Validation Failure (delegates to TransactionValidationService)
    // ===================================================================

    [Fact]
    public async Task AddTransaction_ValidationFails_ReturnsValidationError()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.TypeCode = ""; // Mandatory field empty

        var result = await _sut.AddTransactionAsync(request);

        Assert.False(result.IsSuccess);
        Assert.Contains("Type CD can NOT be empty", result.Message);
    }

    [Fact]
    public async Task AddTransaction_ValidationFails_DoesNotPersist()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.TypeCode = ""; // Mandatory field empty

        await _sut.AddTransactionAsync(request);

        Assert.Empty(_transactionRepo.GetAll());
    }

    [Fact]
    public async Task AddTransaction_ValidationFails_DoesNotGenerateId()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.TypeCode = "";

        await _sut.AddTransactionAsync(request);

        Assert.False(_idGenerator.WasGenerated);
    }

    // ===================================================================
    // Confirmation Flow (COTRN02C.cbl:429-437)
    // ===================================================================

    [Fact]
    public async Task AddTransaction_ConfirmNotY_ReturnsNeedsConfirmation()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "N";

        var result = await _sut.AddTransactionAsync(request);

        Assert.False(result.IsSuccess);
        Assert.True(result.ConfirmationRequired);
    }

    [Fact]
    public async Task AddTransaction_ConfirmEmpty_ReturnsNeedsConfirmation()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "";

        var result = await _sut.AddTransactionAsync(request);

        Assert.False(result.IsSuccess);
        Assert.True(result.ConfirmationRequired);
    }

    [Fact]
    public async Task AddTransaction_NeedsConfirmation_DoesNotPersist()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "N";

        await _sut.AddTransactionAsync(request);

        Assert.Empty(_transactionRepo.GetAll());
    }

    // ===================================================================
    // Duplicate Key Handling (COTRN02C.cbl:735-738)
    // ===================================================================

    [Fact]
    public async Task AddTransaction_DuplicateKey_Returns409()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        _transactionRepo.ThrowDuplicateOnAdd = true;

        var request = CreateValidRequest();
        var result = await _sut.AddTransactionAsync(request);

        Assert.False(result.IsSuccess);
        Assert.Contains("Tran ID already exist", result.Message);
    }

    // ===================================================================
    // Copy Last Transaction (COTRN02C.cbl — PF5 handler)
    // ===================================================================

    [Fact]
    public async Task GetLastTransaction_ReturnsLastTransaction()
    {
        _transactionRepo.Add(new Transaction
        {
            Id = "0000000000000001",
            TypeCode = "01",
            CategoryCode = 1001,
            Source = "ONLINE",
            Description = "First",
            Amount = 50.00m,
            MerchantId = 1,
            MerchantName = "Merchant",
            MerchantCity = "City",
            MerchantZip = "12345",
            CardNumber = "4000123456789010",
            OriginationTimestamp = DateTime.Now,
            ProcessingTimestamp = DateTime.Now
        });
        _transactionRepo.Add(new Transaction
        {
            Id = "0000000000000002",
            TypeCode = "02",
            CategoryCode = 2002,
            Source = "BATCH",
            Description = "Second",
            Amount = 75.00m,
            MerchantId = 2,
            MerchantName = "Merchant 2",
            MerchantCity = "City 2",
            MerchantZip = "54321",
            CardNumber = "4000123456789020",
            OriginationTimestamp = DateTime.Now,
            ProcessingTimestamp = DateTime.Now
        });

        var result = await _sut.GetLastTransactionAsync();

        Assert.NotNull(result);
        Assert.Equal("0000000000000002", result.Id);
    }

    [Fact]
    public async Task GetLastTransaction_EmptyTable_ReturnsNull()
    {
        var result = await _sut.GetLastTransactionAsync();

        Assert.Null(result);
    }
}

/// <summary>
/// In-memory test double for ITransactionRepository.
/// </summary>
internal sealed class StubTransactionRepository : ITransactionRepository
{
    private readonly List<Transaction> _transactions = [];

    public bool ThrowDuplicateOnAdd { get; set; }

    public void Add(Transaction transaction)
        => _transactions.Add(transaction);

    public Transaction? GetById(string id)
        => _transactions.FirstOrDefault(t => t.Id == id);

    public IReadOnlyList<Transaction> GetAll()
        => _transactions.AsReadOnly();

    public Task<Transaction?> GetByIdAsync(string transactionId, CancellationToken cancellationToken = default)
        => Task.FromResult(_transactions.FirstOrDefault(t => t.Id == transactionId));

    public Task<IReadOnlyList<Transaction>> GetByAccountIdAsync(
        string accountId, int pageSize, string? startAfterTransactionId = null,
        CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>(_transactions.AsReadOnly());

    public Task<IReadOnlyList<Transaction>> GetPageAsync(
        int pageSize, string? cursor = null, CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<Transaction>>(_transactions.OrderBy(t => t.Id).Take(pageSize).ToList());

    public Task AddAsync(Transaction transaction, CancellationToken cancellationToken = default)
    {
        if (ThrowDuplicateOnAdd)
        {
            throw new DuplicateTransactionException(transaction.Id);
        }

        _transactions.Add(transaction);
        return Task.CompletedTask;
    }

    public Task<Transaction?> GetLastTransactionAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(_transactions.OrderByDescending(t => t.Id).FirstOrDefault());
}

/// <summary>
/// In-memory test double for ITransactionIdGenerator.
/// </summary>
internal sealed class StubTransactionIdGenerator : ITransactionIdGenerator
{
    public string NextId { get; set; } = "0000000000000001";
    public bool WasGenerated { get; private set; }

    public Task<string> GenerateNextIdAsync(CancellationToken cancellationToken = default)
    {
        WasGenerated = true;
        return Task.FromResult(NextId);
    }
}
