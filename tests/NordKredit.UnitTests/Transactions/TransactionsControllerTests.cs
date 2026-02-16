using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.Transactions;

namespace NordKredit.UnitTests.Transactions;

/// <summary>
/// Unit tests for TransactionsController HTTP response mapping.
/// COBOL source: COTRN02C.cbl, COTRN00C.cbl — verifies REST API contract for transaction operations.
/// Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention).
/// </summary>
public class TransactionsControllerTests
{
    private readonly StubCardCrossReferenceRepository _crossRefRepo = new();
    private readonly StubTransactionRepository _transactionRepo = new();
    private readonly StubTransactionIdGenerator _idGenerator = new();
    private readonly StubTransactionListRepository _listRepo = new();
    private readonly TransactionsController _controller;

    public TransactionsControllerTests()
    {
        var validationService = new TransactionValidationService(_crossRefRepo);
        var addService = new TransactionAddService(validationService, _idGenerator, _transactionRepo);
        var detailService = new TransactionDetailService(_transactionRepo);
        var listService = new TransactionListService(_listRepo);
        _controller = new TransactionsController(addService, detailService, listService);
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
    // POST /api/transactions — 201 Created (COTRN02C.cbl:723-730)
    // ===================================================================

    [Fact]
    public async Task Post_ValidRequest_Returns201Created()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";

        var result = await _controller.AddTransaction(CreateValidRequest(), CancellationToken.None);

        var created = Assert.IsType<CreatedResult>(result);
        Assert.Equal(201, created.StatusCode);
    }

    [Fact]
    public async Task Post_ValidRequest_LocationHeaderContainsTransactionId()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";

        var result = await _controller.AddTransaction(CreateValidRequest(), CancellationToken.None);

        var created = Assert.IsType<CreatedResult>(result);
        Assert.Equal("/api/transactions/0000000000000001", created.Location);
    }

    [Fact]
    public async Task Post_ValidRequest_BodyContainsTransactionIdAndMessage()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";

        var result = await _controller.AddTransaction(CreateValidRequest(), CancellationToken.None);

        var created = Assert.IsType<CreatedResult>(result);
        var body = created.Value;
        Assert.NotNull(body);
        var transactionId = body.GetType().GetProperty("TransactionId")?.GetValue(body)?.ToString();
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("0000000000000001", transactionId);
        Assert.Contains("Transaction added successfully", message);
    }

    // ===================================================================
    // POST /api/transactions — 400 Bad Request (validation failure)
    // ===================================================================

    [Fact]
    public async Task Post_ValidationError_Returns400BadRequest()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.TypeCode = ""; // Required field empty

        var result = await _controller.AddTransaction(request, CancellationToken.None);

        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    [Fact]
    public async Task Post_NeedsConfirmation_Returns400WithConfirmationFlag()
    {
        SetupValidCrossReference();
        var request = CreateValidRequest();
        request.Confirm = "N";

        var result = await _controller.AddTransaction(request, CancellationToken.None);

        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var confirmationRequired = body.GetType().GetProperty("ConfirmationRequired")?.GetValue(body);
        Assert.Equal(true, confirmationRequired);
    }

    // ===================================================================
    // POST /api/transactions — 409 Conflict (COTRN02C.cbl:735-738)
    // ===================================================================

    [Fact]
    public async Task Post_DuplicateKey_Returns409Conflict()
    {
        SetupValidCrossReference();
        _idGenerator.NextId = "0000000000000001";
        _transactionRepo.ThrowDuplicateOnAdd = true;

        var result = await _controller.AddTransaction(CreateValidRequest(), CancellationToken.None);

        var conflict = Assert.IsType<ConflictObjectResult>(result);
        Assert.Equal(409, conflict.StatusCode);
    }

    // ===================================================================
    // GET /api/transactions/last — Copy Last Transaction (PF5)
    // ===================================================================

    [Fact]
    public async Task GetLast_TransactionExists_Returns200Ok()
    {
        _transactionRepo.Add(new Transaction
        {
            Id = "0000000000000001",
            TypeCode = "01",
            CategoryCode = 1001,
            Source = "ONLINE",
            Description = "Test",
            Amount = 50.00m,
            MerchantId = 1,
            MerchantName = "Merchant",
            MerchantCity = "City",
            MerchantZip = "12345",
            CardNumber = "4000123456789010",
            OriginationTimestamp = DateTime.Now,
            ProcessingTimestamp = DateTime.Now
        });

        var result = await _controller.GetLastTransaction(CancellationToken.None);

        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task GetLast_NoTransactions_Returns404NotFound()
    {
        var result = await _controller.GetLastTransaction(CancellationToken.None);

        Assert.IsType<NotFoundObjectResult>(result);
    }

    // ===================================================================
    // GET /api/transactions/{id} — Transaction Detail (COTRN01C.cbl:85-296)
    // Regulations: FFFS 2014:5 Ch.8, PSD2 Art.94, GDPR Art.15
    // ===================================================================

    [Fact]
    public async Task GetById_ExistingTransaction_Returns200Ok()
    {
        // GIVEN transaction "0000000000000042" exists
        _transactionRepo.Add(new Transaction
        {
            Id = "0000000000000042",
            TypeCode = "SA",
            CategoryCode = 5010,
            Source = "ONLINE",
            Description = "Monthly rent payment",
            Amount = 12500.00m,
            MerchantId = 123456789,
            MerchantName = "Stockholm Housing AB",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            CardNumber = "4000123456780042",
            OriginationTimestamp = new DateTime(2026, 1, 15, 10, 30, 0),
            ProcessingTimestamp = new DateTime(2026, 1, 15, 14, 45, 0)
        });

        // WHEN GET /api/transactions/0000000000000042 is called
        var result = await _controller.GetTransaction("0000000000000042", CancellationToken.None);

        // THEN 200 OK
        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task GetById_ExistingTransaction_ReturnsAll14FieldsWithMaskedCard()
    {
        // GIVEN transaction "0000000000000042" exists
        _transactionRepo.Add(new Transaction
        {
            Id = "0000000000000042",
            TypeCode = "SA",
            CategoryCode = 5010,
            Source = "ONLINE",
            Description = "Monthly rent payment",
            Amount = 12500.00m,
            MerchantId = 123456789,
            MerchantName = "Stockholm Housing AB",
            MerchantCity = "Stockholm",
            MerchantZip = "11122",
            CardNumber = "4000123456780042",
            OriginationTimestamp = new DateTime(2026, 1, 15, 10, 30, 0),
            ProcessingTimestamp = new DateTime(2026, 1, 15, 14, 45, 0)
        });

        // WHEN GET /api/transactions/0000000000000042 is called
        var result = await _controller.GetTransaction("0000000000000042", CancellationToken.None);

        // THEN all 14 fields populated with card number masked
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<TransactionDetailResponse>(ok.Value);
        Assert.Equal("0000000000000042", body.TransactionId);
        Assert.Equal("************0042", body.CardNumber);
        Assert.Equal("SA", body.TypeCode);
        Assert.Equal(5010, body.CategoryCode);
        Assert.Equal("ONLINE", body.Source);
        Assert.Equal(12500.00m, body.Amount);
        Assert.Equal("Monthly rent payment", body.Description);
        Assert.Equal(new DateTime(2026, 1, 15, 10, 30, 0), body.OriginationTimestamp);
        Assert.Equal(new DateTime(2026, 1, 15, 14, 45, 0), body.ProcessingTimestamp);
        Assert.Equal(123456789, body.MerchantId);
        Assert.Equal("Stockholm Housing AB", body.MerchantName);
        Assert.Equal("Stockholm", body.MerchantCity);
        Assert.Equal("11122", body.MerchantZip);
    }

    [Fact]
    public async Task GetById_NonExistentTransaction_Returns404NotFound()
    {
        // GIVEN no transaction with ID "9999999999999999" exists
        // WHEN GET /api/transactions/9999999999999999 is called
        var result = await _controller.GetTransaction("9999999999999999", CancellationToken.None);

        // THEN 404 with "Transaction ID NOT found"
        var notFound = Assert.IsType<NotFoundObjectResult>(result);
        var body = notFound.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Transaction ID NOT found", message);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public async Task GetById_EmptyOrWhitespace_Returns400BadRequest(string transactionId)
    {
        // GIVEN empty/whitespace transaction ID
        // WHEN GET /api/transactions/{id} is called
        var result = await _controller.GetTransaction(transactionId, CancellationToken.None);

        // THEN 400 with "Transaction ID cannot be empty"
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Transaction ID cannot be empty", message);
    }

    // ===================================================================
    // GET /api/transactions — Transaction List (COTRN00C.cbl:94-328)
    // ===================================================================

    [Fact]
    public async Task GetTransactions_25Records_NoCursor_Returns200WithFirst10()
    {
        // GIVEN the TRANSACT table contains 25 transactions
        _listRepo.SeedTransactions(25);

        // WHEN GET /api/transactions is called with no cursor
        var result = await _controller.GetTransactions(
            null, null, null, CancellationToken.None);

        // THEN 200 OK with first 10 transactions
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<TransactionListResponse>(ok.Value);
        Assert.Equal(10, body.Transactions.Count);
        Assert.True(body.HasNextPage);
        Assert.NotNull(body.NextCursor);
    }

    [Fact]
    public async Task GetTransactions_WithCursor_ReturnsNextPage()
    {
        // GIVEN the user has a nextCursor from a previous response
        _listRepo.SeedTransactions(25);

        // WHEN GET /api/transactions?cursor={nextCursor} is called
        var result = await _controller.GetTransactions(
            "0000000000000010", null, null, CancellationToken.None);

        // THEN the next 10 transactions are returned
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<TransactionListResponse>(ok.Value);
        Assert.Equal(10, body.Transactions.Count);
        Assert.Equal("0000000000000011", body.Transactions[0].TransactionId);
    }

    [Fact]
    public async Task GetTransactions_EmptyTable_Returns200WithEmptyArray()
    {
        // GIVEN the TRANSACT table is empty
        // (no seeding)

        // WHEN GET /api/transactions is called
        var result = await _controller.GetTransactions(
            null, null, null, CancellationToken.None);

        // THEN 200 OK with empty array and hasNextPage: false
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<TransactionListResponse>(ok.Value);
        Assert.Empty(body.Transactions);
        Assert.False(body.HasNextPage);
    }

    [Fact]
    public async Task GetTransactions_NumericFromTransactionId_Returns200()
    {
        // GIVEN a numeric transactionId filter
        _listRepo.SeedTransactions(25);

        // WHEN GET /api/transactions?fromTransactionId={id} is called
        var result = await _controller.GetTransactions(
            null, "0000000000000015", null, CancellationToken.None);

        // THEN transactions starting from that ID are returned
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<TransactionListResponse>(ok.Value);
        Assert.Equal("0000000000000016", body.Transactions[0].TransactionId);
    }

    [Fact]
    public async Task GetTransactions_NonNumericFromTransactionId_Returns400()
    {
        // GIVEN a non-numeric transactionId filter
        // COBOL: COTRN00C.cbl:209-213 — IS NUMERIC check
        _listRepo.SeedTransactions(25);

        // WHEN GET /api/transactions?fromTransactionId=abc is called
        var result = await _controller.GetTransactions(
            null, "abc", null, CancellationToken.None);

        // THEN 400 Bad Request with "Transaction ID must be numeric"
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        var body = badRequest.Value;
        Assert.NotNull(body);
        var message = body.GetType().GetProperty("Message")?.GetValue(body)?.ToString();
        Assert.Equal("Transaction ID must be numeric", message);
    }

    [Fact]
    public async Task GetTransactions_BackwardDirectionNoCursor_Returns200WithMessage()
    {
        // GIVEN the user is on page 1
        // COBOL: COTRN00C.cbl — PF7 at page 1
        _listRepo.SeedTransactions(25);

        // WHEN GET /api/transactions?direction=backward is called
        var result = await _controller.GetTransactions(
            null, null, "backward", CancellationToken.None);

        // THEN 200 with message "Already at the top of the page" and page 1 data
        var ok = Assert.IsType<OkObjectResult>(result);
        var body = Assert.IsType<TransactionListResponse>(ok.Value);
        Assert.Equal(10, body.Transactions.Count);
        Assert.Equal("Already at the top of the page", body.Message);
    }
}
