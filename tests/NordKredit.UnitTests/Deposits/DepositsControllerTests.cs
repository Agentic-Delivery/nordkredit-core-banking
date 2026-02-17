using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Deposits;

/// <summary>
/// Tests for the DepositsController.
/// Business rules: DEP-BR-001 through DEP-BR-009.
/// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive 2014/49/EU,
///              PSD2 Art. 64, GDPR Art. 5/15.
/// </summary>
public class DepositsControllerTests
{
    private readonly InMemoryDepositAccountRepository _accountRepo = new();
    private readonly InMemoryTestSavingsProductRepository _productRepo = new();

    private DepositsController CreateController() =>
        new(_accountRepo, _productRepo)
        {
            ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext()
            }
        };

    private DepositsController CreateControllerWithIfMatch(string etagValue)
    {
        var controller = CreateController();
        controller.Request.Headers.IfMatch = etagValue;
        return controller;
    }

    // =================================================================
    // GET /api/deposits — List deposit accounts
    // =================================================================

    [Fact]
    public async Task ListDeposits_ReturnsOk()
    {
        // GIVEN deposit accounts exist
        await _accountRepo.AddAsync(CreateTestAccount("12345678901"));
        await _accountRepo.AddAsync(CreateTestAccount("12345678902"));

        // WHEN listing deposits
        var controller = CreateController();
        var result = await controller.ListDeposits(null, CancellationToken.None);

        // THEN returns OK with accounts
        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task ListDeposits_WithInvalidCursor_ReturnsBadRequest()
    {
        // GIVEN an invalid cursor value
        var controller = CreateController();

        // WHEN listing with invalid cursor
        var result = await controller.ListDeposits("INVALID", CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // GET /api/deposits/{accountId} — Deposit account detail
    // =================================================================

    [Fact]
    public async Task GetDeposit_ValidId_ReturnsOk()
    {
        // GIVEN a deposit account exists
        var account = CreateTestAccount("12345678901");
        await _accountRepo.AddAsync(account);

        // WHEN requesting by account ID
        var controller = CreateController();
        var result = await controller.GetDeposit("12345678901", CancellationToken.None);

        // THEN returns OK with account details
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<DepositAccountResponse>(okResult.Value);
        Assert.Equal("12345678901", response.AccountId);
        Assert.Equal("Active", response.Status);
    }

    [Fact]
    public async Task GetDeposit_InvalidId_ReturnsBadRequest()
    {
        // GIVEN an invalid account ID
        var controller = CreateController();

        // WHEN requesting with invalid ID
        var result = await controller.GetDeposit("INVALID", CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task GetDeposit_NotFound_ReturnsNotFound()
    {
        // GIVEN no account exists for the ID
        var controller = CreateController();

        // WHEN requesting a non-existent account
        var result = await controller.GetDeposit("99999999999", CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    // =================================================================
    // POST /api/deposits — Create deposit account
    // =================================================================

    [Fact]
    public async Task CreateDeposit_ValidRequest_ReturnsCreated()
    {
        // GIVEN a valid deposit creation request
        var controller = CreateController();
        var request = new CreateDepositRequest
        {
            AccountId = "12345678901",
            ProductType = DepositProductType.DemandSavings,
            HolderName = "ANNA SVENSSON",
            DisclosureGroupId = "SAV_STD_01"
        };

        // WHEN creating the deposit
        var result = await controller.CreateDeposit(request, CancellationToken.None);

        // THEN returns Created
        var createdResult = Assert.IsType<CreatedAtActionResult>(result);
        var response = Assert.IsType<DepositAccountResponse>(createdResult.Value);
        Assert.Equal("12345678901", response.AccountId);
        Assert.Equal("Active", response.Status);
    }

    [Fact]
    public async Task CreateDeposit_InvalidAccountId_ReturnsBadRequest()
    {
        // GIVEN an invalid account ID
        var controller = CreateController();
        var request = new CreateDepositRequest
        {
            AccountId = "INVALID",
            HolderName = "TEST"
        };

        // WHEN creating the deposit
        var result = await controller.CreateDeposit(request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task CreateDeposit_InvalidDisclosureGroupId_ReturnsBadRequest()
    {
        // GIVEN an empty disclosure group ID
        var controller = CreateController();
        var request = new CreateDepositRequest
        {
            AccountId = "12345678901",
            HolderName = "TEST",
            DisclosureGroupId = ""
        };

        // WHEN creating the deposit
        var result = await controller.CreateDeposit(request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task CreateDeposit_Duplicate_ReturnsConflict()
    {
        // GIVEN a deposit account already exists
        await _accountRepo.AddAsync(CreateTestAccount("12345678901"));

        var controller = CreateController();
        var request = new CreateDepositRequest
        {
            AccountId = "12345678901",
            HolderName = "TEST",
            DisclosureGroupId = "SAV_STD_01"
        };

        // WHEN creating a duplicate
        var result = await controller.CreateDeposit(request, CancellationToken.None);

        // THEN returns Conflict
        Assert.IsType<ConflictObjectResult>(result);
    }

    // =================================================================
    // PUT /api/deposits/{accountId}/status — Close deposit / status transition
    // =================================================================

    [Fact]
    public async Task UpdateDepositStatus_ValidTransition_ReturnsOk()
    {
        // GIVEN an active deposit account
        var account = CreateTestAccount("12345678901");
        account.RowVersion = [0x01, 0x02, 0x03, 0x04];
        await _accountRepo.AddAsync(account);

        var controller = CreateControllerWithIfMatch(
            Convert.ToBase64String([0x01, 0x02, 0x03, 0x04]));

        // WHEN transitioning to Closed
        var request = new UpdateDepositStatusRequest { Status = DepositAccountStatus.Closed };
        var result = await controller.UpdateDepositStatus("12345678901", request, CancellationToken.None);

        // THEN returns OK with updated status
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<DepositAccountResponse>(okResult.Value);
        Assert.Equal("Closed", response.Status);
    }

    [Fact]
    public async Task UpdateDepositStatus_MissingETag_Returns428()
    {
        // GIVEN no ETag in request
        var controller = CreateController();
        var request = new UpdateDepositStatusRequest { Status = DepositAccountStatus.Closed };

        // WHEN updating status
        var result = await controller.UpdateDepositStatus("12345678901", request, CancellationToken.None);

        // THEN returns 428 Precondition Required
        var statusResult = Assert.IsType<ObjectResult>(result);
        Assert.Equal(428, statusResult.StatusCode);
    }

    [Fact]
    public async Task UpdateDepositStatus_InvalidETag_ReturnsBadRequest()
    {
        // GIVEN an invalid ETag format
        var controller = CreateControllerWithIfMatch("not-base64!!!");
        var request = new UpdateDepositStatusRequest { Status = DepositAccountStatus.Closed };

        // WHEN updating status
        var result = await controller.UpdateDepositStatus("12345678901", request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task UpdateDepositStatus_NotFound_ReturnsNotFound()
    {
        // GIVEN no account exists
        var controller = CreateControllerWithIfMatch(Convert.ToBase64String([0x01]));
        var request = new UpdateDepositStatusRequest { Status = DepositAccountStatus.Closed };

        // WHEN updating status
        var result = await controller.UpdateDepositStatus("99999999999", request, CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    [Fact]
    public async Task UpdateDepositStatus_ConcurrencyConflict_ReturnsConflict()
    {
        // GIVEN an account with different RowVersion
        var account = CreateTestAccount("12345678901");
        account.RowVersion = [0x01, 0x02, 0x03, 0x04];
        await _accountRepo.AddAsync(account);

        var controller = CreateControllerWithIfMatch(
            Convert.ToBase64String([0xFF, 0xFF, 0xFF, 0xFF]));

        // WHEN updating with stale ETag
        var request = new UpdateDepositStatusRequest { Status = DepositAccountStatus.Closed };
        var result = await controller.UpdateDepositStatus("12345678901", request, CancellationToken.None);

        // THEN returns Conflict
        Assert.IsType<ConflictObjectResult>(result);
    }

    [Fact]
    public async Task UpdateDepositStatus_InvalidTransition_ReturnsBadRequest()
    {
        // GIVEN a closed account
        var account = CreateTestAccount("12345678901");
        account.Status = DepositAccountStatus.Closed;
        account.RowVersion = [0x01];
        await _accountRepo.AddAsync(account);

        var controller = CreateControllerWithIfMatch(Convert.ToBase64String([0x01]));

        // WHEN trying invalid transition
        var request = new UpdateDepositStatusRequest { Status = DepositAccountStatus.Active };
        var result = await controller.UpdateDepositStatus("12345678901", request, CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // GET /api/deposits/{accountId}/interest — Interest inquiry
    // =================================================================

    [Fact]
    public async Task GetInterestInquiry_ValidAccount_ReturnsOk()
    {
        // GIVEN an active deposit with balance and a matching savings product
        var account = CreateTestAccount("12345678901");
        account.CurrentBalance = 100000.00m;
        account.AccruedInterest = 12.5000m;
        await _accountRepo.AddAsync(account);

        _productRepo.AddProduct(new SavingsProduct
        {
            ProductId = "SAV_STD_01",
            AnnualRate = 0.025m,
            DayCountBasis = 365
        });

        // WHEN requesting interest inquiry
        var controller = CreateController();
        var result = await controller.GetInterestInquiry("12345678901", CancellationToken.None);

        // THEN returns OK with interest details
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<InterestInquiryResponse>(okResult.Value);
        Assert.Equal("12345678901", response.AccountId);
        Assert.Equal(100000.00m, response.CurrentBalance);
        Assert.Equal(12.5000m, response.AccruedInterest);
        Assert.True(response.DailyInterestAmount > 0);
    }

    [Fact]
    public async Task GetInterestInquiry_InvalidAccountId_ReturnsBadRequest()
    {
        // GIVEN an invalid account ID
        var controller = CreateController();

        // WHEN requesting interest inquiry
        var result = await controller.GetInterestInquiry("INVALID", CancellationToken.None);

        // THEN returns BadRequest
        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task GetInterestInquiry_AccountNotFound_ReturnsNotFound()
    {
        // GIVEN no account exists
        var controller = CreateController();

        // WHEN requesting interest inquiry
        var result = await controller.GetInterestInquiry("99999999999", CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    [Fact]
    public async Task GetInterestInquiry_ProductNotFound_ReturnsNotFound()
    {
        // GIVEN an account exists but product is missing
        var account = CreateTestAccount("12345678901");
        account.DisclosureGroupId = "NONEXISTENT";
        await _accountRepo.AddAsync(account);

        // WHEN requesting interest inquiry
        var controller = CreateController();
        var result = await controller.GetInterestInquiry("12345678901", CancellationToken.None);

        // THEN returns NotFound
        Assert.IsType<NotFoundObjectResult>(result);
    }

    // =================================================================
    // Helpers
    // =================================================================

    private static DepositAccount CreateTestAccount(string accountId) => new()
    {
        Id = accountId,
        Status = DepositAccountStatus.Active,
        ProductType = DepositProductType.DemandSavings,
        CurrentBalance = 50000.00m,
        HolderName = "TEST HOLDER",
        DisclosureGroupId = "SAV_STD_01",
        OpenedDate = new DateTime(2024, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    };
}

/// <summary>
/// In-memory deposit account repository for controller unit tests.
/// </summary>
internal sealed class InMemoryDepositAccountRepository : IDepositAccountRepository
{
    private readonly List<DepositAccount> _accounts = [];

    public Task<DepositAccount?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default) =>
        Task.FromResult(_accounts.FirstOrDefault(a => a.Id == accountId));

    public Task<IReadOnlyList<DepositAccount>> GetPageAsync(int pageSize, string? afterAccountId = null, CancellationToken cancellationToken = default)
    {
        var query = _accounts.AsEnumerable();

        if (!string.IsNullOrEmpty(afterAccountId))
        {
            query = query.Where(a => string.Compare(a.Id, afterAccountId, StringComparison.Ordinal) > 0);
        }

        IReadOnlyList<DepositAccount> result = [.. query.OrderBy(a => a.Id).Take(pageSize)];
        return Task.FromResult(result);
    }

    public Task<IReadOnlyList<DepositAccount>> GetActiveAccountsAsync(CancellationToken cancellationToken = default)
    {
        IReadOnlyList<DepositAccount> result = [.. _accounts.Where(a => a.Status == DepositAccountStatus.Active)];
        return Task.FromResult(result);
    }

    public Task AddAsync(DepositAccount account, CancellationToken cancellationToken = default)
    {
        _accounts.Add(account);
        return Task.CompletedTask;
    }

    public Task UpdateAsync(DepositAccount account, CancellationToken cancellationToken = default) =>
        Task.CompletedTask;
}

/// <summary>
/// In-memory savings product repository for controller unit tests.
/// </summary>
internal sealed class InMemoryTestSavingsProductRepository : ISavingsProductRepository
{
    private readonly List<SavingsProduct> _products = [];

    public void AddProduct(SavingsProduct product) => _products.Add(product);

    public Task<SavingsProduct?> GetByProductIdAsync(string productId, CancellationToken cancellationToken = default) =>
        Task.FromResult(_products.FirstOrDefault(p => p.ProductId == productId));

    public Task<IReadOnlyList<SavingsProduct>> GetAllAsync(CancellationToken cancellationToken = default)
    {
        IReadOnlyList<SavingsProduct> result = [.. _products];
        return Task.FromResult(result);
    }
}
