using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using NordKredit.Api.Controllers;
using NordKredit.Domain.AccountManagement;

namespace NordKredit.UnitTests.AccountManagement;

/// <summary>
/// Tests for the AccountsController.
/// Business rules: ACCT-BR-001 through ACCT-BR-008.
/// Regulations: PSD2 Art. 97, GDPR Art. 15/17, FSA FFFS 2014:5 Ch. 3-4.
/// </summary>
public class AccountsControllerTests
{
    private readonly InMemoryAccountManagementRepository _repository = new();

    private AccountsController CreateController() =>
        new(_repository)
        {
            ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext()
            }
        };

    private AccountsController CreateControllerWithIfMatch(string etagValue)
    {
        var controller = CreateController();
        controller.Request.Headers.IfMatch = etagValue;
        return controller;
    }

    // =================================================================
    // GET /api/accounts/{accountId} — Account detail
    // =================================================================

    [Fact]
    public async Task GetAccount_ValidId_ReturnsOk()
    {
        var account = CreateTestAccount("12345678901");
        await _repository.AddAsync(account);

        var controller = CreateController();
        var result = await controller.GetAccount("12345678901", CancellationToken.None);

        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<AccountResponse>(okResult.Value);
        Assert.Equal("12345678901", response.AccountId);
        Assert.Equal("Active", response.Status);
    }

    [Fact]
    public async Task GetAccount_InvalidId_ReturnsBadRequest()
    {
        var controller = CreateController();
        var result = await controller.GetAccount("INVALID", CancellationToken.None);

        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task GetAccount_NotFound_ReturnsNotFound()
    {
        var controller = CreateController();
        var result = await controller.GetAccount("99999999999", CancellationToken.None);

        Assert.IsType<NotFoundObjectResult>(result);
    }

    // =================================================================
    // GET /api/accounts — Account list
    // =================================================================

    [Fact]
    public async Task ListAccounts_ReturnsOk()
    {
        await _repository.AddAsync(CreateTestAccount("12345678901"));
        await _repository.AddAsync(CreateTestAccount("12345678902"));

        var controller = CreateController();
        var result = await controller.ListAccounts(null, CancellationToken.None);

        Assert.IsType<OkObjectResult>(result);
    }

    [Fact]
    public async Task ListAccounts_WithInvalidCursor_ReturnsBadRequest()
    {
        var controller = CreateController();
        var result = await controller.ListAccounts("INVALID", CancellationToken.None);

        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // POST /api/accounts — Create account
    // =================================================================

    [Fact]
    public async Task CreateAccount_ValidRequest_ReturnsCreated()
    {
        var controller = CreateController();
        var request = new CreateAccountRequest
        {
            AccountId = "12345678901",
            AccountType = AccountType.Credit,
            HolderName = "JOHN DOE",
            CreditLimit = 5000.00m,
            CashCreditLimit = 1000.00m
        };

        var result = await controller.CreateAccount(request, CancellationToken.None);

        var createdResult = Assert.IsType<CreatedAtActionResult>(result);
        var response = Assert.IsType<AccountResponse>(createdResult.Value);
        Assert.Equal("12345678901", response.AccountId);
        Assert.Equal("Active", response.Status);
    }

    [Fact]
    public async Task CreateAccount_InvalidId_ReturnsBadRequest()
    {
        var controller = CreateController();
        var request = new CreateAccountRequest
        {
            AccountId = "INVALID",
            HolderName = "JOHN DOE"
        };

        var result = await controller.CreateAccount(request, CancellationToken.None);

        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task CreateAccount_Duplicate_ReturnsConflict()
    {
        await _repository.AddAsync(CreateTestAccount("12345678901"));

        var controller = CreateController();
        var request = new CreateAccountRequest
        {
            AccountId = "12345678901",
            HolderName = "JOHN DOE"
        };

        var result = await controller.CreateAccount(request, CancellationToken.None);

        Assert.IsType<ConflictObjectResult>(result);
    }

    // =================================================================
    // PUT /api/accounts/{accountId}/status — Update status
    // =================================================================

    [Fact]
    public async Task UpdateAccountStatus_ValidTransition_ReturnsOk()
    {
        var account = CreateTestAccount("12345678901");
        account.RowVersion = [0x01, 0x02, 0x03, 0x04];
        await _repository.AddAsync(account);

        var controller = CreateControllerWithIfMatch(
            Convert.ToBase64String([0x01, 0x02, 0x03, 0x04]));

        var request = new UpdateAccountStatusRequest { Status = AccountStatus.Dormant };
        var result = await controller.UpdateAccountStatus("12345678901", request, CancellationToken.None);

        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<AccountResponse>(okResult.Value);
        Assert.Equal("Dormant", response.Status);
    }

    [Fact]
    public async Task UpdateAccountStatus_MissingETag_Returns428()
    {
        var controller = CreateController();
        var request = new UpdateAccountStatusRequest { Status = AccountStatus.Dormant };

        var result = await controller.UpdateAccountStatus("12345678901", request, CancellationToken.None);

        var statusResult = Assert.IsType<ObjectResult>(result);
        Assert.Equal(428, statusResult.StatusCode);
    }

    [Fact]
    public async Task UpdateAccountStatus_InvalidETag_ReturnsBadRequest()
    {
        var controller = CreateControllerWithIfMatch("not-base64!!!");

        var request = new UpdateAccountStatusRequest { Status = AccountStatus.Dormant };
        var result = await controller.UpdateAccountStatus("12345678901", request, CancellationToken.None);

        Assert.IsType<BadRequestObjectResult>(result);
    }

    [Fact]
    public async Task UpdateAccountStatus_NotFound_ReturnsNotFound()
    {
        var controller = CreateControllerWithIfMatch(Convert.ToBase64String([0x01]));

        var request = new UpdateAccountStatusRequest { Status = AccountStatus.Dormant };
        var result = await controller.UpdateAccountStatus("99999999999", request, CancellationToken.None);

        Assert.IsType<NotFoundObjectResult>(result);
    }

    [Fact]
    public async Task UpdateAccountStatus_ConcurrencyConflict_ReturnsConflict()
    {
        var account = CreateTestAccount("12345678901");
        account.RowVersion = [0x01, 0x02, 0x03, 0x04];
        await _repository.AddAsync(account);

        var controller = CreateControllerWithIfMatch(
            Convert.ToBase64String([0xFF, 0xFF, 0xFF, 0xFF]));

        var request = new UpdateAccountStatusRequest { Status = AccountStatus.Dormant };
        var result = await controller.UpdateAccountStatus("12345678901", request, CancellationToken.None);

        Assert.IsType<ConflictObjectResult>(result);
    }

    [Fact]
    public async Task UpdateAccountStatus_InvalidTransition_ReturnsBadRequest()
    {
        var account = CreateTestAccount("12345678901");
        account.Status = AccountStatus.Closed;
        account.RowVersion = [0x01];
        await _repository.AddAsync(account);

        var controller = CreateControllerWithIfMatch(Convert.ToBase64String([0x01]));

        var request = new UpdateAccountStatusRequest { Status = AccountStatus.Active };
        var result = await controller.UpdateAccountStatus("12345678901", request, CancellationToken.None);

        Assert.IsType<BadRequestObjectResult>(result);
    }

    // =================================================================
    // Helpers
    // =================================================================

    private static Account CreateTestAccount(string id) => new()
    {
        Id = id,
        Status = AccountStatus.Active,
        AccountType = AccountType.Credit,
        HolderName = "TEST HOLDER",
        CreditLimit = 5000.00m,
        CashCreditLimit = 1000.00m,
        OpenedDate = new DateTime(2020, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    };
}

/// <summary>
/// In-memory repository for controller unit tests.
/// </summary>
internal sealed class InMemoryAccountManagementRepository : IAccountManagementRepository
{
    private readonly List<Account> _accounts = [];

    public Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default) =>
        Task.FromResult(_accounts.FirstOrDefault(a => a.Id == accountId));

    public Task<IReadOnlyList<Account>> GetAllAsync(int pageSize, string? afterAccountId, CancellationToken cancellationToken = default)
    {
        var query = _accounts.AsEnumerable();

        if (!string.IsNullOrEmpty(afterAccountId))
        {
            query = query.Where(a => string.Compare(a.Id, afterAccountId, StringComparison.Ordinal) > 0);
        }

        IReadOnlyList<Account> result = [.. query.OrderBy(a => a.Id).Take(pageSize)];
        return Task.FromResult(result);
    }

    public Task AddAsync(Account account, CancellationToken cancellationToken = default)
    {
        _accounts.Add(account);
        return Task.CompletedTask;
    }

    public Task UpdateAsync(Account account, CancellationToken cancellationToken = default) =>
        Task.CompletedTask;
}
