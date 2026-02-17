using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Deposits;

/// <summary>
/// Tests for the DepositAccountService.
/// COBOL source: CBTRN02C.cbl (batch transaction posting).
/// Business rules: DEP-BR-003, DEP-BR-004.
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, Deposit Guarantee Directive.
/// </summary>
public class DepositAccountServiceTests
{
    // =================================================================
    // DEP-BR-004: Interest accrual
    // =================================================================

    [Fact]
    public async Task AccrueInterest_ActiveAccountWithBalance_AccruesInterest()
    {
        var account = CreateTestAccount(balance: 100000.00m, groupId: "SAV-STD");
        var product = CreateTestProduct(productId: "SAV-STD", annualRate: 0.025m);
        var accountRepo = new StubDepositAccountRepository(account);
        var productRepo = new StubSavingsProductRepository(product);
        var service = new DepositAccountService(accountRepo, productRepo);

        var result = await service.AccrueInterestAsync("12345678901");

        Assert.NotNull(result);
        Assert.Equal(6.8493m, result.Value);
        Assert.Equal(6.8493m, account.AccruedInterest);
    }

    [Fact]
    public async Task AccrueInterest_InactiveAccount_ReturnsNull()
    {
        var account = CreateTestAccount(status: DepositAccountStatus.Dormant);
        var accountRepo = new StubDepositAccountRepository(account);
        var productRepo = new StubSavingsProductRepository();
        var service = new DepositAccountService(accountRepo, productRepo);

        var result = await service.AccrueInterestAsync("12345678901");

        Assert.Null(result);
    }

    [Fact]
    public async Task AccrueInterest_ZeroBalance_ReturnsZero()
    {
        var account = CreateTestAccount(balance: 0m);
        var accountRepo = new StubDepositAccountRepository(account);
        var productRepo = new StubSavingsProductRepository();
        var service = new DepositAccountService(accountRepo, productRepo);

        var result = await service.AccrueInterestAsync("12345678901");

        Assert.NotNull(result);
        Assert.Equal(0m, result.Value);
    }

    [Fact]
    public async Task AccrueInterest_AccountNotFound_ReturnsNull()
    {
        var accountRepo = new StubDepositAccountRepository();
        var productRepo = new StubSavingsProductRepository();
        var service = new DepositAccountService(accountRepo, productRepo);

        var result = await service.AccrueInterestAsync("99999999999");

        Assert.Null(result);
    }

    [Fact]
    public async Task AccrueInterest_ProductNotFound_ReturnsNull()
    {
        var account = CreateTestAccount(balance: 100000.00m, groupId: "UNKNOWN");
        var accountRepo = new StubDepositAccountRepository(account);
        var productRepo = new StubSavingsProductRepository();
        var service = new DepositAccountService(accountRepo, productRepo);

        var result = await service.AccrueInterestAsync("12345678901");

        Assert.Null(result);
    }

    [Fact]
    public async Task AccrueInterest_UpdatesRepository()
    {
        var account = CreateTestAccount(balance: 100000.00m, groupId: "SAV-STD");
        var product = CreateTestProduct(productId: "SAV-STD", annualRate: 0.025m);
        var accountRepo = new StubDepositAccountRepository(account);
        var productRepo = new StubSavingsProductRepository(product);
        var service = new DepositAccountService(accountRepo, productRepo);

        await service.AccrueInterestAsync("12345678901");

        Assert.True(accountRepo.UpdateCalled);
    }

    [Fact]
    public async Task GetById_ExistingAccount_ReturnsAccount()
    {
        var account = CreateTestAccount();
        var accountRepo = new StubDepositAccountRepository(account);
        var productRepo = new StubSavingsProductRepository();
        var service = new DepositAccountService(accountRepo, productRepo);

        var result = await service.GetByIdAsync("12345678901");

        Assert.NotNull(result);
        Assert.Equal("12345678901", result.Id);
    }

    [Fact]
    public async Task GetById_NonExistentAccount_ReturnsNull()
    {
        var accountRepo = new StubDepositAccountRepository();
        var productRepo = new StubSavingsProductRepository();
        var service = new DepositAccountService(accountRepo, productRepo);

        var result = await service.GetByIdAsync("99999999999");

        Assert.Null(result);
    }

    // =================================================================
    // Helpers
    // =================================================================

    private static DepositAccount CreateTestAccount(
        string id = "12345678901",
        decimal balance = 50000.00m,
        string groupId = "SAV-STD",
        DepositAccountStatus status = DepositAccountStatus.Active) => new()
        {
            Id = id,
            Status = status,
            ProductType = DepositProductType.DemandSavings,
            CurrentBalance = balance,
            DisclosureGroupId = groupId,
            HolderName = "JOHN DOE",
            OpenedDate = new DateTime(2020, 1, 15),
            RowVersion = [1, 2, 3, 4, 5, 6, 7, 8]
        };

    private static SavingsProduct CreateTestProduct(
        string productId = "SAV-STD",
        decimal annualRate = 0.025m) => new()
        {
            ProductId = productId,
            Description = "Standard Savings",
            AnnualRate = annualRate,
            DayCountBasis = 365
        };

    // =================================================================
    // Stub repositories
    // =================================================================

    private sealed class StubDepositAccountRepository : IDepositAccountRepository
    {
        private readonly DepositAccount? _account;
        public bool UpdateCalled { get; private set; }

        public StubDepositAccountRepository(DepositAccount? account = null)
        {
            _account = account;
        }

        public Task<DepositAccount?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_account?.Id == accountId ? _account : null);

        public Task<IReadOnlyList<DepositAccount>> GetPageAsync(int pageSize, string? afterAccountId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<DepositAccount>>(_account is not null ? [_account] : []);

        public Task<IReadOnlyList<DepositAccount>> GetActiveAccountsAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<DepositAccount>>(_account is { Status: DepositAccountStatus.Active } ? [_account] : []);

        public Task AddAsync(DepositAccount account, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;

        public Task UpdateAsync(DepositAccount account, CancellationToken cancellationToken = default)
        {
            UpdateCalled = true;
            return Task.CompletedTask;
        }
    }

    private sealed class StubSavingsProductRepository : ISavingsProductRepository
    {
        private readonly SavingsProduct? _product;

        public StubSavingsProductRepository(SavingsProduct? product = null)
        {
            _product = product;
        }

        public Task<SavingsProduct?> GetByProductIdAsync(string productId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_product?.ProductId == productId ? _product : null);

        public Task<IReadOnlyList<SavingsProduct>> GetAllAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<SavingsProduct>>(_product is not null ? [_product] : []);
    }
}
