using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for deposit interest accrual and posting BDD scenarios (DEP-BR-004, DEP-BR-007).
/// COBOL source: Dedicated interest calculation batch program.
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive.
/// </summary>
[Binding]
[Scope(Feature = "Deposit interest accrual and posting")]
public sealed class InterestAccrualStepDefinitions
{
    private DepositAccount _account = null!;
    private readonly StubDepositAccountRepository _accountRepo = new();
    private readonly StubSavingsProductRepository _productRepo = new();
    private DepositAccountService _service = null!;
    private decimal? _serviceResult;

    [BeforeScenario]
    public void SetUp() =>
        _service = new DepositAccountService(_accountRepo, _productRepo);

    [Given(@"a deposit account with balance (.+) and accrued interest (.+)")]
    public void GivenADepositAccountWithBalanceAndAccruedInterest(decimal balance, decimal accruedInterest) =>
        _account = new DepositAccount
        {
            Id = "12345678901",
            Status = DepositAccountStatus.Active,
            CurrentBalance = balance,
            AccruedInterest = accruedInterest,
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };

    [Given(@"a deposit account with status ""(.*)"" and balance (.+) linked to product ""(.*)""")]
    public void GivenADepositAccountWithStatusAndBalanceLinkedToProduct(
        string status, decimal balance, string productId)
    {
        _account = new DepositAccount
        {
            Id = "12345678901",
            Status = Enum.Parse<DepositAccountStatus>(status),
            CurrentBalance = balance,
            DisclosureGroupId = productId,
            RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
        };
        _accountRepo.AddAccount(_account);
    }

    [Given(@"the savings product ""(.*)"" has annual rate (.+) and day count basis (\d+)")]
    public void GivenTheSavingsProductHasAnnualRateAndDayCountBasis(
        string productId, decimal annualRate, int dayCountBasis) =>
        _productRepo.AddProduct(new SavingsProduct
        {
            ProductId = productId,
            Description = "Test Product",
            AnnualRate = annualRate,
            DayCountBasis = dayCountBasis
        });

    [When(@"I accrue daily interest of (.+)")]
    public void WhenIAccrueDailyInterestOf(decimal dailyInterest) =>
        _account.AccrueInterest(dailyInterest);

    [When(@"I post interest to the account")]
    public void WhenIPostInterestToTheAccount() =>
        _account.PostInterest();

    [When(@"I accrue interest via the service")]
    public async Task WhenIAccrueInterestViaTheService() =>
        _serviceResult = await _service.AccrueInterestAsync(_account.Id);

    [Then(@"the accrued interest is (.+)")]
    public void ThenTheAccruedInterestIs(decimal expected) =>
        Assert.Equal(expected, _account.AccruedInterest);

    [Then(@"the account balance is (.+)")]
    public void ThenTheAccountBalanceIs(decimal expectedBalance) =>
        Assert.Equal(expectedBalance, _account.CurrentBalance);

    [Then(@"the current cycle credit is (.+)")]
    public void ThenTheCurrentCycleCreditIs(decimal expectedCredit) =>
        Assert.Equal(expectedCredit, _account.CurrentCycleCredit);

    [Then(@"the service returns null")]
    public void ThenTheServiceReturnsNull() =>
        Assert.Null(_serviceResult);

    [Then(@"the service returns (.+)")]
    public void ThenTheServiceReturns(decimal expected) =>
        Assert.Equal(expected, _serviceResult);

    /// <summary>
    /// In-memory stub repository for interest accrual BDD scenarios.
    /// Matches COBOL VSAM ACCTFILE behavior.
    /// </summary>
    internal sealed class StubDepositAccountRepository : IDepositAccountRepository
    {
        private readonly List<DepositAccount> _accounts = [];

        public void AddAccount(DepositAccount account) => _accounts.Add(account);

        public Task<DepositAccount?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_accounts.FirstOrDefault(a => a.Id == accountId));

        public Task<IReadOnlyList<DepositAccount>> GetPageAsync(
            int pageSize, string? afterAccountId = null, CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<DepositAccount>>([]);

        public Task<IReadOnlyList<DepositAccount>> GetActiveAccountsAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<DepositAccount>>([.. _accounts.Where(a => a.Status == DepositAccountStatus.Active)]);

        public Task AddAsync(DepositAccount account, CancellationToken cancellationToken = default)
        {
            _accounts.Add(account);
            return Task.CompletedTask;
        }

        public Task UpdateAsync(DepositAccount account, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }

    internal sealed class StubSavingsProductRepository : ISavingsProductRepository
    {
        private readonly List<SavingsProduct> _products = [];

        public void AddProduct(SavingsProduct product) => _products.Add(product);

        public Task<SavingsProduct?> GetByProductIdAsync(string productId, CancellationToken cancellationToken = default) =>
            Task.FromResult(_products.FirstOrDefault(p => p.ProductId == productId));

        public Task<IReadOnlyList<SavingsProduct>> GetAllAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<SavingsProduct>>([.. _products]);
    }
}
