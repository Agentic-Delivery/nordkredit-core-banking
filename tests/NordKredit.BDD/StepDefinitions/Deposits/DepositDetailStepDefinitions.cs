using System.Globalization;
using NordKredit.Domain.Deposits;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Deposits;

/// <summary>
/// Step definitions for deposit account detail lookup BDD scenarios (DEP-BR-001, DEP-BR-003).
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), CBTRN02C.cbl (transaction processing).
/// Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 15 (right of access),
///              Deposit Guarantee Directive 2014/49/EU.
/// </summary>
[Binding]
[Scope(Feature = "Deposit account detail lookup")]
public sealed class DepositDetailStepDefinitions
{
    private readonly StubDepositAccountRepository _accountRepo = new();
    private DepositAccountService _service = null!;
    private DepositAccount? _response;

    [BeforeScenario]
    public void SetUp() =>
        _service = new DepositAccountService(_accountRepo, new StubSavingsProductRepository());

    [Given(@"the deposit repository contains the following accounts")]
    public void GivenTheDepositRepositoryContainsTheFollowingAccounts(Table table)
    {
        foreach (var row in table.Rows)
        {
            _accountRepo.AddAccount(new DepositAccount
            {
                Id = row["AccountId"],
                HolderName = row["HolderName"],
                Status = Enum.Parse<DepositAccountStatus>(row["Status"]),
                ProductType = Enum.Parse<DepositProductType>(row["ProductType"]),
                CurrentBalance = decimal.Parse(row["CurrentBalance"], CultureInfo.InvariantCulture),
                DisclosureGroupId = row["DisclosureGroupId"],
                OpenedDate = DateTime.Parse(row["OpenedDate"], CultureInfo.InvariantCulture),
                RowVersion = [0, 0, 0, 0, 0, 0, 0, 1]
            });
        }
    }

    [When(@"I request deposit detail for account ID ""(.*)""")]
    public async Task WhenIRequestDepositDetailForAccountId(string accountId) =>
        _response = await _service.GetByIdAsync(accountId);

    [Then(@"the deposit detail response contains account ID ""(.*)""")]
    public void ThenTheDepositDetailResponseContainsAccountId(string expectedId)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedId, _response.Id);
    }

    [Then(@"the deposit detail response contains holder name ""(.*)""")]
    public void ThenTheDepositDetailResponseContainsHolderName(string expectedName)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedName, _response.HolderName);
    }

    [Then(@"the deposit detail response contains status ""(.*)""")]
    public void ThenTheDepositDetailResponseContainsStatus(string expectedStatus)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedStatus, _response.Status.ToString());
    }

    [Then(@"the deposit detail response contains product type ""(.*)""")]
    public void ThenTheDepositDetailResponseContainsProductType(string expectedType)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedType, _response.ProductType.ToString());
    }

    [Then(@"the deposit detail response contains balance (.+)")]
    public void ThenTheDepositDetailResponseContainsBalance(decimal expectedBalance)
    {
        Assert.NotNull(_response);
        Assert.Equal(expectedBalance, _response.CurrentBalance);
    }

    [Then(@"the deposit detail response is null")]
    public void ThenTheDepositDetailResponseIsNull() =>
        Assert.Null(_response);

    /// <summary>
    /// In-memory stub repository for deposit detail BDD scenarios.
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
            Task.FromResult<IReadOnlyList<DepositAccount>>([]);

        public Task AddAsync(DepositAccount account, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;

        public Task UpdateAsync(DepositAccount account, CancellationToken cancellationToken = default) =>
            Task.CompletedTask;
    }

    internal sealed class StubSavingsProductRepository : ISavingsProductRepository
    {
        public Task<SavingsProduct?> GetByProductIdAsync(string productId, CancellationToken cancellationToken = default) =>
            Task.FromResult<SavingsProduct?>(null);

        public Task<IReadOnlyList<SavingsProduct>> GetAllAsync(CancellationToken cancellationToken = default) =>
            Task.FromResult<IReadOnlyList<SavingsProduct>>([]);
    }
}
