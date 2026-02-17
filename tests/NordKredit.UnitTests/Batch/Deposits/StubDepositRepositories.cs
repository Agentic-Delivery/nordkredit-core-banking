using NordKredit.Domain.Deposits;

namespace NordKredit.UnitTests.Batch.Deposits;

/// <summary>
/// In-memory stub for IDepositAccountRepository used in batch function tests.
/// </summary>
internal sealed class StubDepositAccountRepository : IDepositAccountRepository
{
    private readonly List<DepositAccount> _accounts = [];
    private readonly List<DepositAccount> _activeAccounts = [];

    public bool ThrowOnRead { get; set; }

    public void Add(DepositAccount account) => _accounts.Add(account);

    public void AddActive(DepositAccount account)
    {
        _accounts.Add(account);
        _activeAccounts.Add(account);
    }

    public Task<DepositAccount?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default)
        => Task.FromResult(_accounts.Find(a => a.Id == accountId));

    public Task<IReadOnlyList<DepositAccount>> GetPageAsync(
        int pageSize, string? afterAccountId = null, CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<DepositAccount>>(_accounts.AsReadOnly());

    public Task<IReadOnlyList<DepositAccount>> GetActiveAccountsAsync(CancellationToken cancellationToken = default)
    {
        if (ThrowOnRead)
        {
            throw new InvalidOperationException("Deposit account source is unavailable");
        }

        return Task.FromResult<IReadOnlyList<DepositAccount>>(_activeAccounts.AsReadOnly());
    }

    public Task AddAsync(DepositAccount account, CancellationToken cancellationToken = default)
    {
        _accounts.Add(account);
        return Task.CompletedTask;
    }

    public Task UpdateAsync(DepositAccount account, CancellationToken cancellationToken = default)
        => Task.CompletedTask;
}

/// <summary>
/// In-memory stub for ISavingsProductRepository used in batch function tests.
/// </summary>
internal sealed class StubSavingsProductRepository : ISavingsProductRepository
{
    private readonly List<SavingsProduct> _products = [];

    public void Add(SavingsProduct product) => _products.Add(product);

    public Task<SavingsProduct?> GetByProductIdAsync(string productId, CancellationToken cancellationToken = default)
        => Task.FromResult(_products.Find(p => p.ProductId == productId));

    public Task<IReadOnlyList<SavingsProduct>> GetAllAsync(CancellationToken cancellationToken = default)
        => Task.FromResult<IReadOnlyList<SavingsProduct>>(_products.AsReadOnly());
}
