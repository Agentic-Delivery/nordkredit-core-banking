namespace NordKredit.Domain.AccountManagement;

/// <summary>
/// Repository for account management operations (full CRUD).
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), VSAM ACCTFILE.
/// Business rule: ACCT-BR-001 (account record data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, GDPR Art. 17.
/// </summary>
public interface IAccountManagementRepository
{
    Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default);

    Task<IReadOnlyList<Account>> GetAllAsync(int pageSize, string? afterAccountId, CancellationToken cancellationToken = default);

    Task AddAsync(Account account, CancellationToken cancellationToken = default);

    Task UpdateAsync(Account account, CancellationToken cancellationToken = default);
}
