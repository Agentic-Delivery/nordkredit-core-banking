namespace NordKredit.Domain.Deposits;

/// <summary>
/// Repository for deposit account operations.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), VSAM ACCTFILE.
/// Business rule: DEP-BR-001 (deposit account data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, GDPR Art. 17.
/// </summary>
public interface IDepositAccountRepository
{
    /// <summary>
    /// Reads a single deposit account by its account ID.
    /// COBOL: VSAM READ on ACCTFILE by primary key ACCT-ID.
    /// </summary>
    Task<DepositAccount?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves a page of deposit accounts using keyset pagination.
    /// COBOL: Sequential read of ACCTFILE with STARTBR + READNEXT.
    /// </summary>
    Task<IReadOnlyList<DepositAccount>> GetPageAsync(
        int pageSize,
        string? afterAccountId = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all active deposit accounts for interest calculation batch.
    /// COBOL: Sequential read of ACCTFILE, filter ACCT-ACTIVE-STATUS = 'Y'.
    /// Business rule: DEP-BR-004.
    /// </summary>
    Task<IReadOnlyList<DepositAccount>> GetActiveAccountsAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Creates a new deposit account.
    /// Business rule: DEP-BR-002.
    /// </summary>
    Task AddAsync(DepositAccount account, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates an existing deposit account with optimistic concurrency check.
    /// COBOL: REWRITE ACCOUNT-RECORD.
    /// </summary>
    Task UpdateAsync(DepositAccount account, CancellationToken cancellationToken = default);
}
