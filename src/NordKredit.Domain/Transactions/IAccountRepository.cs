namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for account records used by transaction processing.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), VSAM ACCTFILE.
/// Used by CBTRN02C to update balances during daily posting.
/// </summary>
public interface IAccountRepository
{
    Task<Account?> GetByIdAsync(string accountId, CancellationToken cancellationToken = default);

    Task UpdateAsync(Account account, CancellationToken cancellationToken = default);
}
