namespace NordKredit.Domain.Deposits;

/// <summary>
/// Domain service for deposit account operations.
/// COBOL source: CBTRN02C.cbl (batch transaction posting), CVACT01Y.cpy (account record).
/// Business rules: DEP-BR-003 (deposit posting), DEP-BR-004 (interest calculation),
///                 DEP-BR-005 (maturity processing).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, Deposit Guarantee Directive.
/// </summary>
public class DepositAccountService
{
    private readonly IDepositAccountRepository _depositAccountRepository;
    private readonly ISavingsProductRepository _savingsProductRepository;

    public DepositAccountService(
        IDepositAccountRepository depositAccountRepository,
        ISavingsProductRepository savingsProductRepository)
    {
        _depositAccountRepository = depositAccountRepository;
        _savingsProductRepository = savingsProductRepository;
    }

    /// <summary>
    /// Retrieves a deposit account by ID.
    /// COBOL: VSAM READ on ACCTFILE by primary key.
    /// </summary>
    public async Task<DepositAccount?> GetByIdAsync(
        string accountId,
        CancellationToken cancellationToken = default) =>
        await _depositAccountRepository.GetByIdAsync(accountId, cancellationToken);

    /// <summary>
    /// Calculates and accrues daily interest for a deposit account.
    /// COBOL source: Dedicated interest calculation batch (not yet in repository).
    /// Business rule: DEP-BR-004.
    /// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6, Deposit Guarantee Directive.
    /// </summary>
    /// <returns>The daily interest amount accrued, or null if the account is ineligible.</returns>
    public async Task<decimal?> AccrueInterestAsync(
        string accountId,
        CancellationToken cancellationToken = default)
    {
        var account = await _depositAccountRepository.GetByIdAsync(accountId, cancellationToken);
        if (account is null)
        {
            return null;
        }

        // Skip inactive accounts — DEP-BR-004
        if (account.Status != DepositAccountStatus.Active)
        {
            return null;
        }

        // Skip zero or negative balance accounts
        if (account.CurrentBalance <= 0)
        {
            return 0m;
        }

        var product = await _savingsProductRepository.GetByProductIdAsync(
            account.DisclosureGroupId, cancellationToken);
        if (product is null)
        {
            return null;
        }

        var dailyInterest = InterestCalculation.CalculateDailyInterest(
            account.CurrentBalance, product);

        account.AccrueInterest(dailyInterest);
        await _depositAccountRepository.UpdateAsync(account, cancellationToken);

        return dailyInterest;
    }
}
