namespace NordKredit.Domain.Deposits;

/// <summary>
/// Repository for term deposit metadata.
/// Business rule: DEP-BR-005 (term deposit maturity and renewal processing).
/// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive.
/// </summary>
public interface ITermDepositRepository
{
    /// <summary>
    /// Reads term deposit metadata by account ID.
    /// </summary>
    Task<TermDeposit?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all term deposits maturing on or before the given date.
    /// Business rule: DEP-BR-005 — maturity processing batch.
    /// </summary>
    Task<IReadOnlyList<TermDeposit>> GetMaturingByDateAsync(
        DateTime maturityDate,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Creates a new term deposit record.
    /// </summary>
    Task AddAsync(TermDeposit termDeposit, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates an existing term deposit record (e.g., after renewal).
    /// </summary>
    Task UpdateAsync(TermDeposit termDeposit, CancellationToken cancellationToken = default);
}
