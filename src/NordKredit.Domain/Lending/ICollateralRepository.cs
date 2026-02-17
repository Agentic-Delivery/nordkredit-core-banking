namespace NordKredit.Domain.Lending;

/// <summary>
/// Repository for collateral management operations.
/// COBOL source: Dedicated program not yet in repository (inferred VSAM COLLATERAL file).
/// Business rule: LND-BR-006 (collateral management and valuation).
/// Regulations: FSA FFFS 2014:5 Ch. 6, 8, CRR Art. 194-217.
/// </summary>
public interface ICollateralRepository
{
    /// <summary>
    /// Reads a single collateral record by its ID.
    /// COBOL: READ COLLATERAL-FILE using COLLATERAL-ID.
    /// </summary>
    Task<Collateral?> GetByIdAsync(string collateralId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all collateral records linked to a loan account.
    /// Supports one-to-many relationship (multiple collateral per loan).
    /// </summary>
    Task<IReadOnlyList<Collateral>> GetByLoanAccountIdAsync(string loanAccountId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Registers a new collateral record.
    /// COBOL: WRITE COLLATERAL-RECORD.
    /// </summary>
    Task AddAsync(Collateral collateral, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates an existing collateral record (revaluation, status change, release).
    /// COBOL: REWRITE COLLATERAL-RECORD.
    /// </summary>
    Task UpdateAsync(Collateral collateral, CancellationToken cancellationToken = default);
}
