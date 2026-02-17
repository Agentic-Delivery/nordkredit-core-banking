namespace NordKredit.Domain.Deposits;

/// <summary>
/// Repository for savings product configuration.
/// COBOL source: CVTRA02Y.cpy (disclosure group records), VSAM disclosure group file.
/// Business rule: DEP-BR-006 (savings product configuration and tiered rates).
/// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 6.
/// </summary>
public interface ISavingsProductRepository
{
    /// <summary>
    /// Reads a savings product by its product/disclosure group ID.
    /// COBOL: READ DISCLOSURE-GROUP-FILE using ACCT-GROUP-ID.
    /// </summary>
    Task<SavingsProduct?> GetByProductIdAsync(string productId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all savings products.
    /// </summary>
    Task<IReadOnlyList<SavingsProduct>> GetAllAsync(CancellationToken cancellationToken = default);
}
