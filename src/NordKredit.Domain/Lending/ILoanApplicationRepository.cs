namespace NordKredit.Domain.Lending;

/// <summary>
/// Repository for loan application operations.
/// COBOL source: Dedicated program not yet in repository.
/// Business rule: LND-BR-003 (loan origination and credit assessment).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8, AML 2017:11.
/// </summary>
public interface ILoanApplicationRepository
{
    /// <summary>
    /// Reads a single loan application by its ID.
    /// </summary>
    Task<LoanApplication?> GetByIdAsync(string applicationId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves loan applications for a customer.
    /// </summary>
    Task<IReadOnlyList<LoanApplication>> GetByCustomerIdAsync(string customerId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Creates a new loan application record.
    /// </summary>
    Task AddAsync(LoanApplication application, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates an existing loan application (status change, approval/rejection).
    /// </summary>
    Task UpdateAsync(LoanApplication application, CancellationToken cancellationToken = default);
}
