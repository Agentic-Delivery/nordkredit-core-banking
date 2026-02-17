namespace NordKredit.Domain.Lending;

/// <summary>
/// Repository for loan account operations.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), VSAM ACCTFILE.
/// Business rules: LND-BR-001 (data structure), LND-BR-005 (balance update).
/// Regulations: FSA FFFS 2014:5 Ch. 3, 6, PSD2 Art. 64, GDPR Art. 17.
/// </summary>
public interface ILoanRepository
{
    /// <summary>
    /// Reads a single loan account by its account ID.
    /// COBOL: READ ACCOUNT-FILE using FD-ACCT-ID.
    /// </summary>
    Task<Loan?> GetByAccountIdAsync(string accountId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves a forward page of loan accounts ordered by account ID (keyset pagination).
    /// COBOL: CBTRN02C.cbl — sequential read with STARTBR/READNEXT pattern.
    /// </summary>
    Task<IReadOnlyList<Loan>> GetPageAsync(int pageSize, string? afterAccountId = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// Creates a new loan account record.
    /// COBOL: WRITE ACCOUNT-RECORD to ACCTFILE.
    /// Business rule: LND-BR-003 (loan origination).
    /// </summary>
    Task AddAsync(Loan loan, CancellationToken cancellationToken = default);

    /// <summary>
    /// Updates an existing loan account record with optimistic concurrency check.
    /// COBOL: REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD.
    /// Business rule: LND-BR-005 (balance update after repayment).
    /// </summary>
    Task UpdateAsync(Loan loan, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all loans with the specified status.
    /// Used by lending batch functions for bulk processing (amortization, delinquency monitoring).
    /// Business rule: LND-BR-008 (delinquency management).
    /// </summary>
    Task<IReadOnlyList<Loan>> GetByStatusAsync(LoanStatus status, CancellationToken cancellationToken = default);
}
