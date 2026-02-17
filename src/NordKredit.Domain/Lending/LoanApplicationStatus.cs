namespace NordKredit.Domain.Lending;

/// <summary>
/// Loan application processing status.
/// COBOL source: Dedicated program not yet in repository.
/// Business rule: LND-BR-003 (loan origination and credit assessment).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8, 9.
/// </summary>
public enum LoanApplicationStatus
{
    /// <summary>Application submitted, awaiting AML/KYC and credit assessment.</summary>
    Pending,

    /// <summary>Application approved — loan account can be created.</summary>
    Approved,

    /// <summary>Application rejected — AML/KYC failure, insufficient creditworthiness, or exceeds maximum limit.</summary>
    Rejected,

    /// <summary>Application cancelled by applicant before decision.</summary>
    Cancelled
}
