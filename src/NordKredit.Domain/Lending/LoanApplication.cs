namespace NordKredit.Domain.Lending;

/// <summary>
/// Loan origination application entity.
/// COBOL source: Dedicated program not yet in repository (inferred from CVACT01Y.cpy).
/// Business rule: LND-BR-003 (loan origination and credit assessment).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8 (creditworthiness),
///              AML 2017:11 (customer due diligence), GDPR Art. 6(1)(b) (contract performance).
/// </summary>
public class LoanApplication
{
    /// <summary>Unique application identifier.</summary>
    public string ApplicationId { get; set; } = string.Empty;

    /// <summary>Customer identifier for the applicant.</summary>
    public string CustomerId { get; set; } = string.Empty;

    /// <summary>
    /// Requested credit amount. decimal for COBOL PIC S9(10)V99 precision.
    /// Regulations: Consumer Credit Directive Art. 10(2)(d) — total amount of credit.
    /// </summary>
    public decimal RequestedAmount { get; set; }

    /// <summary>Requested loan term in months (for term loans).</summary>
    public int RequestedTermMonths { get; set; }

    /// <summary>Loan product type requested.</summary>
    public LoanType LoanType { get; set; } = LoanType.RevolvingCredit;

    /// <summary>Application processing status.</summary>
    public LoanApplicationStatus Status { get; set; } = LoanApplicationStatus.Pending;

    /// <summary>Applicant name. Supports Swedish characters (Å, Ä, Ö) via nvarchar.</summary>
    public string ApplicantName { get; set; } = string.Empty;

    /// <summary>Date the application was submitted.</summary>
    public DateTime ApplicationDate { get; set; }

    /// <summary>
    /// Whether AML/KYC verification has passed.
    /// AML 2017:11 §3 — customer due diligence required before credit relationship.
    /// </summary>
    public bool AmlKycPassed { get; set; }

    /// <summary>
    /// Whether creditworthiness assessment has passed.
    /// FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8.
    /// </summary>
    public bool CreditAssessmentPassed { get; set; }

    /// <summary>Approved credit limit (null if not yet approved). decimal for precision.</summary>
    public decimal? ApprovedCreditLimit { get; set; }

    /// <summary>
    /// Rejection reason (null if not rejected).
    /// Consumer Credit Directive Art. 9 — adverse action notification.
    /// </summary>
    public string? RejectionReason { get; set; }
}
