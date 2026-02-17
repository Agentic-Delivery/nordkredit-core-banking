namespace NordKredit.Domain.Lending;

/// <summary>
/// Loan product type classification.
/// COBOL source: CVACT01Y.cpy — inferred from ACCT-GROUP-ID (disclosure group).
/// Business rule: LND-BR-001 (loan account data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10.
/// </summary>
public enum LoanType
{
    /// <summary>Revolving credit facility with a credit limit. COBOL: Primary model in CardDemo source.</summary>
    RevolvingCredit,

    /// <summary>Fixed-term loan with scheduled amortization payments.</summary>
    TermLoan,

    /// <summary>Residential mortgage — subject to Finansinspektionen 85% LTV cap.</summary>
    Mortgage
}
