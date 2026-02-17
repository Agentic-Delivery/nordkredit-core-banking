namespace NordKredit.Domain.Lending;

/// <summary>
/// Loan lifecycle status.
/// COBOL source: CVACT01Y.cpy — ACCT-ACTIVE-STATUS PIC X(01).
/// COBOL values: 'Y' (active), 'N' (inactive).
/// Extended for .NET: Active, Delinquent, Defaulted, Frozen, PaidOff, Closed.
/// Business rule: LND-BR-008 (delinquency management).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive, Inkassolagen 1974:182.
/// </summary>
public enum LoanStatus
{
    /// <summary>Loan is active and can process transactions. COBOL: 'Y'.</summary>
    Active,

    /// <summary>Loan has missed payments — in early or stage 1-3 delinquency. Collections process initiated.</summary>
    Delinquent,

    /// <summary>Loan is in default — severe delinquency, write-off candidate. Escalated to legal/external collections.</summary>
    Defaulted,

    /// <summary>Loan is frozen — regulatory hold or fraud investigation. All operations blocked.</summary>
    Frozen,

    /// <summary>Loan is fully repaid — balance is zero. Awaiting closure and collateral release.</summary>
    PaidOff,

    /// <summary>Loan is closed — terminal state. GDPR Art. 17 right to erasure applies after retention period.</summary>
    Closed
}
