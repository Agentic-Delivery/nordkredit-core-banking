namespace NordKredit.Domain.Deposits;

/// <summary>
/// Deposit account lifecycle status.
/// COBOL source: CVACT01Y.cpy — ACCT-ACTIVE-STATUS PIC X(01).
/// COBOL values: 'Y' (active), 'N' (inactive).
/// Extended for .NET: Active, Dormant, Frozen, Closed.
/// Business rule: DEP-BR-001 (deposit account data structure), DEP-BR-009 (dormancy management).
/// Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 5(1)(e), Deposit Guarantee Directive.
/// </summary>
public enum DepositAccountStatus
{
    /// <summary>Account is active and can process deposits/withdrawals. COBOL: 'Y'.</summary>
    Active,

    /// <summary>Account is dormant — no customer-initiated activity for extended period.</summary>
    Dormant,

    /// <summary>Account is frozen — regulatory hold or fraud investigation.</summary>
    Frozen,

    /// <summary>Account is closed — terminal state. GDPR Art. 17 right to erasure applies.</summary>
    Closed
}
