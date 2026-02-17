namespace NordKredit.Domain.AccountManagement;

/// <summary>
/// Account lifecycle status.
/// COBOL source: CVACT01Y.cpy — ACCT-ACTIVE-STATUS PIC X(01).
/// COBOL values: 'Y' (active), 'N' (inactive).
/// Extended for .NET: Active (A), Dormant (D), Frozen (F), Closed (C).
/// Business rule: ACCT-BR-005 (account status transitions).
/// Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97, GDPR Art. 17.
/// </summary>
public enum AccountStatus
{
    /// <summary>Account is active and can process transactions. COBOL: 'Y'.</summary>
    Active,

    /// <summary>Account is dormant — no activity for extended period. Transactions blocked until reactivated.</summary>
    Dormant,

    /// <summary>Account is frozen — regulatory hold or fraud investigation. All operations blocked.</summary>
    Frozen,

    /// <summary>Account is closed — terminal state. GDPR Art. 17 right to erasure applies.</summary>
    Closed
}
