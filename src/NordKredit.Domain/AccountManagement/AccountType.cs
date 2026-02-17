namespace NordKredit.Domain.AccountManagement;

/// <summary>
/// Account type classification.
/// COBOL source: CVACT01Y.cpy — ACCT-GROUP-ID (disclosure/interest group).
/// Business rule: ACCT-BR-001 (account record data structure).
/// Regulations: FSA FFFS 2014:5 Ch. 3.
/// </summary>
public enum AccountType
{
    /// <summary>Standard personal checking account.</summary>
    Checking,

    /// <summary>Savings account with interest accrual.</summary>
    Savings,

    /// <summary>Credit account with credit limit.</summary>
    Credit
}
