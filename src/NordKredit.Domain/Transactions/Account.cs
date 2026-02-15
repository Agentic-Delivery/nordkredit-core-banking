namespace NordKredit.Domain.Transactions;

/// <summary>
/// Account record used by transaction processing.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Contains only the fields needed for transaction domain operations.
/// Full account management entity will reside in AccountManagement domain.
/// </summary>
public class Account
{
    /// <summary>Account identifier. COBOL: ACCT-ID PIC 9(11).</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>Active status ('A' = active, 'D' = dormant). COBOL: ACCT-ACTIVE-STATUS PIC X(01).</summary>
    public string ActiveStatus { get; set; } = string.Empty;

    /// <summary>Current balance. COBOL: ACCT-CURR-BAL PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CurrentBalance { get; set; }

    /// <summary>Credit limit. COBOL: ACCT-CREDIT-LIMIT PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CreditLimit { get; set; }

    /// <summary>Cash credit limit. COBOL: ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CashCreditLimit { get; set; }

    /// <summary>Current cycle credit total. COBOL: ACCT-CURR-CYC-CREDIT PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CurrentCycleCredit { get; set; }

    /// <summary>Current cycle debit total. COBOL: ACCT-CURR-CYC-DEBIT PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CurrentCycleDebit { get; set; }
}
