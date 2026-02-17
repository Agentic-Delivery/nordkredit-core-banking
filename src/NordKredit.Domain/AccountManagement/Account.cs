namespace NordKredit.Domain.AccountManagement;

/// <summary>
/// Account management entity with lifecycle state machine.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Business rules: ACCT-BR-001 (data structure), ACCT-BR-004 (balance management),
///                 ACCT-BR-005 (status transitions), ACCT-BR-006 (expiration date),
///                 ACCT-BR-007 (credit limit enforcement).
/// Regulations: GDPR Art. 5(1)(c)(d), FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64.
///
/// This is the full account management entity. The Transactions.Account entity
/// contains only the subset of fields needed for transaction processing.
/// </summary>
public class Account
{
    /// <summary>Account identifier. COBOL: ACCT-ID PIC 9(11).</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>
    /// Account lifecycle status.
    /// COBOL: ACCT-ACTIVE-STATUS PIC X(01) — 'Y'/'N'.
    /// Extended: Active (A), Dormant (D), Frozen (F), Closed (C).
    /// Business rule: ACCT-BR-005.
    /// </summary>
    public AccountStatus Status { get; set; } = AccountStatus.Active;

    /// <summary>Account type classification. COBOL: ACCT-GROUP-ID.</summary>
    public AccountType AccountType { get; set; } = AccountType.Checking;

    /// <summary>Current balance. COBOL: ACCT-CURR-BAL PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CurrentBalance { get; set; }

    /// <summary>Credit limit. COBOL: ACCT-CREDIT-LIMIT PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CreditLimit { get; set; }

    /// <summary>Cash credit limit. COBOL: ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99. Maps to SQL decimal(12,2).</summary>
    public decimal CashCreditLimit { get; set; }

    /// <summary>Current cycle credit total. COBOL: ACCT-CURR-CYC-CREDIT PIC S9(10)V99.</summary>
    public decimal CurrentCycleCredit { get; set; }

    /// <summary>Current cycle debit total. COBOL: ACCT-CURR-CYC-DEBIT PIC S9(10)V99.</summary>
    public decimal CurrentCycleDebit { get; set; }

    /// <summary>
    /// Account expiration date. COBOL: ACCT-EXPIRAION-DATE (sic — typo in COBOL source).
    /// Null means no expiration — account never expires.
    /// Business rule: ACCT-BR-006.
    /// </summary>
    public DateTime? ExpirationDate { get; set; }

    /// <summary>Account holder name. Supports Swedish characters (Å, Ä, Ö) via nvarchar.</summary>
    public string HolderName { get; set; } = string.Empty;

    /// <summary>Date the account was opened.</summary>
    public DateTime OpenedDate { get; set; }

    /// <summary>Date the account was closed. Null if still open. GDPR Art. 17 applies after closure.</summary>
    public DateTime? ClosedDate { get; set; }

    /// <summary>Optimistic concurrency token. Replaces COBOL field-by-field comparison with SQL rowversion.</summary>
    public byte[] RowVersion { get; set; } = [];

    /// <summary>
    /// Transitions the account to a new status with lifecycle state machine enforcement.
    /// Business rule: ACCT-BR-005.
    /// Valid transitions:
    ///   Active → Dormant, Frozen, Closed
    ///   Dormant → Active, Frozen, Closed
    ///   Frozen → Active, Dormant, Closed
    ///   Closed → (terminal — no transitions)
    /// </summary>
    public AccountValidationResult TransitionTo(AccountStatus newStatus)
    {
        var validation = AccountValidationService.ValidateStatusTransition(Status, newStatus);
        if (!validation.IsValid)
        {
            return validation;
        }

        Status = newStatus;

        if (newStatus == AccountStatus.Closed)
        {
            ClosedDate = DateTime.UtcNow;
        }

        return AccountValidationResult.Success();
    }

    /// <summary>
    /// Applies a transaction amount to account balance.
    /// COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
    /// Business rule: ACCT-BR-004.
    /// Regulations: FSA FFFS 2014:5 Ch. 3 &amp; 7, PSD2 Art. 64.
    /// </summary>
    public void ApplyTransaction(decimal amount)
    {
        CurrentBalance += amount;

        if (amount >= 0)
        {
            CurrentCycleCredit += amount;
        }
        else
        {
            CurrentCycleDebit += amount;
        }
    }

    /// <summary>
    /// Checks if a transaction amount would exceed the credit limit.
    /// COBOL source: CBTRN02C.cbl:403-413.
    /// Business rule: ACCT-BR-007.
    /// </summary>
    public bool WouldExceedCreditLimit(decimal transactionAmount)
    {
        var projectedBalance = CurrentCycleCredit - CurrentCycleDebit + transactionAmount;
        return projectedBalance > CreditLimit;
    }

    /// <summary>
    /// Checks if the account has expired as of the given date.
    /// COBOL source: CBTRN02C.cbl:414-420.
    /// Business rule: ACCT-BR-006.
    /// </summary>
    public bool IsExpired(DateTime asOfDate) =>
        ExpirationDate.HasValue && ExpirationDate.Value < asOfDate;
}
