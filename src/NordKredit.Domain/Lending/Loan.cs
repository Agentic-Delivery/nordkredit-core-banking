namespace NordKredit.Domain.Lending;

/// <summary>
/// Loan account entity representing a lending facility.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Business rules: LND-BR-001 (data structure), LND-BR-002 (credit limit enforcement),
///                 LND-BR-005 (repayment processing), LND-BR-009 (expiration enforcement).
/// Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), GDPR Art. 5(1)(c)(d),
///              Consumer Credit Directive 2008/48/EC Art. 10, PSD2 Art. 64.
/// </summary>
public class Loan
{
    /// <summary>Account identifier. COBOL: ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>
    /// Loan lifecycle status.
    /// COBOL: ACCT-ACTIVE-STATUS PIC X(01) — 'Y'/'N'.
    /// Extended: Active, Delinquent, Defaulted, Frozen, PaidOff, Closed.
    /// Business rule: LND-BR-008.
    /// </summary>
    public LoanStatus ActiveStatus { get; set; } = LoanStatus.Active;

    /// <summary>Loan product type. COBOL: inferred from ACCT-GROUP-ID.</summary>
    public LoanType LoanType { get; set; } = LoanType.RevolvingCredit;

    /// <summary>
    /// Current outstanding balance. COBOL: ACCT-CURR-BAL PIC S9(10)V99.
    /// Maps to SQL decimal(12,2). Negative = overpayment credit.
    /// Business rule: LND-BR-001, LND-BR-005.
    /// </summary>
    public decimal CurrentBalance { get; set; }

    /// <summary>
    /// Maximum authorized credit (purchase limit).
    /// COBOL: ACCT-CREDIT-LIMIT PIC S9(10)V99. Maps to SQL decimal(12,2).
    /// Business rule: LND-BR-002.
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10.
    /// </summary>
    public decimal CreditLimit { get; set; }

    /// <summary>
    /// Cash advance credit limit (separate ceiling).
    /// COBOL: ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99. Maps to SQL decimal(12,2).
    /// Business rule: LND-BR-001.
    /// </summary>
    public decimal CashCreditLimit { get; set; }

    /// <summary>
    /// Current cycle charges (utilization).
    /// COBOL: ACCT-CURR-CYC-CREDIT PIC S9(10)V99.
    /// Business rule: LND-BR-002, LND-BR-005.
    /// </summary>
    public decimal CurrentCycleCredit { get; set; }

    /// <summary>
    /// Current cycle payments (repayments). Stored as negative values.
    /// COBOL: ACCT-CURR-CYC-DEBIT PIC S9(10)V99.
    /// Business rule: LND-BR-002, LND-BR-005.
    /// </summary>
    public decimal CurrentCycleDebit { get; set; }

    /// <summary>
    /// Account/loan maturity date. COBOL: ACCT-EXPIRAION-DATE PIC X(10) (sic — typo in COBOL source).
    /// Null means no expiration — revolving credit without fixed maturity.
    /// Business rule: LND-BR-009.
    /// </summary>
    public DateTime? ExpirationDate { get; set; }

    /// <summary>
    /// Disclosure/interest rate group assignment.
    /// COBOL: ACCT-GROUP-ID PIC X(10).
    /// Links to interest rate schedule for LND-BR-004.
    /// </summary>
    public string DisclosureGroupId { get; set; } = string.Empty;

    /// <summary>Borrower name. Supports Swedish characters (Å, Ä, Ö) via nvarchar.</summary>
    public string BorrowerName { get; set; } = string.Empty;

    /// <summary>Date the loan was originated.</summary>
    public DateTime OriginationDate { get; set; }

    /// <summary>Optimistic concurrency token. Replaces COBOL field-by-field comparison with SQL rowversion.</summary>
    public byte[] RowVersion { get; set; } = [];

    /// <summary>
    /// Available credit remaining on the facility.
    /// Derived: CreditLimit - (CycleCredit - CycleDebit).
    /// Business rule: LND-BR-001.
    /// </summary>
    public decimal AvailableCredit => CreditLimit - (CurrentCycleCredit - CurrentCycleDebit);

    /// <summary>
    /// Checks if a transaction amount would exceed the credit limit.
    /// COBOL source: CBTRN02C.cbl:403-413 (1500-B-LOOKUP-ACCT).
    /// Business rule: LND-BR-002.
    /// Formula: projected = CycleCredit - CycleDebit + amount; exceeds if projected > limit.
    /// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10.
    /// </summary>
    public bool WouldExceedCreditLimit(decimal transactionAmount)
    {
        var projectedBalance = CurrentCycleCredit - CurrentCycleDebit + transactionAmount;
        return projectedBalance > CreditLimit;
    }

    /// <summary>
    /// Applies a transaction amount to loan balance.
    /// COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
    /// Business rule: LND-BR-005.
    /// Positive amounts are charges (added to CycleCredit).
    /// Negative amounts are repayments (added to CycleDebit).
    /// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, Art. 89.
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
    /// Checks if the loan has expired (past maturity) as of the given date.
    /// COBOL source: CBTRN02C.cbl:414-420.
    /// Business rule: LND-BR-009.
    /// </summary>
    public bool IsExpired(DateTime asOfDate) =>
        ExpirationDate.HasValue && ExpirationDate.Value < asOfDate;

    /// <summary>
    /// Transitions the loan to a new status with lifecycle state machine enforcement.
    /// Business rule: LND-BR-008.
    /// Valid transitions:
    ///   Active → Delinquent, Frozen, PaidOff, Closed
    ///   Delinquent → Active, Defaulted, Frozen
    ///   Defaulted → Closed
    ///   Frozen → Active, Delinquent, Closed
    ///   PaidOff → Closed
    ///   Closed → (terminal — no transitions)
    /// </summary>
    public LoanValidationResult TransitionTo(LoanStatus newStatus)
    {
        var validation = LoanValidationService.ValidateStatusTransition(ActiveStatus, newStatus);
        if (!validation.IsValid)
        {
            return validation;
        }

        ActiveStatus = newStatus;
        return LoanValidationResult.Success();
    }
}
