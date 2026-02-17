namespace NordKredit.Domain.Deposits;

/// <summary>
/// Deposit account entity representing savings and term deposit accounts.
/// COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), 300 bytes.
/// Business rules: DEP-BR-001 (data structure), DEP-BR-003 (balance update),
///                 DEP-BR-004 (interest accrual), DEP-BR-005 (maturity),
///                 DEP-BR-009 (dormancy management).
/// Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 5(1)(c)(d),
///              Deposit Guarantee Directive 2014/49/EU, PSD2 Art. 64.
/// </summary>
public class DepositAccount
{
    /// <summary>Account identifier. COBOL: ACCT-ID PIC 9(11).</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>
    /// Account lifecycle status.
    /// COBOL: ACCT-ACTIVE-STATUS PIC X(01) — 'Y'/'N'.
    /// Extended: Active, Dormant, Frozen, Closed.
    /// Business rule: DEP-BR-009.
    /// </summary>
    public DepositAccountStatus Status { get; set; } = DepositAccountStatus.Active;

    /// <summary>
    /// Deposit product type classification.
    /// COBOL: ACCT-GROUP-ID (disclosure/interest group).
    /// Business rule: DEP-BR-006.
    /// </summary>
    public DepositProductType ProductType { get; set; } = DepositProductType.DemandSavings;

    /// <summary>
    /// Current deposit balance.
    /// COBOL: ACCT-CURR-BAL PIC S9(10)V99. Maps to SQL decimal(12,2).
    /// Business rule: DEP-BR-001, DEP-BR-003.
    /// Regulations: Deposit Guarantee Directive (coverage calculation).
    /// </summary>
    public decimal CurrentBalance { get; set; }

    /// <summary>
    /// Current cycle deposit inflows.
    /// COBOL: ACCT-CURR-CYC-CREDIT PIC S9(10)V99.
    /// Business rule: DEP-BR-003.
    /// </summary>
    public decimal CurrentCycleCredit { get; set; }

    /// <summary>
    /// Current cycle withdrawal outflows.
    /// COBOL: ACCT-CURR-CYC-DEBIT PIC S9(10)V99.
    /// Business rule: DEP-BR-003.
    /// </summary>
    public decimal CurrentCycleDebit { get; set; }

    /// <summary>
    /// Disclosure/interest rate group assignment.
    /// COBOL: ACCT-GROUP-ID PIC X(10).
    /// Links to interest rate schedules for deposit products.
    /// Business rule: DEP-BR-004, DEP-BR-006.
    /// </summary>
    public string DisclosureGroupId { get; set; } = string.Empty;

    /// <summary>
    /// Term deposit maturity date. Null for demand deposit accounts.
    /// COBOL: ACCT-EXPIRAION-DATE PIC X(10), format YYYY-MM-DD.
    /// Business rule: DEP-BR-005.
    /// </summary>
    public DateTime? MaturityDate { get; set; }

    /// <summary>Account holder name. Supports Swedish characters (Å, Ä, Ö) via nvarchar.</summary>
    public string HolderName { get; set; } = string.Empty;

    /// <summary>Date the account was opened. Business rule: DEP-BR-002.</summary>
    public DateTime OpenedDate { get; set; }

    /// <summary>Date the account was closed. Null if still open. GDPR Art. 17 applies after closure.</summary>
    public DateTime? ClosedDate { get; set; }

    /// <summary>
    /// Accrued (unposted) interest balance.
    /// Uses 4 decimal places for intermediate precision.
    /// Business rule: DEP-BR-004.
    /// Regulations: Deposit Guarantee Directive (coverage includes accrued interest).
    /// </summary>
    public decimal AccruedInterest { get; set; }

    /// <summary>Date of last interest posting to balance. Business rule: DEP-BR-004.</summary>
    public DateTime? LastInterestPostingDate { get; set; }

    /// <summary>Optimistic concurrency token. Replaces COBOL field-by-field comparison with SQL rowversion.</summary>
    public byte[] RowVersion { get; set; } = [];

    /// <summary>
    /// Applies a transaction amount to deposit account balance.
    /// COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
    /// Business rule: DEP-BR-003.
    /// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64.
    /// Positive amounts = deposits (inflows), negative amounts = withdrawals (outflows).
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
    /// Checks if the deposit has matured as of the given date.
    /// COBOL source: CBTRN02C.cbl:414-420.
    /// Business rule: DEP-BR-005.
    /// Returns false for demand deposits (null maturity date).
    /// </summary>
    public bool IsMatured(DateTime asOfDate) =>
        MaturityDate.HasValue && MaturityDate.Value < asOfDate;

    /// <summary>
    /// Transitions the account to a new status with lifecycle state machine enforcement.
    /// Business rule: DEP-BR-009 (dormancy management).
    /// Valid transitions:
    ///   Active → Dormant, Frozen, Closed
    ///   Dormant → Active, Frozen, Closed
    ///   Frozen → Active, Dormant, Closed
    ///   Closed → (terminal — no transitions)
    /// </summary>
    public DepositValidationResult TransitionTo(DepositAccountStatus newStatus)
    {
        var validation = DepositValidationService.ValidateStatusTransition(Status, newStatus);
        if (!validation.IsValid)
        {
            return validation;
        }

        Status = newStatus;

        if (newStatus == DepositAccountStatus.Closed)
        {
            ClosedDate = DateTime.UtcNow;
        }

        return DepositValidationResult.Success();
    }

    /// <summary>
    /// Posts accrued interest to the deposit account balance.
    /// COBOL source: Dedicated interest calculation batch (not yet in repository).
    /// Business rule: DEP-BR-004.
    /// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive.
    /// Accrued interest is added to balance and cycle credit, then reset to zero.
    /// </summary>
    public void PostInterest()
    {
        CurrentBalance += AccruedInterest;
        CurrentCycleCredit += AccruedInterest;
        AccruedInterest = 0m;
    }

    /// <summary>
    /// Accrues daily interest (4 decimal intermediate precision).
    /// Business rule: DEP-BR-004.
    /// </summary>
    public void AccrueInterest(decimal dailyInterest) =>
        AccruedInterest += dailyInterest;
}
