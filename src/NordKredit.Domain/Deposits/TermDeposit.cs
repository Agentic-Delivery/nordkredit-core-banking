namespace NordKredit.Domain.Deposits;

/// <summary>
/// Term deposit metadata for fixed-term deposit accounts.
/// Extends DepositAccount with maturity and renewal processing information.
/// Business rule: DEP-BR-005 (term deposit maturity and renewal processing).
/// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive, PSD2 Art. 57.
/// </summary>
public class TermDeposit
{
    /// <summary>Account identifier (foreign key to DepositAccount). COBOL: ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>
    /// Term length in months.
    /// Business rule: DEP-BR-005.
    /// </summary>
    public int TermMonths { get; set; }

    /// <summary>
    /// Fixed annual interest rate locked at account opening (as decimal, e.g. 0.035 for 3.5%).
    /// Business rule: DEP-BR-005.
    /// </summary>
    public decimal FixedRate { get; set; }

    /// <summary>
    /// Original principal amount at time of deposit.
    /// COBOL: PIC S9(10)V99. Maps to SQL decimal(12,2).
    /// Business rule: DEP-BR-005.
    /// </summary>
    public decimal PrincipalAmount { get; set; }

    /// <summary>
    /// Renewal instruction at maturity.
    /// Business rule: DEP-BR-005.
    /// </summary>
    public RenewalInstruction RenewalInstruction { get; set; } = RenewalInstruction.Hold;

    /// <summary>
    /// Linked demand deposit account ID for payout on maturity.
    /// Used when RenewalInstruction is Payout.
    /// </summary>
    public string? LinkedAccountId { get; set; }

    /// <summary>
    /// Date the term deposit was created/renewed.
    /// Business rule: DEP-BR-005.
    /// </summary>
    public DateTime StartDate { get; set; }

    /// <summary>
    /// Number of times the term deposit has been renewed.
    /// Business rule: DEP-BR-005.
    /// </summary>
    public int RenewalCount { get; set; }
}
