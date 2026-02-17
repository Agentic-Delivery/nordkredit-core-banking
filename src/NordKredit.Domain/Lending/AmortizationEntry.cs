namespace NordKredit.Domain.Lending;

/// <summary>
/// Single entry in a loan amortization schedule.
/// COBOL source: Dedicated program not yet in repository.
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: Consumer Credit Directive 2008/48/EC Art. 10 (credit agreement information).
///              All amounts use decimal for precision — COBOL PIC S9(10)V99.
/// </summary>
public class AmortizationEntry
{
    /// <summary>Payment period number (1-based).</summary>
    public int PeriodNumber { get; set; }

    /// <summary>Scheduled payment date for this period.</summary>
    public DateTime PaymentDate { get; set; }

    /// <summary>Total payment amount for this period. decimal for COBOL PIC S9(10)V99 precision.</summary>
    public decimal PaymentAmount { get; set; }

    /// <summary>Principal portion of the payment. decimal for COBOL PIC S9(10)V99 precision.</summary>
    public decimal PrincipalPortion { get; set; }

    /// <summary>Interest portion of the payment. decimal for COBOL PIC S9(10)V99 precision.</summary>
    public decimal InterestPortion { get; set; }

    /// <summary>Remaining principal after this payment. decimal for COBOL PIC S9(10)V99 precision.</summary>
    public decimal RemainingPrincipal { get; set; }
}
