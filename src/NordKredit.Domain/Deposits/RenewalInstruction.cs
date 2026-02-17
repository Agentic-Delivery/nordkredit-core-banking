namespace NordKredit.Domain.Deposits;

/// <summary>
/// Term deposit renewal instruction at maturity.
/// Business rule: DEP-BR-005 (term deposit maturity and renewal processing).
/// Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 57.
/// </summary>
public enum RenewalInstruction
{
    /// <summary>Automatically renew at current or updated rate for the same term.</summary>
    AutoRenew,

    /// <summary>Transfer principal and interest to linked demand deposit account.</summary>
    Payout,

    /// <summary>Hold funds and notify customer for manual action.</summary>
    Hold
}
