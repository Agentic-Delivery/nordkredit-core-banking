namespace NordKredit.Domain.Deposits;

/// <summary>
/// Deposit product type classification.
/// COBOL source: CVACT01Y.cpy — ACCT-GROUP-ID (disclosure/interest group).
/// Business rule: DEP-BR-006 (savings product configuration).
/// Regulations: FSA FFFS 2014:5 Ch. 3, Deposit Guarantee Directive.
/// </summary>
public enum DepositProductType
{
    /// <summary>Demand savings account (sparkonto) — variable rate, unlimited withdrawals. Code: DD.</summary>
    DemandSavings,

    /// <summary>Term deposit (bundet konto) — fixed rate, no withdrawal until maturity. Code: TD.</summary>
    TermDeposit,

    /// <summary>Children's savings account (barnsparkonto) — higher rate, restricted withdrawals. Code: CS.</summary>
    ChildrensSavings,

    /// <summary>Business deposit account (företagskonto) — variable rate, per-agreement terms. Code: BD.</summary>
    BusinessDeposit
}
