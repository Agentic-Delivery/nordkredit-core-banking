namespace NordKredit.Domain.Transactions;

/// <summary>
/// Rejected daily transaction record — written to DailyRejects table when validation fails.
/// COBOL source: CBTRN02C.cbl:370-422 — rejected records written with fail code and description.
/// Regulations: PSD2 Art.97 (SCA), FFFS 2014:5 Ch.4 §3 (credit risk),
/// EBA Guidelines (creditworthiness).
/// </summary>
public class DailyReject
{
    /// <summary>Transaction ID of the rejected transaction. COBOL: DALYTRAN-ID PIC X(16).</summary>
    public string TransactionId { get; set; } = string.Empty;

    /// <summary>Card number. COBOL: DALYTRAN-CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Account ID from cross-reference lookup. COBOL: XREF-ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>Rejection code. 100=invalid card, 101=no account, 102=overlimit, 103=expired.</summary>
    public int RejectCode { get; set; }

    /// <summary>Human-readable rejection reason.</summary>
    public string RejectReason { get; set; } = string.Empty;

    /// <summary>Transaction amount at time of rejection. COBOL: DALYTRAN-AMT PIC S9(09)V99.</summary>
    public decimal TransactionAmount { get; set; }

    /// <summary>Timestamp when the rejection was recorded.</summary>
    public DateTime RejectedAt { get; set; }
}
