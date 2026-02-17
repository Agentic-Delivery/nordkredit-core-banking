namespace NordKredit.Domain.Transactions;

/// <summary>
/// A single detail line in the daily transaction report.
/// COBOL source: CBTRN03C.cbl:361-370 — TRANSACTION-DETAIL-REPORT layout.
/// Enriched with type/category descriptions from lookup tables.
/// Regulations: FSA FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility).
/// </summary>
public class TransactionReportLine
{
    /// <summary>Transaction ID. COBOL: TRAN-ID PIC X(16).</summary>
    public required string TransactionId { get; init; }

    /// <summary>Account ID resolved from CARDXREF. COBOL: XREF-ACCT-ID PIC 9(11).</summary>
    public required string AccountId { get; init; }

    /// <summary>Formatted type: "code-description". E.g., "01-Purchase".</summary>
    public required string TypeDescription { get; init; }

    /// <summary>Formatted category: "code-description". E.g., "0001-Groceries".</summary>
    public required string CategoryDescription { get; init; }

    /// <summary>Transaction source. COBOL: TRAN-SOURCE PIC X(10).</summary>
    public required string Source { get; init; }

    /// <summary>Transaction amount. COBOL: TRAN-AMT PIC S9(09)V99.</summary>
    public required decimal Amount { get; init; }

    /// <summary>Card number for grouping. COBOL: TRAN-CARD-NUM PIC X(16).</summary>
    public required string CardNumber { get; init; }
}
