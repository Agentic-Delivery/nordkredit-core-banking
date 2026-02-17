namespace NordKredit.Domain.Transactions;

/// <summary>
/// Result of the daily transaction detail report generation.
/// COBOL source: CBTRN03C.cbl:159-373 — report output summary.
/// Contains the enriched report lines plus aggregate totals for validation.
/// Regulations: FSA FFFS 2014:5 Ch.7 (financial reporting), AML 2017:11 Para.3 (monitoring).
/// </summary>
public class TransactionReportResult
{
    /// <summary>Total number of transactions within the date range.</summary>
    public required int TotalTransactions { get; init; }

    /// <summary>Number of detail lines written to the report.</summary>
    public required int DetailLineCount { get; init; }

    /// <summary>Number of page breaks in the report.</summary>
    public required int PageCount { get; init; }

    /// <summary>Number of distinct card/account groups.</summary>
    public required int AccountGroupCount { get; init; }

    /// <summary>Grand total of all transaction amounts. COBOL: WS-GRAND-TOTAL PIC S9(09)V99.</summary>
    public required decimal GrandTotal { get; init; }

    /// <summary>Enriched report lines for structured output (JSON/CSV).</summary>
    public required IReadOnlyList<TransactionReportLine> Lines { get; init; }

    /// <summary>Page totals in order of occurrence for validation.</summary>
    public required IReadOnlyList<decimal> PageTotals { get; init; }

    /// <summary>Account totals in order of occurrence (keyed by card number) for validation.</summary>
    public required IReadOnlyList<AccountTotal> AccountTotals { get; init; }
}

/// <summary>
/// Account-level total for report validation.
/// </summary>
public class AccountTotal
{
    /// <summary>Card number for this account group.</summary>
    public required string CardNumber { get; init; }

    /// <summary>Account ID resolved from CARDXREF.</summary>
    public required string AccountId { get; init; }

    /// <summary>Sum of transaction amounts for this account group.</summary>
    public required decimal Total { get; init; }
}
