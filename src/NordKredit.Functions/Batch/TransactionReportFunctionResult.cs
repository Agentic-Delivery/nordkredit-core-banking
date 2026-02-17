namespace NordKredit.Functions.Batch;

/// <summary>
/// Result of the daily batch transaction report function.
/// COBOL source: CBTRN03C.cbl:159-373 — output summary of the report run.
/// Contains aggregate counts for monitoring and alerting.
/// Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility),
/// AML 2017:11 Para.3 (monitoring).
/// </summary>
public class TransactionReportFunctionResult
{
    /// <summary>Total number of transactions within the date range.</summary>
    public required int TotalTransactions { get; init; }

    /// <summary>Number of detail lines in the report.</summary>
    public required int DetailLineCount { get; init; }

    /// <summary>Number of pages in the report.</summary>
    public required int PageCount { get; init; }

    /// <summary>Number of distinct card/account groups.</summary>
    public required int AccountGroupCount { get; init; }

    /// <summary>Grand total of all transaction amounts. COBOL: WS-GRAND-TOTAL PIC S9(09)V99.</summary>
    public required decimal GrandTotal { get; init; }
}
