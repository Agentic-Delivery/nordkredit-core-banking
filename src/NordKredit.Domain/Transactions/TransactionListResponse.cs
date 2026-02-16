namespace NordKredit.Domain.Transactions;

/// <summary>
/// Response for paginated transaction list.
/// COBOL source: COTRN00C.cbl:279-328 (PROCESS-PAGE-FORWARD — page tracking, 10+1 pattern).
/// Regulations: FFFS 2014:5 Ch.8 (operational info systems), PSD2 Art.94 (transaction history access).
/// </summary>
public sealed class TransactionListResponse
{
    public required IReadOnlyList<TransactionListItem> Transactions { get; init; }
    public required bool HasNextPage { get; init; }
    public string? NextCursor { get; init; }
    public string? Message { get; init; }
}

/// <summary>
/// Single transaction in the list response.
/// COBOL: POPULATE-TRAN-DATA — displays Transaction ID, Date, Description, Amount.
/// </summary>
public sealed class TransactionListItem
{
    public required string TransactionId { get; init; }
    public required DateTime Date { get; init; }
    public required string Description { get; init; }
    public required decimal Amount { get; init; }
}
