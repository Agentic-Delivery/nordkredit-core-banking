namespace NordKredit.Domain.Transactions;

/// <summary>
/// Lists posted transactions with keyset pagination.
/// COBOL source: COTRN00C.cbl:94-328 (CICS transaction CT00 — paginated transaction list).
/// Regulations: FFFS 2014:5 Ch.8 (operational info systems), PSD2 Art.94 (transaction history access).
/// </summary>
public class TransactionListService
{
    /// <summary>
    /// Page size matching COBOL hardcoded 10-record display.
    /// COBOL: COTRN00C.cbl:297 — PERFORM UNTIL WS-IDX >= 11.
    /// </summary>
    internal const int PageSize = 10;

    private readonly ITransactionRepository _transactionRepository;

    public TransactionListService(ITransactionRepository transactionRepository)
    {
        _transactionRepository = transactionRepository;
    }

    /// <summary>
    /// Retrieves a page of transactions using keyset pagination.
    /// Reads PageSize + 1 records to detect next page (COBOL 10+1 pattern).
    /// COBOL: PROCESS-PAGE-FORWARD, COTRN00C.cbl:279-328.
    /// </summary>
    /// <param name="cursor">Transaction ID to start after. Null for first page.</param>
    /// <param name="fromTransactionId">Filter: start from this Transaction ID (resets pagination).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>Paginated transaction list response.</returns>
    public async Task<TransactionListResponse> GetTransactionsAsync(
        string? cursor = null,
        string? fromTransactionId = null,
        CancellationToken cancellationToken = default)
    {
        // If fromTransactionId is provided, use it as the starting position (resets cursor)
        // COBOL: COTRN00C.cbl:206-213 — MOVE TRNIDINI to TRAN-ID
        var effectiveCursor = fromTransactionId ?? cursor;

        // Read PageSize + 1 to detect if next page exists (COBOL 10+1 pattern)
        // COBOL: COTRN00C.cbl:305-313 — read 11th record for next-page indicator
        var records = await _transactionRepository.GetPageAsync(
            PageSize + 1,
            effectiveCursor,
            cancellationToken);

        var hasNextPage = records.Count > PageSize;
        var pageRecords = hasNextPage ? records.Take(PageSize) : records;

        var items = pageRecords.Select(t => new TransactionListItem
        {
            TransactionId = t.Id,
            Date = t.OriginationTimestamp,
            Description = t.Description,
            Amount = t.Amount
        }).ToList();

        string? nextCursor = hasNextPage && items.Count > 0
            ? items[^1].TransactionId
            : null;

        return new TransactionListResponse
        {
            Transactions = items,
            HasNextPage = hasNextPage,
            NextCursor = nextCursor
        };
    }
}
