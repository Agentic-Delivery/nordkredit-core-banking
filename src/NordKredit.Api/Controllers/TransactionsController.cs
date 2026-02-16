using Microsoft.AspNetCore.Mvc;
using NordKredit.Domain.Transactions;

namespace NordKredit.Api.Controllers;

/// <summary>
/// REST API for transaction operations.
/// Replaces COBOL CICS online transaction screens COTRN00C (list) and COTRN02C (add).
/// Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention).
/// </summary>
[ApiController]
[Route("api/transactions")]
public class TransactionsController : ControllerBase
{
    private readonly TransactionAddService _addService;
    private readonly TransactionListService _listService;

    public TransactionsController(TransactionAddService addService, TransactionListService listService)
    {
        _addService = addService;
        _listService = listService;
    }

    /// <summary>
    /// Lists posted transactions with keyset pagination.
    /// COBOL: COTRN00C.cbl:94-328 (CICS transaction CT00 — paginated transaction list).
    /// Regulations: FFFS 2014:5 Ch.8 (operational info systems), PSD2 Art.94 (transaction history access).
    /// </summary>
    /// <param name="cursor">Keyset cursor — Transaction ID to start after. Null for first page.</param>
    /// <param name="fromTransactionId">Filter: start from this Transaction ID (resets pagination). Must be numeric.</param>
    /// <param name="direction">Navigation direction. "backward" at page 1 returns message.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>200 OK with paginated transactions, 400 for non-numeric fromTransactionId.</returns>
    [HttpGet]
    public async Task<IActionResult> GetTransactions(
        [FromQuery] string? cursor,
        [FromQuery] string? fromTransactionId,
        [FromQuery] string? direction,
        CancellationToken cancellationToken)
    {
        // Validate fromTransactionId is numeric (COBOL: COTRN00C.cbl:209-213 — IS NUMERIC check)
        if (!string.IsNullOrEmpty(fromTransactionId) && !fromTransactionId.All(char.IsAsciiDigit))
        {
            return BadRequest(new { Message = "Transaction ID must be numeric" });
        }

        // Handle backward direction with no cursor (PF7 at page 1)
        // COBOL: COTRN00C.cbl — "You are already at the top of the page..."
        if (string.Equals(direction, "backward", StringComparison.OrdinalIgnoreCase)
            && string.IsNullOrEmpty(cursor))
        {
            var firstPage = await _listService.GetTransactionsAsync(
                cancellationToken: cancellationToken);

            return Ok(new TransactionListResponse
            {
                Transactions = firstPage.Transactions,
                HasNextPage = firstPage.HasNextPage,
                NextCursor = firstPage.NextCursor,
                Message = "Already at the top of the page"
            });
        }

        var result = await _listService.GetTransactionsAsync(
            cursor, fromTransactionId, cancellationToken);

        return Ok(result);
    }

    /// <summary>
    /// Creates a new transaction.
    /// COBOL: COTRN02C.cbl PROCESS-ENTER-KEY → ADD-TRANSACTION → WRITE-TRANSACT-FILE.
    /// </summary>
    /// <param name="request">Transaction add request with all required fields.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>201 Created with transaction ID, 400 for validation errors, 409 for duplicate key.</returns>
    [HttpPost]
    public async Task<IActionResult> AddTransaction(
        [FromBody] TransactionAddRequest request,
        CancellationToken cancellationToken)
    {
        var result = await _addService.AddTransactionAsync(request, cancellationToken);

        if (result.IsSuccess)
        {
            return Created($"/api/transactions/{result.TransactionId}",
                new { result.TransactionId, result.Message });
        }

        if (result.ConfirmationRequired)
        {
            return BadRequest(new { result.ConfirmationRequired, result.Message });
        }

        if (result.Message.Contains("already exist", StringComparison.OrdinalIgnoreCase))
        {
            return Conflict(new { result.Message });
        }

        return BadRequest(new { result.Message });
    }

    /// <summary>
    /// Retrieves the last transaction for copy-last-transaction convenience.
    /// COBOL: COPY-LAST-TRAN-DATA (PF5 handler), COTRN02C.cbl.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>200 OK with the last transaction, or 404 if no transactions exist.</returns>
    [HttpGet("last")]
    public async Task<IActionResult> GetLastTransaction(CancellationToken cancellationToken)
    {
        var transaction = await _addService.GetLastTransactionAsync(cancellationToken);

        if (transaction is null)
        {
            return NotFound(new { Message = "No transactions found." });
        }

        return Ok(transaction);
    }
}
