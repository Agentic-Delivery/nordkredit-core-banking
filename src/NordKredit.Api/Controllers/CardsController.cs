using Microsoft.AspNetCore.Mvc;
using NordKredit.Domain.CardManagement;

namespace NordKredit.Api.Controllers;

/// <summary>
/// REST API for card management operations.
/// Replaces COBOL CICS programs COCRDSLC (detail) with REST endpoints.
/// COBOL source: COCRDSLC.cbl:608-812 (card detail lookup).
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access), FFFS 2014:5 Ch. 8 section 4.
/// </summary>
[ApiController]
[Route("api/cards")]
public class CardsController : ControllerBase
{
    private readonly CardDetailService _detailService;

    public CardsController(CardDetailService detailService)
    {
        _detailService = detailService;
    }

    /// <summary>
    /// Retrieves full detail of a single card by card number.
    /// COBOL: COCRDSLC.cbl:736-774 — 9100-GETCARD-BYACCTCARD (CICS READ on CARDDAT).
    /// CVV code excluded from response per PCI-DSS compliance.
    /// Regulations: PSD2 Art. 97, GDPR Art. 15.
    /// </summary>
    /// <param name="cardNumber">16-digit card number (primary key).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>200 OK with card detail, 400 for invalid input, 404 if not found.</returns>
    [HttpGet("{cardNumber}")]
    public async Task<IActionResult> GetCard(
        string cardNumber,
        CancellationToken cancellationToken)
    {
        // COBOL: COCRDSLC.cbl:685-724 — 2220-EDIT-CARD (card number validation)
        var validation = CardValidationService.ValidateCardNumber(cardNumber, required: true);
        if (!validation.IsValid)
        {
            return BadRequest(new { Message = validation.ErrorMessage });
        }

        var detail = await _detailService.GetByCardNumberAsync(cardNumber, cancellationToken);

        if (detail is null)
        {
            // COBOL: COCRDSLC.cbl:745-748 — DFHRESP(NOTFND) handler
            return NotFound(new { Message = "Did not find cards for this search condition" });
        }

        return Ok(detail);
    }

    /// <summary>
    /// Retrieves cards by account ID.
    /// COBOL: COCRDSLC.cbl:779-812 — 9150-GETCARD-BYACCT (CICS READ on CARDAIX alternate index).
    /// Returns first card for the account, matching COBOL CICS READ behavior.
    /// Regulations: PSD2 Art. 97, GDPR Art. 15.
    /// </summary>
    /// <param name="accountId">11-digit account ID for account-based lookup.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>200 OK with card detail, 400 for invalid input, 404 if not found.</returns>
    [HttpGet]
    public async Task<IActionResult> GetCardByAccount(
        [FromQuery] string? accountId,
        CancellationToken cancellationToken)
    {
        // COBOL: COCRDSLC.cbl:647-683 — 2210-EDIT-ACCOUNT (account number validation)
        var validation = CardValidationService.ValidateAccountNumber(accountId, required: true);
        if (!validation.IsValid)
        {
            return BadRequest(new { Message = validation.ErrorMessage });
        }

        var detail = await _detailService.GetByAccountIdAsync(accountId!, cancellationToken);

        if (detail is null)
        {
            // COBOL: COCRDSLC.cbl:788-791 — DFHRESP(NOTFND) handler
            return NotFound(new { Message = "Did not find cards for this search condition" });
        }

        return Ok(detail);
    }
}
