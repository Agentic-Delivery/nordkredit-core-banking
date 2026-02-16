using Microsoft.AspNetCore.Mvc;
using NordKredit.Domain.CardManagement;

namespace NordKredit.Api.Controllers;

/// <summary>
/// REST API for card management operations.
/// Replaces COBOL CICS programs COCRDLIC (list) and COCRDSLC (detail) with REST endpoints.
/// COBOL source: COCRDLIC.cbl:1123-1411 (pagination/filtering), COCRDSLC.cbl:608-812 (detail).
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access), FFFS 2014:5 Ch. 8 §4.
/// </summary>
[ApiController]
[Route("api/cards")]
public class CardsController : ControllerBase
{
    private readonly CardDetailService _detailService;
    private readonly CardListService _listService;

    public CardsController(CardDetailService detailService, CardListService listService)
    {
        _detailService = detailService;
        _listService = listService;
    }

    /// <summary>
    /// Lists cards with keyset pagination and optional filtering.
    /// COBOL: COCRDLIC.cbl:1123-1411 — 9000-READ-FORWARD / 9100-READ-BACKWARD / 9500-FILTER-RECORDS.
    /// Page size = 7 matching COBOL WS-MAX-SCREEN-LINES.
    /// CARD-BR-001: Pagination and filtering. CARD-BR-002: Selection routing ('S' → GET detail, 'U' → PUT update).
    /// Regulations: PSD2 Art. 97, GDPR Art. 15, FFFS 2014:5 Ch. 8 §4.
    /// </summary>
    /// <param name="afterCardNumber">Forward cursor — card number to start after. Null for first page.</param>
    /// <param name="beforeCardNumber">Backward cursor — card number to start before.</param>
    /// <param name="accountId">Optional account ID filter (11 digits). COBOL: 9500-FILTER-RECORDS.</param>
    /// <param name="cardNumber">Optional card number filter (16 digits). COBOL: 9500-FILTER-RECORDS.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>200 OK with paginated card list, 400 for invalid filters.</returns>
    [HttpGet]
    public async Task<IActionResult> ListCards(
        [FromQuery] string? afterCardNumber,
        [FromQuery] string? beforeCardNumber,
        [FromQuery] string? accountId,
        [FromQuery] string? cardNumber,
        CancellationToken cancellationToken)
    {
        // COBOL: COCRDLIC.cbl:1003-1034 — validate account filter (optional)
        var accountValidation = CardValidationService.ValidateAccountNumber(accountId, required: false);
        if (!accountValidation.IsValid)
        {
            return BadRequest(new { Message = accountValidation.ErrorMessage });
        }

        // COBOL: COCRDLIC.cbl — validate card number filter (optional)
        var cardValidation = CardValidationService.ValidateCardNumber(cardNumber, required: false);
        if (!cardValidation.IsValid)
        {
            return BadRequest(new { Message = cardValidation.ErrorMessage });
        }

        // Backward pagination: COBOL 9100-READ-BACKWARD
        if (!string.IsNullOrEmpty(beforeCardNumber))
        {
            var result = await _listService.GetCardsBackwardAsync(
                beforeCardNumber, accountId, cardNumber, cancellationToken);
            return Ok(result);
        }

        // Forward pagination (default): COBOL 9000-READ-FORWARD
        var forwardResult = await _listService.GetCardsForwardAsync(
            afterCardNumber, accountId, cardNumber, cancellationToken);
        return Ok(forwardResult);
    }

    /// <summary>
    /// Retrieves full detail of a single card by card number.
    /// COBOL: COCRDSLC.cbl:736-774 — 9100-GETCARD-BYACCTCARD (CICS READ on CARDDAT).
    /// CARD-BR-002: Maps 'S' selection action to this endpoint.
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
}
