using Microsoft.AspNetCore.Mvc;
using NordKredit.Domain.CardManagement;

namespace NordKredit.Api.Controllers;

/// <summary>
/// REST API for card management operations.
/// Replaces COBOL CICS program COCRDLIC (card list with pagination/filtering).
/// CARD-BR-002 routing: CICS action code 'S' → GET /api/cards/{cardNumber},
/// 'U' → PUT /api/cards/{cardNumber} (handled by respective endpoints).
/// Regulations: PSD2 Art. 97, GDPR Art. 15, FFFS 2014:5 Ch. 8 §4.
/// </summary>
[ApiController]
[Route("api/cards")]
public class CardsController : ControllerBase
{
    private readonly CardListService _listService;

    public CardsController(CardListService listService)
    {
        _listService = listService;
    }

    /// <summary>
    /// Lists cards with keyset pagination and optional filtering.
    /// COBOL: COCRDLIC.cbl:1123-1411 (CICS transaction — paginated card list).
    /// Business rules: CARD-BR-001 (pagination/filtering), CARD-BR-004/005 (input validation).
    /// Regulations: PSD2 Art. 97 (SCA for card data access), GDPR Art. 15 (right of access).
    /// </summary>
    /// <param name="afterCardNumber">Keyset cursor — Card number to start after (forward). Null for first page.</param>
    /// <param name="beforeCardNumber">Keyset cursor — Card number to start before (backward).</param>
    /// <param name="accountId">Optional account ID filter (11 digits). COBOL: COCRDLIC.cbl:1003-1034.</param>
    /// <param name="cardNumber">Optional card number filter (16 digits). COBOL: COCRDSLC.cbl:685-724.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>200 OK with paginated cards, 400 for invalid filters.</returns>
    [HttpGet]
    public async Task<IActionResult> GetCards(
        [FromQuery] string? afterCardNumber,
        [FromQuery] string? beforeCardNumber,
        [FromQuery] string? accountId,
        [FromQuery] string? cardNumber,
        CancellationToken cancellationToken)
    {
        // Validate account ID filter — CARD-BR-004 (COCRDLIC.cbl:1003-1034)
        var accountValidation = CardValidationService.ValidateAccountNumber(accountId, required: false);
        if (!accountValidation.IsValid)
        {
            return BadRequest(new { Message = accountValidation.ErrorMessage });
        }

        // Validate card number filter — CARD-BR-005 (COCRDSLC.cbl:685-724)
        var cardValidation = CardValidationService.ValidateCardNumber(cardNumber, required: false);
        if (!cardValidation.IsValid)
        {
            return BadRequest(new { Message = cardValidation.ErrorMessage });
        }

        var result = await _listService.GetCardsAsync(
            afterCardNumber, beforeCardNumber, accountId, cardNumber, cancellationToken);

        return Ok(result);
    }
}
