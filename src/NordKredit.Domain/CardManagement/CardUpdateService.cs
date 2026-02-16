namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Orchestrates the card update workflow: validate → detect changes → commit.
/// Replaces COBOL CICS program COCRDUPC state machine with a stateless REST pattern.
/// COBOL source: COCRDUPC.cbl:275-290 (state machine), 429-543 (main EVALUATE),
///               948-1027 (action decisions), 1420-1523 (write processing + concurrency check).
/// Business rules: CARD-BR-007 (state machine), CARD-BR-008 (optimistic concurrency).
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 §4 (operational risk management).
/// </summary>
public class CardUpdateService
{
    private readonly ICardRepository _cardRepository;

    public CardUpdateService(ICardRepository cardRepository)
    {
        _cardRepository = cardRepository;
    }

    /// <summary>
    /// Executes the card update workflow.
    /// Maps COBOL state machine to: validate fields → fetch card → detect changes → apply and save.
    /// Optimistic concurrency uses SQL rowversion (replaces COBOL field-by-field comparison).
    /// </summary>
    /// <param name="cardNumber">16-digit card number (primary key).</param>
    /// <param name="request">Update request with new field values.</param>
    /// <param name="rowVersion">Concurrency token from ETag (Base64-encoded rowversion).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>Result indicating success, no-change, conflict, validation failure, or write failure.</returns>
    public async Task<CardUpdateResult> UpdateCardAsync(
        string cardNumber,
        CardUpdateRequest request,
        byte[] rowVersion,
        CancellationToken cancellationToken = default)
    {
        // COBOL: COCRDUPC.cbl:806-947 — 1200-EDIT-MAP-INPUTS (field validation)
        var validationResult = CardUpdateValidator.ValidateUpdate(
            request.EmbossedName,
            request.ActiveStatus,
            request.ExpiryMonth,
            request.ExpiryYear);

        if (!validationResult.IsValid)
        {
            // COBOL: CHANGES-NOT-OK state — return all validation errors
            return CardUpdateResult.ValidationFailure(validationResult.Errors);
        }

        // COBOL: 9200-WRITE-PROCESSING — READ card with UPDATE lock
        var card = await _cardRepository.GetByCardNumberAsync(cardNumber, cancellationToken);
        if (card is null)
        {
            return CardUpdateResult.NotFound();
        }

        // COBOL: COCRDUPC.cbl 1200-EDIT-MAP-INPUTS — change detection (case-insensitive)
        var changeResult = CardUpdateValidator.DetectChanges(
            card,
            request.EmbossedName!,
            request.ActiveStatus,
            request.ExpiryMonth,
            request.ExpiryYear);

        if (!changeResult.HasChanges)
        {
            // COBOL: SHOW-DETAILS state with "No change detected" message
            return CardUpdateResult.NoChange(changeResult.Message!, card);
        }

        // Apply new values to card entity
        // COBOL: COCRDUPC.cbl:1461-1475 — prepare update record
        card.EmbossedName = request.EmbossedName!;
        card.ActiveStatus = request.ActiveStatus;
        // Expiry day carried from original record (not user-editable).
        // Clamp day to the last valid day of the new month/year to avoid invalid dates
        // (e.g., original day 31 with new month June → clamp to 30).
        var maxDay = DateTime.DaysInMonth(request.ExpiryYear, request.ExpiryMonth);
        var day = Math.Min(card.ExpirationDate.Day, maxDay);
        card.ExpirationDate = new DateOnly(request.ExpiryYear, request.ExpiryMonth, day);
        // Set the rowversion for optimistic concurrency check
        card.RowVersion = rowVersion;

        try
        {
            // COBOL: COCRDUPC.cbl:1477-1483 — REWRITE CARD-RECORD
            await _cardRepository.UpdateAsync(card, cancellationToken);
        }
        catch (ConcurrencyConflictException)
        {
            // COBOL: DATA-WAS-CHANGED-BEFORE-UPDATE — re-read and return refreshed data
            var refreshedCard = await _cardRepository.GetByCardNumberAsync(cardNumber, cancellationToken);
            return CardUpdateResult.Conflict(
                "Record changed by some one else. Please review",
                refreshedCard!);
        }
        catch (Exception ex) when (ex is not ConcurrencyConflictException
                                    and not OperationCanceledException)
        {
            // COBOL: LOCKED-BUT-UPDATE-FAILED — write failed after lock acquisition
            return CardUpdateResult.WriteFailure("Update of record failed");
        }

        // COBOL: CHANGES-OKAYED-AND-DONE — success
        return CardUpdateResult.Success(card);
    }
}
