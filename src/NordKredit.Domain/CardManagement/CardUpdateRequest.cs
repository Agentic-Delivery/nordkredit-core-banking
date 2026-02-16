namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Request DTO for card update (PUT /api/cards/{cardNumber}).
/// COBOL source: COCRDUPC.cbl:806-947 — user-editable fields from the update screen.
/// Expiry day is NOT user-editable and is carried from the original record.
/// Regulations: PSD2 Art. 97, FFFS 2014:5 Ch. 8 §4.
/// </summary>
public sealed class CardUpdateRequest
{
    /// <summary>Cardholder name. COBOL: CCUP-NEW-CRDNAME.</summary>
    public string? EmbossedName { get; init; }

    /// <summary>Active status flag ('Y' or 'N'). COBOL: CCUP-NEW-CRDSTCD.</summary>
    public char ActiveStatus { get; init; }

    /// <summary>Expiry month (1-12). COBOL: CCUP-NEW-EXPMON.</summary>
    public int ExpiryMonth { get; init; }

    /// <summary>Expiry year (1950-2099). COBOL: CCUP-NEW-EXPYEAR.</summary>
    public int ExpiryYear { get; init; }
}
