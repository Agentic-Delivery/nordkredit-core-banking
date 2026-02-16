namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Response for paginated card list.
/// COBOL source: COCRDLIC.cbl:1123-1411 (PROCESS-PAGE-FORWARD/BACKWARD — page tracking, 7+1 pattern).
/// Regulations: PSD2 Art. 97 (SCA for card data access), GDPR Art. 15 (right of access).
/// </summary>
public sealed class CardListResponse
{
    public required IReadOnlyList<CardListItem> Cards { get; init; }
    public required bool HasNextPage { get; init; }
    public required bool HasPreviousPage { get; init; }
    public string? FirstCardNumber { get; init; }
    public string? LastCardNumber { get; init; }
    public string? Message { get; init; }
}

/// <summary>
/// Single card in the list response.
/// COBOL: POPULATE-CARD-DATA — displays Card Number, Account ID, Embossed Name, Status, Expiry.
/// CVV code is NEVER included in list response (PCI-DSS).
/// </summary>
public sealed class CardListItem
{
    public required string CardNumber { get; init; }
    public required string AccountId { get; init; }
    public required string EmbossedName { get; init; }
    public required DateOnly ExpirationDate { get; init; }
    public required char ActiveStatus { get; init; }
}
