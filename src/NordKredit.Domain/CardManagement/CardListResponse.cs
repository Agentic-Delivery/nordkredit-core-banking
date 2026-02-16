namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Response for paginated card list.
/// COBOL source: COCRDLIC.cbl:1123-1411 (PROCESS-PAGE-FORWARD/BACKWARD — keyset pagination).
/// Page size = 7 matching COBOL WS-MAX-SCREEN-LINES.
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).
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
/// CVV code excluded per PCI-DSS compliance.
/// COBOL: COCRDLIC.cbl — MOVE CARD-RECORD TO WS-SCREEN-LINE.
/// </summary>
public sealed class CardListItem
{
    public required string CardNumber { get; init; }
    public required string AccountId { get; init; }
    public required string EmbossedName { get; init; }
    public required string ExpirationDate { get; init; }
    public required char ActiveStatus { get; init; }
}
