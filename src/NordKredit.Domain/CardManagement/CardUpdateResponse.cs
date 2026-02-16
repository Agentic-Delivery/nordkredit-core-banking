namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Response DTO for card update operations.
/// Includes card detail and ETag for subsequent concurrency-controlled updates.
/// COBOL source: COCRDUPC.cbl:275-290 — state machine response to update screen.
/// CVV code excluded per PCI-DSS compliance.
/// Regulations: PSD2 Art. 97, GDPR Art. 15, FFFS 2014:5 Ch. 8 §4.
/// </summary>
public sealed class CardUpdateResponse
{
    public required string CardNumber { get; init; }
    public required string AccountId { get; init; }
    public required string EmbossedName { get; init; }
    public required string ExpirationDate { get; init; }
    public required char ActiveStatus { get; init; }
    public required string ETag { get; init; }
}
