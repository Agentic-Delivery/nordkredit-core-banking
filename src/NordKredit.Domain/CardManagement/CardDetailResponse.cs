namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Read-only detail view of a single card.
/// COBOL source: COCRDSLC.cbl:608-812 (CICS program — card detail screen).
/// CVV code is excluded from response per PCI-DSS compliance.
/// Regulations: PSD2 Art. 97 (SCA for payment instrument access), GDPR Art. 15 (right of access).
/// </summary>
public sealed class CardDetailResponse
{
    public required string CardNumber { get; init; }
    public required string AccountId { get; init; }
    public required string EmbossedName { get; init; }
    public required string ExpirationDate { get; init; }
    public required char ActiveStatus { get; init; }
}
