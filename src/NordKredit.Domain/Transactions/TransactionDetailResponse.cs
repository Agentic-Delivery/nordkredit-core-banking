namespace NordKredit.Domain.Transactions;

/// <summary>
/// Read-only detail view of a single transaction.
/// COBOL source: COTRN01C.cbl:85-296 (CICS transaction CT01 — transaction detail screen).
/// Card number is masked per PCI-DSS compliance (show only last 4 digits).
/// Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention),
/// GDPR Art.15 (right of access to personal data).
/// </summary>
public sealed class TransactionDetailResponse
{
    public required string TransactionId { get; init; }
    public required string CardNumber { get; init; }
    public required string TypeCode { get; init; }
    public required int CategoryCode { get; init; }
    public required string Source { get; init; }
    public required decimal Amount { get; init; }
    public required string Description { get; init; }
    public required DateTime OriginationTimestamp { get; init; }
    public required DateTime ProcessingTimestamp { get; init; }
    public required int MerchantId { get; init; }
    public required string MerchantName { get; init; }
    public required string MerchantCity { get; init; }
    public required string MerchantZip { get; init; }
}
