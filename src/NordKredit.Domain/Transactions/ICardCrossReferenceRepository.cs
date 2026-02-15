namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for card-to-account cross-reference lookups.
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), VSAM CARDXREF file.
/// Used by CBTRN01C to verify card numbers during daily transaction processing.
/// </summary>
public interface ICardCrossReferenceRepository
{
    Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default);
}
