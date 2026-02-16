namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Repository for card cross-reference records.
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), VSAM CARDXREF file.
/// Provides card-to-customer and card-to-account lookups for card management operations.
/// </summary>
public interface ICardCrossReferenceRepository
{
    /// <summary>
    /// Reads a cross-reference by card number.
    /// COBOL: VSAM READ on CARDXREF by primary key.
    /// </summary>
    Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Reads all cross-references for a customer.
    /// Supports multi-card-per-customer lookups for AML/KYC compliance.
    /// </summary>
    Task<IReadOnlyList<CardCrossReference>> GetByCustomerIdAsync(int customerId, CancellationToken cancellationToken = default);
}
