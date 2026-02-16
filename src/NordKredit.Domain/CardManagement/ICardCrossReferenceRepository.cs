namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Repository for card cross-reference lookups.
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), VSAM CARDXREF file.
/// Provides card-to-customer and customer-to-cards lookups for AML/KYC compliance.
/// Regulations: GDPR Art. 5(1)(c), AML/KYC.
/// </summary>
public interface ICardCrossReferenceRepository
{
    /// <summary>
    /// Retrieves a cross-reference by card number (primary key lookup).
    /// COBOL: VSAM CARDXREF keyed read by XREF-CARD-NUM.
    /// </summary>
    Task<CardCrossReference?> GetByCardNumberAsync(string cardNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all cross-references for a customer.
    /// Supports multi-card-per-customer and multi-account-per-customer lookups.
    /// </summary>
    Task<IReadOnlyList<CardCrossReference>> GetByCustomerIdAsync(string customerId, CancellationToken cancellationToken = default);
}
