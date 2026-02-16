namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Card record entity.
/// COBOL source: CVACT02Y.cpy (CARD-RECORD), 150 bytes.
/// Primary key: CARD-NUM (16-digit card number). Alternate index: CARD-ACCT-ID (AccountId).
/// Regulations: GDPR Art. 5(1)(c)(d), PSD2 Art. 97.
/// PCI-DSS review required: CVV code is stored — evaluate whether storage is appropriate under current PCI DSS standards.
/// </summary>
public class Card
{
    /// <summary>Card number (primary key). COBOL: CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Account identifier (foreign key). COBOL: CARD-ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>
    /// Card verification value. COBOL: CARD-CVV-CD PIC 9(03).
    /// PCI-DSS review: CVV storage must be evaluated under current PCI DSS standards.
    /// </summary>
    public string CvvCode { get; set; } = string.Empty;

    /// <summary>
    /// Cardholder name as embossed on card. COBOL: CARD-EMBOSSED-NAME PIC X(50).
    /// Stored uppercase. Supports Swedish characters (Å, Ä, Ö) via nvarchar(50).
    /// </summary>
    public string EmbossedName { get; set; } = string.Empty;

    /// <summary>
    /// Card expiration date. COBOL: CARD-EXPIRAION-DATE PIC X(10) format YYYY-MM-DD.
    /// Migrated to DateOnly for proper date semantics.
    /// </summary>
    public DateOnly ExpirationDate { get; set; }

    /// <summary>Active status flag ('Y' = active, 'N' = inactive). COBOL: CARD-ACTIVE-STATUS PIC X(01).</summary>
    public char ActiveStatus { get; set; }

    /// <summary>
    /// Concurrency token for optimistic concurrency control.
    /// Replaces COBOL field-by-field comparison with SQL Server rowversion.
    /// </summary>
    public byte[]? RowVersion { get; set; }
}
