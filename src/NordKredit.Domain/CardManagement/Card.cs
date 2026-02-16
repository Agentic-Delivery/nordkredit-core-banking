namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Card record entity.
/// COBOL source: CVACT02Y.cpy:1-14 (CARD-RECORD), 150 bytes.
/// Used by card list (COCRDLIC), detail (COCRDSLC), and update (COCRDUPC) programs.
/// Regulations: GDPR Art. 5(1)(c)(d), PSD2 Art. 97.
/// </summary>
public class Card
{
    /// <summary>Card number (primary key). COBOL: CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Account identifier (foreign key). COBOL: CARD-ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>
    /// Card verification value. COBOL: CARD-CVV-CD PIC 9(03).
    /// PCI-DSS review required: evaluate whether CVV storage is appropriate post-migration.
    /// </summary>
    public string CvvCode { get; set; } = string.Empty;

    /// <summary>
    /// Cardholder name as embossed on card. COBOL: CARD-EMBOSSED-NAME PIC X(50).
    /// Supports Swedish characters (Å, Ä, Ö) via nvarchar(50) in Azure SQL.
    /// </summary>
    public string EmbossedName { get; set; } = string.Empty;

    /// <summary>
    /// Expiration date. COBOL: CARD-EXPIRAION-DATE PIC X(10), format YYYY-MM-DD.
    /// Mapped to DateOnly replacing COBOL string representation.
    /// </summary>
    public DateOnly ExpirationDate { get; set; }

    /// <summary>Active status flag: 'Y' = active, 'N' = inactive. COBOL: CARD-ACTIVE-STATUS PIC X(01).</summary>
    public char ActiveStatus { get; set; }

    /// <summary>
    /// Optimistic concurrency token. Replaces COBOL field-by-field comparison with SQL rowversion.
    /// </summary>
    public byte[] RowVersion { get; set; } = [];
}
