namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Card-to-customer-to-account cross-reference record.
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), 50 bytes.
/// Links card numbers to customer and account identifiers for AML/KYC compliance.
/// Regulations: GDPR Art. 5(1)(c), AML/KYC.
/// </summary>
public class CardCrossReference
{
    /// <summary>Card number (primary key, foreign key to Card). COBOL: XREF-CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Customer identifier. COBOL: XREF-CUST-ID PIC 9(09).</summary>
    public string CustomerId { get; set; } = string.Empty;

    /// <summary>Account identifier. COBOL: XREF-ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;
}
