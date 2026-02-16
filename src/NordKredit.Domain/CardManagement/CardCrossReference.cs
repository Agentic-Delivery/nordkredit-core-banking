namespace NordKredit.Domain.CardManagement;

/// <summary>
/// Card-to-customer-to-account cross-reference record.
/// COBOL source: CVACT03Y.cpy:1-11 (CARD-XREF-RECORD), 50 bytes.
/// Establishes the three-way relationship between cards, customers, and accounts.
/// Regulations: GDPR Art. 5(1)(c) (data minimization), AML/KYC (card-to-customer linkage).
/// </summary>
public class CardCrossReference
{
    /// <summary>Card number (primary key, foreign key to Card). COBOL: XREF-CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Customer identifier. COBOL: XREF-CUST-ID PIC 9(09).</summary>
    public int CustomerId { get; set; }

    /// <summary>Account identifier. COBOL: XREF-ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;
}
