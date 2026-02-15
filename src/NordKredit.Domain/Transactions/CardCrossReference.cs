namespace NordKredit.Domain.Transactions;

/// <summary>
/// Card-to-account cross-reference record.
/// COBOL source: CVACT03Y.cpy (CARD-XREF-RECORD), 50 bytes.
/// Links card numbers to customer and account identifiers.
/// </summary>
public class CardCrossReference
{
    /// <summary>Card number. COBOL: XREF-CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Customer identifier. COBOL: XREF-CUST-ID PIC 9(09).</summary>
    public int CustomerId { get; set; }

    /// <summary>Account identifier. COBOL: XREF-ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;
}
