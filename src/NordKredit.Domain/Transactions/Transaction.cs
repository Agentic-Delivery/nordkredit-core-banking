namespace NordKredit.Domain.Transactions;

/// <summary>
/// Posted transaction record.
/// COBOL source: CVTRA05Y.cpy (TRAN-RECORD), 350 bytes.
/// All monetary fields use decimal (never float/double) per COBOL PIC S9(09)V99.
/// </summary>
public class Transaction
{
    /// <summary>Transaction ID. COBOL: TRAN-ID PIC X(16).</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>Transaction type code (e.g., "DB", "CR"). COBOL: TRAN-TYPE-CD PIC X(02).</summary>
    public string TypeCode { get; set; } = string.Empty;

    /// <summary>Category code (e.g., 1001-9999). COBOL: TRAN-CAT-CD PIC 9(04).</summary>
    public int CategoryCode { get; set; }

    /// <summary>Source system (e.g., "ONLINE", "BATCH"). COBOL: TRAN-SOURCE PIC X(10).</summary>
    public string Source { get; set; } = string.Empty;

    /// <summary>Transaction description. COBOL: TRAN-DESC PIC X(100).</summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>Transaction amount. COBOL: TRAN-AMT PIC S9(09)V99. Maps to SQL decimal(11,2).</summary>
    public decimal Amount { get; set; }

    /// <summary>Merchant identifier. COBOL: TRAN-MERCHANT-ID PIC 9(09).</summary>
    public int MerchantId { get; set; }

    /// <summary>Merchant name. COBOL: TRAN-MERCHANT-NAME PIC X(50).</summary>
    public string MerchantName { get; set; } = string.Empty;

    /// <summary>Merchant city. COBOL: TRAN-MERCHANT-CITY PIC X(50).</summary>
    public string MerchantCity { get; set; } = string.Empty;

    /// <summary>Merchant postal code. COBOL: TRAN-MERCHANT-ZIP PIC X(10).</summary>
    public string MerchantZip { get; set; } = string.Empty;

    /// <summary>Card number. COBOL: TRAN-CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Origination timestamp. COBOL: TRAN-ORIG-TS PIC X(26).</summary>
    public DateTime OriginationTimestamp { get; set; }

    /// <summary>Processing timestamp. COBOL: TRAN-PROC-TS PIC X(26).</summary>
    public DateTime ProcessingTimestamp { get; set; }
}
