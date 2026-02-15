namespace NordKredit.Domain.Transactions;

/// <summary>
/// Daily transaction record — represents unposted transactions from daily batch input.
/// COBOL source: CVTRA06Y.cpy (DALYTRAN-RECORD), 350 bytes.
/// Same layout as Transaction (TRAN-RECORD) but sourced from sequential daily input file.
/// Used by CBTRN01C (verification) and CBTRN02C (posting).
/// </summary>
public class DailyTransaction
{
    /// <summary>Transaction ID. COBOL: DALYTRAN-ID PIC X(16).</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>Transaction type code. COBOL: DALYTRAN-TYPE-CD PIC X(02).</summary>
    public string TypeCode { get; set; } = string.Empty;

    /// <summary>Category code. COBOL: DALYTRAN-CAT-CD PIC 9(04).</summary>
    public int CategoryCode { get; set; }

    /// <summary>Source system. COBOL: DALYTRAN-SOURCE PIC X(10).</summary>
    public string Source { get; set; } = string.Empty;

    /// <summary>Transaction description. COBOL: DALYTRAN-DESC PIC X(100).</summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>Transaction amount. COBOL: DALYTRAN-AMT PIC S9(09)V99. Maps to SQL decimal(11,2).</summary>
    public decimal Amount { get; set; }

    /// <summary>Merchant identifier. COBOL: DALYTRAN-MERCHANT-ID PIC 9(09).</summary>
    public int MerchantId { get; set; }

    /// <summary>Merchant name. COBOL: DALYTRAN-MERCHANT-NAME PIC X(50).</summary>
    public string MerchantName { get; set; } = string.Empty;

    /// <summary>Merchant city. COBOL: DALYTRAN-MERCHANT-CITY PIC X(50).</summary>
    public string MerchantCity { get; set; } = string.Empty;

    /// <summary>Merchant postal code. COBOL: DALYTRAN-MERCHANT-ZIP PIC X(10).</summary>
    public string MerchantZip { get; set; } = string.Empty;

    /// <summary>Card number. COBOL: DALYTRAN-CARD-NUM PIC X(16).</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Origination timestamp. COBOL: DALYTRAN-ORIG-TS PIC X(26).</summary>
    public DateTime OriginationTimestamp { get; set; }

    /// <summary>Processing timestamp. COBOL: DALYTRAN-PROC-TS PIC X(26).</summary>
    public DateTime ProcessingTimestamp { get; set; }
}
