namespace NordKredit.Domain.Transactions;

/// <summary>
/// Input for adding a new transaction via the online screen.
/// COBOL source: COTRN02C.cbl — maps to COTRN2AI screen input fields.
/// </summary>
public class TransactionAddRequest
{
    /// <summary>Account identifier. COBOL: ACTIDINI OF COTRN2AI.</summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>Card number. COBOL: CARDNINI OF COTRN2AI.</summary>
    public string CardNumber { get; set; } = string.Empty;

    /// <summary>Transaction type code. COBOL: TTYPCDI OF COTRN2AI.</summary>
    public string TypeCode { get; set; } = string.Empty;

    /// <summary>Category code. COBOL: TCATCDI OF COTRN2AI.</summary>
    public string CategoryCode { get; set; } = string.Empty;

    /// <summary>Transaction source. COBOL: TRNSRCI OF COTRN2AI.</summary>
    public string Source { get; set; } = string.Empty;

    /// <summary>Transaction description. COBOL: TDESCI OF COTRN2AI.</summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>Amount in format ±99999999.99. COBOL: TRNAMTI OF COTRN2AI.</summary>
    public string Amount { get; set; } = string.Empty;

    /// <summary>Origination date in YYYY-MM-DD format. COBOL: TORIGDTI OF COTRN2AI.</summary>
    public string OriginationDate { get; set; } = string.Empty;

    /// <summary>Processing date in YYYY-MM-DD format. COBOL: TPROCDTI OF COTRN2AI.</summary>
    public string ProcessingDate { get; set; } = string.Empty;

    /// <summary>Merchant identifier. COBOL: MCHNTIDI OF COTRN2AI.</summary>
    public string MerchantId { get; set; } = string.Empty;

    /// <summary>Merchant name. COBOL: MCHNTNMI OF COTRN2AI.</summary>
    public string MerchantName { get; set; } = string.Empty;

    /// <summary>Merchant city. COBOL: MCHNTCTI OF COTRN2AI.</summary>
    public string MerchantCity { get; set; } = string.Empty;

    /// <summary>Merchant zip code. COBOL: MCHNTZPI OF COTRN2AI.</summary>
    public string MerchantZip { get; set; } = string.Empty;

    /// <summary>Confirmation flag. COBOL: CONFIRMI OF COTRN2AI. 'Y' to confirm.</summary>
    public string Confirm { get; set; } = string.Empty;
}
