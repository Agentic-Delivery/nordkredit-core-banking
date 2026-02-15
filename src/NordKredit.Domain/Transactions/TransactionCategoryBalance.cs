namespace NordKredit.Domain.Transactions;

/// <summary>
/// Transaction category balance record — tracks balance per account/type/category combination.
/// COBOL source: CVTRA01Y.cpy (TRAN-CAT-BAL-RECORD), 50 bytes.
/// Composite key: AccountId + TypeCode + CategoryCode (TRAN-CAT-KEY).
/// </summary>
public class TransactionCategoryBalance
{
    /// <summary>Account identifier. COBOL: TRANCAT-ACCT-ID PIC 9(11).</summary>
    public string AccountId { get; set; } = string.Empty;

    /// <summary>Transaction type code. COBOL: TRANCAT-TYPE-CD PIC X(02).</summary>
    public string TypeCode { get; set; } = string.Empty;

    /// <summary>Category code. COBOL: TRANCAT-CD PIC 9(04).</summary>
    public int CategoryCode { get; set; }

    /// <summary>Category balance. COBOL: TRAN-CAT-BAL PIC S9(09)V99. Maps to SQL decimal(11,2).</summary>
    public decimal Balance { get; set; }
}
