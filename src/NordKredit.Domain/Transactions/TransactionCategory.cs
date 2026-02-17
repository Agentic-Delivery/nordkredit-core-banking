namespace NordKredit.Domain.Transactions;

/// <summary>
/// Transaction category lookup record.
/// COBOL source: CVTRA04Y.cpy (TRAN-CAT-RECORD), VSAM TRANCATG file.
/// Maps type+category composite key to human-readable descriptions for report enrichment.
/// Regulations: FSA FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility).
/// </summary>
public class TransactionCategory
{
    /// <summary>Transaction type code. COBOL: TRAN-TYPE PIC X(02).</summary>
    public string TypeCode { get; set; } = string.Empty;

    /// <summary>Category code. COBOL: TRAN-CAT-CD PIC 9(04).</summary>
    public int CategoryCode { get; set; }

    /// <summary>Category description. COBOL: TRAN-CAT-DESC PIC X(50).</summary>
    public string Description { get; set; } = string.Empty;
}
