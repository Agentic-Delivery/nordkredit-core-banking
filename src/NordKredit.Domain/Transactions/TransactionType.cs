namespace NordKredit.Domain.Transactions;

/// <summary>
/// Transaction type lookup record.
/// COBOL source: CVTRA03Y.cpy (TRAN-TYPE-RECORD), VSAM TRANTYPE file.
/// Maps type codes to human-readable descriptions for report enrichment.
/// Regulations: FSA FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility).
/// </summary>
public class TransactionType
{
    /// <summary>Transaction type code. COBOL: TRAN-TYPE PIC X(02).</summary>
    public string TypeCode { get; set; } = string.Empty;

    /// <summary>Transaction type description. COBOL: TRAN-TYPE-DESC PIC X(50).</summary>
    public string Description { get; set; } = string.Empty;
}
