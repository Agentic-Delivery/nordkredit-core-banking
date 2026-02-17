namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for transaction type lookups.
/// COBOL source: CVTRA03Y.cpy (TRAN-TYPE-RECORD), VSAM TRANTYPE file.
/// Used by CBTRN03C for report enrichment — type code to description mapping.
/// </summary>
public interface ITransactionTypeRepository
{
    Task<TransactionType?> GetByCodeAsync(string typeCode, CancellationToken cancellationToken = default);
}
