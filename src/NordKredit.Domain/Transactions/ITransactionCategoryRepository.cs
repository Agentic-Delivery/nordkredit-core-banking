namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for transaction category lookups.
/// COBOL source: CVTRA04Y.cpy (TRAN-CAT-RECORD), VSAM TRANCATG file.
/// Used by CBTRN03C for report enrichment — type+category code to description mapping.
/// </summary>
public interface ITransactionCategoryRepository
{
    Task<TransactionCategory?> GetAsync(string typeCode, int categoryCode, CancellationToken cancellationToken = default);
}
