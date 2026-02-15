namespace NordKredit.Domain.Transactions;

/// <summary>
/// Repository for transaction category balance records.
/// COBOL source: VSAM TRAN-CAT-BAL file, keyed by AccountId + TypeCode + CategoryCode.
/// Updated by CBTRN02C during daily posting.
/// </summary>
public interface ITransactionCategoryBalanceRepository
{
    Task<TransactionCategoryBalance?> GetAsync(
        string accountId,
        string typeCode,
        int categoryCode,
        CancellationToken cancellationToken = default);

    Task<IReadOnlyList<TransactionCategoryBalance>> GetByAccountIdAsync(
        string accountId,
        CancellationToken cancellationToken = default);

    Task UpsertAsync(TransactionCategoryBalance balance, CancellationToken cancellationToken = default);
}
