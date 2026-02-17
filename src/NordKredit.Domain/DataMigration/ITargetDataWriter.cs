namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Writes converted records to the Azure SQL target database.
/// Supports batch writes with transactional semantics for rollback.
/// Regulation: GDPR Art.17 — right to erasure must be preservable after migration.
/// </summary>
public interface ITargetDataWriter
{
    /// <summary>
    /// Writes a batch of converted records to the target table within a transaction.
    /// Returns the number of records successfully written.
    /// </summary>
    Task<int> WriteBatchAsync(
        IReadOnlyList<ConvertedRecord> records,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Rolls back records written in a specific batch (identified by primary keys and table).
    /// Used when a sync batch fails validation.
    /// </summary>
    Task RollbackBatchAsync(
        string tableName,
        IReadOnlyList<string> primaryKeys,
        CancellationToken cancellationToken = default);
}
