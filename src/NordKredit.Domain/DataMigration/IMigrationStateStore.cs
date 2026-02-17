namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Persists and retrieves sync state checkpoints for incremental migration.
/// Supports rollback by tracking versioned checkpoints per table.
/// Regulation: DORA Art.11 — ICT system testing documentation.
/// </summary>
public interface IMigrationStateStore
{
    /// <summary>
    /// Gets the latest non-rolled-back sync state for a table.
    /// Returns null if no sync has occurred for this table.
    /// </summary>
    Task<MigrationSyncState?> GetLatestAsync(
        string tableName,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Saves a new sync state checkpoint.
    /// </summary>
    Task SaveAsync(
        MigrationSyncState state,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Marks a sync state as rolled back and restores the previous version.
    /// Returns the restored state, or null if no previous version exists.
    /// </summary>
    Task<MigrationSyncState?> RollbackAsync(
        string tableName,
        CancellationToken cancellationToken = default);
}
