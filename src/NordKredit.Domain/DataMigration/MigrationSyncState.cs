namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Tracks the sync checkpoint for a single table during incremental migration.
/// Each sync batch creates a new checkpoint version enabling rollback.
/// Regulation: DORA Art.11 — ICT system testing documentation.
/// </summary>
public class MigrationSyncState
{
    /// <summary>Unique identifier for this sync state record.</summary>
    public required string Id { get; init; }

    /// <summary>The table being synced (Azure SQL target table name).</summary>
    public required string TableName { get; init; }

    /// <summary>The last successfully synced source timestamp for this table.</summary>
    public required DateTimeOffset LastSyncTimestamp { get; set; }

    /// <summary>Auto-incrementing version for rollback support.</summary>
    public required int Version { get; set; }

    /// <summary>Number of records synced in this batch.</summary>
    public required int RecordCount { get; set; }

    /// <summary>When this checkpoint was created.</summary>
    public required DateTimeOffset CreatedAt { get; init; }

    /// <summary>Whether this checkpoint has been rolled back.</summary>
    public bool IsRolledBack { get; set; }
}
