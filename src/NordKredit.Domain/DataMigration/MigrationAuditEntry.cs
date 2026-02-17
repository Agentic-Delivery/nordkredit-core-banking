namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Audit log entry for a single record migration, tracking the full lifecycle
/// from source read through conversion to target write.
/// Regulation: GDPR Art.17 (right to erasure traceability), DORA Art.11 (audit trail).
/// </summary>
public class MigrationAuditEntry
{
    /// <summary>Unique identifier for this audit entry.</summary>
    public required string Id { get; init; }

    /// <summary>Correlation ID for the batch this record belongs to.</summary>
    public required string BatchCorrelationId { get; init; }

    /// <summary>Source table name.</summary>
    public required string SourceTable { get; init; }

    /// <summary>Target table name.</summary>
    public required string TargetTable { get; init; }

    /// <summary>Primary key of the record.</summary>
    public required string PrimaryKey { get; init; }

    /// <summary>Type of change applied.</summary>
    public required ChangeType ChangeType { get; init; }

    /// <summary>Whether the migration succeeded.</summary>
    public required bool Success { get; init; }

    /// <summary>Error message if the migration failed.</summary>
    public string? ErrorMessage { get; init; }

    /// <summary>Data residency region (must be EU/Sweden Central).</summary>
    public required string DataRegion { get; init; }

    /// <summary>When this audit entry was created.</summary>
    public required DateTimeOffset CreatedAt { get; init; }
}
