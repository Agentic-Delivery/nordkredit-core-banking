namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Persists audit entries for all data migration operations.
/// Required for GDPR Art.17 (right to erasure traceability) and DORA Art.11 (audit trail).
/// </summary>
public interface IMigrationAuditLog
{
    /// <summary>
    /// Logs a batch of audit entries for migrated records.
    /// </summary>
    Task LogBatchAsync(
        IReadOnlyList<MigrationAuditEntry> entries,
        CancellationToken cancellationToken = default);
}
