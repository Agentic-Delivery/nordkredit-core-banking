namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Configuration for the data migration pipeline.
/// Controls batch sizes, data residency, and enabled tables.
/// </summary>
public class MigrationPipelineConfiguration
{
    /// <summary>Maximum number of records per sync batch. Default: 1000.</summary>
    public int BatchSize { get; init; } = 1000;

    /// <summary>
    /// Data residency region. Must be EU/Sweden Central for GDPR compliance.
    /// Used in audit log entries.
    /// </summary>
    public string DataRegion { get; init; } = "Sweden Central";

    /// <summary>
    /// Table mappings defining source→target field conversions.
    /// Keyed by source table name.
    /// </summary>
    public IReadOnlyDictionary<string, TableMapping> TableMappings { get; init; } =
        new Dictionary<string, TableMapping>();
}
