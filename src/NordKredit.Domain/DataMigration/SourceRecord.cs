namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Represents a single record read from the Db2 source system during migration.
/// Contains raw field data (EBCDIC bytes) and metadata about the change.
/// COBOL source: All copybook structures referenced in schema mapping document.
/// Regulation: GDPR Art. 5(1)(d) — accuracy of personal data during migration.
/// </summary>
public class SourceRecord
{
    /// <summary>The source table name (e.g., "ACCTFILE", "CARDDAT").</summary>
    public required string TableName { get; init; }

    /// <summary>The type of change detected.</summary>
    public required ChangeType ChangeType { get; init; }

    /// <summary>Primary key value(s) as a string (composite keys joined with '|').</summary>
    public required string PrimaryKey { get; init; }

    /// <summary>
    /// Raw field values keyed by COBOL field name.
    /// Values are byte arrays (EBCDIC-encoded) for text/numeric fields,
    /// or pre-converted strings for already-processed fields.
    /// </summary>
    public required IReadOnlyDictionary<string, object> Fields { get; init; }

    /// <summary>Timestamp of the change in the source system.</summary>
    public required DateTimeOffset SourceTimestamp { get; init; }
}

/// <summary>
/// The type of change detected in the source system.
/// </summary>
public enum ChangeType
{
    Insert,
    Update,
    Delete
}
