namespace NordKredit.Domain.DataMigration;

/// <summary>
/// A source record after EBCDIC-to-Unicode conversion and type mapping.
/// Field values are .NET types (string, decimal, DateTime, etc.) ready for Azure SQL.
/// Regulation: GDPR Art. 5(1)(d) — accuracy of personal data during migration.
/// </summary>
public class ConvertedRecord
{
    /// <summary>Target Azure SQL table name (e.g., "Accounts", "Cards").</summary>
    public required string TargetTable { get; init; }

    /// <summary>The type of change to apply.</summary>
    public required ChangeType ChangeType { get; init; }

    /// <summary>Primary key value(s) as a string.</summary>
    public required string PrimaryKey { get; init; }

    /// <summary>
    /// Converted field values keyed by Azure SQL column name.
    /// Values are .NET types: string, decimal, int, DateTime, DateOnly, etc.
    /// </summary>
    public required IReadOnlyDictionary<string, object?> Fields { get; init; }

    /// <summary>Timestamp of the change in the source system.</summary>
    public required DateTimeOffset SourceTimestamp { get; init; }
}
