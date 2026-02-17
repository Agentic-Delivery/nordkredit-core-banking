namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Defines the field-level mapping between a Db2 source table and an Azure SQL target table.
/// Based on schema mapping from #143.
/// COBOL source: Copybook structures (CVACT01Y, CVACT02Y, CVACT03Y, CVTRA05Y, etc.).
/// </summary>
public class TableMapping
{
    /// <summary>Source Db2/VSAM table name.</summary>
    public required string SourceTable { get; init; }

    /// <summary>Target Azure SQL table name.</summary>
    public required string TargetTable { get; init; }

    /// <summary>Primary key column name(s) in the target table.</summary>
    public required IReadOnlyList<string> PrimaryKeyColumns { get; init; }

    /// <summary>Field-level mappings from source to target.</summary>
    public required IReadOnlyList<FieldMapping> Fields { get; init; }

    /// <summary>Foreign key relationships for referential integrity validation.</summary>
    public IReadOnlyList<ForeignKeyMapping> ForeignKeys { get; init; } = [];
}

/// <summary>
/// Maps a single field from COBOL/Db2 source to Azure SQL target.
/// </summary>
public class FieldMapping
{
    /// <summary>Source COBOL field name.</summary>
    public required string SourceField { get; init; }

    /// <summary>Target Azure SQL column name.</summary>
    public required string TargetColumn { get; init; }

    /// <summary>The COBOL data type for conversion.</summary>
    public required CobolFieldType FieldType { get; init; }

    /// <summary>Decimal scale for numeric fields (e.g., 2 for PIC 9(5)V99).</summary>
    public int Scale { get; init; }
}

/// <summary>
/// Defines a foreign key relationship for referential integrity validation.
/// </summary>
public class ForeignKeyMapping
{
    /// <summary>Column in this table that references another table.</summary>
    public required string Column { get; init; }

    /// <summary>Referenced table name.</summary>
    public required string ReferencedTable { get; init; }

    /// <summary>Referenced column name.</summary>
    public required string ReferencedColumn { get; init; }
}

/// <summary>
/// COBOL field types that require different conversion strategies.
/// Based on the schema mapping document from #143.
/// </summary>
public enum CobolFieldType
{
    /// <summary>PIC X(n) — alphanumeric text, convert via EbcdicConverter.ConvertToUnicode.</summary>
    Alphanumeric,

    /// <summary>PIC 9(n) — unsigned numeric, convert via EbcdicConverter.ConvertZonedDecimal.</summary>
    ZonedDecimal,

    /// <summary>PIC S9(n)Vnn — signed numeric with implied decimal, convert via EbcdicConverter.ConvertZonedDecimal.</summary>
    SignedZonedDecimal,

    /// <summary>COMP-3 — packed decimal, convert via EbcdicConverter.ConvertPackedDecimal.</summary>
    PackedDecimal,

    /// <summary>PIC X(n) containing a date string — convert to DateOnly after EBCDIC decode.</summary>
    Date,

    /// <summary>PIC X(n) containing a timestamp — convert to DateTime after EBCDIC decode.</summary>
    Timestamp,

    /// <summary>PIC 9(n) — numeric stored as string to preserve leading zeros (e.g., AccountId).</summary>
    NumericString
}
