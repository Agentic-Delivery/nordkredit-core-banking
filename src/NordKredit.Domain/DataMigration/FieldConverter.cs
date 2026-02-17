namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Converts raw EBCDIC field values from Db2 source records to .NET types
/// using the EBCDIC converter and schema mapping field type definitions.
/// COBOL source: All copybook field conversions per schema mapping (#143).
/// Regulation: GDPR Art. 5(1)(d) — accuracy of personal data during migration.
/// </summary>
public class FieldConverter
{
    private readonly IEbcdicConverter _ebcdicConverter;

    public FieldConverter(IEbcdicConverter ebcdicConverter)
    {
        _ebcdicConverter = ebcdicConverter;
    }

    /// <summary>
    /// Converts a source record's raw fields to .NET types based on the table mapping.
    /// Returns a ConvertedRecord with Azure SQL column names and typed values.
    /// </summary>
    public ConvertedRecord Convert(SourceRecord source, TableMapping mapping)
    {
        var convertedFields = new Dictionary<string, object?>();

        foreach (var fieldMapping in mapping.Fields)
        {
            if (!source.Fields.TryGetValue(fieldMapping.SourceField, out var rawValue))
            {
                convertedFields[fieldMapping.TargetColumn] = null;
                continue;
            }

            convertedFields[fieldMapping.TargetColumn] = ConvertField(rawValue, fieldMapping);
        }

        return new ConvertedRecord
        {
            TargetTable = mapping.TargetTable,
            ChangeType = source.ChangeType,
            PrimaryKey = source.PrimaryKey,
            Fields = convertedFields,
            SourceTimestamp = source.SourceTimestamp
        };
    }

    private object? ConvertField(object rawValue, FieldMapping fieldMapping)
    {
        if (rawValue is byte[] bytes)
        {
            return fieldMapping.FieldType switch
            {
                CobolFieldType.Alphanumeric => _ebcdicConverter.ConvertToUnicode(bytes).TrimEnd(),
                CobolFieldType.ZonedDecimal => _ebcdicConverter.ConvertZonedDecimal(bytes, fieldMapping.Scale),
                CobolFieldType.SignedZonedDecimal => _ebcdicConverter.ConvertZonedDecimal(bytes, fieldMapping.Scale),
                CobolFieldType.PackedDecimal => _ebcdicConverter.ConvertPackedDecimal(bytes, fieldMapping.Scale),
                CobolFieldType.NumericString => _ebcdicConverter.ConvertToUnicode(bytes).Trim(),
                CobolFieldType.Date => ParseDate(_ebcdicConverter.ConvertToUnicode(bytes).Trim()),
                CobolFieldType.Timestamp => ParseTimestamp(_ebcdicConverter.ConvertToUnicode(bytes).Trim()),
                _ => throw new ArgumentException($"Unknown field type: {fieldMapping.FieldType}")
            };
        }

        // Already a converted value (e.g., string passed through)
        return rawValue;
    }

    private static DateOnly? ParseDate(string dateString)
    {
        if (string.IsNullOrWhiteSpace(dateString))
        {
            return null;
        }

        // COBOL date format: YYYY-MM-DD
        if (DateOnly.TryParseExact(dateString, "yyyy-MM-dd", null, System.Globalization.DateTimeStyles.None, out var date))
        {
            return date;
        }

        return null;
    }

    private static DateTime? ParseTimestamp(string timestampString)
    {
        if (string.IsNullOrWhiteSpace(timestampString))
        {
            return null;
        }

        // COBOL timestamp format: YYYY-MM-DD-HH.MM.SS.FFFFFF
        if (DateTime.TryParseExact(timestampString, "yyyy-MM-dd-HH.mm.ss.ffffff",
            null, System.Globalization.DateTimeStyles.None, out var timestamp))
        {
            return timestamp;
        }

        return null;
    }
}
