using NordKredit.Domain.DataMigration;

namespace NordKredit.UnitTests.DataMigration;

/// <summary>
/// Unit tests for FieldConverter — converts EBCDIC source fields to .NET types.
/// Validates EBCDIC text, packed decimal, zoned decimal, date, and timestamp conversions.
/// COBOL source: All copybook field conversions during Db2 → Azure SQL migration.
/// Regulation: GDPR Art. 5(1)(d) — accuracy of personal data during migration.
/// </summary>
public class FieldConverterTests
{
    private readonly StubEbcdicConverter _ebcdicConverter = new();
    private readonly FieldConverter _converter;

    public FieldConverterTests()
    {
        _converter = new FieldConverter(_ebcdicConverter);
    }

    // ===================================================================
    // AC: EBCDIC-to-Unicode conversion is applied to all text fields
    // Alphanumeric (PIC X) fields are converted and trimmed
    // ===================================================================

    [Fact]
    public void Convert_AlphanumericField_ConvertsAndTrims()
    {
        var mapping = CreateMapping("Accounts", fields:
        [
            new FieldMapping { SourceField = "ACCT-ACTIVE-STATUS", TargetColumn = "ActiveStatus", FieldType = CobolFieldType.Alphanumeric }
        ]);
        byte[] ebcdicBytes = [0xC1]; // 'A' in EBCDIC
        var source = CreateSourceRecord("ACCTFILE", fields: new()
        {
            ["ACCT-ACTIVE-STATUS"] = ebcdicBytes
        });
        _ebcdicConverter.UnicodeResult = "A   "; // With trailing spaces

        var result = _converter.Convert(source, mapping);

        Assert.Equal("A", result.Fields["ActiveStatus"]);
    }

    // ===================================================================
    // AC: EBCDIC-to-Unicode conversion handles Swedish characters (Å, Ä, Ö)
    // ===================================================================

    [Fact]
    public void Convert_SwedishCharacters_PreservesCorrectly()
    {
        var mapping = CreateMapping("Cards", fields:
        [
            new FieldMapping { SourceField = "CARD-EMBOSSED-NAME", TargetColumn = "EmbossedName", FieldType = CobolFieldType.Alphanumeric }
        ]);
        byte[] ebcdicBytes = [0x01];
        var source = CreateSourceRecord("CARDDAT", fields: new()
        {
            ["CARD-EMBOSSED-NAME"] = ebcdicBytes
        });
        _ebcdicConverter.UnicodeResult = "Björk Ström";

        var result = _converter.Convert(source, mapping);

        Assert.Equal("Björk Ström", result.Fields["EmbossedName"]);
    }

    // ===================================================================
    // AC: Packed decimal (COMP-3) conversion for numeric fields
    // ===================================================================

    [Fact]
    public void Convert_PackedDecimalField_ConvertsWithScale()
    {
        var mapping = CreateMapping("Accounts", fields:
        [
            new FieldMapping { SourceField = "ACCT-CURR-BAL", TargetColumn = "CurrentBalance", FieldType = CobolFieldType.PackedDecimal, Scale = 2 }
        ]);
        byte[] packed = [0x12, 0x34, 0x5C]; // 123.45
        var source = CreateSourceRecord("ACCTFILE", fields: new()
        {
            ["ACCT-CURR-BAL"] = packed
        });
        _ebcdicConverter.PackedDecimalResult = 123.45m;

        var result = _converter.Convert(source, mapping);

        Assert.Equal(123.45m, result.Fields["CurrentBalance"]);
    }

    // ===================================================================
    // AC: Zoned decimal conversion for signed numeric fields
    // ===================================================================

    [Fact]
    public void Convert_SignedZonedDecimalField_ConvertsWithScale()
    {
        var mapping = CreateMapping("Transactions", fields:
        [
            new FieldMapping { SourceField = "TRAN-AMT", TargetColumn = "Amount", FieldType = CobolFieldType.SignedZonedDecimal, Scale = 2 }
        ]);
        byte[] zoned = [0xF1, 0xF0, 0xF0, 0xD0];
        var source = CreateSourceRecord("TRANSACT", fields: new()
        {
            ["TRAN-AMT"] = zoned
        });
        _ebcdicConverter.ZonedDecimalResult = -100.00m;

        var result = _converter.Convert(source, mapping);

        Assert.Equal(-100.00m, result.Fields["Amount"]);
    }

    // ===================================================================
    // AC: NumericString preserves leading zeros (e.g., AccountId PIC 9(11))
    // ===================================================================

    [Fact]
    public void Convert_NumericStringField_PreservesLeadingZeros()
    {
        var mapping = CreateMapping("Accounts", fields:
        [
            new FieldMapping { SourceField = "ACCT-ID", TargetColumn = "Id", FieldType = CobolFieldType.NumericString }
        ]);
        byte[] ebcdicBytes = [0x01];
        var source = CreateSourceRecord("ACCTFILE", fields: new()
        {
            ["ACCT-ID"] = ebcdicBytes
        });
        _ebcdicConverter.UnicodeResult = "00012345678";

        var result = _converter.Convert(source, mapping);

        Assert.Equal("00012345678", result.Fields["Id"]);
    }

    // ===================================================================
    // AC: Date fields convert from COBOL date format to DateOnly
    // ===================================================================

    [Fact]
    public void Convert_DateField_ParsesCobolFormat()
    {
        var mapping = CreateMapping("Cards", fields:
        [
            new FieldMapping { SourceField = "CARD-EXPIRY-DATE", TargetColumn = "ExpirationDate", FieldType = CobolFieldType.Date }
        ]);
        byte[] ebcdicBytes = [0x01];
        var source = CreateSourceRecord("CARDDAT", fields: new()
        {
            ["CARD-EXPIRY-DATE"] = ebcdicBytes
        });
        _ebcdicConverter.UnicodeResult = "2027-03-15";

        var result = _converter.Convert(source, mapping);

        Assert.Equal(new DateOnly(2027, 3, 15), result.Fields["ExpirationDate"]);
    }

    // ===================================================================
    // AC: Timestamp fields convert from COBOL timestamp to DateTime
    // ===================================================================

    [Fact]
    public void Convert_TimestampField_ParsesCobolFormat()
    {
        var mapping = CreateMapping("Transactions", fields:
        [
            new FieldMapping { SourceField = "TRAN-ORIG-TS", TargetColumn = "OriginationTimestamp", FieldType = CobolFieldType.Timestamp }
        ]);
        byte[] ebcdicBytes = [0x01];
        var source = CreateSourceRecord("TRANSACT", fields: new()
        {
            ["TRAN-ORIG-TS"] = ebcdicBytes
        });
        _ebcdicConverter.UnicodeResult = "2026-02-17-14.30.00.000000";

        var result = _converter.Convert(source, mapping);

        Assert.Equal(new DateTime(2026, 2, 17, 14, 30, 0), result.Fields["OriginationTimestamp"]);
    }

    // ===================================================================
    // Missing source field → null in output
    // ===================================================================

    [Fact]
    public void Convert_MissingField_ReturnsNull()
    {
        var mapping = CreateMapping("Accounts", fields:
        [
            new FieldMapping { SourceField = "MISSING-FIELD", TargetColumn = "MissingColumn", FieldType = CobolFieldType.Alphanumeric }
        ]);
        var source = CreateSourceRecord("ACCTFILE");

        var result = _converter.Convert(source, mapping);

        Assert.Null(result.Fields["MissingColumn"]);
    }

    // ===================================================================
    // Change type and primary key are preserved in converted record
    // ===================================================================

    [Fact]
    public void Convert_PreservesChangeTypeAndPrimaryKey()
    {
        var mapping = CreateMapping("Accounts", fields: []);
        var source = CreateSourceRecord("ACCTFILE", changeType: ChangeType.Update, primaryKey: "00012345678");

        var result = _converter.Convert(source, mapping);

        Assert.Equal(ChangeType.Update, result.ChangeType);
        Assert.Equal("00012345678", result.PrimaryKey);
        Assert.Equal("Accounts", result.TargetTable);
    }

    // ===================================================================
    // Empty date string → null
    // ===================================================================

    [Fact]
    public void Convert_EmptyDateField_ReturnsNull()
    {
        var mapping = CreateMapping("Cards", fields:
        [
            new FieldMapping { SourceField = "DATE-FIELD", TargetColumn = "SomeDate", FieldType = CobolFieldType.Date }
        ]);
        byte[] ebcdicBytes = [0x01];
        var source = CreateSourceRecord("CARDDAT", fields: new()
        {
            ["DATE-FIELD"] = ebcdicBytes
        });
        _ebcdicConverter.UnicodeResult = "          "; // Spaces

        var result = _converter.Convert(source, mapping);

        Assert.Null(result.Fields["SomeDate"]);
    }

    // ===================================================================
    // Multiple fields in one record are all converted
    // ===================================================================

    [Fact]
    public void Convert_MultipleFields_AllConverted()
    {
        var mapping = CreateMapping("Accounts", fields:
        [
            new FieldMapping { SourceField = "ACCT-ID", TargetColumn = "Id", FieldType = CobolFieldType.NumericString },
            new FieldMapping { SourceField = "ACCT-STATUS", TargetColumn = "ActiveStatus", FieldType = CobolFieldType.Alphanumeric },
            new FieldMapping { SourceField = "ACCT-BAL", TargetColumn = "CurrentBalance", FieldType = CobolFieldType.PackedDecimal, Scale = 2 }
        ]);
        byte[] b1 = [0x01];
        byte[] b2 = [0x02];
        byte[] b3 = [0x03];
        var source = CreateSourceRecord("ACCTFILE", fields: new()
        {
            ["ACCT-ID"] = b1,
            ["ACCT-STATUS"] = b2,
            ["ACCT-BAL"] = b3
        });
        _ebcdicConverter.UnicodeResult = "00012345678";
        _ebcdicConverter.PackedDecimalResult = 500.00m;

        var result = _converter.Convert(source, mapping);

        Assert.Equal(3, result.Fields.Count);
    }

    // ===================================================================
    // Pre-converted string values pass through
    // ===================================================================

    [Fact]
    public void Convert_PreConvertedStringValue_PassesThrough()
    {
        var mapping = CreateMapping("Accounts", fields:
        [
            new FieldMapping { SourceField = "ALREADY-STRING", TargetColumn = "StringCol", FieldType = CobolFieldType.Alphanumeric }
        ]);
        var source = CreateSourceRecord("ACCTFILE", fields: new()
        {
            ["ALREADY-STRING"] = "Already a string"
        });

        var result = _converter.Convert(source, mapping);

        Assert.Equal("Already a string", result.Fields["StringCol"]);
    }

    // ===================================================================
    // Helpers
    // ===================================================================

    private static TableMapping CreateMapping(string targetTable, IReadOnlyList<FieldMapping>? fields = null) =>
        new()
        {
            SourceTable = "SOURCE",
            TargetTable = targetTable,
            PrimaryKeyColumns = ["Id"],
            Fields = fields ?? [],
            ForeignKeys = []
        };

    private static SourceRecord CreateSourceRecord(
        string tableName,
        Dictionary<string, object>? fields = null,
        ChangeType changeType = ChangeType.Insert,
        string primaryKey = "PK001") =>
        new()
        {
            TableName = tableName,
            ChangeType = changeType,
            PrimaryKey = primaryKey,
            Fields = fields ?? [],
            SourceTimestamp = DateTimeOffset.UtcNow
        };
}

// ===================================================================
// Test doubles
// ===================================================================

internal sealed class StubEbcdicConverter : IEbcdicConverter
{
    public string UnicodeResult { get; set; } = "";
    public decimal PackedDecimalResult { get; set; }
    public decimal ZonedDecimalResult { get; set; }

    public string ConvertToUnicode(byte[] ebcdicBytes) => UnicodeResult;
    public decimal ConvertPackedDecimal(byte[] packedBytes, int scale = 0) => PackedDecimalResult;
    public decimal ConvertZonedDecimal(byte[] zonedBytes, int scale = 0) => ZonedDecimalResult;
}
