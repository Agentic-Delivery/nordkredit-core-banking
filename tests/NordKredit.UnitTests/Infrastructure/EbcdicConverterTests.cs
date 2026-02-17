using NordKredit.Infrastructure.DataMigration;

namespace NordKredit.UnitTests.Infrastructure;

/// <summary>
/// Tests for <see cref="EbcdicConverter"/>.
/// Verifies EBCDIC (IBM Code Page 1143) to Unicode conversion,
/// packed decimal (COMP-3), and zoned decimal conversions.
/// COBOL source: All copybook field conversions during Db2 → Azure SQL migration.
/// </summary>
public class EbcdicConverterTests
{
    // ──────────────────────────────────────────────
    // EBCDIC ↔ Unicode text conversion tests
    // ──────────────────────────────────────────────

    [Fact]
    public void ConvertToUnicode_AsciiLetters_ReturnsCorrectString()
    {
        // EBCDIC Code Page 1143: 'A' = 0xC1, 'B' = 0xC2, 'C' = 0xC3
        byte[] ebcdic = [0xC1, 0xC2, 0xC3];

        var result = EbcdicConverter.ConvertToUnicode(ebcdic);

        Assert.Equal("ABC", result);
    }

    [Fact]
    public void ConvertToUnicode_SwedishCharacters_ReturnsCorrectString()
    {
        // IBM Code Page 1143 Swedish EBCDIC:
        // We test round-trip to ensure the encoding handles Swedish chars properly
        var swedishText = "ÅÄÖåäö";

        var ebcdicBytes = EbcdicConverter.ConvertToEbcdic(swedishText);
        var roundTripped = EbcdicConverter.ConvertToUnicode(ebcdicBytes);

        Assert.Equal(swedishText, roundTripped);
    }

    [Fact]
    public void ConvertToUnicode_Digits_ReturnsCorrectString()
    {
        // EBCDIC digits: '0' = 0xF0, '1' = 0xF1, ... '9' = 0xF9
        byte[] ebcdic = [0xF1, 0xF2, 0xF3];

        var result = EbcdicConverter.ConvertToUnicode(ebcdic);

        Assert.Equal("123", result);
    }

    [Fact]
    public void ConvertToUnicode_EmptyArray_ReturnsEmptyString()
    {
        var result = EbcdicConverter.ConvertToUnicode([]);

        Assert.Equal(string.Empty, result);
    }

    [Fact]
    public void ConvertToEbcdic_EmptyString_ReturnsEmptyArray()
    {
        var result = EbcdicConverter.ConvertToEbcdic(string.Empty);

        Assert.Empty(result);
    }

    [Fact]
    public void ConvertRoundTrip_MixedSwedishAndAscii_PreservesContent()
    {
        // Simulate a typical customer name field from COBOL copybook
        var customerName = "Göran Ström";

        var ebcdicBytes = EbcdicConverter.ConvertToEbcdic(customerName);
        var roundTripped = EbcdicConverter.ConvertToUnicode(ebcdicBytes);

        Assert.Equal(customerName, roundTripped);
    }

    [Fact]
    public void ConvertRoundTrip_SpacePaddedField_PreservesContent()
    {
        // COBOL fields are typically space-padded (EBCDIC space = 0x40)
        var paddedName = "Erik   ";

        var ebcdicBytes = EbcdicConverter.ConvertToEbcdic(paddedName);
        var roundTripped = EbcdicConverter.ConvertToUnicode(ebcdicBytes);

        Assert.Equal(paddedName, roundTripped);
    }

    // ──────────────────────────────────────────────
    // Packed decimal (COMP-3) conversion tests
    // ──────────────────────────────────────────────

    [Fact]
    public void ConvertPackedDecimal_PositiveInteger_ReturnsCorrectValue()
    {
        // COMP-3: 12345+ → 3 bytes: 0x12, 0x34, 0x5C
        // Each nibble = one digit, last nibble 0xC = positive
        byte[] packed = [0x12, 0x34, 0x5C];

        var result = EbcdicConverter.ConvertPackedDecimal(packed);

        Assert.Equal(12345m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_NegativeInteger_ReturnsCorrectValue()
    {
        // COMP-3: -12345 → 3 bytes: 0x12, 0x34, 0x5D
        // Last nibble 0xD = negative
        byte[] packed = [0x12, 0x34, 0x5D];

        var result = EbcdicConverter.ConvertPackedDecimal(packed);

        Assert.Equal(-12345m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_WithScale_ReturnsCorrectDecimalValue()
    {
        // PIC 9(5)V99 COMP-3: value 12345.67 → stored integer 1234567
        // 7 digits + 1 sign nibble = 8 nibbles = 4 bytes: 0x12, 0x34, 0x56, 0x7C
        // scale=2 means last 2 digits are after decimal point
        byte[] packed = [0x12, 0x34, 0x56, 0x7C];

        var result = EbcdicConverter.ConvertPackedDecimal(packed, scale: 2);

        Assert.Equal(12345.67m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_Zero_ReturnsZero()
    {
        // Zero: 0x00, 0x0C (positive zero)
        byte[] packed = [0x00, 0x0C];

        var result = EbcdicConverter.ConvertPackedDecimal(packed);

        Assert.Equal(0m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_UnsignedPositive_ReturnsCorrectValue()
    {
        // 0xF = unsigned/positive (common in COBOL display fields converted to COMP-3)
        byte[] packed = [0x99, 0x9F];

        var result = EbcdicConverter.ConvertPackedDecimal(packed);

        Assert.Equal(999m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_SingleByte_ReturnsCorrectValue()
    {
        // Single digit packed: 5+ stored as 0x5C
        byte[] packed = [0x5C];

        var result = EbcdicConverter.ConvertPackedDecimal(packed);

        Assert.Equal(5m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_LargeValue_ReturnsCorrectValue()
    {
        // Typical account balance: PIC 9(9)V99 COMP-3
        // Value: 123456789.12 → stored integer 12345678912 (11 digits)
        // 11 digits + 1 sign = 12 nibbles = 6 bytes: 0x12, 0x34, 0x56, 0x78, 0x91, 0x2C
        byte[] packed = [0x12, 0x34, 0x56, 0x78, 0x91, 0x2C];

        var result = EbcdicConverter.ConvertPackedDecimal(packed, scale: 2);

        Assert.Equal(123456789.12m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_NegativeWithScale_ReturnsCorrectValue()
    {
        // Negative amount: -100.50 → PIC S9(5)V99 COMP-3
        // Stored integer: 10050 (5 digits + 1 sign = 6 nibbles = 3 bytes)
        // Packed: 0x10, 0x05, 0x0D
        byte[] packed = [0x10, 0x05, 0x0D];

        var result = EbcdicConverter.ConvertPackedDecimal(packed, scale: 2);

        Assert.Equal(-100.50m, result);
    }

    [Fact]
    public void ConvertPackedDecimal_EmptyArray_ThrowsArgumentException() =>
        Assert.Throws<ArgumentException>(() => EbcdicConverter.ConvertPackedDecimal([]));

    // ──────────────────────────────────────────────
    // Zoned decimal conversion tests
    // ──────────────────────────────────────────────

    [Fact]
    public void ConvertZonedDecimal_PositiveInteger_ReturnsCorrectValue()
    {
        // Zoned decimal: 12345 positive
        // Each byte: zone nibble (0xF) + digit nibble
        // Last byte: sign nibble (0xC for positive) + digit nibble
        // '1'=0xF1, '2'=0xF2, '3'=0xF3, '4'=0xF4, '5'=0xC5
        byte[] zoned = [0xF1, 0xF2, 0xF3, 0xF4, 0xC5];

        var result = EbcdicConverter.ConvertZonedDecimal(zoned);

        Assert.Equal(12345m, result);
    }

    [Fact]
    public void ConvertZonedDecimal_NegativeInteger_ReturnsCorrectValue()
    {
        // Zoned decimal: -12345
        // Last byte sign nibble 0xD = negative
        // '1'=0xF1, '2'=0xF2, '3'=0xF3, '4'=0xF4, '5'=0xD5
        byte[] zoned = [0xF1, 0xF2, 0xF3, 0xF4, 0xD5];

        var result = EbcdicConverter.ConvertZonedDecimal(zoned);

        Assert.Equal(-12345m, result);
    }

    [Fact]
    public void ConvertZonedDecimal_WithScale_ReturnsCorrectDecimalValue()
    {
        // PIC 9(3)V99: value 123.45
        // Stored as 5 zoned bytes: 0xF1, 0xF2, 0xF3, 0xF4, 0xC5
        byte[] zoned = [0xF1, 0xF2, 0xF3, 0xF4, 0xC5];

        var result = EbcdicConverter.ConvertZonedDecimal(zoned, scale: 2);

        Assert.Equal(123.45m, result);
    }

    [Fact]
    public void ConvertZonedDecimal_UnsignedPositive_ReturnsCorrectValue()
    {
        // Unsigned: all bytes have 0xF zone including last byte
        // '1'=0xF1, '2'=0xF2, '3'=0xF3
        byte[] zoned = [0xF1, 0xF2, 0xF3];

        var result = EbcdicConverter.ConvertZonedDecimal(zoned);

        Assert.Equal(123m, result);
    }

    [Fact]
    public void ConvertZonedDecimal_Zero_ReturnsZero()
    {
        byte[] zoned = [0xF0, 0xF0, 0xC0];

        var result = EbcdicConverter.ConvertZonedDecimal(zoned);

        Assert.Equal(0m, result);
    }

    [Fact]
    public void ConvertZonedDecimal_EmptyArray_ThrowsArgumentException() =>
        Assert.Throws<ArgumentException>(() => EbcdicConverter.ConvertZonedDecimal([]));

    [Fact]
    public void ConvertZonedDecimal_SingleDigit_ReturnsCorrectValue()
    {
        // Single digit: 7 positive → 0xC7
        byte[] zoned = [0xC7];

        var result = EbcdicConverter.ConvertZonedDecimal(zoned);

        Assert.Equal(7m, result);
    }
}
