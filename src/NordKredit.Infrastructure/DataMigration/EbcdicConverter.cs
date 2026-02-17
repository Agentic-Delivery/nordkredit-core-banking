using System.Text;

namespace NordKredit.Infrastructure.DataMigration;

/// <summary>
/// Converts EBCDIC-encoded data from the IBM mainframe to Unicode/UTF-8.
/// Handles IBM Code Page 1143 (Swedish/Finnish EBCDIC variant) for text fields,
/// packed decimal (COMP-3) for numeric fields, and zoned decimal fields.
/// COBOL source: All copybook text/numeric field conversions during Db2 → Azure SQL migration.
/// Regulation: GDPR Art. 5(1)(d) — accuracy of personal data during migration.
/// </summary>
public static class EbcdicConverter
{
    private static readonly Encoding _ebcdicEncoding;

    static EbcdicConverter()
    {
        Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
        _ebcdicEncoding = Encoding.GetEncoding(1143);
    }

    /// <summary>
    /// Converts EBCDIC bytes (IBM Code Page 1143) to a Unicode string.
    /// </summary>
    /// <param name="ebcdicBytes">The EBCDIC-encoded byte array.</param>
    /// <returns>The decoded Unicode string.</returns>
    public static string ConvertToUnicode(byte[] ebcdicBytes) =>
        _ebcdicEncoding.GetString(ebcdicBytes);

    /// <summary>
    /// Converts a Unicode string to EBCDIC bytes (IBM Code Page 1143).
    /// Used for parallel-run comparison where data must be sent back to mainframe format.
    /// </summary>
    /// <param name="unicodeString">The Unicode string to encode.</param>
    /// <returns>The EBCDIC-encoded byte array.</returns>
    public static byte[] ConvertToEbcdic(string unicodeString) =>
        _ebcdicEncoding.GetBytes(unicodeString);

    /// <summary>
    /// Converts a COBOL COMP-3 (packed decimal) byte array to a .NET decimal value.
    /// Each byte stores two digits (one per nibble), with the last nibble as sign
    /// (0xC = positive, 0xD = negative, 0xF = unsigned positive).
    /// </summary>
    /// <param name="packedBytes">The packed decimal byte array.</param>
    /// <param name="scale">Number of implied decimal places (e.g., PIC 9(5)V99 COMP-3 → scale=2).</param>
    /// <returns>The decimal value.</returns>
    public static decimal ConvertPackedDecimal(byte[] packedBytes, int scale = 0)
    {
        if (packedBytes.Length == 0)
        {
            throw new ArgumentException("Packed decimal byte array cannot be empty.", nameof(packedBytes));
        }

        long value = 0;

        // Process all bytes: each byte has two nibbles (digits),
        // except the last nibble of the last byte which is the sign.
        for (var i = 0; i < packedBytes.Length; i++)
        {
            var highNibble = (packedBytes[i] >> 4) & 0x0F;
            var lowNibble = packedBytes[i] & 0x0F;

            if (i < packedBytes.Length - 1)
            {
                // Both nibbles are digits
                value = (value * 10) + highNibble;
                value = (value * 10) + lowNibble;
            }
            else
            {
                // Last byte: high nibble is digit, low nibble is sign
                value = (value * 10) + highNibble;
            }
        }

        // Determine sign from last nibble of last byte
        var signNibble = packedBytes[^1] & 0x0F;
        var isNegative = signNibble == 0x0D;

        if (isNegative)
        {
            value = -value;
        }

        // Apply scale (implied decimal places)
        if (scale > 0)
        {
            return value / (decimal)Math.Pow(10, scale);
        }

        return value;
    }

    /// <summary>
    /// Converts a COBOL zoned decimal byte array to a .NET decimal value.
    /// Each byte stores one digit in the low nibble; the zone (high nibble) of the
    /// last byte indicates the sign (0xC/0xF = positive, 0xD = negative).
    /// </summary>
    /// <param name="zonedBytes">The zoned decimal byte array.</param>
    /// <param name="scale">Number of implied decimal places.</param>
    /// <returns>The decimal value.</returns>
    public static decimal ConvertZonedDecimal(byte[] zonedBytes, int scale = 0)
    {
        if (zonedBytes.Length == 0)
        {
            throw new ArgumentException("Zoned decimal byte array cannot be empty.", nameof(zonedBytes));
        }

        long value = 0;

        for (var i = 0; i < zonedBytes.Length; i++)
        {
            var digit = zonedBytes[i] & 0x0F;
            value = (value * 10) + digit;
        }

        // Sign is in the high nibble of the last byte
        var signNibble = (zonedBytes[^1] >> 4) & 0x0F;
        var isNegative = signNibble == 0x0D;

        if (isNegative)
        {
            value = -value;
        }

        if (scale > 0)
        {
            return value / (decimal)Math.Pow(10, scale);
        }

        return value;
    }
}
