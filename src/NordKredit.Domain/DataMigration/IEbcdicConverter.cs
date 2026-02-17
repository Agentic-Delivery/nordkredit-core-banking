namespace NordKredit.Domain.DataMigration;

/// <summary>
/// Domain interface for EBCDIC conversion operations.
/// Infrastructure layer implements this using the concrete EbcdicConverter.
/// Keeps the domain layer free of infrastructure dependencies (CodePages encoding).
/// </summary>
public interface IEbcdicConverter
{
    /// <summary>Converts EBCDIC bytes (IBM Code Page 1143) to a Unicode string.</summary>
    string ConvertToUnicode(byte[] ebcdicBytes);

    /// <summary>Converts a COBOL COMP-3 packed decimal to a .NET decimal.</summary>
    decimal ConvertPackedDecimal(byte[] packedBytes, int scale = 0);

    /// <summary>Converts a COBOL zoned decimal to a .NET decimal.</summary>
    decimal ConvertZonedDecimal(byte[] zonedBytes, int scale = 0);
}
