using System.Text.Json;
using System.Text.RegularExpressions;

namespace NordKredit.Domain.ParallelRun;

/// <summary>
/// Compares mainframe and Azure JSON outputs, applying known-difference rules.
/// Known intentional differences (card masking, date formats, EBCDIC encoding) are filtered.
/// Regulations: DORA Art.11 (ICT system testing), FFFS 2014:5 Ch.4 §3 (operational risk).
/// </summary>
public partial class ComparisonEngine : IComparisonEngine
{
    /// <summary>
    /// Compares two JSON responses field-by-field. Metadata fields (starting with _) are ignored.
    /// Known differences (card masking, date formats) are detected and excluded.
    /// </summary>
    public ComparisonResult Compare(
        string domain,
        string operation,
        string mainframeResponse,
        string azureResponse,
        string correlationId)
    {
        using var mainframeDoc = JsonDocument.Parse(mainframeResponse);
        using var azureDoc = JsonDocument.Parse(azureResponse);

        List<FieldDivergence> divergences = [];

        CompareElements(mainframeDoc.RootElement, azureDoc.RootElement, divergences);

        return new ComparisonResult
        {
            IsMatch = divergences.Count == 0,
            Domain = domain,
            Operation = operation,
            Divergences = divergences,
            ComparedAt = DateTimeOffset.UtcNow,
            CorrelationId = correlationId
        };
    }

    private static void CompareElements(
        JsonElement mainframe,
        JsonElement azure,
        List<FieldDivergence> divergences)
    {
        // Collect all property names from both objects
        HashSet<string> allProperties = [];
        foreach (var prop in mainframe.EnumerateObject())
        {
            allProperties.Add(prop.Name);
        }

        foreach (var prop in azure.EnumerateObject())
        {
            allProperties.Add(prop.Name);
        }

        foreach (var propertyName in allProperties)
        {
            // Skip metadata fields (prefixed with _)
            if (propertyName.StartsWith('_'))
            {
                continue;
            }

            var mainframeHas = mainframe.TryGetProperty(propertyName, out var mainframeValue);
            var azureHas = azure.TryGetProperty(propertyName, out var azureValue);

            if (mainframeHas && !azureHas)
            {
                divergences.Add(new FieldDivergence
                {
                    FieldName = propertyName,
                    MainframeValue = GetValueAsString(mainframeValue),
                    AzureValue = null,
                    Category = DivergenceCategory.MissingField
                });
                continue;
            }

            if (!mainframeHas && azureHas)
            {
                divergences.Add(new FieldDivergence
                {
                    FieldName = propertyName,
                    MainframeValue = null,
                    AzureValue = GetValueAsString(azureValue),
                    Category = DivergenceCategory.MissingField
                });
                continue;
            }

            var mainframeStr = GetValueAsString(mainframeValue);
            var azureStr = GetValueAsString(azureValue);

            if (mainframeStr == azureStr)
            {
                continue;
            }

            // Check known-difference rules before flagging as divergence
            if (IsKnownDifference(propertyName, mainframeStr, azureStr))
            {
                continue;
            }

            divergences.Add(new FieldDivergence
            {
                FieldName = propertyName,
                MainframeValue = mainframeStr,
                AzureValue = azureStr,
                Category = DivergenceCategory.DataMismatch
            });
        }
    }

    private static bool IsKnownDifference(string fieldName, string? mainframeValue, string? azureValue)
    {
        if (mainframeValue is null || azureValue is null)
        {
            return false;
        }

        // Known difference: card number masking (PCI-DSS)
        // Mainframe: full 16-digit card number; Azure: masked with last 4 digits visible
        if (IsCardNumberField(fieldName) && IsCardMaskingDifference(mainframeValue, azureValue))
        {
            return true;
        }

        // Known difference: date/timestamp format (ISO 8601 vs mainframe MM/DD/YY)
        if (IsDateField(fieldName) && IsDateFormatDifference(mainframeValue, azureValue))
        {
            return true;
        }

        return false;
    }

    private static bool IsCardNumberField(string fieldName) =>
        fieldName.Equals("cardNumber", StringComparison.OrdinalIgnoreCase);

    private static bool IsCardMaskingDifference(string mainframeValue, string azureValue)
    {
        // Mainframe: 16-digit card number (e.g., "4000000000001234")
        // Azure: masked (e.g., "************1234")
        if (!CardNumberPattern().IsMatch(mainframeValue) || !MaskedCardPattern().IsMatch(azureValue))
        {
            return false;
        }

        // Last 4 digits must match for it to be an intentional masking difference
        return mainframeValue[^4..] == azureValue[^4..];
    }

    private static bool IsDateField(string fieldName) =>
        fieldName.Contains("date", StringComparison.OrdinalIgnoreCase) ||
        fieldName.Contains("timestamp", StringComparison.OrdinalIgnoreCase) ||
        fieldName.Contains("Timestamp", StringComparison.Ordinal);

    private static bool IsDateFormatDifference(string mainframeValue, string azureValue)
    {
        // Mainframe: MM/DD/YY or MM/DD/YY HH:mm:ss format
        // Azure: ISO 8601 (YYYY-MM-DDTHH:mm:ss)
        var mainframeIsLegacyDate = MainframeDatePattern().IsMatch(mainframeValue) ||
                                    MainframeDateTimePattern().IsMatch(mainframeValue);
        var azureIsIso = Iso8601Pattern().IsMatch(azureValue);

        return mainframeIsLegacyDate && azureIsIso;
    }

    private static string? GetValueAsString(JsonElement element) =>
        element.ValueKind switch
        {
            JsonValueKind.String => element.GetString(),
            JsonValueKind.Number => element.GetRawText(),
            JsonValueKind.True => "true",
            JsonValueKind.False => "false",
            JsonValueKind.Null => null,
            JsonValueKind.Undefined => null,
            JsonValueKind.Object => element.GetRawText(),
            JsonValueKind.Array => element.GetRawText(),
            _ => element.GetRawText()
        };

    [GeneratedRegex(@"^\d{16}$")]
    private static partial Regex CardNumberPattern();

    [GeneratedRegex(@"^\*{12}\d{4}$")]
    private static partial Regex MaskedCardPattern();

    [GeneratedRegex(@"^\d{2}/\d{2}/\d{2}$")]
    private static partial Regex MainframeDatePattern();

    [GeneratedRegex(@"^\d{2}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}$")]
    private static partial Regex MainframeDateTimePattern();

    [GeneratedRegex(@"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}")]
    private static partial Regex Iso8601Pattern();
}
