using System.Text.Json;

namespace NordKredit.ComparisonTests.CardManagement;

/// <summary>
/// Comparison tests for card detail operation — parallel-run validation against mainframe output.
/// COBOL source: COCRDSLC.cbl:608-812 (card detail lookup — primary key and alternate index).
/// Business rules: CARD-BR-003 (card detail lookup), CARD-BR-009 (data structure).
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).
///
/// Golden file: CardManagement/GoldenFiles/card-detail-by-cardnumber.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - CVV code: Mainframe returns CVV in CICS screen. Migrated system excludes CVV from API
///   response per PCI-DSS compliance (intentional security improvement).
/// - Character encoding: Mainframe uses EBCDIC. Migrated system uses Unicode (UTF-8).
///   Swedish characters (Å, Ä, Ö) are functionally equivalent after EBCDIC-to-Unicode conversion.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareCardDetail_ByCardNumber_MatchesMainframeOutput
/// - CompareCardDetail_ByAccountId_MatchesMainframeOutput
/// - CompareCardDetail_NotFound_MatchesMainframeOutput
/// </summary>
public class CardDetailComparisonTests
{
    private const string _goldenFilePath = "CardManagement/GoldenFiles/card-detail-by-cardnumber.json";

    [Fact]
    public void GoldenFile_Exists() =>
        Assert.True(File.Exists(_goldenFilePath), $"Golden file not found: {_goldenFilePath}");

    [Fact]
    public void GoldenFile_IsValidJson()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.NotNull(document);
    }

    [Fact]
    public void GoldenFile_ContainsExpectedFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("cardNumber", out _));
        Assert.True(root.TryGetProperty("accountId", out _));
        Assert.True(root.TryGetProperty("embossedName", out _));
        Assert.True(root.TryGetProperty("expirationDate", out _));
        Assert.True(root.TryGetProperty("activeStatus", out _));
    }

    [Fact]
    public void GoldenFile_ExpirationDateFormat_IsYYYYMMDD()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var dateStr = document.RootElement.GetProperty("expirationDate").GetString();

        Assert.NotNull(dateStr);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", dateStr);
    }
}
