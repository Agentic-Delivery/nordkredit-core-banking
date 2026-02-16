using System.Text.Json;

namespace NordKredit.ComparisonTests.CardManagement;

/// <summary>
/// Comparison tests for card update operation — parallel-run validation against mainframe output.
/// COBOL source: COCRDUPC.cbl:275-290 (state machine), 429-543 (main EVALUATE),
///               948-1027 (action decisions), 1420-1523 (write processing + concurrency).
/// Business rules: CARD-BR-006 (field validation), CARD-BR-007 (state machine), CARD-BR-008 (concurrency).
/// Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 §4 (operational risk).
///
/// Golden file: CardManagement/GoldenFiles/card-update-success.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences (documented in golden file _knownDifferences):
/// 1. Concurrency: Mainframe uses field-by-field comparison (COCRDUPC.cbl:1498-1523).
///    Migrated system uses SQL rowversion (ETag-based). Both detect concurrent modifications.
/// 2. Error collection: Mainframe returns only first validation error (WS-RETURN-MSG-OFF).
///    Migrated system collects all errors simultaneously — intentional improvement.
/// 3. Character encoding: Mainframe uses EBCDIC with Swedish codepage.
///    Migrated system uses Unicode (nvarchar) — supports Å, Ä, Ö plus additional Unicode letters.
/// 4. Expiry day clamping: Mainframe carries original day unchanged.
///    Migrated system clamps day to valid range when month/year change (e.g., Jan 31 → Feb 28).
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareCardUpdate_Success_MatchesMainframeOutput
/// - CompareCardUpdate_ValidationFailure_MatchesMainframeOutput
/// - CompareCardUpdate_NoChange_MatchesMainframeOutput
/// - CompareCardUpdate_ConcurrencyConflict_MatchesMainframeOutput
/// </summary>
public class CardUpdateComparisonTests
{
    private const string _goldenFilePath = "CardManagement/GoldenFiles/card-update-success.json";

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
    public void GoldenFile_DocumentsKnownDifferences()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("_knownDifferences", out var differences));
        Assert.True(differences.TryGetProperty("concurrencyMechanism", out _));
        Assert.True(differences.TryGetProperty("errorCollection", out _));
        Assert.True(differences.TryGetProperty("characterEncoding", out _));
        Assert.True(differences.TryGetProperty("expiryDayClamping", out _));
    }

    [Fact]
    public void GoldenFile_ContainsExpectedUpdateResult()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("cardNumber", out _));
        Assert.True(root.TryGetProperty("embossedName", out _));
        Assert.True(root.TryGetProperty("message", out _));
    }
}
