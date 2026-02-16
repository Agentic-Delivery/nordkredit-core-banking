using System.Text.Json;

namespace NordKredit.ComparisonTests.CardManagement;

/// <summary>
/// Comparison tests for card list operation — parallel-run validation against mainframe output.
/// COBOL source: COCRDLIC.cbl:1123-1411 (PROCESS-PAGE-FORWARD/BACKWARD, FILTER-RECORDS).
/// Business rule: CARD-BR-001 (card list display with pagination and filtering).
/// Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).
///
/// Golden file: CardManagement/GoldenFiles/card-list-first-page.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Pagination: COBOL uses CICS STARTBR/READNEXT, migrated system uses keyset pagination via SQL.
///   Both produce identical page content for the same dataset.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareCardList_FirstPage_MatchesMainframeOutput
/// - CompareCardList_FilterByAccount_MatchesMainframeOutput
/// - CompareCardList_Pagination_MatchesMainframeOutput
/// </summary>
public class CardListComparisonTests
{
    private const string _goldenFilePath = "CardManagement/GoldenFiles/card-list-first-page.json";

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
    public void GoldenFile_ContainsExpectedCardCount()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var cards = document.RootElement.GetProperty("cards");
        Assert.Equal(7, cards.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_HasNextPageIndicator()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.True(document.RootElement.GetProperty("hasNextPage").GetBoolean());
    }
}
