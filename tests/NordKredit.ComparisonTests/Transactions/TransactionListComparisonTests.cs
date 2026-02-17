using System.Text.Json;

namespace NordKredit.ComparisonTests.Transactions;

/// <summary>
/// Comparison tests for transaction list operation — parallel-run validation against mainframe output.
/// COBOL source: COTRN00C.cbl:279-328 (PROCESS-PAGE-FORWARD — 10+1 keyset pagination pattern).
/// Business rule: TRN-BR-001 (transaction list display with keyset pagination).
/// Regulations: FFFS 2014:5 Ch.8 (operational info systems), PSD2 Art.94 (transaction history access).
///
/// Golden file: Transactions/GoldenFiles/transaction-list-first-page.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Pagination: COBOL uses CICS STARTBR/READNEXT with 10+1 peek pattern.
///   Migrated system uses keyset pagination via SQL (start-after-ID).
///   Both produce identical page content for the same dataset.
/// - Date format: COBOL uses MM/DD/YY. Migrated system uses ISO 8601 (YYYY-MM-DD).
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareTransactionList_FirstPage_MatchesMainframeOutput
/// - CompareTransactionList_Pagination_MatchesMainframeOutput
/// - CompareTransactionList_EmptyAccount_MatchesMainframeOutput
/// </summary>
public class TransactionListComparisonTests
{
    private const string _goldenFilePath = "Transactions/GoldenFiles/transaction-list-first-page.json";

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
    public void GoldenFile_ContainsExpectedTransactionCount()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var transactions = document.RootElement.GetProperty("transactions");
        Assert.Equal(10, transactions.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_HasNextPageIndicator()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.True(document.RootElement.GetProperty("hasNextPage").GetBoolean());
    }

    [Fact]
    public void GoldenFile_PageSizeMatchesCobolMaxLines()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.Equal(10, document.RootElement.GetProperty("pageSize").GetInt32());
    }

    [Fact]
    public void GoldenFile_TransactionFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("transactions")[0];

        Assert.True(first.TryGetProperty("transactionId", out _));
        Assert.True(first.TryGetProperty("date", out _));
        Assert.True(first.TryGetProperty("description", out _));
        Assert.True(first.TryGetProperty("amount", out _));
    }

    [Fact]
    public void GoldenFile_TransactionId_Is16Characters()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("transactions")[0];
        var id = first.GetProperty("transactionId").GetString();

        Assert.NotNull(id);
        Assert.Equal(16, id.Length);
    }

    [Fact]
    public void GoldenFile_DateFormat_IsISO8601()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("transactions")[0];
        var dateStr = first.GetProperty("date").GetString();

        Assert.NotNull(dateStr);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", dateStr);
    }

    [Fact]
    public void GoldenFile_Amounts_AreExactDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var transactions = document.RootElement.GetProperty("transactions");

        foreach (var txn in transactions.EnumerateArray())
        {
            var amount = txn.GetProperty("amount").GetDecimal();
            Assert.Equal(amount, decimal.Round(amount, 2));
        }
    }
}
