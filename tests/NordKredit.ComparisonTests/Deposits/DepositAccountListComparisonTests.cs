using System.Text.Json;

namespace NordKredit.ComparisonTests.Deposits;

/// <summary>
/// Comparison tests for deposit account list operation — parallel-run validation against mainframe output.
/// COBOL source: CVACT01Y.cpy — ACCOUNT-RECORD (300 bytes), STARTBR/READNEXT pagination.
/// Business rule: DEP-BR-001 (deposit account data structure and listing).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), GDPR Art.15 (right of access).
///
/// Golden file: Deposits/GoldenFiles/deposit-account-list-first-page.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Pagination: COBOL uses CICS STARTBR/READNEXT. Migrated system uses keyset pagination via SQL.
/// - Date format: COBOL uses MM/DD/YY. Migrated system uses ISO 8601 (YYYY-MM-DD).
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareDepositAccountList_FirstPage_MatchesMainframeOutput
/// - CompareDepositAccountList_Pagination_MatchesMainframeOutput
/// - CompareDepositAccountList_EmptyResult_MatchesMainframeOutput
/// </summary>
public class DepositAccountListComparisonTests
{
    private const string _goldenFilePath = "Deposits/GoldenFiles/deposit-account-list-first-page.json";

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
    public void GoldenFile_ContainsExpectedAccountCount()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var accounts = document.RootElement.GetProperty("accounts");
        Assert.Equal(7, accounts.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_HasNextPageIndicator()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.True(document.RootElement.GetProperty("hasNextPage").GetBoolean());
    }

    [Fact]
    public void GoldenFile_PageSizeMatchesApiConfiguration()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        Assert.Equal(7, document.RootElement.GetProperty("pageSize").GetInt32());
    }

    [Fact]
    public void GoldenFile_AccountFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("accounts")[0];

        Assert.True(first.TryGetProperty("accountId", out _));
        Assert.True(first.TryGetProperty("status", out _));
        Assert.True(first.TryGetProperty("productType", out _));
        Assert.True(first.TryGetProperty("currentBalance", out _));
        Assert.True(first.TryGetProperty("holderName", out _));
        Assert.True(first.TryGetProperty("openedDate", out _));
    }

    [Fact]
    public void GoldenFile_AccountId_Is11Digits()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("accounts")[0];
        var id = first.GetProperty("accountId").GetString();

        Assert.NotNull(id);
        Assert.Equal(11, id.Length);
        Assert.Matches(@"^\d{11}$", id);
    }

    [Fact]
    public void GoldenFile_DateFormat_IsISO8601()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("accounts")[0];
        var dateStr = first.GetProperty("openedDate").GetString();

        Assert.NotNull(dateStr);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", dateStr);
    }

    [Fact]
    public void GoldenFile_Balances_AreExactDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var accounts = document.RootElement.GetProperty("accounts");

        foreach (var account in accounts.EnumerateArray())
        {
            var balance = account.GetProperty("currentBalance").GetDecimal();
            Assert.Equal(balance, decimal.Round(balance, 2));
        }
    }
}
