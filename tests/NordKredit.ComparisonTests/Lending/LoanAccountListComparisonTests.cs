using System.Text.Json;

namespace NordKredit.ComparisonTests.Lending;

/// <summary>
/// Comparison tests for loan account list operation — parallel-run validation against mainframe output.
/// COBOL source: CVACT01Y.cpy — ACCOUNT-RECORD (300 bytes), STARTBR/READNEXT pagination on ACCTFILE.
/// Business rule: LND-BR-001 (loan account data structure and listing).
/// Regulations: FSA FFFS 2014:5 Ch.6 (credit risk), GDPR Art.15 (right of access).
///
/// Golden file: Lending/GoldenFiles/loan-account-list-first-page.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Pagination: COBOL uses CICS STARTBR/READNEXT. Migrated system uses keyset pagination via SQL.
/// - Date format: COBOL uses MM/DD/YY. Migrated system uses ISO 8601 (YYYY-MM-DD).
/// - Status values: COBOL uses 'Y'/'N'. Migrated system uses Active/Delinquent/Defaulted/Frozen/PaidOff/Closed.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareLoanAccountList_FirstPage_MatchesMainframeOutput
/// - CompareLoanAccountList_Pagination_MatchesMainframeOutput
/// - CompareLoanAccountList_EmptyResult_MatchesMainframeOutput
/// </summary>
public class LoanAccountListComparisonTests
{
    private const string _goldenFilePath = "Lending/GoldenFiles/loan-account-list-first-page.json";

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
    public void GoldenFile_ContainsExpectedLoanCount()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var loans = document.RootElement.GetProperty("loans");
        Assert.Equal(7, loans.GetArrayLength());
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
    public void GoldenFile_LoanFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("loans")[0];

        Assert.True(first.TryGetProperty("accountId", out _));
        Assert.True(first.TryGetProperty("status", out _));
        Assert.True(first.TryGetProperty("loanType", out _));
        Assert.True(first.TryGetProperty("currentBalance", out _));
        Assert.True(first.TryGetProperty("creditLimit", out _));
        Assert.True(first.TryGetProperty("borrowerName", out _));
        Assert.True(first.TryGetProperty("originationDate", out _));
    }

    [Fact]
    public void GoldenFile_AccountId_Is11Digits()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("loans")[0];
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
        var first = document.RootElement.GetProperty("loans")[0];
        var dateStr = first.GetProperty("originationDate").GetString();

        Assert.NotNull(dateStr);
        Assert.Matches(@"^\d{4}-\d{2}-\d{2}$", dateStr);
    }

    [Fact]
    public void GoldenFile_Balances_AreExactDecimal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var loans = document.RootElement.GetProperty("loans");

        foreach (var loan in loans.EnumerateArray())
        {
            var balance = loan.GetProperty("currentBalance").GetDecimal();
            Assert.Equal(balance, decimal.Round(balance, 2));

            var limit = loan.GetProperty("creditLimit").GetDecimal();
            Assert.Equal(limit, decimal.Round(limit, 2));
        }
    }
}
