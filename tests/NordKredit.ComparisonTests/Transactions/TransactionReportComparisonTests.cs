using System.Text.Json;

namespace NordKredit.ComparisonTests.Transactions;

/// <summary>
/// Comparison tests for daily transaction detail report — parallel-run validation against mainframe output.
/// COBOL source: CBTRN03C.cbl:159-373 (TRANSACTION-DETAIL-REPORT — card grouping, page totals, grand total).
/// Business rule: TRN-BR-006 (daily transaction detail report generation).
/// Regulations: FSA FFFS 2014:5 Ch.7 (financial reporting), AML 2017:11 Para.3 (transaction monitoring).
///
/// Golden file: Transactions/GoldenFiles/transaction-daily-report.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// The report is structured as:
/// - Detail lines grouped by card number
/// - Page totals every 20 lines
/// - Account totals on card number change
/// - Grand total at end
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareTransactionReport_DailyOutput_MatchesMainframeOutput
/// - CompareTransactionReport_Totals_MatchesMainframeOutput
/// - CompareTransactionReport_Grouping_MatchesMainframeOutput
/// </summary>
public class TransactionReportComparisonTests
{
    private const string _goldenFilePath = "Transactions/GoldenFiles/transaction-daily-report.json";

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
    public void GoldenFile_ContainsReportSummaryFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("totalTransactions", out _));
        Assert.True(root.TryGetProperty("detailLineCount", out _));
        Assert.True(root.TryGetProperty("pageCount", out _));
        Assert.True(root.TryGetProperty("accountGroupCount", out _));
        Assert.True(root.TryGetProperty("grandTotal", out _));
    }

    [Fact]
    public void GoldenFile_DetailLineCount_MatchesTotalTransactions()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var total = root.GetProperty("totalTransactions").GetInt32();
        var lineCount = root.GetProperty("detailLineCount").GetInt32();
        var lines = root.GetProperty("lines").GetArrayLength();

        Assert.Equal(total, lineCount);
        Assert.Equal(total, lines);
    }

    [Fact]
    public void GoldenFile_ReportLines_HaveRequiredFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("lines")[0];

        Assert.True(first.TryGetProperty("transactionId", out _));
        Assert.True(first.TryGetProperty("accountId", out _));
        Assert.True(first.TryGetProperty("typeDescription", out _));
        Assert.True(first.TryGetProperty("categoryDescription", out _));
        Assert.True(first.TryGetProperty("source", out _));
        Assert.True(first.TryGetProperty("amount", out _));
        Assert.True(first.TryGetProperty("cardNumber", out _));
    }

    [Fact]
    public void GoldenFile_GrandTotal_MatchesSumOfLineAmounts()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var grandTotal = root.GetProperty("grandTotal").GetDecimal();
        var lines = root.GetProperty("lines");

        decimal sumOfAmounts = 0;
        foreach (var line in lines.EnumerateArray())
        {
            sumOfAmounts += line.GetProperty("amount").GetDecimal();
        }

        Assert.Equal(grandTotal, sumOfAmounts);
    }

    [Fact]
    public void GoldenFile_AccountTotals_MatchGroupedLineAmounts()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var accountTotals = root.GetProperty("accountTotals");
        var lines = root.GetProperty("lines");

        foreach (var acctTotal in accountTotals.EnumerateArray())
        {
            var cardNumber = acctTotal.GetProperty("cardNumber").GetString();
            var expectedTotal = acctTotal.GetProperty("total").GetDecimal();

            decimal groupSum = 0;
            foreach (var line in lines.EnumerateArray())
            {
                if (line.GetProperty("cardNumber").GetString() == cardNumber)
                {
                    groupSum += line.GetProperty("amount").GetDecimal();
                }
            }

            Assert.Equal(expectedTotal, groupSum);
        }
    }

    [Fact]
    public void GoldenFile_AccountGroupCount_MatchesDistinctCards()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var groupCount = root.GetProperty("accountGroupCount").GetInt32();
        var accountTotals = root.GetProperty("accountTotals").GetArrayLength();

        Assert.Equal(groupCount, accountTotals);
    }

    [Fact]
    public void GoldenFile_PageTotals_SumToGrandTotal()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var grandTotal = root.GetProperty("grandTotal").GetDecimal();
        var pageTotals = root.GetProperty("pageTotals");

        decimal sumOfPages = 0;
        foreach (var pt in pageTotals.EnumerateArray())
        {
            sumOfPages += pt.GetDecimal();
        }

        Assert.Equal(grandTotal, sumOfPages);
    }

    [Fact]
    public void GoldenFile_Amounts_UseExactDecimalPrecision()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var lines = document.RootElement.GetProperty("lines");

        foreach (var line in lines.EnumerateArray())
        {
            var amount = line.GetProperty("amount").GetDecimal();
            Assert.Equal(amount, decimal.Round(amount, 2));
        }
    }
}
