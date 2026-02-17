using System.Text.Json;

namespace NordKredit.ComparisonTests.Transactions;

/// <summary>
/// Comparison tests for batch transaction posting — parallel-run validation against mainframe output.
/// COBOL source: CBTRN02C.cbl:424-579 (POST-TRANSACTIONS — TCATBAL upsert, ACCT balance update, WRITE TRANFILE).
/// Business rule: TRN-BR-005 (daily posting with validation and balance updates).
/// Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
/// DORA Art.11 (ICT change management).
///
/// Golden file: Transactions/GoldenFiles/transaction-posting-batch.json
/// Contains expected mainframe output captured during parallel-run testing.
///
/// Known intentional differences:
/// - Atomicity: Mainframe posts transactions sequentially with partial failures possible (non-atomic).
///   Migrated system wraps each post in IUnitOfWork transaction (atomic per transaction) —
///   intentional improvement for data integrity.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareTransactionPosting_BatchResult_MatchesMainframeOutput
/// - CompareTransactionPosting_BalanceUpdates_MatchesMainframeOutput
/// - CompareTransactionPosting_CycleCreditDebit_MatchesMainframeOutput
/// </summary>
public class TransactionPostingComparisonTests
{
    private const string _goldenFilePath = "Transactions/GoldenFiles/transaction-posting-batch.json";

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
    public void GoldenFile_ContainsPostingSummary()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("totalProcessed", out _));
        Assert.True(root.TryGetProperty("postedCount", out _));
        Assert.True(root.TryGetProperty("skippedCount", out _));
        Assert.True(root.TryGetProperty("failedCount", out _));
    }

    [Fact]
    public void GoldenFile_PostingCounts_AreConsistent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var total = root.GetProperty("totalProcessed").GetInt32();
        var posted = root.GetProperty("postedCount").GetInt32();
        var skipped = root.GetProperty("skippedCount").GetInt32();
        var failed = root.GetProperty("failedCount").GetInt32();

        Assert.Equal(total, posted + skipped + failed);
    }

    [Fact]
    public void GoldenFile_DocumentsKnownDifferences()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("_knownDifferences", out var differences));
        Assert.True(differences.TryGetProperty("atomicity", out _));
    }

    [Fact]
    public void GoldenFile_BalanceUpdates_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var balances = document.RootElement.GetProperty("balanceUpdates");

        Assert.True(balances.GetArrayLength() > 0);
    }

    [Fact]
    public void GoldenFile_BalanceUpdates_HaveRequiredFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("balanceUpdates")[0];

        Assert.True(first.TryGetProperty("accountId", out _));
        Assert.True(first.TryGetProperty("previousBalance", out _));
        Assert.True(first.TryGetProperty("newBalance", out _));
        Assert.True(first.TryGetProperty("totalCredits", out _));
        Assert.True(first.TryGetProperty("totalDebits", out _));
    }

    [Fact]
    public void GoldenFile_BalanceUpdates_Arithmetic_IsCorrect()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var balances = document.RootElement.GetProperty("balanceUpdates");

        foreach (var balance in balances.EnumerateArray())
        {
            var previous = balance.GetProperty("previousBalance").GetDecimal();
            var expected = balance.GetProperty("newBalance").GetDecimal();
            var credits = balance.GetProperty("totalCredits").GetDecimal();
            var debits = balance.GetProperty("totalDebits").GetDecimal();

            Assert.Equal(expected, previous + credits + debits);
        }
    }

    [Fact]
    public void GoldenFile_Amounts_UseExactDecimalPrecision()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var balances = document.RootElement.GetProperty("balanceUpdates");

        foreach (var balance in balances.EnumerateArray())
        {
            var prev = balance.GetProperty("previousBalance").GetDecimal();
            var next = balance.GetProperty("newBalance").GetDecimal();

            Assert.Equal(prev, decimal.Round(prev, 2));
            Assert.Equal(next, decimal.Round(next, 2));
        }
    }
}
