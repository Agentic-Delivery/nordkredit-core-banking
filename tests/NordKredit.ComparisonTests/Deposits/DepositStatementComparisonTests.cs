using System.Text.Json;

namespace NordKredit.ComparisonTests.Deposits;

/// <summary>
/// Comparison tests for deposit statement generation — parallel-run validation against mainframe output.
/// COBOL source: Dedicated statement generation batch program — interest posting and cycle statement.
/// Business rules: DEP-BR-004 (interest posting), DEP-BR-001 (account data).
/// Regulations: FSA FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.57 (information on transactions).
///
/// Golden file: Deposits/GoldenFiles/deposit-statement-output.json
/// Contains expected mainframe output for monthly statement generation batch.
///
/// Known intentional differences:
/// - Statement format: COBOL generates fixed-width print file.
///   Migrated system generates structured JSON. Content is semantically identical.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareStatementGeneration_MonthlyOutput_MatchesMainframeOutput
/// - CompareStatementGeneration_InterestPosting_MatchesMainframeOutput
/// - CompareStatementGeneration_NoActivity_MatchesMainframeOutput
/// </summary>
public class DepositStatementComparisonTests
{
    private const string _goldenFilePath = "Deposits/GoldenFiles/deposit-statement-output.json";

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
    public void GoldenFile_ContainsBatchSummaryFields()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        Assert.True(root.TryGetProperty("totalAccountsProcessed", out _));
        Assert.True(root.TryGetProperty("statementsGenerated", out _));
        Assert.True(root.TryGetProperty("skippedNoActivity", out _));
        Assert.True(root.TryGetProperty("interestPostedCount", out _));
        Assert.True(root.TryGetProperty("totalInterestPosted", out _));
    }

    [Fact]
    public void GoldenFile_ProcessedCount_EqualsGeneratedPlusSkipped()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var total = root.GetProperty("totalAccountsProcessed").GetInt32();
        var generated = root.GetProperty("statementsGenerated").GetInt32();
        var skipped = root.GetProperty("skippedNoActivity").GetInt32();

        Assert.Equal(total, generated + skipped);
    }

    [Fact]
    public void GoldenFile_StatementsCount_MatchesGeneratedCount()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var generated = root.GetProperty("statementsGenerated").GetInt32();
        var statements = root.GetProperty("statements").GetArrayLength();

        Assert.Equal(generated, statements);
    }

    [Fact]
    public void GoldenFile_StatementFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("statements")[0];

        Assert.True(first.TryGetProperty("accountId", out _));
        Assert.True(first.TryGetProperty("holderName", out _));
        Assert.True(first.TryGetProperty("openingBalance", out _));
        Assert.True(first.TryGetProperty("totalCredits", out _));
        Assert.True(first.TryGetProperty("totalDebits", out _));
        Assert.True(first.TryGetProperty("interestPosted", out _));
        Assert.True(first.TryGetProperty("closingBalance", out _));
        Assert.True(first.TryGetProperty("hasActivity", out _));
    }

    [Fact]
    public void GoldenFile_ClosingBalance_MatchesCalculation()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var statements = document.RootElement.GetProperty("statements");

        foreach (var stmt in statements.EnumerateArray())
        {
            var opening = stmt.GetProperty("openingBalance").GetDecimal();
            var credits = stmt.GetProperty("totalCredits").GetDecimal();
            var debits = stmt.GetProperty("totalDebits").GetDecimal();
            var interest = stmt.GetProperty("interestPosted").GetDecimal();
            var closing = stmt.GetProperty("closingBalance").GetDecimal();

            var expected = decimal.Round(opening + credits + debits + interest, 2);
            Assert.Equal(expected, closing);
        }
    }

    [Fact]
    public void GoldenFile_TotalInterestPosted_MatchesSumOfStatementInterest()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var totalInterest = root.GetProperty("totalInterestPosted").GetDecimal();
        var statements = root.GetProperty("statements");

        decimal sumOfInterest = 0;
        foreach (var stmt in statements.EnumerateArray())
        {
            sumOfInterest += stmt.GetProperty("interestPosted").GetDecimal();
        }

        Assert.Equal(totalInterest, sumOfInterest);
    }

    [Fact]
    public void GoldenFile_InterestPostedCount_MatchesNonZeroInterestStatements()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var root = document.RootElement;

        var interestPostedCount = root.GetProperty("interestPostedCount").GetInt32();
        var statements = root.GetProperty("statements");

        var nonZeroCount = 0;
        foreach (var stmt in statements.EnumerateArray())
        {
            if (stmt.GetProperty("interestPosted").GetDecimal() != 0)
            {
                nonZeroCount++;
            }
        }

        Assert.Equal(interestPostedCount, nonZeroCount);
    }

    [Fact]
    public void GoldenFile_AllStatements_HaveActivity()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var statements = document.RootElement.GetProperty("statements");

        foreach (var stmt in statements.EnumerateArray())
        {
            Assert.True(stmt.GetProperty("hasActivity").GetBoolean());
        }
    }

    [Fact]
    public void GoldenFile_Amounts_UseExactDecimalPrecision()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var statements = document.RootElement.GetProperty("statements");

        foreach (var stmt in statements.EnumerateArray())
        {
            var opening = stmt.GetProperty("openingBalance").GetDecimal();
            Assert.Equal(opening, decimal.Round(opening, 2));

            var closing = stmt.GetProperty("closingBalance").GetDecimal();
            Assert.Equal(closing, decimal.Round(closing, 2));

            var credits = stmt.GetProperty("totalCredits").GetDecimal();
            Assert.Equal(credits, decimal.Round(credits, 2));

            var debits = stmt.GetProperty("totalDebits").GetDecimal();
            Assert.Equal(debits, decimal.Round(debits, 2));
        }
    }
}
