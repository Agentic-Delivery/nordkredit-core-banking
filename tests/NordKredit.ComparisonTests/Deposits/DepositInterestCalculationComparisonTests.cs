using System.Text.Json;
using NordKredit.Domain.Deposits;

namespace NordKredit.ComparisonTests.Deposits;

/// <summary>
/// Comparison tests for deposit interest calculation — parallel-run validation against mainframe output.
/// COBOL source: Dedicated interest calculation batch program — daily interest with tiered rates.
/// Business rules: DEP-BR-004 (interest calculation and accrual), DEP-BR-006 (tiered rates).
/// Regulations: FSA FFFS 2014:5 Ch.3 &amp; 6, Deposit Guarantee Directive (coverage includes accrued interest), PSD2 Art.57.
///
/// Golden file: Deposits/GoldenFiles/deposit-interest-calculation.json
/// Contains expected mainframe output for multiple interest calculation scenarios.
///
/// Known intentional differences:
/// - Decimal precision: COBOL COMP-3 uses PIC S9(10)V9999.
///   Migrated system uses .NET decimal with 4-decimal intermediate precision and MidpointRounding.AwayFromZero.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareInterestCalculation_FlatRate_MatchesMainframeOutput
/// - CompareInterestCalculation_TieredRate_MatchesMainframeOutput
/// - CompareInterestCalculation_ThreeTier_MatchesMainframeOutput
/// </summary>
public class DepositInterestCalculationComparisonTests
{
    private const string _goldenFilePath = "Deposits/GoldenFiles/deposit-interest-calculation.json";

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
    public void GoldenFile_ContainsAllScenarios()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var scenarios = document.RootElement.GetProperty("scenarios");

        Assert.Equal(5, scenarios.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_ScenarioFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("scenarios")[0];

        Assert.True(first.TryGetProperty("name", out _));
        Assert.True(first.TryGetProperty("balance", out _));
        Assert.True(first.TryGetProperty("annualRate", out _));
        Assert.True(first.TryGetProperty("dayCountBasis", out _));
        Assert.True(first.TryGetProperty("expectedDailyInterest", out _));
    }

    [Fact]
    public void FlatRate_DailyInterest_MatchesMainframeOutput()
    {
        var scenario = GetScenario("FlatRate_DemandSavings");
        var product = BuildProduct(scenario);
        var balance = scenario.GetProperty("balance").GetDecimal();
        var expected = scenario.GetProperty("expectedDailyInterest").GetDecimal();

        var actual = InterestCalculation.CalculateDailyInterest(balance, product);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void TieredRate_DailyInterest_MatchesMainframeOutput()
    {
        var scenario = GetScenario("TieredRate_ChildrensSavings");
        var product = BuildProduct(scenario);
        var balance = scenario.GetProperty("balance").GetDecimal();
        var expected = scenario.GetProperty("expectedDailyInterest").GetDecimal();

        var actual = InterestCalculation.CalculateDailyInterest(balance, product);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void ThreeTier_DailyInterest_MatchesMainframeOutput()
    {
        var scenario = GetScenario("ThreeTier_BusinessDeposit");
        var product = BuildProduct(scenario);
        var balance = scenario.GetProperty("balance").GetDecimal();
        var expected = scenario.GetProperty("expectedDailyInterest").GetDecimal();

        var actual = InterestCalculation.CalculateDailyInterest(balance, product);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void ZeroBalance_ReturnsZeroInterest()
    {
        var scenario = GetScenario("ZeroBalance_NoInterest");
        var product = BuildProduct(scenario);
        var balance = scenario.GetProperty("balance").GetDecimal();

        var actual = InterestCalculation.CalculateDailyInterest(balance, product);

        Assert.Equal(0m, actual);
    }

    [Fact]
    public void NegativeBalance_ReturnsZeroInterest()
    {
        var scenario = GetScenario("NegativeBalance_NoInterest");
        var product = BuildProduct(scenario);
        var balance = scenario.GetProperty("balance").GetDecimal();

        var actual = InterestCalculation.CalculateDailyInterest(balance, product);

        Assert.Equal(0m, actual);
    }

    [Fact]
    public void GoldenFile_ExpectedInterest_Uses4DecimalPrecision()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var scenarios = document.RootElement.GetProperty("scenarios");

        foreach (var scenario in scenarios.EnumerateArray())
        {
            var interest = scenario.GetProperty("expectedDailyInterest").GetDecimal();
            Assert.Equal(interest, decimal.Round(interest, 4));
        }
    }

    private static JsonElement GetScenario(string name)
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var scenarios = document.RootElement.GetProperty("scenarios");

        foreach (var scenario in scenarios.EnumerateArray())
        {
            if (scenario.GetProperty("name").GetString() == name)
            {
                return scenario.Clone();
            }
        }

        throw new InvalidOperationException($"Scenario '{name}' not found in golden file");
    }

    private static SavingsProduct BuildProduct(JsonElement scenario)
    {
        var product = new SavingsProduct
        {
            AnnualRate = scenario.GetProperty("annualRate").GetDecimal(),
            DayCountBasis = scenario.GetProperty("dayCountBasis").GetInt32()
        };

        if (scenario.TryGetProperty("tier1Limit", out var tier1Limit) &&
            tier1Limit.ValueKind != JsonValueKind.Null)
        {
            product.Tier1Limit = tier1Limit.GetDecimal();
        }

        if (scenario.TryGetProperty("tier2Rate", out var tier2Rate) &&
            tier2Rate.ValueKind != JsonValueKind.Null)
        {
            product.Tier2Rate = tier2Rate.GetDecimal();
        }

        if (scenario.TryGetProperty("tier2Limit", out var tier2Limit) &&
            tier2Limit.ValueKind != JsonValueKind.Null)
        {
            product.Tier2Limit = tier2Limit.GetDecimal();
        }

        if (scenario.TryGetProperty("tier3Rate", out var tier3Rate) &&
            tier3Rate.ValueKind != JsonValueKind.Null)
        {
            product.Tier3Rate = tier3Rate.GetDecimal();
        }

        return product;
    }
}
