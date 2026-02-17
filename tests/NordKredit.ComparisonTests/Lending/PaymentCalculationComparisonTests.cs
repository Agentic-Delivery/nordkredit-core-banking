using System.Text.Json;
using NordKredit.Domain.Lending;

namespace NordKredit.ComparisonTests.Lending;

/// <summary>
/// Comparison tests for loan payment calculations — parallel-run validation against mainframe output.
/// COBOL source: Dedicated interest calculation batch program — daily interest with Actual/360 convention.
/// Business rules: LND-BR-004 (interest calculation), LND-BR-005 (repayment processing).
/// Regulations: FSA FFFS 2014:5 Ch.6, Consumer Credit Directive Art.10/16 (early repayment right),
///              Swedish Consumer Credit Act (konsumentkreditlagen 2010:1846) §36.
///
/// Golden file: Lending/GoldenFiles/payment-calculation.json
/// Contains expected mainframe output for interest calculation and early repayment scenarios.
///
/// Known intentional differences:
/// - Decimal precision: COBOL COMP-3 uses PIC S9(10)V99.
///   Migrated system uses .NET decimal with MidpointRounding.ToEven (banker's rounding).
/// - Day count convention: Both systems use Actual/360 for SEK lending as per FSA convention.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - ComparePaymentCalculation_DailyInterest_MatchesMainframeOutput
/// - ComparePaymentCalculation_EarlyRepayment_MatchesMainframeOutput
/// - ComparePaymentCalculation_ZeroBalance_MatchesMainframeOutput
/// </summary>
public class PaymentCalculationComparisonTests
{
    private const string _goldenFilePath = "Lending/GoldenFiles/payment-calculation.json";

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
    public void GoldenFile_ContainsAllInterestScenarios()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var scenarios = document.RootElement.GetProperty("interestScenarios");

        Assert.Equal(5, scenarios.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_InterestScenarioFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("interestScenarios")[0];

        Assert.True(first.TryGetProperty("name", out _));
        Assert.True(first.TryGetProperty("balance", out _));
        Assert.True(first.TryGetProperty("annualRate", out _));
        Assert.True(first.TryGetProperty("dayCountConvention", out _));
        Assert.True(first.TryGetProperty("daysInPeriod", out _));
        Assert.True(first.TryGetProperty("expectedInterest", out _));
    }

    [Fact]
    public void StandardTermLoan_DailyInterest_MatchesMainframeOutput()
    {
        var scenario = GetInterestScenario("StandardTermLoan_30Days");
        var balance = scenario.GetProperty("balance").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var days = scenario.GetProperty("daysInPeriod").GetInt32();
        var expected = scenario.GetProperty("expectedInterest").GetDecimal();

        var actual = InterestCalculationService.CalculateDailyInterest(
            balance, annualRate, DayCountConvention.Actual360, days);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void PersonalLoan_DailyInterest_MatchesMainframeOutput()
    {
        var scenario = GetInterestScenario("PersonalLoan_30Days");
        var balance = scenario.GetProperty("balance").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var days = scenario.GetProperty("daysInPeriod").GetInt32();
        var expected = scenario.GetProperty("expectedInterest").GetDecimal();

        var actual = InterestCalculationService.CalculateDailyInterest(
            balance, annualRate, DayCountConvention.Actual360, days);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void Mortgage_DailyInterest_MatchesMainframeOutput()
    {
        var scenario = GetInterestScenario("Mortgage_30Days");
        var balance = scenario.GetProperty("balance").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var days = scenario.GetProperty("daysInPeriod").GetInt32();
        var expected = scenario.GetProperty("expectedInterest").GetDecimal();

        var actual = InterestCalculationService.CalculateDailyInterest(
            balance, annualRate, DayCountConvention.Actual360, days);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void ZeroBalance_ReturnsZeroInterest()
    {
        var scenario = GetInterestScenario("ZeroBalance_NoInterest");
        var balance = scenario.GetProperty("balance").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var days = scenario.GetProperty("daysInPeriod").GetInt32();

        var actual = InterestCalculationService.CalculateDailyInterest(
            balance, annualRate, DayCountConvention.Actual360, days);

        Assert.Equal(0m, actual);
    }

    [Fact]
    public void NegativeBalance_ReturnsZeroInterest()
    {
        var scenario = GetInterestScenario("NegativeBalance_NoInterest");
        var balance = scenario.GetProperty("balance").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var days = scenario.GetProperty("daysInPeriod").GetInt32();

        var actual = InterestCalculationService.CalculateDailyInterest(
            balance, annualRate, DayCountConvention.Actual360, days);

        Assert.Equal(0m, actual);
    }

    [Fact]
    public void EarlyRepayment_AccruedInterest_MatchesMainframeOutput()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var repayment = document.RootElement.GetProperty("earlyRepaymentScenario").Clone();

        var principal = repayment.GetProperty("outstandingPrincipal").GetDecimal();
        var annualRate = repayment.GetProperty("annualRate").GetDecimal();
        var days = repayment.GetProperty("daysAccrued").GetInt32();
        var expectedInterest = repayment.GetProperty("accruedInterest").GetDecimal();

        var actual = InterestCalculationService.CalculateDailyInterest(
            principal, annualRate, DayCountConvention.Actual360, days);

        Assert.Equal(expectedInterest, actual);
    }

    [Fact]
    public void EarlyRepayment_TotalAmount_MatchesMainframeOutput()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var repayment = document.RootElement.GetProperty("earlyRepaymentScenario").Clone();

        var principal = repayment.GetProperty("outstandingPrincipal").GetDecimal();
        var accruedInterest = repayment.GetProperty("accruedInterest").GetDecimal();
        var expectedTotal = repayment.GetProperty("totalRepaymentAmount").GetDecimal();

        Assert.Equal(expectedTotal, principal + accruedInterest);
    }

    [Fact]
    public void GoldenFile_ExpectedInterest_Uses2DecimalPrecision()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var scenarios = document.RootElement.GetProperty("interestScenarios");

        foreach (var scenario in scenarios.EnumerateArray())
        {
            var interest = scenario.GetProperty("expectedInterest").GetDecimal();
            Assert.Equal(interest, decimal.Round(interest, 2));
        }
    }

    private static JsonElement GetInterestScenario(string name)
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var scenarios = document.RootElement.GetProperty("interestScenarios");

        foreach (var scenario in scenarios.EnumerateArray())
        {
            if (scenario.GetProperty("name").GetString() == name)
            {
                return scenario.Clone();
            }
        }

        throw new InvalidOperationException($"Interest scenario '{name}' not found in golden file");
    }
}
