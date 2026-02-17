using System.Text.Json;
using NordKredit.Domain.Lending;

namespace NordKredit.ComparisonTests.Lending;

/// <summary>
/// Comparison tests for amortization schedule output — parallel-run validation against mainframe output.
/// COBOL source: Dedicated amortization program (inferred from CVACT01Y.cpy ACCT-GROUP-ID).
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch.6, Consumer Credit Directive Art.10 (credit agreement information),
///              Art.19 (APR calculation), GDPR Art.15 (right of access — calculation details).
///
/// Golden file: Lending/GoldenFiles/amortization-schedule.json
/// Contains expected mainframe output for multiple amortization schedule scenarios.
///
/// Known intentional differences:
/// - Decimal precision: COBOL COMP-3 uses PIC S9(10)V99.
///   Migrated system uses .NET decimal with MidpointRounding.ToEven (banker's rounding).
/// - Final payment: Both systems adjust final payment to zero out remaining principal.
///
/// Parallel-run stubs (not yet connected to mainframe):
/// - CompareAmortizationSchedule_StandardLoan_MatchesMainframeOutput
/// - CompareAmortizationSchedule_ShortTerm_MatchesMainframeOutput
/// - CompareAmortizationSchedule_ZeroRate_MatchesMainframeOutput
/// </summary>
public class AmortizationScheduleComparisonTests
{
    private const string _goldenFilePath = "Lending/GoldenFiles/amortization-schedule.json";

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

        Assert.Equal(3, scenarios.GetArrayLength());
    }

    [Fact]
    public void GoldenFile_ScenarioFields_ArePresent()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var first = document.RootElement.GetProperty("scenarios")[0];

        Assert.True(first.TryGetProperty("name", out _));
        Assert.True(first.TryGetProperty("principal", out _));
        Assert.True(first.TryGetProperty("annualRate", out _));
        Assert.True(first.TryGetProperty("termMonths", out _));
        Assert.True(first.TryGetProperty("expectedMonthlyPayment", out _));
        Assert.True(first.TryGetProperty("firstPeriod", out _));
        Assert.True(first.TryGetProperty("lastPeriod", out _));
    }

    [Fact]
    public void StandardTermLoan_MonthlyPayment_MatchesMainframeOutput()
    {
        var scenario = GetScenario("StandardTermLoan_5Year");
        var principal = scenario.GetProperty("principal").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var termMonths = scenario.GetProperty("termMonths").GetInt32();
        var expected = scenario.GetProperty("expectedMonthlyPayment").GetDecimal();

        var actual = InterestCalculationService.CalculateMonthlyPayment(principal, annualRate, termMonths);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void StandardTermLoan_FirstPeriod_MatchesMainframeOutput()
    {
        var scenario = GetScenario("StandardTermLoan_5Year");
        var schedule = GenerateSchedule(scenario);
        var expectedFirst = scenario.GetProperty("firstPeriod");

        Assert.Equal(expectedFirst.GetProperty("periodNumber").GetInt32(), schedule[0].PeriodNumber);
        Assert.Equal(expectedFirst.GetProperty("paymentAmount").GetDecimal(), schedule[0].PaymentAmount);
        Assert.Equal(expectedFirst.GetProperty("principalPortion").GetDecimal(), schedule[0].PrincipalPortion);
        Assert.Equal(expectedFirst.GetProperty("interestPortion").GetDecimal(), schedule[0].InterestPortion);
        Assert.Equal(expectedFirst.GetProperty("remainingPrincipal").GetDecimal(), schedule[0].RemainingPrincipal);
    }

    [Fact]
    public void StandardTermLoan_LastPeriod_MatchesMainframeOutput()
    {
        var scenario = GetScenario("StandardTermLoan_5Year");
        var schedule = GenerateSchedule(scenario);
        var expectedLast = scenario.GetProperty("lastPeriod");
        var last = schedule[^1];

        Assert.Equal(expectedLast.GetProperty("periodNumber").GetInt32(), last.PeriodNumber);
        Assert.Equal(expectedLast.GetProperty("paymentAmount").GetDecimal(), last.PaymentAmount);
        Assert.Equal(expectedLast.GetProperty("principalPortion").GetDecimal(), last.PrincipalPortion);
        Assert.Equal(expectedLast.GetProperty("interestPortion").GetDecimal(), last.InterestPortion);
        Assert.Equal(expectedLast.GetProperty("remainingPrincipal").GetDecimal(), last.RemainingPrincipal);
    }

    [Fact]
    public void ShortTermLoan_MonthlyPayment_MatchesMainframeOutput()
    {
        var scenario = GetScenario("ShortTermPersonalLoan_1Year");
        var principal = scenario.GetProperty("principal").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var termMonths = scenario.GetProperty("termMonths").GetInt32();
        var expected = scenario.GetProperty("expectedMonthlyPayment").GetDecimal();

        var actual = InterestCalculationService.CalculateMonthlyPayment(principal, annualRate, termMonths);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void ShortTermLoan_LastPeriod_RemainingPrincipalIsZero()
    {
        var scenario = GetScenario("ShortTermPersonalLoan_1Year");
        var schedule = GenerateSchedule(scenario);

        Assert.Equal(0m, schedule[^1].RemainingPrincipal);
    }

    [Fact]
    public void ZeroRate_MonthlyPayment_MatchesMainframeOutput()
    {
        var scenario = GetScenario("ZeroRate_InterestFreeLoan");
        var principal = scenario.GetProperty("principal").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var termMonths = scenario.GetProperty("termMonths").GetInt32();
        var expected = scenario.GetProperty("expectedMonthlyPayment").GetDecimal();

        var actual = InterestCalculationService.CalculateMonthlyPayment(principal, annualRate, termMonths);

        Assert.Equal(expected, actual);
    }

    [Fact]
    public void ZeroRate_AllInterestPortions_AreZero()
    {
        var scenario = GetScenario("ZeroRate_InterestFreeLoan");
        var schedule = GenerateSchedule(scenario);

        foreach (var entry in schedule)
        {
            Assert.Equal(0m, entry.InterestPortion);
        }
    }

    [Fact]
    public void GoldenFile_Amounts_Use2DecimalPrecision()
    {
        var json = File.ReadAllText(_goldenFilePath);
        using var document = JsonDocument.Parse(json);
        var scenarios = document.RootElement.GetProperty("scenarios");

        foreach (var scenario in scenarios.EnumerateArray())
        {
            var payment = scenario.GetProperty("expectedMonthlyPayment").GetDecimal();
            Assert.Equal(payment, decimal.Round(payment, 2));
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

    private static IReadOnlyList<AmortizationEntry> GenerateSchedule(JsonElement scenario)
    {
        var principal = scenario.GetProperty("principal").GetDecimal();
        var annualRate = scenario.GetProperty("annualRate").GetDecimal();
        var termMonths = scenario.GetProperty("termMonths").GetInt32();

        return InterestCalculationService.GenerateAmortizationSchedule(
            principal, annualRate, termMonths, new DateTime(2025, 1, 15));
    }
}
