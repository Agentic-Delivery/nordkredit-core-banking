using NordKredit.Domain.Lending;

namespace NordKredit.UnitTests.Lending;

/// <summary>
/// Tests for the AmortizationSchedule entity and interest calculation.
/// COBOL source: Dedicated program not yet in repository (inferred from ACCT-GROUP-ID).
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10, 19.
/// </summary>
public class AmortizationScheduleTests
{
    [Fact]
    public void AmortizationEntry_ShouldStoreAllFields()
    {
        var entry = new AmortizationEntry
        {
            PeriodNumber = 1,
            PaymentDate = new DateTime(2025, 2, 1),
            PaymentAmount = 2997.75m,
            PrincipalPortion = 497.75m,
            InterestPortion = 2500.00m,
            RemainingPrincipal = 499502.25m
        };

        Assert.Equal(1, entry.PeriodNumber);
        Assert.Equal(new DateTime(2025, 2, 1), entry.PaymentDate);
        Assert.Equal(2997.75m, entry.PaymentAmount);
        Assert.Equal(497.75m, entry.PrincipalPortion);
        Assert.Equal(2500.00m, entry.InterestPortion);
        Assert.Equal(499502.25m, entry.RemainingPrincipal);
    }

    [Fact]
    public void AmortizationEntry_PreservesDecimalPrecision()
    {
        // All intermediate values must use decimal for precision
        var entry = new AmortizationEntry
        {
            PaymentAmount = 2997.75m,
            PrincipalPortion = 497.75m,
            InterestPortion = 2500.00m
        };

        Assert.Equal(entry.PaymentAmount, entry.PrincipalPortion + entry.InterestPortion);
    }

    // =================================================================
    // LND-BR-004: Interest calculation
    // =================================================================

    [Fact]
    public void CalculateDailyInterest_ActualOver360_ReturnsCorrectAmount()
    {
        // Scenario 1: 100,000 * (0.085 / 360) * 1 = 23.611... -> 23.61 (rounded)
        var dailyInterest = InterestCalculationService.CalculateDailyInterest(
            100000.00m, 0.085m, DayCountConvention.Actual360, 1);

        Assert.Equal(23.61m, dailyInterest);
    }

    [Fact]
    public void CalculateDailyInterest_30Over360_ReturnsCorrectAmount()
    {
        // 100,000 * (0.085 / 360) * 30 = 708.33
        var monthlyInterest = InterestCalculationService.CalculateDailyInterest(
            100000.00m, 0.085m, DayCountConvention.Thirty360, 30);

        Assert.Equal(708.33m, monthlyInterest);
    }

    [Fact]
    public void CalculateDailyInterest_ZeroBalance_ReturnsZero()
    {
        // Scenario 2: No interest on zero balance
        var interest = InterestCalculationService.CalculateDailyInterest(
            0.00m, 0.085m, DayCountConvention.Actual360, 1);

        Assert.Equal(0m, interest);
    }

    [Fact]
    public void CalculateDailyInterest_NegativeBalance_ReturnsZero()
    {
        // Edge case: negative balance (overpayment) — no interest charged
        var interest = InterestCalculationService.CalculateDailyInterest(
            -500.00m, 0.085m, DayCountConvention.Actual360, 1);

        Assert.Equal(0m, interest);
    }

    [Fact]
    public void CalculateMonthlyPayment_StandardTermLoan_ReturnsCorrectAmount()
    {
        // Scenario 4: 500,000 at 6% for 360 months
        // Monthly payment ≈ 2997.75 (annuity formula)
        var payment = InterestCalculationService.CalculateMonthlyPayment(
            500000.00m, 0.06m, 360);

        Assert.Equal(2997.75m, payment);
    }

    [Fact]
    public void CalculateMonthlyPayment_ZeroRate_ReturnsPrincipalDividedByTerm()
    {
        // Zero interest: payment = principal / months
        var payment = InterestCalculationService.CalculateMonthlyPayment(
            120000.00m, 0.00m, 12);

        Assert.Equal(10000.00m, payment);
    }

    [Fact]
    public void GenerateSchedule_FirstPayment_SplitsCorrectly()
    {
        // Scenario 4: First payment of 500,000 at 6%/yr
        // Interest = 500,000 * 0.06/12 = 2500.00
        // Principal = 2997.75 - 2500.00 = 497.75
        var schedule = InterestCalculationService.GenerateAmortizationSchedule(
            500000.00m, 0.06m, 360, new DateTime(2025, 1, 1));

        Assert.Equal(360, schedule.Count);
        Assert.Equal(2500.00m, schedule[0].InterestPortion);
        Assert.Equal(497.75m, schedule[0].PrincipalPortion);
        Assert.Equal(499502.25m, schedule[0].RemainingPrincipal);
    }

    [Fact]
    public void GenerateSchedule_LastPayment_ReducesPrincipalToZero()
    {
        var schedule = InterestCalculationService.GenerateAmortizationSchedule(
            500000.00m, 0.06m, 360, new DateTime(2025, 1, 1));

        // Last payment should bring remaining principal to 0
        Assert.Equal(0m, schedule[^1].RemainingPrincipal);
    }

    [Fact]
    public void GenerateSchedule_EachPaymentHasDecreasingInterest()
    {
        var schedule = InterestCalculationService.GenerateAmortizationSchedule(
            100000.00m, 0.06m, 12, new DateTime(2025, 1, 1));

        for (int i = 1; i < schedule.Count; i++)
        {
            Assert.True(schedule[i].InterestPortion <= schedule[i - 1].InterestPortion,
                $"Interest at period {i + 1} should be <= period {i}");
        }
    }
}
