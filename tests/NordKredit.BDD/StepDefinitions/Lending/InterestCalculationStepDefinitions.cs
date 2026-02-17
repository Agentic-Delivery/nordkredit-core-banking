using System.Globalization;
using NordKredit.Domain.Lending;
using TechTalk.SpecFlow;

namespace NordKredit.BDD.StepDefinitions.Lending;

/// <summary>
/// Step definitions for interest calculation and amortization schedule BDD scenarios (LND-BR-004).
/// COBOL source: Dedicated program (inferred from ACCT-GROUP-ID in CVACT01Y.cpy).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10, 19 (APR).
/// </summary>
[Binding]
[Scope(Feature = "Interest calculation and amortization schedule")]
public sealed class InterestCalculationStepDefinitions
{
    private decimal _calculatedInterest;
    private decimal _monthlyPayment;
    private IReadOnlyList<AmortizationEntry> _schedule = null!;

    [When(@"I calculate daily interest on balance (.+) at annual rate (.+) using Actual/360 for (\d+) days?")]
    public void WhenICalculateDailyInterest(decimal balance, decimal annualRate, int days) =>
        _calculatedInterest = InterestCalculationService.CalculateDailyInterest(
            balance, annualRate, DayCountConvention.Actual360, days);

    [Then(@"the calculated interest is (.+)")]
    public void ThenTheCalculatedInterestIs(decimal expected) =>
        Assert.Equal(expected, _calculatedInterest);

    [When(@"I calculate monthly payment for principal (.+) at annual rate (.+) for (\d+) months")]
    public void WhenICalculateMonthlyPayment(decimal principal, decimal annualRate, int termMonths) =>
        _monthlyPayment = InterestCalculationService.CalculateMonthlyPayment(
            principal, annualRate, termMonths);

    [Then(@"the monthly payment is (.+)")]
    public void ThenTheMonthlyPaymentIs(decimal expected) =>
        Assert.Equal(expected, _monthlyPayment);

    [When(@"I generate an amortization schedule for principal (.+) at annual rate (.+) for (\d+) months starting ""(.+)""")]
    public void WhenIGenerateAnAmortizationSchedule(
        decimal principal, decimal annualRate, int termMonths, string startDate) =>
        _schedule = InterestCalculationService.GenerateAmortizationSchedule(
            principal, annualRate, termMonths, DateTime.Parse(startDate, CultureInfo.InvariantCulture));

    [Then(@"the schedule contains (\d+) entries")]
    public void ThenTheScheduleContainsEntries(int expected) =>
        Assert.Equal(expected, _schedule.Count);

    [Then(@"the first period interest portion is (.+)")]
    public void ThenTheFirstPeriodInterestPortionIs(decimal expected) =>
        Assert.Equal(expected, _schedule[0].InterestPortion);

    [Then(@"the first period payment date is ""(.+)""")]
    public void ThenTheFirstPeriodPaymentDateIs(string expected) =>
        Assert.Equal(DateTime.Parse(expected, CultureInfo.InvariantCulture), _schedule[0].PaymentDate);

    [Then(@"the final period remaining principal is (.+)")]
    public void ThenTheFinalPeriodRemainingPrincipalIs(decimal expected) =>
        Assert.Equal(expected, _schedule[^1].RemainingPrincipal);
}
