namespace NordKredit.Domain.Lending;

/// <summary>
/// Interest calculation and amortization schedule generation.
/// COBOL source: Dedicated program not yet in repository (inferred from ACCT-GROUP-ID in CVACT01Y.cpy).
/// Business rule: LND-BR-004 (interest calculation and amortization schedule).
/// Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10, 19 (APR calculation).
///              GDPR Art. 15 (right of access — calculation details must be available).
///
/// All intermediate calculations use decimal to avoid floating-point precision loss.
/// Final amounts are rounded to 2 decimal places (banker's rounding).
/// </summary>
public static class InterestCalculationService
{
    /// <summary>
    /// Calculates interest for a period using the specified day count convention.
    /// COBOL: Nightly interest calculation batch — must complete by 06:00 (batch SLA).
    /// Business rule: LND-BR-004.
    ///
    /// Formula: balance × (annualRate / 360) × daysInPeriod, rounded to 2 decimals.
    /// Returns 0 for zero or negative balances (no interest on overpayments).
    /// </summary>
    /// <param name="balance">Outstanding loan balance (ACCT-CURR-BAL).</param>
    /// <param name="annualRate">Annual interest rate as decimal (e.g., 0.085 for 8.5%).</param>
    /// <param name="convention">Day count convention (Actual/360 or 30/360).</param>
    /// <param name="daysInPeriod">Number of days in the calculation period.</param>
    /// <returns>Interest amount rounded to 2 decimal places.</returns>
    public static decimal CalculateDailyInterest(
        decimal balance,
        decimal annualRate,
        DayCountConvention convention,
        int daysInPeriod)
    {
        if (balance <= 0m)
        {
            return 0m;
        }

        var divisor = convention switch
        {
            DayCountConvention.Actual360 => 360m,
            DayCountConvention.Thirty360 => 360m,
            _ => throw new ArgumentOutOfRangeException(nameof(convention), convention, "Unsupported day count convention"),
        };
        var dailyRate = annualRate / divisor;
        var interest = balance * dailyRate * daysInPeriod;

        // Banker's rounding (MidpointRounding.ToEven) to minimize bias
        return Math.Round(interest, 2, MidpointRounding.ToEven);
    }

    /// <summary>
    /// Calculates the fixed monthly payment for a term loan (annuity formula).
    /// COBOL source: Dedicated program not yet in repository.
    /// Business rule: LND-BR-004.
    /// Regulations: Consumer Credit Directive Art. 10 (credit agreement terms).
    ///
    /// Formula: P × [r(1+r)^n] / [(1+r)^n - 1]
    /// where P = principal, r = monthly rate, n = number of payments.
    /// </summary>
    /// <param name="principal">Loan principal amount.</param>
    /// <param name="annualRate">Annual interest rate as decimal (e.g., 0.06 for 6%).</param>
    /// <param name="termMonths">Loan term in months.</param>
    /// <returns>Monthly payment amount rounded to 2 decimal places.</returns>
    public static decimal CalculateMonthlyPayment(
        decimal principal,
        decimal annualRate,
        int termMonths)
    {
        if (annualRate == 0m)
        {
            return Math.Round(principal / termMonths, 2, MidpointRounding.ToEven);
        }

        var monthlyRate = annualRate / 12m;

        // Use double for the power calculation, then convert back to decimal
        var ratePlusOne = (double)monthlyRate + 1.0;
        var compoundFactor = (decimal)Math.Pow(ratePlusOne, termMonths);

        var payment = principal * monthlyRate * compoundFactor / (compoundFactor - 1m);

        return Math.Round(payment, 2, MidpointRounding.ToEven);
    }

    /// <summary>
    /// Generates a full amortization schedule for a term loan.
    /// COBOL source: Dedicated program not yet in repository.
    /// Business rule: LND-BR-004.
    /// Regulations: Consumer Credit Directive Art. 10 (credit agreement information).
    ///
    /// Each period splits the fixed payment into interest and principal portions.
    /// The final payment is adjusted to bring remaining principal to exactly zero.
    /// </summary>
    /// <param name="principal">Loan principal amount.</param>
    /// <param name="annualRate">Annual interest rate as decimal.</param>
    /// <param name="termMonths">Loan term in months.</param>
    /// <param name="startDate">First payment date.</param>
    /// <returns>List of amortization entries, one per payment period.</returns>
    public static IReadOnlyList<AmortizationEntry> GenerateAmortizationSchedule(
        decimal principal,
        decimal annualRate,
        int termMonths,
        DateTime startDate)
    {
        var monthlyPayment = CalculateMonthlyPayment(principal, annualRate, termMonths);
        var monthlyRate = annualRate / 12m;
        var remainingPrincipal = principal;
        var entries = new List<AmortizationEntry>(termMonths);

        for (var period = 1; period <= termMonths; period++)
        {
            var interestPortion = Math.Round(remainingPrincipal * monthlyRate, 2, MidpointRounding.ToEven);
            var principalPortion = monthlyPayment - interestPortion;

            // Final payment adjustment to avoid rounding residual
            if (period == termMonths)
            {
                principalPortion = remainingPrincipal;
                var finalPayment = principalPortion + interestPortion;
                entries.Add(new AmortizationEntry
                {
                    PeriodNumber = period,
                    PaymentDate = startDate.AddMonths(period - 1),
                    PaymentAmount = finalPayment,
                    PrincipalPortion = principalPortion,
                    InterestPortion = interestPortion,
                    RemainingPrincipal = 0m
                });
            }
            else
            {
                remainingPrincipal -= principalPortion;
                entries.Add(new AmortizationEntry
                {
                    PeriodNumber = period,
                    PaymentDate = startDate.AddMonths(period - 1),
                    PaymentAmount = monthlyPayment,
                    PrincipalPortion = principalPortion,
                    InterestPortion = interestPortion,
                    RemainingPrincipal = remainingPrincipal
                });
            }
        }

        return entries;
    }
}
