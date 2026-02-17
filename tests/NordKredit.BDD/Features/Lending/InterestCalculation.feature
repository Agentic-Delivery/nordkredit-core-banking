@BDD @Lending @LND-BR-004
Feature: Interest calculation and amortization schedule
    As a bank system
    I want to calculate interest and generate amortization schedules
    So that loan terms comply with Consumer Credit Directive disclosure requirements

    COBOL source: Dedicated program (inferred from ACCT-GROUP-ID in CVACT01Y.cpy).
    Business rule: LND-BR-004 (interest calculation and amortization schedule).
    Regulations: FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10, 19 (APR).

    # ===================================================================
    # Daily interest calculation (Actual/360 convention)
    # Formula: balance × (annualRate / 360) × daysInPeriod
    # ===================================================================

    Scenario: Daily interest calculated for active loan balance
        # 100000 × (0.085 / 360) × 1 = 23.611... → rounded to 23.61
        When I calculate daily interest on balance 100000.00 at annual rate 0.085 using Actual/360 for 1 day
        Then the calculated interest is 23.61

    Scenario: Daily interest for 30-day period
        # 100000 × (0.085 / 360) × 30 = 708.333... → rounded to 708.33
        When I calculate daily interest on balance 100000.00 at annual rate 0.085 using Actual/360 for 30 days
        Then the calculated interest is 708.33

    Scenario: Zero balance yields zero interest
        When I calculate daily interest on balance 0.00 at annual rate 0.085 using Actual/360 for 1 day
        Then the calculated interest is 0.00

    Scenario: Negative balance (overpayment) yields zero interest
        When I calculate daily interest on balance -5000.00 at annual rate 0.085 using Actual/360 for 1 day
        Then the calculated interest is 0.00

    # ===================================================================
    # Monthly payment calculation (annuity formula)
    # Formula: P × [r(1+r)^n] / [(1+r)^n - 1]
    # Consumer Credit Directive Art. 10 — loan terms disclosure
    # ===================================================================

    Scenario: Monthly payment for term loan
        When I calculate monthly payment for principal 500000.00 at annual rate 0.06 for 360 months
        Then the monthly payment is 2997.75

    Scenario: Monthly payment with zero interest rate
        # 240000 / 12 = 20000
        When I calculate monthly payment for principal 240000.00 at annual rate 0.00 for 12 months
        Then the monthly payment is 20000.00

    # ===================================================================
    # Amortization schedule generation
    # Consumer Credit Directive Art. 10 — credit agreement information
    # ===================================================================

    Scenario: Amortization schedule has correct number of periods
        When I generate an amortization schedule for principal 120000.00 at annual rate 0.06 for 12 months starting "2025-01-01"
        Then the schedule contains 12 entries

    Scenario: First period of amortization schedule is correct
        When I generate an amortization schedule for principal 120000.00 at annual rate 0.06 for 12 months starting "2025-01-01"
        Then the first period interest portion is 600.00
        And the first period payment date is "2025-01-01"

    Scenario: Final period brings remaining principal to zero
        When I generate an amortization schedule for principal 120000.00 at annual rate 0.06 for 12 months starting "2025-01-01"
        Then the final period remaining principal is 0.00
