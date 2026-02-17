@BDD @Deposits @DEP-BR-004 @DEP-BR-006
Feature: Deposit interest calculation with tiered rates
    As a bank system
    I want to calculate daily interest for deposit accounts based on product rate schedules
    So that interest accrual complies with FSA disclosure requirements

    COBOL source: Dedicated interest calculation batch program.
    Business rules: DEP-BR-004 (interest calculation and accrual), DEP-BR-006 (tiered rates).
    Regulations: FSA FFFS 2014:5 Ch. 3 & 6, Deposit Guarantee Directive, PSD2 Art. 57.

    # ===================================================================
    # DEP-BR-004: Flat rate daily interest calculation
    # Formula: balance * annualRate / dayCountBasis
    # 4 decimal intermediate precision, MidpointRounding.AwayFromZero
    # ===================================================================

    Scenario: Daily interest for flat rate product using 365-day basis
        Given a savings product with annual rate 0.025 and day count basis 365
        When I calculate daily deposit interest on balance 100000.00
        Then the daily interest is 6.8493

    Scenario: Daily interest for flat rate product using 360-day basis
        Given a savings product with annual rate 0.025 and day count basis 360
        When I calculate daily deposit interest on balance 100000.00
        Then the daily interest is 6.9444

    Scenario: Zero balance yields zero interest
        Given a savings product with annual rate 0.025 and day count basis 365
        When I calculate daily deposit interest on balance 0.00
        Then the daily interest is 0.0000

    Scenario: Negative balance yields zero interest
        Given a savings product with annual rate 0.025 and day count basis 365
        When I calculate daily deposit interest on balance -5000.00
        Then the daily interest is 0.0000

    # ===================================================================
    # DEP-BR-006: Tiered rate interest calculation
    # COBOL source: CVTRA02Y.cpy (disclosure group records)
    # Tier 1: base rate up to Tier1Limit
    # Tier 2: Tier2Rate from Tier1Limit to Tier2Limit
    # Tier 3: Tier3Rate above Tier2Limit
    # ===================================================================

    Scenario: Tiered interest when balance is within tier 1
        Given a tiered savings product with
            | AnnualRate | Tier1Limit | Tier2Rate | Tier2Limit | Tier3Rate | DayCountBasis |
            | 0.02       | 50000      | 0.03      | 100000     | 0.04      | 365           |
        When I calculate daily deposit interest on balance 30000.00
        Then the daily interest is 1.6438

    Scenario: Tiered interest when balance spans tier 1 and tier 2
        Given a tiered savings product with
            | AnnualRate | Tier1Limit | Tier2Rate | Tier2Limit | Tier3Rate | DayCountBasis |
            | 0.02       | 50000      | 0.03      | 100000     | 0.04      | 365           |
        When I calculate daily deposit interest on balance 75000.00
        # Tier 1: 50000 * 0.02 / 365 = 2.7397
        # Tier 2: 25000 * 0.03 / 365 = 2.0548
        # Total: 4.7945
        Then the daily interest is 4.7945

    Scenario: Tiered interest when balance spans all three tiers
        Given a tiered savings product with
            | AnnualRate | Tier1Limit | Tier2Rate | Tier2Limit | Tier3Rate | DayCountBasis |
            | 0.02       | 50000      | 0.03      | 100000     | 0.04      | 365           |
        When I calculate daily deposit interest on balance 150000.00
        # Tier 1: 50000 * 0.02 / 365 = 2.7397
        # Tier 2: 50000 * 0.03 / 365 = 4.1096
        # Tier 3: 50000 * 0.04 / 365 = 5.4795
        # Total: 12.3288
        Then the daily interest is 12.3288
