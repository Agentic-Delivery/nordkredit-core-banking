@BDD @Lending @LND-BR-006
Feature: Collateral management and LTV ratio calculation
    As a bank system
    I want to calculate loan-to-value ratios and manage collateral
    So that credit risk mitigation complies with FSA and CRR requirements

    COBOL source: Dedicated program (inferred VSAM COLLATERAL file).
    Business rule: LND-BR-006 (collateral management and valuation).
    Regulations: FSA FFFS 2014:5 Ch. 6, 8 (assets), CRR Art. 194-217,
                 Finansinspektionen mortgage cap (85% LTV).

    # ===================================================================
    # LTV ratio calculation
    # Formula: loanBalance / collateralValue × 100
    # Finansinspektionen mortgage cap: 85%
    # ===================================================================

    Scenario: LTV ratio calculated correctly for a mortgage
        Given a collateral valued at 2000000.00
        When I calculate the LTV ratio for a loan balance of 1500000.00
        Then the LTV ratio is 75.00

    Scenario: LTV ratio within Finansinspektionen 85% cap
        Given a collateral valued at 2000000.00
        When I calculate the LTV ratio for a loan balance of 1700000.00
        Then the LTV ratio is 85.00

    Scenario: LTV ratio exceeding Finansinspektionen 85% cap
        Given a collateral valued at 2000000.00
        When I calculate the LTV ratio for a loan balance of 1800000.00
        Then the LTV ratio is 90.00

    Scenario: Zero loan balance yields zero LTV
        Given a collateral valued at 2000000.00
        When I calculate the LTV ratio for a loan balance of 0.00
        Then the LTV ratio is 0.00

    Scenario: Zero collateral value yields maximum LTV
        Given a collateral valued at 0.00
        When I calculate the LTV ratio for a loan balance of 1000000.00
        Then the LTV ratio is maximum
