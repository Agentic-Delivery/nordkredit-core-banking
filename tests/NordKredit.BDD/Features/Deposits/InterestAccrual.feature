@BDD @Deposits @DEP-BR-004 @DEP-BR-007
Feature: Deposit interest accrual and posting
    As a bank system
    I want to accrue daily interest and post it to account balances
    So that deposit holders receive interest per FSA disclosure schedules

    COBOL source: Dedicated interest calculation batch program.
    Business rules: DEP-BR-004 (interest calculation and accrual), DEP-BR-007 (interest posting).
    Regulations: FSA FFFS 2014:5 Ch. 3 & 6, Deposit Guarantee Directive.

    # ===================================================================
    # DEP-BR-004: Interest accrual (adding daily interest to AccruedInterest)
    # ===================================================================

    Scenario: Daily interest is accrued to account
        Given a deposit account with balance 100000.00 and accrued interest 0.0000
        When I accrue daily interest of 6.8493
        Then the accrued interest is 6.8493

    Scenario: Multiple days of accrual accumulate
        Given a deposit account with balance 100000.00 and accrued interest 0.0000
        When I accrue daily interest of 6.8493
        And I accrue daily interest of 6.8493
        And I accrue daily interest of 6.8493
        Then the accrued interest is 20.5479

    # ===================================================================
    # DEP-BR-007: Interest posting (transferring AccruedInterest to balance)
    # Accrued interest is added to balance and cycle credit, then reset to zero
    # ===================================================================

    Scenario: Accrued interest posted to balance
        Given a deposit account with balance 100000.00 and accrued interest 205.4790
        When I post interest to the account
        Then the account balance is 100205.4790
        And the current cycle credit is 205.4790
        And the accrued interest is 0.0000

    Scenario: Zero accrued interest posting has no effect on balance
        Given a deposit account with balance 50000.00 and accrued interest 0.0000
        When I post interest to the account
        Then the account balance is 50000.0000
        And the accrued interest is 0.0000

    # ===================================================================
    # DEP-BR-004: Interest accrual service — eligibility checks
    # ===================================================================

    Scenario: Interest accrual skips inactive accounts
        Given a deposit account with status "Dormant" and balance 100000.00 linked to product "SAVINGS01"
        And the savings product "SAVINGS01" has annual rate 0.025 and day count basis 365
        When I accrue interest via the service
        Then the service returns null

    Scenario: Interest accrual returns zero for zero-balance accounts
        Given a deposit account with status "Active" and balance 0.00 linked to product "SAVINGS01"
        And the savings product "SAVINGS01" has annual rate 0.025 and day count basis 365
        When I accrue interest via the service
        Then the service returns 0.00

    Scenario: Interest accrual succeeds for active account with positive balance
        Given a deposit account with status "Active" and balance 100000.00 linked to product "SAVINGS01"
        And the savings product "SAVINGS01" has annual rate 0.025 and day count basis 365
        When I accrue interest via the service
        Then the service returns 6.8493
