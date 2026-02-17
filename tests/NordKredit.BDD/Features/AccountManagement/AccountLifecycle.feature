@BDD @AccountManagement @ACCT-BR-005
Feature: Account lifecycle state machine
    As a bank system
    I want to enforce account lifecycle state transitions
    So that accounts follow regulatory requirements for status management

    COBOL source: COCRDUPC.cbl:845-876 (card level), CBTRN02C.cbl:414-420 (account level).
    Business rule: ACCT-BR-005 (account status transitions).
    Regulations: FSA FFFS 2014:5 Ch. 4 §3, PSD2 Art. 97, GDPR Art. 17.

    # ===================================================================
    # Valid state transitions
    # ===================================================================

    Scenario: Active account can be set to dormant
        Given an account with status "Active"
        When I transition the account to "Dormant"
        Then the transition succeeds
        And the account status is "Dormant"

    Scenario: Active account can be frozen
        Given an account with status "Active"
        When I transition the account to "Frozen"
        Then the transition succeeds
        And the account status is "Frozen"

    Scenario: Active account can be closed
        Given an account with status "Active"
        When I transition the account to "Closed"
        Then the transition succeeds
        And the account status is "Closed"
        And the account has a closed date

    Scenario: Dormant account can be reactivated
        Given an account with status "Dormant"
        When I transition the account to "Active"
        Then the transition succeeds
        And the account status is "Active"

    Scenario: Frozen account can be reactivated
        Given an account with status "Frozen"
        When I transition the account to "Active"
        Then the transition succeeds
        And the account status is "Active"

    # ===================================================================
    # Invalid state transitions
    # ===================================================================

    Scenario: Closed account cannot be reactivated
        Given an account with status "Closed"
        When I transition the account to "Active"
        Then the transition fails
        And the transition error is "Cannot transition from Closed status — terminal state"

    Scenario: Closed account cannot be set to dormant
        Given an account with status "Closed"
        When I transition the account to "Dormant"
        Then the transition fails
        And the transition error is "Cannot transition from Closed status — terminal state"

    Scenario: Same status transition is rejected
        Given an account with status "Active"
        When I transition the account to "Active"
        Then the transition fails
        And the transition error is "Account is already in the requested status"
