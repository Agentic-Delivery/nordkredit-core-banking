@BDD @Deposits @DEP-BR-009 @DEP-BR-008
Feature: Deposit account lifecycle state machine
    As a bank system
    I want to enforce valid status transitions for deposit accounts
    So that account lifecycle complies with FSA and GDPR requirements

    COBOL source: CVACT01Y.cpy (ACCT-ACTIVE-STATUS PIC X(01) — 'Y'/'N').
    Business rule: DEP-BR-009 (dormancy management), DEP-BR-008 (dormancy and freezing).
    Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 5(1)(e), GDPR Art. 17 (right to erasure on closure),
                 Deposit Guarantee Directive.

    # ===================================================================
    # DEP-BR-009: Valid transitions from Active
    # Active -> Dormant, Frozen, Closed
    # ===================================================================

    Scenario: Active account transitions to Dormant
        Given a deposit account with status "Active"
        When I transition the account to "Dormant"
        Then the transition result is valid
        And the account status is "Dormant"

    Scenario: Active account transitions to Frozen
        Given a deposit account with status "Active"
        When I transition the account to "Frozen"
        Then the transition result is valid
        And the account status is "Frozen"

    Scenario: Active account transitions to Closed
        Given a deposit account with status "Active"
        When I transition the account to "Closed"
        Then the transition result is valid
        And the account status is "Closed"
        And the account has a closed date

    # ===================================================================
    # DEP-BR-009: Valid transitions from Dormant
    # Dormant -> Active, Frozen, Closed
    # ===================================================================

    Scenario: Dormant account transitions to Active
        Given a deposit account with status "Dormant"
        When I transition the account to "Active"
        Then the transition result is valid
        And the account status is "Active"

    Scenario: Dormant account transitions to Frozen
        Given a deposit account with status "Dormant"
        When I transition the account to "Frozen"
        Then the transition result is valid
        And the account status is "Frozen"

    Scenario: Dormant account transitions to Closed
        Given a deposit account with status "Dormant"
        When I transition the account to "Closed"
        Then the transition result is valid
        And the account status is "Closed"

    # ===================================================================
    # DEP-BR-009: Valid transitions from Frozen
    # Frozen -> Active, Dormant, Closed
    # ===================================================================

    Scenario: Frozen account transitions to Active
        Given a deposit account with status "Frozen"
        When I transition the account to "Active"
        Then the transition result is valid
        And the account status is "Active"

    Scenario: Frozen account transitions to Dormant
        Given a deposit account with status "Frozen"
        When I transition the account to "Dormant"
        Then the transition result is valid
        And the account status is "Dormant"

    Scenario: Frozen account transitions to Closed
        Given a deposit account with status "Frozen"
        When I transition the account to "Closed"
        Then the transition result is valid
        And the account status is "Closed"

    # ===================================================================
    # DEP-BR-009: Closed is terminal — no transitions allowed
    # ===================================================================

    Scenario: Closed account cannot transition to Active
        Given a deposit account with status "Closed"
        When I transition the account to "Active"
        Then the transition result is invalid
        And the transition error is "Cannot transition from Closed status — terminal state"

    Scenario: Closed account cannot transition to Dormant
        Given a deposit account with status "Closed"
        When I transition the account to "Dormant"
        Then the transition result is invalid
        And the transition error is "Cannot transition from Closed status — terminal state"

    # ===================================================================
    # DEP-BR-009: Same-status transition rejected
    # ===================================================================

    Scenario: Same-status transition rejected
        Given a deposit account with status "Active"
        When I transition the account to "Active"
        Then the transition result is invalid
        And the transition error is "Account is already in the requested status"
