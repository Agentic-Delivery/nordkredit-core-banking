@BDD @Lending @LND-BR-008
Feature: Loan status lifecycle state machine
    As a bank system
    I want to enforce valid status transitions on loans
    So that the loan lifecycle complies with regulatory requirements

    COBOL source: Dedicated program (inferred from LND-BR-008).
    Business rule: LND-BR-008 (delinquency management — loan status state machine).
    Regulations: FSA FFFS 2014:5 Ch. 6, Inkassolagen 1974:182.

    # ===================================================================
    # Valid transitions from Active
    # Active → Delinquent, Frozen, PaidOff, Closed
    # ===================================================================

    Scenario: Active loan can transition to Delinquent
        Given a loan with status "Active"
        When I transition the loan to "Delinquent"
        Then the status transition is valid
        And the loan status is "Delinquent"

    Scenario: Active loan can transition to Frozen
        Given a loan with status "Active"
        When I transition the loan to "Frozen"
        Then the status transition is valid
        And the loan status is "Frozen"

    Scenario: Active loan can transition to PaidOff
        Given a loan with status "Active"
        When I transition the loan to "PaidOff"
        Then the status transition is valid
        And the loan status is "PaidOff"

    Scenario: Active loan can transition to Closed
        Given a loan with status "Active"
        When I transition the loan to "Closed"
        Then the status transition is valid
        And the loan status is "Closed"

    Scenario: Active loan cannot transition to Defaulted
        Given a loan with status "Active"
        When I transition the loan to "Defaulted"
        Then the status transition is invalid
        And the transition error is "Invalid transition from Active to Defaulted"

    # ===================================================================
    # Valid transitions from Delinquent
    # Delinquent → Active, Defaulted, Frozen
    # ===================================================================

    Scenario: Delinquent loan can transition to Active (cured)
        Given a loan with status "Delinquent"
        When I transition the loan to "Active"
        Then the status transition is valid
        And the loan status is "Active"

    Scenario: Delinquent loan can transition to Defaulted
        Given a loan with status "Delinquent"
        When I transition the loan to "Defaulted"
        Then the status transition is valid
        And the loan status is "Defaulted"

    Scenario: Delinquent loan can transition to Frozen
        Given a loan with status "Delinquent"
        When I transition the loan to "Frozen"
        Then the status transition is valid
        And the loan status is "Frozen"

    Scenario: Delinquent loan cannot transition to PaidOff
        Given a loan with status "Delinquent"
        When I transition the loan to "PaidOff"
        Then the status transition is invalid
        And the transition error is "Invalid transition from Delinquent to PaidOff"

    # ===================================================================
    # Valid transitions from Defaulted
    # Defaulted → Closed
    # ===================================================================

    Scenario: Defaulted loan can transition to Closed
        Given a loan with status "Defaulted"
        When I transition the loan to "Closed"
        Then the status transition is valid
        And the loan status is "Closed"

    Scenario: Defaulted loan cannot transition to Active
        Given a loan with status "Defaulted"
        When I transition the loan to "Active"
        Then the status transition is invalid
        And the transition error is "Invalid transition from Defaulted to Active"

    # ===================================================================
    # Valid transitions from Frozen
    # Frozen → Active, Delinquent, Closed
    # ===================================================================

    Scenario: Frozen loan can transition to Active
        Given a loan with status "Frozen"
        When I transition the loan to "Active"
        Then the status transition is valid
        And the loan status is "Active"

    Scenario: Frozen loan can transition to Delinquent
        Given a loan with status "Frozen"
        When I transition the loan to "Delinquent"
        Then the status transition is valid
        And the loan status is "Delinquent"

    Scenario: Frozen loan can transition to Closed
        Given a loan with status "Frozen"
        When I transition the loan to "Closed"
        Then the status transition is valid
        And the loan status is "Closed"

    # ===================================================================
    # Valid transitions from PaidOff
    # PaidOff → Closed
    # ===================================================================

    Scenario: PaidOff loan can transition to Closed
        Given a loan with status "PaidOff"
        When I transition the loan to "Closed"
        Then the status transition is valid
        And the loan status is "Closed"

    Scenario: PaidOff loan cannot transition to Active
        Given a loan with status "PaidOff"
        When I transition the loan to "Active"
        Then the status transition is invalid
        And the transition error is "Invalid transition from PaidOff to Active"

    # ===================================================================
    # Closed is terminal
    # ===================================================================

    Scenario: Closed loan cannot transition to any status
        Given a loan with status "Closed"
        When I transition the loan to "Active"
        Then the status transition is invalid
        And the transition error is "Cannot transition from Closed status — terminal state"

    # ===================================================================
    # Same-status transition rejected
    # ===================================================================

    Scenario: Transitioning to same status is rejected
        Given a loan with status "Active"
        When I transition the loan to "Active"
        Then the status transition is invalid
        And the transition error is "Loan is already in the requested status"
