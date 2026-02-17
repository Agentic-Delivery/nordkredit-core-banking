@BDD @Deposits @DEP-BR-001
Feature: Deposit account input validation
    As a bank system
    I want to validate deposit account inputs
    So that malformed data never reaches the persistence layer

    COBOL source: CVACT01Y.cpy — account field validation.
    Business rule: DEP-BR-001 (deposit account data structure).
    Regulations: FSA FFFS 2014:5 Ch. 4 §3 (operational risk — input validation), PSD2 Art. 97.

    # ===================================================================
    # DEP-BR-001: Account ID validation
    # COBOL: ACCT-ID PIC 9(11) — 11 digits, purely numeric, not all zeros
    # ===================================================================

    Scenario: Valid 11-digit account ID accepted
        When I validate deposit account ID "12345678901"
        Then the deposit validation result is valid

    Scenario: Blank account ID rejected
        When I validate deposit account ID ""
        Then the deposit validation result is invalid
        And the deposit validation error is "Account number not provided"

    Scenario: Non-numeric account ID rejected
        When I validate deposit account ID "ABCDEFGHIJK"
        Then the deposit validation result is invalid
        And the deposit validation error is "Account number must be an 11-digit number"

    Scenario: All-zeros account ID rejected
        When I validate deposit account ID "00000000000"
        Then the deposit validation result is invalid
        And the deposit validation error is "Account number must be an 11-digit number"

    Scenario: Short account ID rejected
        When I validate deposit account ID "1234567"
        Then the deposit validation result is invalid
        And the deposit validation error is "Account number must be an 11-digit number"

    Scenario: Account ID with spaces rejected
        When I validate deposit account ID "123 4567890"
        Then the deposit validation result is invalid
        And the deposit validation error is "Account number must be an 11-digit number"

    # ===================================================================
    # DEP-BR-001: Disclosure group ID validation
    # COBOL: ACCT-GROUP-ID PIC X(10) — max 10 characters
    # ===================================================================

    Scenario: Valid disclosure group ID accepted
        When I validate disclosure group ID "SAVINGS01"
        Then the deposit validation result is valid

    Scenario: Blank disclosure group ID rejected
        When I validate disclosure group ID ""
        Then the deposit validation result is invalid
        And the deposit validation error is "Disclosure group ID not provided"

    Scenario: Disclosure group ID exceeding 10 characters rejected
        When I validate disclosure group ID "TOOLONGGROUP"
        Then the deposit validation result is invalid
        And the deposit validation error is "Disclosure group ID must not exceed 10 characters"
