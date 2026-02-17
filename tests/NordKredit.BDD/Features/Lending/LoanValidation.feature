@BDD @Lending @LND-BR-002 @LND-BR-009
Feature: Loan account and credit limit input validation
    As a bank system
    I want to validate loan account inputs
    So that malformed data never reaches the persistence layer

    COBOL source: CBTRN02C.cbl:370-421 (1500-VALIDATE-TRAN), COCRDLIC.cbl:1003-1034.
    Business rules: LND-BR-002 (credit limit enforcement), LND-BR-009 (expiration enforcement).
    Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), PSD2 Art. 64 (transaction integrity).

    # ===================================================================
    # LND-BR-002: Account ID validation
    # COBOL: EDIT-ACCOUNT paragraph — 11 digits, purely numeric, not all zeros
    # ===================================================================

    Scenario: Valid 11-digit loan account ID accepted
        When I validate loan account ID "12345678901"
        Then the loan validation result is valid

    Scenario: Blank loan account ID rejected
        When I validate loan account ID ""
        Then the loan validation result is invalid
        And the loan validation error is "Account number not provided"

    Scenario: Non-numeric loan account ID rejected
        When I validate loan account ID "ABCDEFGHIJK"
        Then the loan validation result is invalid
        And the loan validation error is "ACCOUNT ID MUST BE A 11 DIGIT NUMBER"

    Scenario: All-zeros loan account ID rejected
        When I validate loan account ID "00000000000"
        Then the loan validation result is invalid
        And the loan validation error is "ACCOUNT ID MUST BE A 11 DIGIT NUMBER"

    Scenario: Short loan account ID rejected
        When I validate loan account ID "12345"
        Then the loan validation result is invalid
        And the loan validation error is "ACCOUNT ID MUST BE A 11 DIGIT NUMBER"

    # ===================================================================
    # LND-BR-002: Credit limit validation
    # COBOL: PIC S9(10)V99 — positive, within COBOL field capacity
    # ===================================================================

    Scenario: Valid credit limit accepted
        When I validate credit limit 50000.00
        Then the loan validation result is valid

    Scenario: Zero credit limit rejected
        When I validate credit limit 0
        Then the loan validation result is invalid
        And the loan validation error is "Credit limit must be greater than zero"

    Scenario: Negative credit limit rejected
        When I validate credit limit -1000
        Then the loan validation result is invalid
        And the loan validation error is "Credit limit must be greater than zero"

    Scenario: Credit limit exceeding COBOL maximum rejected
        When I validate credit limit 99999999999.99
        Then the loan validation result is invalid
        And the loan validation error is "Credit limit exceeds maximum allowed value"

    # ===================================================================
    # LND-BR-005: Transaction amount validation
    # COBOL: DALYTRAN-AMT — non-zero required
    # ===================================================================

    Scenario: Valid positive transaction amount accepted
        When I validate transaction amount 500.00
        Then the loan validation result is valid

    Scenario: Valid negative transaction amount (repayment) accepted
        When I validate transaction amount -250.00
        Then the loan validation result is valid

    Scenario: Zero transaction amount rejected
        When I validate transaction amount 0
        Then the loan validation result is invalid
        And the loan validation error is "Transaction amount cannot be zero"
