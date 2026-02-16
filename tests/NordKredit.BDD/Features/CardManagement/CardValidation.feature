@BDD @CardManagement @CARD-BR-004 @CARD-BR-005
Feature: Account and card number input validation
    As a bank system
    I want to validate account and card number inputs
    So that malformed data never reaches the persistence layer

    COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-724, COCRDUPC.cbl:721-800.
    Business rules: CARD-BR-004 (account number validation), CARD-BR-005 (card number validation).
    Regulations: FFFS 2014:5 Ch. 4 §3 (operational risk — input validation).

    # ===================================================================
    # CARD-BR-004: Account number validation
    # COBOL: EDIT-ACCOUNT paragraph — 11 digits, purely numeric, not all zeros
    # ===================================================================

    Scenario: Valid 11-digit account number accepted
        When I validate account number "12345678901" as required
        Then the validation result is valid

    Scenario: Blank account number rejected when required
        When I validate account number "" as required
        Then the validation result is invalid
        And the validation error is "Account number not provided"

    Scenario: Blank account number accepted when optional
        When I validate account number "" as optional
        Then the validation result is valid

    Scenario: Non-numeric account number rejected
        When I validate account number "ABCDEFGHIJK" as required
        Then the validation result is invalid
        And the validation error is "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"

    Scenario: All-zeros account number rejected
        When I validate account number "00000000000" as required
        Then the validation result is invalid
        And the validation error is "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"

    Scenario: Mixed alphanumeric account number rejected
        When I validate account number "1234567890A" as required
        Then the validation result is invalid
        And the validation error is "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"

    Scenario: Account number with embedded spaces rejected
        When I validate account number "123 4567890" as required
        Then the validation result is invalid
        And the validation error is "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"

    # ===================================================================
    # CARD-BR-005: Card number validation
    # COBOL: EDIT-CARD paragraph — 16 digits, purely numeric, not all zeros
    # ===================================================================

    Scenario: Valid 16-digit card number accepted
        When I validate card number "4000123456789012" as required
        Then the validation result is valid

    Scenario: Blank card number rejected when required
        When I validate card number "" as required
        Then the validation result is invalid
        And the validation error is "Card number not provided"

    Scenario: Blank card number accepted when optional
        When I validate card number "" as optional
        Then the validation result is valid

    Scenario: Non-numeric card number rejected
        When I validate card number "ABCD123456789012" as required
        Then the validation result is invalid
        And the validation error is "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"

    Scenario: All-zeros card number rejected
        When I validate card number "0000000000000000" as required
        Then the validation result is invalid
        And the validation error is "Card number not provided"

    Scenario: Short card number rejected
        When I validate card number "400012345678" as required
        Then the validation result is invalid
        And the validation error is "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"

    Scenario: Card number with special characters rejected
        When I validate card number "4000-1234-5678-9012" as required
        Then the validation result is invalid
        And the validation error is "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"
