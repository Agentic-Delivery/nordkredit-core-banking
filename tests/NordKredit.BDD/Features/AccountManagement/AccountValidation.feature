@BDD @AccountManagement @ACCT-BR-002
Feature: Account ID input validation
    As a bank system
    I want to validate account ID inputs
    So that malformed data never reaches the persistence layer

    COBOL source: COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760.
    Business rule: ACCT-BR-002 (account ID input validation).
    Regulations: FSA FFFS 2014:5 Ch. 4 §3 (operational risk — input validation).

    Scenario: Valid 11-digit account ID accepted
        When I validate account ID "12345678901" as required
        Then the account validation result is valid

    Scenario: Blank account ID rejected when required
        When I validate account ID "" as required
        Then the account validation result is invalid
        And the account validation error is "Account number not provided"

    Scenario: Blank account ID accepted when optional
        When I validate account ID "" as optional
        Then the account validation result is valid

    Scenario: Non-numeric account ID rejected
        When I validate account ID "ABCDEFGHIJK" as required
        Then the account validation result is invalid
        And the account validation error is "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"

    Scenario: All-zeros account ID rejected
        When I validate account ID "00000000000" as required
        Then the account validation result is invalid
        And the account validation error is "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"

    Scenario: Short account ID rejected
        When I validate account ID "1234567890" as required
        Then the account validation result is invalid
        And the account validation error is "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"
