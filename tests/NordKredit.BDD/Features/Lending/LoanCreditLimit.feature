@BDD @Lending @LND-BR-002
Feature: Loan credit limit enforcement
    As a bank system
    I want to enforce credit limits on loan accounts
    So that transactions exceeding the authorized limit are rejected

    COBOL source: CBTRN02C.cbl:403-413 (1500-B-LOOKUP-ACCT).
    Business rule: LND-BR-002 (credit limit enforcement).
    Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk), Consumer Credit Directive Art. 10.

    Background:
        Given a loan account with the following details
            | AccountId   | CreditLimit | CurrentCycleCredit | CurrentCycleDebit |
            | 12345678901 | 50000.00    | 30000.00           | -5000.00          |

    # COBOL: CBTRN02C.cbl:403-413 — projected balance check
    # Available credit = 50000 - (30000 - (-5000)) = 50000 - 35000 = 15000
    Scenario: Transaction within available credit accepted
        When I check if transaction amount 10000.00 would exceed the credit limit
        Then the credit limit check result is "not exceeded"

    # COBOL: CBTRN02C.cbl:403-413 — projected balance exceeds limit
    Scenario: Transaction exceeding available credit rejected
        When I check if transaction amount 20000.00 would exceed the credit limit
        Then the credit limit check result is "exceeded"

    # Edge case: exact limit usage
    Scenario: Transaction using exact remaining credit accepted
        When I check if transaction amount 15000.00 would exceed the credit limit
        Then the credit limit check result is "not exceeded"

    Scenario: Available credit is correctly calculated
        Then the available credit is 15000.00
