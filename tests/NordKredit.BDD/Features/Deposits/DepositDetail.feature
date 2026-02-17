@BDD @Deposits @DEP-BR-001 @DEP-BR-003
Feature: Deposit account detail lookup
    As a bank operator
    I want to view detailed information for a single deposit account
    So that I can review account data for customer service or compliance

    COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), CBTRN02C.cbl (transaction processing).
    Business rules: DEP-BR-001 (data structure), DEP-BR-003 (balance inquiry).
    Regulations: FSA FFFS 2014:5 Ch. 3, GDPR Art. 15 (right of access),
                 Deposit Guarantee Directive 2014/49/EU.

    Background:
        Given the deposit repository contains the following accounts
            | AccountId   | HolderName      | Status | ProductType    | CurrentBalance | DisclosureGroupId | OpenedDate |
            | 12345678901 | ERIK SVENSSON   | Active | DemandSavings  | 50000.00       | SAVINGS01         | 2023-01-15 |
            | 99999999999 | ANNA LINDQVIST  | Closed | TermDeposit    | 0.00           | TERM12M           | 2022-06-01 |

    # COBOL: VSAM READ on ACCTFILE by primary key ACCT-ID
    Scenario: Successful lookup by account ID
        When I request deposit detail for account ID "12345678901"
        Then the deposit detail response contains account ID "12345678901"
        And the deposit detail response contains holder name "ERIK SVENSSON"
        And the deposit detail response contains status "Active"
        And the deposit detail response contains product type "DemandSavings"
        And the deposit detail response contains balance 50000.00

    # COBOL: Account not found — FD-ACCT-ID returns no record
    Scenario: Account not found
        When I request deposit detail for account ID "00000000001"
        Then the deposit detail response is null

    # COBOL: CVACT01Y.cpy — closed account can still be queried (GDPR Art. 15)
    Scenario: Closed account still returns detail
        When I request deposit detail for account ID "99999999999"
        Then the deposit detail response contains account ID "99999999999"
        And the deposit detail response contains status "Closed"
