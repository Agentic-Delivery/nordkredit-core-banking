@BDD @Deposits @DEP-BR-002
Feature: Deposit account opening
    As a bank operator
    I want to open new deposit accounts with proper validation
    So that account creation complies with FSA and AML requirements

    COBOL source: CVACT01Y.cpy (ACCOUNT-RECORD), WRITE ACCOUNT-RECORD to ACCTFILE.
    Business rule: DEP-BR-002 (account opening).
    Regulations: FSA FFFS 2014:5 Ch. 3, AML 2017:11, Deposit Guarantee Directive.

    # ===================================================================
    # DEP-BR-002: New account defaults
    # COBOL: Initial WRITE to ACCTFILE sets default values
    # ===================================================================

    Scenario: New demand savings account has correct defaults
        Given I create a deposit account with
            | AccountId   | HolderName    | ProductType   | DisclosureGroupId |
            | 12345678901 | ERIK SVENSSON | DemandSavings | SAVINGS01         |
        Then the new account has status "Active"
        And the new account has balance 0.00
        And the new account has current cycle credit 0.00
        And the new account has current cycle debit 0.00
        And the new account has accrued interest 0.0000
        And the new account has an opened date

    Scenario: New term deposit account records maturity date
        Given I create a deposit account with maturity date "2026-06-15" and
            | AccountId   | HolderName     | ProductType | DisclosureGroupId |
            | 99999999999 | ANNA LINDQVIST | TermDeposit | TERM12M           |
        Then the new account has status "Active"
        And the new account has maturity date "2026-06-15"

    # COBOL: CVACT01Y.cpy — Swedish name encoding (Å, Ä, Ö via nvarchar)
    Scenario: Account holder name supports Swedish characters
        Given I create a deposit account with
            | AccountId   | HolderName       | ProductType   | DisclosureGroupId |
            | 55555555555 | BJÖRK ÅKERSTRÖM  | DemandSavings | SAVINGS01         |
        Then the new account has holder name "BJÖRK ÅKERSTRÖM"
