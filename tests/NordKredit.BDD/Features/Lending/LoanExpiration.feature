@BDD @Lending @LND-BR-009
Feature: Loan expiration enforcement
    As a bank system
    I want to detect expired loans based on maturity date
    So that expired loans are blocked from further transactions

    COBOL source: CBTRN02C.cbl:414-420.
    Business rule: LND-BR-009 (expiration enforcement).
    Regulations: FSA FFFS 2014:5 Ch. 6 (credit risk management).

    # ===================================================================
    # Expiration check
    # COBOL: ACCT-EXPIRAION-DATE PIC X(10) (sic — typo in COBOL source)
    # ===================================================================

    Scenario: Loan with future expiration date is not expired
        Given a loan with expiration date "2030-12-31"
        When I check if the loan is expired as of "2025-06-15"
        Then the loan is not expired

    Scenario: Loan with past expiration date is expired
        Given a loan with expiration date "2024-01-01"
        When I check if the loan is expired as of "2025-06-15"
        Then the loan is expired

    Scenario: Loan with no expiration date (revolving credit) is never expired
        Given a loan with no expiration date
        When I check if the loan is expired as of "2025-06-15"
        Then the loan is not expired

    Scenario: Loan expiring today is not yet expired
        Given a loan with expiration date "2025-06-15"
        When I check if the loan is expired as of "2025-06-15"
        Then the loan is not expired

    Scenario: Loan one day past expiration is expired
        Given a loan with expiration date "2025-06-14"
        When I check if the loan is expired as of "2025-06-15"
        Then the loan is expired
