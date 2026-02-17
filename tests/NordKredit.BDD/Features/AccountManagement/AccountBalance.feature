@BDD @AccountManagement @ACCT-BR-004 @ACCT-BR-007
Feature: Account balance management and credit limit enforcement
    As a bank system
    I want to track account balances and enforce credit limits
    So that financial integrity is maintained

    COBOL source: CBTRN02C.cbl:403-413 (credit limit), CBTRN02C.cbl:545-560 (balance update).
    Business rules: ACCT-BR-004 (balance management), ACCT-BR-007 (credit limit enforcement).
    Regulations: FSA FFFS 2014:5 Ch. 3 & 7, PSD2 Art. 64, DORA Art. 11.

    Scenario: Credit transaction increases balance and cycle credit
        Given an account with balance 100.00 and cycle credit 50.00 and cycle debit 0.00
        When I apply a transaction of 200.00
        Then the account balance is 300.00
        And the cycle credit is 250.00

    Scenario: Debit transaction decreases balance and increases cycle debit
        Given an account with balance 500.00 and cycle credit 0.00 and cycle debit -100.00
        When I apply a transaction of -75.50
        Then the account balance is 424.50
        And the cycle debit is -175.50

    Scenario: Transaction within credit limit is allowed
        Given an account with credit limit 5000.00 and cycle credit 3000.00 and cycle debit -1000.00
        When I check if a transaction of 500.00 would exceed the credit limit
        Then the credit limit check returns false

    Scenario: Transaction exceeding credit limit is detected
        Given an account with credit limit 5000.00 and cycle credit 3000.00 and cycle debit -1000.00
        When I check if a transaction of 2001.00 would exceed the credit limit
        Then the credit limit check returns true
