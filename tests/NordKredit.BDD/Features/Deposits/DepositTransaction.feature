@BDD @Deposits @DEP-BR-003
Feature: Deposit transaction posting
    As a bank system
    I want to apply deposit and withdrawal transactions to account balances
    So that account balances accurately reflect all financial activity

    COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
    Business rule: DEP-BR-003 (deposit posting — balance updates).
    Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64.

    Background:
        Given a deposit account with balance 10000.00

    # COBOL: CBTRN02C.cbl:545-550 — positive amount = deposit (inflow)
    Scenario: Deposit increases balance and cycle credit
        When I apply a transaction of 5000.00
        Then the account balance is 15000.00
        And the current cycle credit is 5000.00
        And the current cycle debit is 0.00

    # COBOL: CBTRN02C.cbl:551-560 — negative amount = withdrawal (outflow)
    Scenario: Withdrawal decreases balance and increases cycle debit
        When I apply a transaction of -3000.00
        Then the account balance is 7000.00
        And the current cycle credit is 0.00
        And the current cycle debit is -3000.00

    # COBOL: CBTRN02C.cbl:545-560 — multiple transactions accumulate
    Scenario: Multiple transactions accumulate correctly
        When I apply a transaction of 2000.00
        And I apply a transaction of -500.00
        And I apply a transaction of 1500.00
        Then the account balance is 13000.00
        And the current cycle credit is 3500.00
        And the current cycle debit is -500.00

    # COBOL: CBTRN02C.cbl:545-560 — zero amount has no effect
    Scenario: Zero amount transaction has no effect
        When I apply a transaction of 0.00
        Then the account balance is 10000.00
        And the current cycle credit is 0.00
        And the current cycle debit is 0.00
