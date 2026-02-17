@BDD @Lending @LND-BR-005
Feature: Loan repayment and transaction processing
    As a bank system
    I want to correctly apply charges and repayments to loan balances
    So that account balances reflect all transactions accurately

    COBOL source: CBTRN02C.cbl:545-560 (2800-UPDATE-ACCOUNT-REC).
    Business rule: LND-BR-005 (repayment processing).
    Regulations: FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, Art. 89.

    Background:
        Given a loan account with the following balances
            | AccountId   | CurrentBalance | CurrentCycleCredit | CurrentCycleDebit |
            | 12345678901 | 25000.00       | 25000.00           | 0.00              |

    # COBOL: CBTRN02C.cbl:545-560 — positive amount = charge
    Scenario: Charge increases balance and cycle credit
        When I apply a transaction of 5000.00 to the loan
        Then the current balance is 30000.00
        And the current cycle credit is 30000.00
        And the current cycle debit is 0.00

    # COBOL: CBTRN02C.cbl:545-560 — negative amount = repayment
    Scenario: Repayment decreases balance and increases cycle debit
        When I apply a transaction of -10000.00 to the loan
        Then the current balance is 15000.00
        And the current cycle credit is 25000.00
        And the current cycle debit is -10000.00

    # Full repayment scenario
    Scenario: Full repayment brings balance to zero
        When I apply a transaction of -25000.00 to the loan
        Then the current balance is 0.00
        And the current cycle debit is -25000.00

    # Overpayment results in negative balance (credit)
    Scenario: Overpayment creates credit balance
        When I apply a transaction of -30000.00 to the loan
        Then the current balance is -5000.00
