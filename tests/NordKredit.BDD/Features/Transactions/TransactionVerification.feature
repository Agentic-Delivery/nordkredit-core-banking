@BDD @Transactions @TRN-BR-004
Feature: Daily batch card and account verification
    As the batch processing system
    I want to verify each daily transaction's card and account before posting
    So that only transactions with valid cards and accounts proceed to posting

    COBOL source: CBTRN01C.cbl:154-251 (daily batch card verification step 1 of 3).
    Business rules: TRN-BR-004 (daily batch card verification).
    Regulations: PSD2 Art.97 (transaction authorization), FFFS 2014:5 Ch.4 S3
                 (operational risk), AML/KYC (source verification).

    # COBOL: CBTRN01C.cbl:170-184 — card found in cross-reference and account exists
    Scenario: All transactions verified successfully
        Given the daily transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | Description   |
            | DTX00000001 | 4000123456789012 | SA       | 5010         | 100.00 | GROCERY STORE |
            | DTX00000002 | 4000123456789099 | SA       | 5020         | 50.00  | GAS STATION   |
        And the card cross-reference file contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 12345678901 | 1001       |
            | 4000123456789099 | 12345678901 | 1001       |
        And the account repository contains
            | Id          | CurrentBalance |
            | 12345678901 | 5000.00        |
        When I run the daily card verification
        Then all 2 transactions are verified
        And the verified transaction for "DTX00000001" has account ID "12345678901"

    # COBOL: CBTRN01C.cbl — card number not in cross-reference file
    Scenario: Transaction fails verification when card not in cross-reference
        Given the daily transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | Description   |
            | DTX00000003 | 9999999999999999 | SA       | 5010         | 75.00  | UNKNOWN CARD  |
        And the card cross-reference file is empty
        And the account repository is empty
        When I run the daily card verification
        Then 0 transactions are verified
        And the verification failure reason for "DTX00000003" is "Card number could not be verified"

    # COBOL: CBTRN01C.cbl — card verified but linked account not found
    Scenario: Transaction fails verification when account not found
        Given the daily transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | Description    |
            | DTX00000004 | 4000123456789012 | SA       | 5010         | 200.00 | MISSING ACCT   |
        And the card cross-reference file contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 99999999999 | 1001       |
        And the account repository is empty
        When I run the daily card verification
        Then 0 transactions are verified
        And the verification failure reason for "DTX00000004" is "Account not found"

    # Mixed batch: some pass, some fail
    Scenario: Mixed batch with verified and failed transactions
        Given the daily transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | Description   |
            | DTX00000005 | 4000123456789012 | SA       | 5010         | 100.00 | VALID TXN     |
            | DTX00000006 | 8888888888888888 | SA       | 5020         | 50.00  | INVALID CARD  |
        And the card cross-reference file contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 12345678901 | 1001       |
        And the account repository contains
            | Id          | CurrentBalance |
            | 12345678901 | 5000.00        |
        When I run the daily card verification
        Then 1 transaction is verified
        And 1 transaction failed verification

    # Edge case: empty batch
    Scenario: Empty batch produces no results
        Given the daily transaction repository is empty
        When I run the daily card verification
        Then all 0 transactions are verified
