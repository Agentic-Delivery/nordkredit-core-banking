@BDD @Transactions @TRN-BR-005
Feature: Daily batch transaction posting with balance updates
    As the batch processing system
    I want to post verified transactions updating category and account balances atomically
    So that financial records are accurate and consistent

    COBOL source: CBTRN02C.cbl:193-579 (daily batch transaction posting step 2 of 3).
    Business rules: TRN-BR-005 (daily batch transaction posting).
    Regulations: FFFS 2014:5 Ch.3 (accurate records), FFFS 2014:5 Ch.16 (financial reporting),
                 PSD2 Art.94 (retention).

    # COBOL: CBTRN02C.cbl:424-579 — full posting loop, 3 atomic operations per transaction
    Scenario: Successful posting of a single credit transaction
        Given a verified transaction exists
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount  | AccountId   |
            | DTX00000001 | 4000123456789012 | SA       | 5010         | 100.00  | 12345678901 |
        And the account "12345678901" has balance 5000.00 and cycle credit 0.00 and cycle debit 0.00
        When I post the validated transactions
        Then the posting result shows 1 posted and 0 skipped and 0 failed
        And the account "12345678901" has balance 5100.00
        And the account "12345678901" has cycle credit 100.00
        And the account "12345678901" has cycle debit 0.00

    # COBOL: CBTRN02C.cbl:480-510 — debit transaction (negative amount)
    Scenario: Successful posting of a debit transaction
        Given a verified transaction exists
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount  | AccountId   |
            | DTX00000002 | 4000123456789012 | SA       | 5020         | -75.00  | 12345678901 |
        And the account "12345678901" has balance 5000.00 and cycle credit 0.00 and cycle debit 0.00
        When I post the validated transactions
        Then the posting result shows 1 posted and 0 skipped and 0 failed
        And the account "12345678901" has balance 4925.00
        And the account "12345678901" has cycle credit 0.00
        And the account "12345678901" has cycle debit 75.00

    # COBOL: CBTRN02C.cbl:467-542 — category balance upsert (new category)
    Scenario: Category balance created for new type-category combination
        Given a verified transaction exists
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | AccountId   |
            | DTX00000003 | 4000123456789012 | SA       | 5010         | 200.00 | 12345678901 |
        And the account "12345678901" has balance 1000.00 and cycle credit 0.00 and cycle debit 0.00
        And no category balance exists for account "12345678901" type "SA" category 5010
        When I post the validated transactions
        Then the category balance for account "12345678901" type "SA" category 5010 is 200.00

    # COBOL: CBTRN02C.cbl:467-542 — category balance upsert (existing category)
    Scenario: Category balance updated for existing type-category combination
        Given a verified transaction exists
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | AccountId   |
            | DTX00000004 | 4000123456789012 | SA       | 5010         | 150.00 | 12345678901 |
        And the account "12345678901" has balance 1000.00 and cycle credit 0.00 and cycle debit 0.00
        And the category balance for account "12345678901" type "SA" category 5010 is 300.00
        When I post the validated transactions
        Then the category balance for account "12345678901" type "SA" category 5010 is 450.00

    # Invalid transactions are skipped (already rejected during validation)
    Scenario: Invalid transactions skipped during posting
        Given an invalid validated transaction exists
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | AccountId   |
            | DTX00000005 | 4000123456789012 | SA       | 5010         | 50.00  | 12345678901 |
        When I post the validated transactions
        Then the posting result shows 0 posted and 1 skipped and 0 failed

    # Multiple transactions in a single batch
    Scenario: Batch posting of multiple transactions
        Given a verified transaction exists
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount  | AccountId   |
            | DTX00000006 | 4000123456789012 | SA       | 5010         | 100.00  | 12345678901 |
        And a verified transaction exists
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount  | AccountId   |
            | DTX00000007 | 4000123456789012 | SA       | 5020         | -30.00  | 12345678901 |
        And the account "12345678901" has balance 2000.00 and cycle credit 0.00 and cycle debit 0.00
        When I post the validated transactions
        Then the posting result shows 2 posted and 0 skipped and 0 failed
        And the account "12345678901" has balance 2070.00
