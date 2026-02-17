@BDD @Transactions @TRN-BR-006
Feature: Daily transaction detail report generation
    As the batch processing system
    I want to generate a daily transaction detail report with page and account totals
    So that financial reporting requirements are met

    COBOL source: CBTRN03C.cbl:158-374 (daily transaction detail report step 3 of 3).
    Business rules: TRN-BR-006 (daily transaction detail report).
    Regulations: FFFS 2014:5 Ch.7 (financial reporting), PSD2 Art.94 (accessibility),
                 AML 2017:11 Para.3 (monitoring).

    Background:
        Given the transaction type lookup contains
            | TypeCode | Description   |
            | SA       | Sales         |
            | RF       | Refund        |
        And the transaction category lookup contains
            | TypeCode | CategoryCode | Description     |
            | SA       | 5010         | Grocery         |
            | SA       | 5020         | Fuel            |
            | RF       | 5010         | Grocery Return  |

    # COBOL: CBTRN03C.cbl:159-373 — complete report generation with all totals
    Scenario: Generate report for single card with multiple transactions
        Given the posted transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount  | OriginationTimestamp |
            | TRN00000001 | 4000123456789012 | SA       | 5010         | 100.00  | 2024-01-15           |
            | TRN00000002 | 4000123456789012 | SA       | 5020         | 50.00   | 2024-01-15           |
        And the card cross-reference lookup contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 12345678901 | 1001       |
        When I generate the daily report for date range "2024-01-15" to "2024-01-15"
        Then the report contains 2 detail lines
        And the report grand total is 150.00
        And the report has 1 account group
        And the account total for card "4000123456789012" is 150.00

    # COBOL: CBTRN03C.cbl:158-217 — card change detection triggers account total
    Scenario: Report groups transactions by card number with account totals
        Given the posted transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount  | OriginationTimestamp |
            | TRN00000003 | 4000123456789012 | SA       | 5010         | 100.00  | 2024-01-15           |
            | TRN00000004 | 4000123456789099 | SA       | 5020         | 75.00   | 2024-01-15           |
        And the card cross-reference lookup contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 12345678901 | 1001       |
            | 4000123456789099 | 99999999999 | 1002       |
        When I generate the daily report for date range "2024-01-15" to "2024-01-15"
        Then the report has 2 account groups
        And the account total for card "4000123456789012" is 100.00
        And the account total for card "4000123456789099" is 75.00
        And the report grand total is 175.00

    # COBOL: CBTRN03C.cbl:274-290 — page total every 20 lines (WS-PAGE-SIZE)
    Scenario: Page totals generated every 20 lines
        Given the posted transaction repository contains 25 transactions for card "4000123456789012" with amount 10.00 each on "2024-01-15"
        And the card cross-reference lookup contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 12345678901 | 1001       |
        When I generate the daily report for date range "2024-01-15" to "2024-01-15" with page size 20
        Then the report contains 25 detail lines
        And the report has 2 page totals
        And page total 1 is 200.00
        And page total 2 is 50.00

    # COBOL: CBTRN03C.cbl:361-370 — enriched report lines with type/category descriptions
    Scenario: Report lines enriched with type and category descriptions
        Given the posted transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | OriginationTimestamp |
            | TRN00000030 | 4000123456789012 | SA       | 5010         | 42.50  | 2024-01-15           |
        And the card cross-reference lookup contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 12345678901 | 1001       |
        When I generate the daily report for date range "2024-01-15" to "2024-01-15"
        Then report line 1 has type description "SA-Sales"
        And report line 1 has category description "5010-Grocery"

    # Edge case: no transactions in date range
    Scenario: Empty report for date range with no transactions
        When I generate the daily report for date range "2099-01-01" to "2099-01-31"
        Then the report contains 0 detail lines
        And the report grand total is 0.00
        And the report has 0 account groups

    # COBOL: CBTRN03C.cbl AF-1 — ABEND when cross-reference lookup fails
    Scenario: Report generation fails when card cross-reference missing
        Given the posted transaction repository contains
            | Id          | CardNumber       | TypeCode | CategoryCode | Amount | OriginationTimestamp |
            | TRN00000031 | 9999999999999999 | SA       | 5010         | 10.00  | 2024-01-15           |
        When I generate the daily report for date range "2024-01-15" to "2024-01-15"
        Then the report generation fails with a data integrity error
