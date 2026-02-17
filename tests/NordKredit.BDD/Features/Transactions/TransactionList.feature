@BDD @Transactions @TRN-BR-001
Feature: Transaction list display with keyset pagination
    As a bank operator
    I want to view a paginated list of posted transactions
    So that I can browse transaction history for audit and customer service

    COBOL source: COTRN00C.cbl:94-328 (CICS transaction CT00 — paginated transaction list).
    Business rules: TRN-BR-001 (transaction list display with pagination).
    Regulations: FFFS 2014:5 Ch.8 (operational info systems), PSD2 Art.94 (transaction history access).

    Background:
        Given the transaction repository contains the following transactions
            | Id          | Description        | Amount  | OriginationTimestamp |
            | TRN00000001 | GROCERY STORE      | 45.50   | 2024-01-15           |
            | TRN00000002 | GAS STATION        | 62.00   | 2024-01-15           |
            | TRN00000003 | ONLINE PURCHASE    | 129.99  | 2024-01-16           |
            | TRN00000004 | RESTAURANT         | 38.75   | 2024-01-16           |
            | TRN00000005 | PHARMACY           | 22.10   | 2024-01-17           |
            | TRN00000006 | ELECTRONICS        | 499.00  | 2024-01-17           |
            | TRN00000007 | CLOTHING STORE     | 87.50   | 2024-01-18           |
            | TRN00000008 | SUBSCRIPTION       | 14.99   | 2024-01-18           |
            | TRN00000009 | UTILITY PAYMENT    | 156.00  | 2024-01-19           |
            | TRN00000010 | TRANSFER OUT       | 500.00  | 2024-01-19           |
            | TRN00000011 | SALARY DEPOSIT     | 3200.00 | 2024-01-20           |
            | TRN00000012 | REFUND             | -25.00  | 2024-01-20           |

    # COBOL: COTRN00C.cbl:279-313 — PROCESS-PAGE-FORWARD first page, WS-IDX >= 11 (10 records)
    Scenario: Display first page of transactions
        When I request the first page of transactions
        Then the transaction list contains 10 transactions
        And the transaction list indicates a next page exists
        And the first transaction ID is "TRN00000001"

    # COBOL: COTRN00C.cbl:305-313 — read 11th record for next-page indicator (keyset cursor)
    Scenario: Navigate forward to next page
        When I request the first page of transactions
        And I request the next page using the last transaction ID as cursor
        Then the transaction list contains 2 transactions
        And the transaction list indicates no next page exists

    # COBOL: COTRN00C.cbl:206-213 — MOVE TRNIDINI to TRAN-ID (filter resets pagination)
    Scenario: Filter by starting transaction ID
        When I request transactions starting from transaction ID "TRN00000005"
        Then the transaction list contains 7 transactions
        And the first transaction ID is "TRN00000006"

    # Edge case: empty repository
    Scenario: No transactions found
        Given the transaction repository is empty
        When I request the first page of transactions
        Then the transaction list contains 0 transactions
        And the transaction list indicates no next page exists

    # COBOL: COTRN00C.cbl:297 — page size is hardcoded to 10
    Scenario: Page size is fixed at 10 records
        When I request the first page of transactions
        Then the transaction list contains exactly 10 transactions
