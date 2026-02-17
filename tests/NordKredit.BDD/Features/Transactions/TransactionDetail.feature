@BDD @Transactions @TRN-BR-002
Feature: Transaction detail view and lookup
    As a bank operator
    I want to view detailed information for a single transaction
    So that I can review transaction data for customer service or compliance

    COBOL source: COTRN01C.cbl:85-296 (CICS transaction CT01 — single transaction read).
    Business rules: TRN-BR-002 (transaction detail view and lookup).
    Regulations: FFFS 2014:5 Ch.8, PSD2 Art.94, GDPR Art.15 (right of access).

    Background:
        Given the transaction repository contains a transaction with
            | Id          | CardNumber       | TypeCode | CategoryCode | Source | Amount  | Description   | OriginationTimestamp | ProcessingTimestamp | MerchantId | MerchantName  | MerchantCity | MerchantZip |
            | TRN00000042 | 4000123456789012 | SA       | 5010         | ONLINE | 125.50  | GROCERY STORE | 2024-01-15           | 2024-01-16         | 12345      | ACME FOODS AB | STOCKHOLM    | 11120       |

    # COBOL: COTRN01C.cbl:275 — READ TRANSACT-FILE INTO WS-TRAN-RECORD
    Scenario: Successful transaction detail lookup
        When I request transaction detail for ID "TRN00000042"
        Then the transaction detail response contains transaction ID "TRN00000042"
        And the transaction detail response contains type code "SA"
        And the transaction detail response contains category code 5010
        And the transaction detail response contains source "ONLINE"
        And the transaction detail response contains amount 125.50
        And the transaction detail response contains description "GROCERY STORE"
        And the transaction detail response contains merchant name "ACME FOODS AB"

    # COBOL: COTRN01C.cbl — transaction not found path
    Scenario: Transaction not found
        When I request transaction detail for ID "TRN99999999"
        Then the transaction detail response is null

    # PCI-DSS: Card number masking — show only last 4 digits
    Scenario: Card number masked for PCI-DSS compliance
        When I request transaction detail for ID "TRN00000042"
        Then the transaction detail response contains masked card number "************9012"

    # COBOL: COTRN01C.cbl — verify all 13 fields are present in detail view
    Scenario: All detail fields populated
        When I request transaction detail for ID "TRN00000042"
        Then the transaction detail response contains merchant city "STOCKHOLM"
        And the transaction detail response contains merchant zip "11120"
        And the transaction detail response contains merchant ID 12345
