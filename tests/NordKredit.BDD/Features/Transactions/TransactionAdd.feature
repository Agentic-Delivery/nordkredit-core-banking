@BDD @Transactions @TRN-BR-003
Feature: Transaction add with validation
    As a bank operator
    I want to add new transactions with input validation
    So that only valid transaction data enters the system

    COBOL source: COTRN02C.cbl:106-784 (CICS transaction CT02 — add transaction).
    Business rules: TRN-BR-003 (transaction add with validation).
    Regulations: FFFS 2014:5 Ch.8 (accurate records), PSD2 Art.94 (transaction retention).

    Background:
        Given the card cross-reference contains
            | CardNumber       | AccountId   | CustomerId |
            | 4000123456789012 | 12345678901 | 1001       |

    # COBOL: COTRN02C.cbl:442-749 — ADD-TRANSACTION after successful validation
    # Note: TypeCode/CategoryCode must be numeric per COBOL VALIDATE-INPUT-DATA-FIELDS.
    # Amount must be in COBOL format ±99999999.99 (12 chars) per COTRN02C.cbl:339-345.
    Scenario: Successful transaction add
        Given the transaction ID generator will return "TRN00000099"
        When I submit a transaction add request with
            | CardNumber       | TypeCode | CategoryCode | Source | Description   | Amount       | OriginationDate | ProcessingDate | MerchantId | MerchantName  | MerchantCity | MerchantZip | Confirm |
            | 4000123456789012 | 01       | 5010         | ONLINE | GROCERY STORE | +00000125.50 | 2024-01-15      | 2024-01-16     | 12345      | ACME FOODS AB | STOCKHOLM    | 11120       | Y       |
        Then the transaction add result is success
        And the assigned transaction ID is "TRN00000099"

    # COBOL: COTRN02C.cbl:164-437 — validation failures before ADD
    Scenario: Validation failure for unknown card number
        When I submit a transaction add request with
            | CardNumber       | TypeCode | CategoryCode | Source | Description   | Amount       | OriginationDate | ProcessingDate | MerchantId | MerchantName  | MerchantCity | MerchantZip | Confirm |
            | 9999999999999999 | 01       | 5010         | ONLINE | GROCERY STORE | +00000125.50 | 2024-01-15      | 2024-01-16     | 12345      | ACME FOODS AB | STOCKHOLM    | 11120       | Y       |
        Then the transaction add result is validation error

    # COBOL: COTRN02C.cbl — confirmation flow (two-step submit)
    Scenario: Confirmation required before first add
        When I submit a transaction add request with
            | CardNumber       | TypeCode | CategoryCode | Source | Description   | Amount       | OriginationDate | ProcessingDate | MerchantId | MerchantName  | MerchantCity | MerchantZip | Confirm |
            | 4000123456789012 | 01       | 5010         | ONLINE | GROCERY STORE | +00000125.50 | 2024-01-15      | 2024-01-16     | 12345      | ACME FOODS AB | STOCKHOLM    | 11120       |         |
        Then the transaction add result is confirmation required

    # COBOL: COTRN02C.cbl:711-738 — WRITE-TRANSACT-FILE duplicate key detection
    Scenario: Duplicate transaction ID rejected
        Given the transaction ID generator will return "TRN00000001"
        And the transaction repository already contains transaction "TRN00000001"
        When I submit a transaction add request with
            | CardNumber       | TypeCode | CategoryCode | Source | Description   | Amount       | OriginationDate | ProcessingDate | MerchantId | MerchantName  | MerchantCity | MerchantZip | Confirm |
            | 4000123456789012 | 01       | 5010         | ONLINE | GROCERY STORE | +00000125.50 | 2024-01-15      | 2024-01-16     | 12345      | ACME FOODS AB | STOCKHOLM    | 11120       | Y       |
        Then the transaction add result is duplicate key

    # COBOL: COPY-LAST-TRAN-DATA (PF5 handler) — retrieve last transaction for convenience copy
    Scenario: Retrieve last transaction for copy
        Given the transaction repository contains a last transaction with ID "TRN00000098"
        When I request the last transaction
        Then the last transaction ID is "TRN00000098"
