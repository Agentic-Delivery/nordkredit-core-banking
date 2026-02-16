@BDD @CardManagement @CARD-BR-007 @CARD-BR-008
Feature: Card update workflow with state machine and optimistic concurrency
    As a bank operator
    I want to update card details with validation and concurrency control
    So that card data is modified safely with regulatory compliance

    COBOL source: COCRDUPC.cbl:275-290 (state machine), 429-543 (main EVALUATE),
                  948-1027 (action decisions), 1420-1523 (write processing + concurrency).
    Business rules: CARD-BR-006 (field validation), CARD-BR-007 (state machine), CARD-BR-008 (concurrency).
    Regulations: PSD2 Art. 97 (SCA), FFFS 2014:5 Ch. 8 §4 (operational risk).

    Background:
        Given the card repository contains a card with
            | CardNumber       | AccountId   | EmbossedName | ExpirationDate | ActiveStatus | RowVersion |
            | 4000123456789012 | 12345678901 | JOHN DOE     | 2027-12-31     | Y            | AQIDBAUGCA==   |

    # ===================================================================
    # CARD-BR-007: Successful update (CHANGES-OKAYED-AND-DONE)
    # COBOL: COCRDUPC.cbl:998-1000
    # ===================================================================

    Scenario: Successful card update with valid changes
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JANE DOE     | Y            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is success
        And the updated card has embossed name "JANE DOE"

    # ===================================================================
    # CARD-BR-007: No change detection (SHOW-DETAILS with message)
    # COBOL: COCRDUPC.cbl 1200-EDIT-MAP-INPUTS
    # ===================================================================

    Scenario: No change detected when identical values submitted
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JOHN DOE     | Y            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is no change
        And the update message is "No change detected with respect to values fetched."

    # COBOL: COCRDUPC.cbl — case-insensitive comparison via UPPER-CASE
    Scenario: Case-insensitive change detection for embossed name
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | John Doe     | Y            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is no change

    # ===================================================================
    # CARD-BR-006: Field validation errors (CHANGES-NOT-OK)
    # COBOL: COCRDUPC.cbl:806-947
    # ===================================================================

    Scenario: Validation failure for blank embossed name
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            |              | Y            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is validation failure
        And the validation errors contain "Card name not provided"

    Scenario: Validation failure for invalid active status
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JANE DOE     | A            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is validation failure
        And the validation errors contain "Card Active Status must be Y or N"

    Scenario: Validation failure for invalid expiry month
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JANE DOE     | Y            | 13          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is validation failure
        And the validation errors contain "Card expiry month must be between 1 and 12"

    Scenario: Validation failure for invalid expiry year
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JANE DOE     | Y            | 12          | 2100       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is validation failure
        And the validation errors contain "Invalid card expiry year"

    # Improvement: collect all errors simultaneously (COBOL only returned first error)
    Scenario: Multiple validation errors returned simultaneously
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            |              | X            | 0           | 1800       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is validation failure
        And the validation errors contain "Card name not provided"
        And the validation errors contain "Card Active Status must be Y or N"
        And the validation errors contain "Card expiry month must be between 1 and 12"
        And the validation errors contain "Invalid card expiry year"

    # ===================================================================
    # CARD-BR-007: Card not found
    # ===================================================================

    Scenario: Update fails when card not found
        When I update card "9999999999999999" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JANE DOE     | Y            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is not found

    # ===================================================================
    # CARD-BR-008: Optimistic concurrency conflict
    # COBOL: COCRDUPC.cbl:1420-1523
    # ===================================================================

    Scenario: Concurrency conflict detected when card modified by another user
        Given the card repository will throw a concurrency conflict on update
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JANE DOE     | Y            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is conflict
        And the update message is "Record changed by some one else. Please review"

    # ===================================================================
    # CARD-BR-008: Write failure after lock
    # COBOL: COCRDUPC.cbl:1477-1483 — REWRITE failure
    # ===================================================================

    Scenario: Update fails due to write failure after lock acquisition
        Given the card repository will throw a write failure on update
        When I update card "4000123456789012" with
            | EmbossedName | ActiveStatus | ExpiryMonth | ExpiryYear |
            | JANE DOE     | Y            | 12          | 2027       |
        And I provide the row version "AQIDBAUGCA=="
        Then the update result is write failure
        And the update message is "Update of record failed"
