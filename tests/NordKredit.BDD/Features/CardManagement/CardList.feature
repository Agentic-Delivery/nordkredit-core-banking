@BDD @CardManagement @CARD-BR-001
Feature: Card list display with pagination and filtering
    As a bank operator
    I want to view a paginated list of cards with optional filters
    So that I can browse and locate specific card records

    COBOL source: COCRDLIC.cbl:1123-1411 (PROCESS-PAGE-FORWARD/BACKWARD, FILTER-RECORDS).
    Business rule: CARD-BR-001 (card list display with pagination and filtering).
    Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).

    Background:
        Given the card repository contains the following cards
            | CardNumber       | AccountId   | EmbossedName | ExpirationDate | ActiveStatus |
            | 4000000000000001 | 12345678901 | JOHN DOE     | 2027-12-31     | Y            |
            | 4000000000000002 | 12345678901 | JANE DOE     | 2028-06-15     | Y            |
            | 4000000000000003 | 99999999999 | ERIK SVENSSON| 2027-03-01     | N            |
            | 4000000000000004 | 12345678901 | ANNA LINDQVIST| 2029-01-31    | Y            |
            | 4000000000000005 | 12345678901 | KARL NILSSON | 2028-09-30     | Y            |
            | 4000000000000006 | 12345678901 | MARIA BERG   | 2027-11-15     | N            |
            | 4000000000000007 | 12345678901 | PER HOLM     | 2028-04-20     | Y            |
            | 4000000000000008 | 12345678901 | LISA STRAND  | 2029-07-10     | Y            |
            | 4000000000000009 | 12345678901 | OLOF NORD    | 2028-02-28     | Y            |
            | 4000000000000010 | 55555555555 | STIG LARSSON | 2027-08-31     | N            |

    # COBOL: COCRDLIC.cbl:1129-1163 — 9000-READ-FORWARD first page, WS-MAX-SCREEN-LINES = 7
    Scenario: Display first page of cards without filters
        When I request the first page of cards without filters
        Then the response contains 7 cards
        And the response indicates a next page exists
        And the response indicates no previous page exists

    # COBOL: COCRDLIC.cbl:1165-1200 — PF8 page down, keyset cursor
    Scenario: Navigate forward to next page
        When I request the first page of cards without filters
        And I request the next page using the last card number as cursor
        Then the response contains 3 cards
        And the response indicates no next page exists
        And the response indicates a previous page exists

    # COBOL: COCRDLIC.cbl:1264-1292 — PF7 page up via 9100-READ-BACKWARD
    Scenario: Navigate backward to previous page
        When I request the first page of cards without filters
        And I request the next page using the last card number as cursor
        And I request the previous page using the first card number as cursor
        Then the response contains 7 cards
        And the response indicates a next page exists
        And the response indicates no previous page exists

    # COBOL: COCRDLIC.cbl:1382-1400 — 9500-FILTER-RECORDS by account ID
    Scenario: Filter by account ID
        When I request cards filtered by account ID "99999999999"
        Then the response contains 1 card
        And the response contains card "4000000000000003"

    # COBOL: COCRDLIC.cbl:1382-1400 — 9500-FILTER-RECORDS by card number
    Scenario: Filter by card number
        When I request cards filtered by card number "4000000000000005"
        Then the response contains 1 card
        And the response contains card "4000000000000005"

    # COBOL: COCRDLIC.cbl:1139-1141 — 'NO RECORDS FOUND FOR THIS SEARCH CONDITION'
    Scenario: No records match filter criteria
        When I request cards filtered by account ID "00000000001"
        Then the response contains 0 cards
        And the response message is "NO RECORDS FOUND FOR THIS SEARCH CONDITION"

    # COBOL: COCRDLIC.cbl:1382-1400 — combined filter (account AND card)
    Scenario: Combined account and card filter
        When I request cards filtered by account ID "12345678901" and card number "4000000000000002"
        Then the response contains 1 card
        And the response contains card "4000000000000002"
