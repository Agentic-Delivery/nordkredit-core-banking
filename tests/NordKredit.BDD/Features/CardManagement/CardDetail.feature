@BDD @CardManagement @CARD-BR-003
Feature: Card detail lookup by card number or account
    As a bank operator
    I want to view detailed information for a single card
    So that I can review card data for customer service or compliance

    COBOL source: COCRDSLC.cbl:608-812 (card detail lookup — primary key and alternate index).
    Business rules: CARD-BR-003 (card detail lookup), CARD-BR-009 (data structure).
    Regulations: PSD2 Art. 97 (SCA), GDPR Art. 15 (right of access).

    Background:
        Given the card repository contains the following cards
            | CardNumber       | AccountId   | EmbossedName | ExpirationDate | ActiveStatus |
            | 4000123456789012 | 12345678901 | JOHN DOE     | 2027-12-31     | Y            |
            | 4000123456789099 | 12345678901 | JANE DOE     | 2028-06-15     | Y            |

    # COBOL: COCRDSLC.cbl:736-774 — 9100-GETCARD-BYACCTCARD (READ CARDDAT by primary key)
    Scenario: Successful lookup by card number
        When I request card detail for card number "4000123456789012"
        Then the card detail response contains card number "4000123456789012"
        And the card detail response contains account ID "12345678901"
        And the card detail response contains embossed name "JOHN DOE"
        And the card detail response contains expiration date "2027-12-31"
        And the card detail response contains active status 'Y'

    # COBOL: COCRDSLC.cbl:779-812 — 9150-GETCARD-BYACCT (READ CARDAIX by alternate index)
    Scenario: Successful lookup by account ID via alternate index
        When I request card detail for account ID "12345678901"
        Then the card detail response contains account ID "12345678901"
        And the card detail response is not null

    # COBOL: COCRDSLC.cbl:753-760 — card not found response
    Scenario: Card not found
        When I request card detail for card number "9999999999999999"
        Then the card detail response is null

    # COBOL: COCRDSLC.cbl:790-797 — account not found via alternate index
    Scenario: Account not found via alternate index
        When I request card detail for account ID "00000000002"
        Then the card detail response is null

    # COBOL: COCRDSLC.cbl:736-774 — verify expiration date format YYYY-MM-DD (CARD-BR-009)
    Scenario: Expiration date format preserved as YYYY-MM-DD
        When I request card detail for card number "4000123456789099"
        Then the card detail response contains expiration date "2028-06-15"

    # PCI-DSS: CVV excluded from detail response
    Scenario: CVV code excluded from detail response
        Given the card "4000123456789012" has CVV code "123"
        When I request card detail for card number "4000123456789012"
        Then the card detail response does not contain a CVV code
