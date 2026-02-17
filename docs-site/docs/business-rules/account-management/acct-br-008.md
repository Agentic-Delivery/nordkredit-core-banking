---
id: "acct-br-008"
title: "Account display and list screen presentation"
domain: "account-management"
cobol_source: "COCRDLIC.cbl:258-260, 847-852, 1167-1175"
requirement_id: "ACCT-BR-008"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
  - "FSA FFFS 2014:5 Ch. 7"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# ACCT-BR-008: Account display and list screen presentation

## Summary

Account information is displayed within the card management screens as part of the card list and card detail views. The card list screen (COCRDLIC.cbl) shows the account ID alongside each card in a 7-row paginated display. The card detail screen (COCRDSLC.cbl) shows the full account context when viewing a specific card. Account data is presented either from the current COMMAREA context (`CDEMO-ACCT-ID`) or from the card record itself (`CARD-ACCT-ID`), with zero-value handling to suppress display of empty account fields. There is no dedicated account list or account detail screen in the available COBOL source — account information is always presented in the context of card operations.

## Business Logic

### Pseudocode

```
-- Card list screen: display account alongside each card (COCRDLIC)
PERFORM DISPLAY-CARD-LIST-ROW:
    FOR each card in page (up to 7 rows):
        MOVE CARD-NUM          TO WS-ROW-CARD-NUM(row-index)
        MOVE CARD-ACCT-ID      TO WS-ROW-ACCTNO(row-index)
        MOVE CARD-ACTIVE-STATUS TO WS-ROW-CARD-STATUS(row-index)

        -- Track first card on page for backward navigation
        IF row-index = 1
            MOVE CARD-ACCT-ID  TO WS-CA-FIRST-CARD-ACCT-ID
            MOVE CARD-NUM      TO WS-CA-FIRST-CARD-NUM
        END-IF

        -- Track last card on page for forward navigation
        MOVE CARD-ACCT-ID      TO WS-CA-LAST-CARD-ACCT-ID

-- Card detail/search screen: display account context (COCRDSLC/COCRDUPC)
PERFORM DISPLAY-ACCOUNT-CONTEXT:
    IF CDEMO-ACCT-ID = 0
        MOVE LOW-VALUES TO screen-account-field
        -- Suppress display of zero account ID
    ELSE
        MOVE CC-ACCT-ID TO screen-account-field
    END-IF

-- Card list header: display account filter (COCRDLIC)
PERFORM DISPLAY-FILTER-CONTEXT:
    IF CC-ACCT-ID NOT = ZEROES AND NOT = SPACES
        MOVE CC-ACCT-ID    TO screen-filter-account
    ELSE
        IF CDEMO-ACCT-ID = 0
            MOVE LOW-VALUES TO screen-filter-account
        ELSE
            MOVE CDEMO-ACCT-ID TO screen-filter-account
        END-IF
    END-IF
```

### Screen Layout — Card List (7 rows)

| Column | Field | Width | Source |
|--------|-------|-------|--------|
| Account No | WS-ROW-ACCTNO | 11 chars | CARD-ACCT-ID from card record |
| Card Number | WS-ROW-CARD-NUM | 16 chars | CARD-NUM from card record |
| Status | WS-ROW-CARD-STATUS | 1 char | CARD-ACTIVE-STATUS from card record |

### Zero-Value Suppression Rules

| Context Field | Value | Display Behavior |
|--------------|-------|-----------------|
| CDEMO-ACCT-ID | Non-zero | Display account ID normally |
| CDEMO-ACCT-ID | 0 | Suppress display (show LOW-VALUES/blank) |
| CC-ACCT-ID | Spaces or zeros | Suppress filter display |
| CARD-ACCT-ID | Any value | Always display (from card record) |

## Source COBOL Reference

**Programs:** `COCRDLIC.cbl`, `COCRDSLC.cbl`

### COCRDLIC.cbl — Screen row data structure

```cobol
000256          05  WS-SCREEN-DATA OCCURS 7 TIMES.
000258                     25 WS-ROW-ACCTNO           PIC X(11).
000259                     25 WS-ROW-CARD-NUM         PIC X(16).
000260                     25 WS-ROW-CARD-STATUS      PIC X(1).
```
*(Lines 256-260 — 7 rows, each with account number, card number, and status)*

### COCRDLIC.cbl — Populating account in card list rows

```cobol
001167                      MOVE CARD-NUM     TO WS-ROW-CARD-NUM(
001168                      WS-SCRN-COUNTER)
001169                      MOVE CARD-ACCT-ID TO
001170                      WS-ROW-ACCTNO(WS-SCRN-COUNTER)
001171                      MOVE CARD-ACTIVE-STATUS
001172                                        TO WS-ROW-CARD-STATUS(
001173                                        WS-SCRN-COUNTER)
001174
001175                      IF WS-SCRN-COUNTER = 1
001176                         MOVE CARD-ACCT-ID
001177                                        TO WS-CA-FIRST-CARD-ACCT-ID
001178                         MOVE CARD-NUM  TO WS-CA-FIRST-CARD-NUM
```
*(Lines 1167-1178 — card data including account ID mapped to screen rows)*

### COCRDLIC.cbl — Account context display with zero suppression

```cobol
000847                   IF CC-ACCT-ID NOT = ZEROES
000848                   AND CC-ACCT-ID NOT = SPACES
000849                     MOVE CC-ACCT-ID   TO ACCTSIDO OF CCRDLIAO
000850                   ELSE
000851                  WHEN CDEMO-ACCT-ID = 0
000852                     MOVE LOW-VALUES   TO ACCTSIDO OF CCRDLIAO
```
*(Lines 847-852 — account filter display with zero suppression)*

### COCRDSLC.cbl — Account context on detail screen

```cobol
000462              IF CDEMO-ACCT-ID = 0
000463                 MOVE LOW-VALUES   TO ACCTSIDO OF CCRDSLAO
000464              ELSE
000465                 MOVE CC-ACCT-ID   TO ACCTSIDO OF CCRDSLAO
000466              END-IF
```
*(Lines 462-466 — account ID displayed on card detail search screen)*

## Acceptance Criteria

### Scenario 1: Card list displays account alongside each card

```gherkin
GIVEN the card list screen is loaded with cards from multiple accounts
WHEN the list is displayed
THEN each row shows the card number, account ID, and card status
  AND up to 7 rows are displayed per page
  AND the account ID is the 11-digit CARD-ACCT-ID from each card record
```

### Scenario 2: Zero account ID suppressed on search screen

```gherkin
GIVEN the card detail search screen is loaded
  AND no account context is set (CDEMO-ACCT-ID = 0)
WHEN the screen is rendered
THEN the account ID field displays as blank (LOW-VALUES)
  AND the user can enter an account ID to search
```

### Scenario 3: Account context preserved from filter

```gherkin
GIVEN the user has entered account filter "12345678901"
  AND the card list is displaying filtered results
WHEN the account context is displayed in the header
THEN the account ID "12345678901" is shown in the filter field
  AND only cards for that account are listed
```

### Scenario 4: Pagination preserves account keys

```gherkin
GIVEN the card list is displaying page 1
WHEN the user navigates to page 2
THEN the first and last card keys (including account IDs) from page 1 are used for navigation
  AND the WS-CA-FIRST-CARD-ACCT-ID stores the first card's account
  AND the WS-CA-LAST-CARD-ACCT-ID stores the last card's account
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication before displaying payment account data | Account display is gated by CICS terminal authentication; the migrated system must enforce SCA before showing account/card data |
| GDPR | Art. 15 | Right of access — data subjects can request access to their personal data | The card list with account filtering provides a mechanism for targeted data retrieval in response to GDPR access requests |
| FSA FFFS 2014:5 | Ch. 7 | Information systems must provide accurate and timely data to authorized users | The screen presentation ensures accurate account-card relationships are displayed from the live VSAM files |

## Edge Cases

1. **No dedicated account screen**: The available COBOL source has no standalone account list or account detail screen. Account data is always shown in the card management context. The migrated system should consider whether a dedicated account management UI is needed.

2. **Zero suppression inconsistency**: The card list and card detail screens use different logic for zero suppression. The list checks for ZEROES and SPACES separately (line 847), while the detail screen checks only for CDEMO-ACCT-ID = 0 (line 462). The migrated system should standardize the null/empty handling.

3. **Account ID from card record vs context**: The card list populates WS-ROW-ACCTNO from `CARD-ACCT-ID` (the card record's embedded account), while the search screen uses `CC-ACCT-ID` from the COMMAREA context. If these diverge (e.g., after a card is reassigned to a different account), the display may be inconsistent.

4. **7-row page limitation**: The card list displays only 7 rows per page. For accounts with many cards, users must paginate through multiple pages. The migrated system should support configurable page sizes.

5. **Screen field as output map**: The `ACCTSIDO OF CCRDLIAO` references a BMS map field (CICS screen definition). In the migrated REST API, this becomes a JSON response field. The field name should be meaningful (e.g., `accountId`) rather than preserving the COBOL map name.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Does the mainframe have a dedicated account management screen/program (e.g., COACCTLC for account list)? If so, the program name and function are needed for extraction. (2) Is there an account detail screen that shows full account information (balance, limits, status, expiration)? (3) Should the migrated system include a dedicated account management UI, or is account data always accessed through card operations? (4) Are there any account-level reports or screens for balance inquiry?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
