---
id: uc-card-01
title: "UC-CARD-01: List Cards"
sidebar_position: 1
---

# UC-CARD-01: List Cards

## Use Case Overview

| Field | Value |
|-------|-------|
| **Use Case ID** | UC-CARD-01 |
| **Title** | List Cards |
| **Domain** | Card Management |
| **Primary Actor** | Bank Operator |
| **Trigger** | Operator navigates to card list screen from main menu |
| **Precondition** | Operator is authenticated via CICS session (PSD2 Art. 97 SCA) |
| **Postcondition** | Card list displayed with navigation and selection options |
| **Source Program** | `COCRDLIC.cbl` |
| **Priority** | High |

## Summary

The card list screen is the hub of the card management subsystem. It displays credit cards in a paginated list (7 per page), supports filtering by account ID and/or card number, and allows the operator to select a card for viewing or updating. Selection transfers control to the appropriate downstream program (card detail or card update) via the shared COMMAREA data contract.

## Business Rules Referenced

| Rule ID | Title | Source Lines |
|---------|-------|-------------|
| [CARD-BR-001](/docs/business-rules/card-management/card-br-001) | Card list display with pagination and filtering | `COCRDLIC.cbl:1123-1411` |
| [CARD-BR-002](/docs/business-rules/card-management/card-br-002) | Card list selection validation | `COCRDLIC.cbl:77-82,1073-1115` |
| [CARD-BR-004](/docs/business-rules/card-management/card-br-004) | Account number input validation | `COCRDLIC.cbl:1003-1034` |
| [CARD-BR-011](/docs/business-rules/card-management/card-br-011) | Inter-program navigation and COMMAREA data contract | `COCRDLIC.cbl:384-406,458-482` |

## User Stories

### US-CARD-01.1: View first page of cards without filters

> As a bank operator, I want to see the first page of cards (up to 7 per page) when I open the card list screen, so that I can browse all cards.

**Business Rule:** CARD-BR-001 (Scenario 1)

**Acceptance Criteria:**

```gherkin
GIVEN the card list screen is loaded for the first time
  AND no account or card number filter is provided
WHEN the system retrieves cards from the CARDDAT file
THEN up to 7 cards are displayed on the screen
  AND the page number is set to 1
  AND the first and last card keys on the page are stored for navigation
```

---

### US-CARD-01.2: Navigate forward/backward through pages

> As a bank operator, I want to navigate forward (PF8) and backward (PF7) through card pages, so that I can browse the full card inventory.

**Business Rule:** CARD-BR-001 (Scenarios 2, 3)

**Acceptance Criteria:**

```gherkin
Scenario: Navigate forward to next page
GIVEN the card list screen is displaying page 1 with 7 cards
  AND more records exist beyond the last displayed card
WHEN the user presses PF8 (page down)
THEN the next 7 cards are displayed
  AND the page number increments to 2
  AND the first and last card keys are updated
```

```gherkin
Scenario: Navigate backward to previous page
GIVEN the card list screen is displaying page 2
WHEN the user presses PF7 (page up)
THEN the previous 7 cards are displayed
  AND the page number decrements to 1
```

**Edge Cases:**
- Backward navigation at first page: PF7 on page 1 keeps the current page displayed without error (COBOL sets WS-START-OF-FILE flag). Migrated system must not navigate to a negative page number.
- Fewer than 7 records: partial pages display only available records without padding empty rows with stale data.
- Exactly 7 filtered results: PF8 discovers ENDFILE, must indicate no more pages.

---

### US-CARD-01.3: Filter cards by account ID

> As a bank operator, I want to filter cards by a specific 11-digit account ID, so that I can see only cards belonging to that account.

**Business Rules:** CARD-BR-001 (Scenario 4), CARD-BR-004 (Scenarios 1, 3-7)

**Acceptance Criteria:**

```gherkin
Scenario: Filter by valid account ID
GIVEN the user enters an account ID "12345678901" in the account filter field
WHEN the card list is retrieved
THEN only cards belonging to account "12345678901" are displayed
  AND cards from other accounts are excluded
```

```gherkin
Scenario: Blank account filter is ignored
GIVEN the user leaves the account filter field blank on the card list screen
WHEN the account filter is validated
THEN no error is raised
  AND the account filter is not applied (all accounts shown)
```

```gherkin
Scenario: Non-numeric account number rejected
GIVEN the user enters "ABCDEFGHIJK" as the account number
WHEN the account number is validated
THEN the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND no data operation is performed
```

```gherkin
Scenario: All-zeros account number rejected
GIVEN the user enters "00000000000" as the account number
WHEN the account number is validated
THEN the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND no data operation is performed
```

**Validation Rules (from CARD-BR-004):**
- Must be exactly 11 digits
- Must be purely numeric (no letters, spaces, or special characters)
- Must not be all zeros (`00000000000`)
- Blank/empty is allowed (filter not applied)

---

### US-CARD-01.4: Filter cards by card number

> As a bank operator, I want to filter cards by a specific 16-digit card number, so that I can find a specific card quickly.

**Business Rule:** CARD-BR-001 (Scenario 5)

**Acceptance Criteria:**

```gherkin
GIVEN the user enters a card number "4000123456789012" in the card filter field
WHEN the card list is retrieved
THEN only the card matching "4000123456789012" is displayed
```

---

### US-CARD-01.5: Combined account and card filter

> As a bank operator, I want to apply both account and card filters simultaneously, so that I can narrow down to an exact card.

**Business Rule:** CARD-BR-001 (Scenario 7)

**Acceptance Criteria:**

```gherkin
GIVEN the user enters both an account ID and a card number filter
WHEN the card list is retrieved
THEN only cards matching BOTH the account ID AND the card number are displayed
```

**Filter Decision Table (from CARD-BR-001):**

| Account Filter | Card Filter | Record Matches Account | Record Matches Card | Outcome |
|---------------|------------|----------------------|--------------------|---------|
| Not provided  | Not provided | N/A                  | N/A                | Record displayed |
| Provided      | Not provided | Yes                  | N/A                | Record displayed |
| Provided      | Not provided | No                   | N/A                | Record skipped |
| Not provided  | Provided     | N/A                  | Yes                | Record displayed |
| Not provided  | Provided     | N/A                  | No                 | Record skipped |
| Provided      | Provided     | Yes                  | Yes                | Record displayed |
| Provided      | Provided     | Yes                  | No                 | Record skipped |
| Provided      | Provided     | No                   | Yes                | Record skipped |
| Provided      | Provided     | No                   | No                 | Record skipped |

---

### US-CARD-01.6: Select card for viewing ('S')

> As a bank operator, I want to select a card by entering 'S' next to it, so that I can view its full details in the card detail screen (COCRDSLC).

**Business Rules:** CARD-BR-002 (Scenario 1), CARD-BR-011 (Scenario 1)

**Acceptance Criteria:**

```gherkin
GIVEN the card list displays cards including card "4000123456789012" on account "12345678901"
  AND the user enters 'S' next to that card
WHEN the selection is validated
THEN the system transfers control to the card detail view program (COCRDSLC)
  AND the selected card's account number and card number are passed via COMMAREA
  AND the detail screen shows the card data with search fields protected (read-only)
```

---

### US-CARD-01.7: Select card for updating ('U')

> As a bank operator, I want to select a card by entering 'U' next to it, so that I can modify its details in the card update screen (COCRDUPC).

**Business Rules:** CARD-BR-002 (Scenario 2), CARD-BR-011 (Scenario 2)

**Acceptance Criteria:**

```gherkin
GIVEN the card list displays cards including card "4000123456789012"
  AND the user enters 'U' next to that card
WHEN the selection is validated
THEN the system transfers control to the card update program (COCRDUPC)
  AND the selected card's account number and card number are passed via COMMAREA
  AND the update screen shows the card data with account/card fields protected
  AND the name, status, and expiry fields are editable
```

---

### US-CARD-01.8: Validate single selection only

> As a bank operator, I want the system to reject multiple simultaneous selections, so that only one card is processed at a time.

**Business Rule:** CARD-BR-002 (Scenario 3)

**Acceptance Criteria:**

```gherkin
GIVEN the card list displays 5 cards
  AND the user enters 'S' next to the 1st card row
  AND the user enters 'U' next to the 4th card row
WHEN the selection is validated
THEN the error message "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE" is displayed
  AND no program transfer occurs
```

---

### US-CARD-01.9: Validate action codes

> As a bank operator, I want the system to reject invalid action codes (only 'S' and 'U' are valid), so that I receive clear feedback on incorrect input.

**Business Rule:** CARD-BR-002 (Scenarios 4, 5)

**Acceptance Criteria:**

```gherkin
Scenario: Invalid action code rejected
GIVEN the card list displays 5 cards
  AND the user enters 'X' next to the 2nd card row
WHEN the selection is validated
THEN the error message "INVALID ACTION CODE" is displayed
  AND no program transfer occurs
```

```gherkin
Scenario: No selection made
GIVEN the card list displays 5 cards
  AND the user does not enter any action code
WHEN the screen is submitted
THEN the card list remains displayed without change
  AND no error message is shown
```

**Selection Decision Table (from CARD-BR-002):**

| Number of Selections | Action Code | Outcome |
|---------------------|-------------|---------|
| 0                   | N/A         | No action taken, list remains displayed |
| 1                   | 'S'         | Transfer to COCRDSLC (card detail view) via XCTL |
| 1                   | 'U'         | Transfer to COCRDUPC (card update) via XCTL |
| 1                   | Other       | Error: "INVALID ACTION CODE" |
| >1                  | Any valid   | Error: "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE" |
| >1                  | Mix of valid/invalid | Error: "INVALID ACTION CODE" (invalid detected first) |

**Edge Cases:**
- Invalid action code before valid ones: the validation loop exits on the first invalid code (fail-fast). If 'X' is in row 2 and 'S' in row 5, the invalid code error is reported first.
- Case sensitivity: COBOL checks for uppercase 'S' and 'U' only. Lowercase 's'/'u' are treated as invalid. The migrated system should decide whether to accept lowercase (domain expert question).
- Empty rows: action codes on unpopulated rows must be ignored.

## Navigation Flow

```
Main Menu (COMEN01C)
     |
     | XCTL (Enter)
     v
Card List (COCRDLIC) <──────┐
     |                       |
     ├── 'S' selection       │ PF3
     │   XCTL                │ XCTL
     v                       │
Card Detail (COCRDSLC) ──────┘
     |
     |
Card List (COCRDLIC) <──────┐
     |                       |
     ├── 'U' selection       │ PF3
     │   XCTL                │ XCTL
     v                       │
Card Update (COCRDUPC) ──────┘
```

**PF3 Exit Behavior (from CARD-BR-011):**
- If `CDEMO-FROM-PROGRAM` is populated: returns to calling program
- If `CDEMO-FROM-PROGRAM` is SPACES or LOW-VALUES: returns to main menu (COMEN01C)

## No Records Found

```gherkin
GIVEN the user enters an account ID that has no associated cards
WHEN the card list is retrieved
THEN the message "NO RECORDS FOUND FOR THIS SEARCH CONDITION" is displayed
  AND no card rows are shown on the screen
```

**Business Rule:** CARD-BR-001 (Scenario 6)

## Regulatory Traceability

| Regulation | Article/Section | Requirement | How This Use Case Satisfies It |
|------------|----------------|-------------|-------------------------------|
| PSD2 | Art. 97 | Strong customer authentication for accessing payment account information | Card list display is gated by CICS terminal authentication; migrated system must enforce SCA before displaying card data |
| GDPR | Art. 15 | Data subject's right of access to personal data | The card list provides authorized staff with access to card records; filtering supports targeted data retrieval as required for access requests |
| FFFS 2014:5 | Ch. 4 §3 | Adequate systems for managing operational risk, including input validation controls | Account number validation (CARD-BR-004) prevents malformed input from reaching data access layers |
| FFFS 2014:5 | Ch. 8 §4 | Internal controls must ensure that only authorized operations are performed | Single-selection constraint and action code validation (CARD-BR-002) prevent unintended operations; navigation flow maintains controlled access to card detail/update functions |

## Migration Notes

1. **Pagination**: VSAM STARTBR/READNEXT keyset pagination must map to SQL keyset pagination (not OFFSET/FETCH) for equivalent ordering and page boundaries.
2. **Filter type**: Filters are exact match only (not partial/wildcard).
3. **Page size**: 7 records per page (distinct from transactions which use 10).
4. **Action codes**: 'S' and 'U' uppercase only; case sensitivity is a domain expert question for the migrated system.
5. **XCTL transfers**: CICS program transfers map to REST API endpoints or client-side routing in the migrated system. COMMAREA context maps to session/JWT claims or API request parameters.
6. **COMMAREA data contract**: The shared CARDDEMO-COMMAREA carries `CDEMO-FROM-PROGRAM`, `CDEMO-ACCT-ID`, `CDEMO-CARD-NUM`, and navigation state between programs. This becomes the API request/response schema in the migrated system.

## Open Questions (Awaiting Domain Expert Review)

1. Should the migrated system accept lowercase action codes ('s', 'u') with uppercase conversion, or maintain strict uppercase-only validation as in the COBOL original?
2. Confirm the 11-digit account number format against the NordKredit account numbering scheme.
3. Verify that SQL-based pagination produces identical ordering and page boundaries as the VSAM keyset pagination.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
