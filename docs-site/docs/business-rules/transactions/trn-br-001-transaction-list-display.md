---
id: "TRN-BR-001"
title: "Transaction list display with paginated browsing"
domain: "transactions"
cobol_source: "COTRN00C.cbl:94-328"
requirement_id: "TRN-BR-001"
regulations:
  - "FFFS 2014:5 Ch. 8 — Operational requirements for information systems"
  - "PSD2 Art. 94 — Access to transaction history"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# TRN-BR-001: Transaction list display with paginated browsing

## Summary

The transaction list screen (CICS transaction CT00) displays transactions from the TRANSACT VSAM file in a paginated list of 10 records per page. Users can browse forward (PF8) and backward (PF7), and optionally filter by entering a starting Transaction ID. The screen tracks the first and last transaction IDs on the current page to enable bidirectional navigation. This is the primary online interface for operations staff to view posted transactions.

## Business Logic

### Pseudocode

```
PERFORM MAIN-FLOW:
    IF no COMMAREA (first entry)
        RETURN to sign-on screen (COSGN00C)
    END-IF

    IF first time entering program
        SET program-reenter flag
        INITIALIZE screen fields
        PERFORM page-forward from beginning
        SEND transaction list screen
    ELSE
        RECEIVE screen input
        EVALUATE key pressed:
            WHEN ENTER  → process selection or filter by Tran ID
            WHEN PF3    → return to main menu (COMEN01C)
            WHEN PF7    → page backward (if page > 1)
            WHEN PF8    → page forward (if more pages exist)
            WHEN OTHER  → display "Invalid key" error
        END-EVALUATE
    END-IF

PROCESS-PAGE-FORWARD:
    STARTBR TRANSACT file at current position
    READ 10 records sequentially (READNEXT)
    FOR each record:
        Display: Transaction ID, Date (MM/DD/YY), Description, Amount
        Track first and last Transaction IDs on page
    END-FOR
    CHECK if 11th record exists (next-page indicator)
    INCREMENT page counter
    ENDBR TRANSACT file

PROCESS-PAGE-BACKWARD:
    STARTBR TRANSACT file at first record on current page
    READ 10 records in reverse (READPREV)
    DECREMENT page counter
    ENDBR TRANSACT file
```

### Decision Table

| User Action | Page Position | Outcome |
|-------------|--------------|---------|
| Enter (no selection) | Any | Refresh page from entered Tran ID or beginning |
| Enter + selection 'S' | Any | Navigate to transaction view (COTRN01C) |
| Enter + invalid selection | Any | Error: "Invalid selection. Valid value is S" |
| PF7 | Page > 1 | Navigate to previous page |
| PF7 | Page 1 | Message: "You are already at the top of the page..." |
| PF8 | More pages exist | Navigate to next page |
| PF8 | Last page | Message: "You are already at the bottom of the page..." |
| PF3 | Any | Return to main menu |
| Other key | Any | Error: "Invalid key pressed" |

## Source COBOL Reference

**Program:** `COTRN00C.cbl`
**Lines:** 94-328

```cobol
000094 PROCEDURE DIVISION.
000095 MAIN-PARA.
000097     SET ERR-FLG-OFF TO TRUE
000098     SET TRANSACT-NOT-EOF TO TRUE
000099     SET NEXT-PAGE-NO TO TRUE
000100     SET SEND-ERASE-YES TO TRUE
...
000146 PROCESS-ENTER-KEY.
000148     EVALUATE TRUE
000149         WHEN SEL0001I OF COTRN0AI NOT = SPACES AND LOW-VALUES
000150             MOVE SEL0001I OF COTRN0AI TO CDEMO-CT00-TRN-SEL-FLG
000151             MOVE TRNID01I OF COTRN0AI TO CDEMO-CT00-TRN-SELECTED
...
000183     IF (CDEMO-CT00-TRN-SEL-FLG NOT = SPACES AND LOW-VALUES) AND
000184        (CDEMO-CT00-TRN-SELECTED NOT = SPACES AND LOW-VALUES)
000185         EVALUATE CDEMO-CT00-TRN-SEL-FLG
000186             WHEN 'S'
000187             WHEN 's'
000188                  MOVE 'COTRN01C' TO CDEMO-TO-PROGRAM
...
000206     IF TRNIDINI OF COTRN0AI = SPACES OR LOW-VALUES
000207         MOVE LOW-VALUES TO TRAN-ID
000208     ELSE
000209         IF TRNIDINI OF COTRN0AI IS NUMERIC
000210             MOVE TRNIDINI OF COTRN0AI TO TRAN-ID
000211         ELSE
000212             MOVE 'Y' TO WS-ERR-FLG
000213             MOVE 'Tran ID must be Numeric ...' TO WS-MESSAGE
...
000279 PROCESS-PAGE-FORWARD.
000290     PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
000291         PERFORM INITIALIZE-TRAN-DATA
000292     END-PERFORM
000297     PERFORM UNTIL WS-IDX >= 11 OR TRANSACT-EOF OR ERR-FLG-ON
000298         PERFORM READNEXT-TRANSACT-FILE
000299         IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
000300             PERFORM POPULATE-TRAN-DATA
000301             COMPUTE WS-IDX = WS-IDX + 1
000302         END-IF
000303     END-PERFORM
```

## Acceptance Criteria

### Scenario 1: Display first page of transactions

```gherkin
GIVEN the TRANSACT file contains 25 transactions
WHEN the user enters the transaction list screen
THEN the first 10 transactions are displayed
  AND the page number shows 1
  AND the next-page indicator is enabled
```

### Scenario 2: Page forward to next page

```gherkin
GIVEN the user is viewing page 1 of transactions
  AND more transactions exist beyond the current page
WHEN the user presses PF8
THEN the next 10 transactions are displayed
  AND the page number increments to 2
```

### Scenario 3: Page backward from page 2

```gherkin
GIVEN the user is viewing page 2 of transactions
WHEN the user presses PF7
THEN the previous 10 transactions are displayed
  AND the page number decrements to 1
```

### Scenario 4: Attempt to page backward from page 1

```gherkin
GIVEN the user is viewing page 1 of transactions
WHEN the user presses PF7
THEN the message "You are already at the top of the page..." is displayed
  AND the page remains on page 1
```

### Scenario 5: Filter by Transaction ID

```gherkin
GIVEN the user enters a numeric Transaction ID in the filter field
WHEN the user presses ENTER
THEN the transaction list refreshes starting from that Transaction ID
  AND the page number resets to 1
```

### Scenario 6: Non-numeric Transaction ID filter

```gherkin
GIVEN the user enters a non-numeric value in the Transaction ID filter field
WHEN the user presses ENTER
THEN the error message "Tran ID must be Numeric ..." is displayed
  AND the transaction list is not refreshed
```

### Scenario 7: Select transaction for viewing

```gherkin
GIVEN the user enters 'S' in the selection field next to a transaction
WHEN the user presses ENTER
THEN the system navigates to the transaction view screen (COTRN01C)
  AND the selected Transaction ID is passed via COMMAREA
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 | Information systems must provide adequate operational support | Transaction list provides operational visibility into posted transactions |
| PSD2 | Art. 94 | Payment service providers must retain transaction data accessible for reference | The paginated browser enables access to the full transaction history |

## Edge Cases

1. **Empty TRANSACT file**: If no transactions exist, the screen displays with all 10 rows blank and no pagination controls active. The page number remains at 0.

2. **Exactly 10 transactions**: The system reads an 11th record to determine if a next page exists. If exactly 10 records exist, the next-page flag is set to 'N' after failing to read record 11.

3. **Transaction ID filter beyond last record**: STARTBR with a TRAN-ID past the last record results in NOTFND, which sets the TRANSACT-EOF flag and displays "You are at the top of the page..." message.

4. **Case-insensitive selection**: The selection field accepts both 'S' and 's' (lines 186-187). Any other non-space character results in "Invalid selection. Valid value is S".

5. **Amount display format**: Transaction amounts are formatted as `+99999999.99` using the `WS-TRAN-AMT` picture clause, preserving sign information for credits and debits.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Are there any access control rules for who can view the transaction list?
- Should the date display format (MM/DD/YY) be preserved or updated to ISO 8601 for the Swedish market?
- What is the expected behavior when the TRANSACT file is locked by a batch job?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
