---
id: "trn-br-001"
title: "Transaction list display with pagination"
domain: "transactions"
cobol_source: "COTRN00C.cbl:94-328"
requirement_id: "TRN-BR-001"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
  - "FSA FFFS 2014:5"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-001: Transaction list display with pagination

## Summary

The transaction list screen displays transactions from the TRANSACT file in a paginated list with a maximum of 10 transactions per page. Users can navigate forward (PF8) and backward (PF7) through the result set. Users can select a transaction by entering 'S' next to it. The list supports filtering by a starting Transaction ID entered in the input field. An optional Transaction ID input allows the user to jump to a specific position in the dataset. This is the primary entry point for transaction inquiry operations, extracted from `COTRN00C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM MAIN-PARA:
    IF COMMAREA is empty (EIBCALEN = 0)
        Return to sign-on screen (COSGN00C)
    END-IF

    IF first entry into program (not reenter)
        SET reenter flag = TRUE
        Initialize screen output to LOW-VALUES
        PERFORM PROCESS-ENTER-KEY
        PERFORM SEND-TRNLST-SCREEN
    ELSE
        RECEIVE screen input
        EVALUATE user key pressed (EIBAID)
            WHEN ENTER
                PERFORM PROCESS-ENTER-KEY
            WHEN PF3
                Return to main menu (COMEN01C)
            WHEN PF7
                PERFORM PROCESS-PF7-KEY (page backward)
            WHEN PF8
                PERFORM PROCESS-PF8-KEY (page forward)
            WHEN OTHER
                Display "Invalid key pressed" error message
        END-EVALUATE
    END-IF

    EXEC CICS RETURN with COMMAREA preserving state

PROCESS-ENTER-KEY:
    Check 10 selection fields (SEL0001 through SEL0010)
    IF a selection flag is set ('S' or 's')
        Transfer control to COTRN01C (transaction view) via XCTL
    ELSE IF selection flag is set but not 'S'
        Display "Invalid selection. Valid value is S"
    END-IF

    IF Transaction ID input is provided
        Validate it is numeric
        IF not numeric
            Display "Tran ID must be Numeric"
            Exit
        END-IF
        Use entered ID as starting position
    ELSE
        Start from beginning (LOW-VALUES)
    END-IF

    Reset page counter to 0
    PERFORM PROCESS-PAGE-FORWARD

PROCESS-PAGE-FORWARD:
    STARTBR on TRANSACT file using TRAN-ID as key
    Skip current record if not initial entry (READNEXT once)
    Initialize all 10 display slots to spaces
    PERFORM READNEXT up to 10 times:
        For each record: populate display fields
        (Transaction ID, date formatted MM/DD/YY, description, amount)
        Track first and last TRAN-ID on page
    After 10 records:
        Attempt one more READNEXT to check if next page exists
        IF more records exist: SET next-page flag = YES
        ELSE: SET next-page flag = NO
    ENDBR on TRANSACT file
    Display page number
    SEND screen

PROCESS-PAGE-BACKWARD:
    STARTBR on TRANSACT file using first TRAN-ID on current page
    Skip current record (READPREV once)
    Initialize all 10 display slots
    Read backwards filling slots 10 down to 1
    PERFORM READPREV up to 10 times:
        Populate display fields in reverse order
    After filling: check if more pages exist backward
    IF page number > 1: SUBTRACT 1 FROM page number
    ENDBR and SEND screen
```

### Decision Table

| User Action | Transaction ID Input | Selection Flag | Outcome |
|---|---|---|---|
| ENTER | Empty | None | Display first page from beginning |
| ENTER | Numeric value | None | Display page starting from that ID |
| ENTER | Non-numeric | None | Error: "Tran ID must be Numeric" |
| ENTER | Any | 'S' or 's' | Transfer to transaction view (COTRN01C) |
| ENTER | Any | Other character | Error: "Invalid selection. Valid value is S" |
| PF7 | N/A | N/A | Navigate to previous page (if page > 1) |
| PF7 (at page 1) | N/A | N/A | Message: "You are already at the top of the page" |
| PF8 | N/A | N/A | Navigate to next page (if more records exist) |
| PF8 (at last page) | N/A | N/A | Message: "You are already at the bottom of the page" |
| PF3 | N/A | N/A | Return to main menu (COMEN01C) |
| Other key | N/A | N/A | Error: "Invalid key pressed" |

## Source COBOL Reference

**Program:** `COTRN00C.cbl`
**Lines:** 94-141 (main control flow), 146-229 (enter key processing), 234-252 (PF7), 257-274 (PF8), 279-328 (page forward), 333-376 (page backward), 381-445 (populate data), 591-619 (STARTBR), 624-653 (READNEXT), 658-687 (READPREV)

```cobol
000050         05 WS-RESP-CD                 PIC S9(09) COMP VALUE ZEROS.
000051         05 WS-REAS-CD                 PIC S9(09) COMP VALUE ZEROS.
000052         05 WS-REC-COUNT               PIC S9(04) COMP VALUE ZEROS.
000053         05 WS-IDX                     PIC S9(04) COMP VALUE ZEROS.
000054         05 WS-PAGE-NUM                PIC S9(04) COMP VALUE ZEROS.
000056         05 WS-TRAN-AMT                PIC +99999999.99.
000057         05 WS-TRAN-DATE               PIC X(08) VALUE '00/00/00'.
```

```cobol
000290              PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
000291                  PERFORM INITIALIZE-TRAN-DATA
000292              END-PERFORM
000295              MOVE 1             TO  WS-IDX
000297              PERFORM UNTIL WS-IDX >= 11 OR TRANSACT-EOF OR ERR-FLG-ON
000298                  PERFORM READNEXT-TRANSACT-FILE
000299                  IF TRANSACT-NOT-EOF AND ERR-FLG-OFF
000300                      PERFORM POPULATE-TRAN-DATA
000301                      COMPUTE WS-IDX = WS-IDX + 1
000302                  END-IF
000303              END-PERFORM
```

### COMMAREA Contract

The program uses COMMAREA to maintain state between CICS transactions:

| Field | PIC | Purpose |
|---|---|---|
| CDEMO-CT00-TRNID-FIRST | X(16) | First transaction ID on current page |
| CDEMO-CT00-TRNID-LAST | X(16) | Last transaction ID on current page |
| CDEMO-CT00-PAGE-NUM | 9(08) | Current page number |
| CDEMO-CT00-NEXT-PAGE-FLG | X(01) | 'Y' if more pages exist forward |
| CDEMO-CT00-TRN-SEL-FLG | X(01) | Selection flag value entered by user |
| CDEMO-CT00-TRN-SELECTED | X(16) | Transaction ID of the selected row |

## Acceptance Criteria

### Scenario 1: Display first page of transactions

GIVEN the transaction list screen is loaded for the first time
  AND no transaction ID filter is provided
WHEN the system reads from the TRANSACT file
THEN up to 10 transactions are displayed on the screen
  AND the page number is set to 1
  AND the first and last transaction IDs on the page are stored for navigation

### Scenario 2: Navigate forward to next page

GIVEN the transaction list screen displays page N
  AND more transactions exist beyond the current page
WHEN the user presses PF8
THEN the next 10 transactions are displayed
  AND the page number increments to N+1
  AND the next-page flag is updated based on remaining records

### Scenario 3: Navigate backward to previous page

GIVEN the transaction list screen displays page N where N > 1
WHEN the user presses PF7
THEN the previous 10 transactions are displayed
  AND the page number decrements to N-1

### Scenario 4: Attempt to navigate backward on first page

GIVEN the transaction list screen displays page 1
WHEN the user presses PF7
THEN the message "You are already at the top of the page..." is displayed
  AND the page contents remain unchanged

### Scenario 5: Attempt to navigate forward on last page

GIVEN the transaction list screen displays the last page
  AND the next-page flag is 'N'
WHEN the user presses PF8
THEN the message "You are already at the bottom of the page..." is displayed
  AND the page contents remain unchanged

### Scenario 6: Select a transaction for viewing

GIVEN the transaction list screen displays transactions
WHEN the user enters 'S' next to a transaction
  AND presses ENTER
THEN control transfers to COTRN01C (transaction view)
  AND the selected transaction ID is passed via COMMAREA

### Scenario 7: Invalid selection character

GIVEN the transaction list screen displays transactions
WHEN the user enters a character other than 'S' or 's' in a selection field
  AND presses ENTER
THEN the message "Invalid selection. Valid value is S" is displayed

### Scenario 8: Filter by transaction ID

GIVEN the transaction list screen is displayed
WHEN the user enters a numeric transaction ID in the input field
  AND presses ENTER
THEN the list displays transactions starting from that ID

### Scenario 9: Non-numeric transaction ID input

GIVEN the transaction list screen is displayed
WHEN the user enters a non-numeric value in the transaction ID field
  AND presses ENTER
THEN the message "Tran ID must be Numeric ..." is displayed

### Scenario 10: Transaction amount formatting

GIVEN a transaction record is read from the TRANSACT file
WHEN the amount is formatted for display
THEN the amount uses the format +99999999.99 (signed, 2 decimal places)
  AND the date is formatted as MM/DD/YY from the timestamp

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 97 | Strong customer authentication for accessing payment account information | Transaction list is gated by CICS terminal authentication; migrated system must enforce SCA before displaying transaction data |
| GDPR | Art. 15 | Data subject's right of access to personal data | The transaction list provides account holders with access to their transaction history; filtering supports targeted data retrieval |
| FSA FFFS 2014:5 | Ch. 7 | Requirements for information systems in financial institutions | Transaction listing provides audit trail access for authorized personnel |

## Edge Cases

1. **Fewer than 10 records in dataset**: When the total number of transactions is fewer than 10, the screen displays only the available records. The ENDFILE response from READNEXT terminates the loop early. The migrated system must handle partial pages without padding empty rows with stale data.

2. **Backward navigation at first page**: When the user presses PF7 on page 1, a message "You are already at the top of the page" is displayed rather than attempting the browse. The SEND-ERASE-NO flag is set to preserve current data.

3. **Transaction ID at file boundaries**: When starting a browse with HIGH-VALUES or LOW-VALUES as the key, the STARTBR positions at the start or end of the file. NOTFND response triggers "You are at the top of the page" message.

4. **Empty COMMAREA**: If EIBCALEN is 0 (no COMMAREA), the program immediately transfers to the sign-on screen (COSGN00C), preventing unauthorized access.

5. **Concurrent browse operations**: The program uses STARTBR/READNEXT/ENDBR sequence for each page display. In the migrated system, this translates to SQL pagination which must preserve the same ordering guarantees as VSAM key-sequential access.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The pagination mechanism using STARTBR/READNEXT on VSAM keys needs validation to ensure the migrated SQL-based pagination produces identical ordering and page boundaries. The 10-records-per-page limit is hardcoded and should be confirmed as a business requirement vs. a screen layout constraint.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
