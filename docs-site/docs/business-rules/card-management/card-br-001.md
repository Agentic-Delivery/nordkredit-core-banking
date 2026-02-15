---
id: "card-br-001"
title: "Card list display with pagination and filtering"
domain: "card-management"
cobol_source: "COCRDLIC.cbl:1123-1411"
requirement_id: "CARD-BR-001"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# CARD-BR-001: Card list display with pagination and filtering

## Summary

The card list screen displays credit cards in a paginated list with a maximum of 7 cards per page. Users can navigate forward and backward through the result set using PF7 (page up) and PF8 (page down). The list supports filtering by account ID and/or card number, showing only records that match the provided criteria. This is the primary entry point for card management operations, extracted from the AWS CardDemo COCRDLIC program.

## Business Logic

### Pseudocode

```
PERFORM DISPLAY-CARD-LIST:
    SET WS-MAX-SCREEN-LINES = 7

    IF filter-account-id IS NOT SPACES
        SET account-filter-active = TRUE
    END-IF
    IF filter-card-number IS NOT SPACES
        SET card-filter-active = TRUE
    END-IF

    IF navigation-direction = FORWARD
        EXEC CICS STARTBR FILE('CARDDAT')
            RIDFLD(last-card-key)
            GTEQ
        END-EXEC
        PERFORM UNTIL line-count >= WS-MAX-SCREEN-LINES
            EXEC CICS READNEXT FILE('CARDDAT')
                INTO(card-record)
                RIDFLD(card-key)
            END-EXEC
            IF RESP = ENDFILE
                SET end-of-file = TRUE
                EXIT PERFORM
            END-IF
            PERFORM 9500-FILTER-RECORDS
            IF record-passes-filter
                MOVE card-record TO screen-line(line-count)
                ADD 1 TO line-count
            END-IF
        END-PERFORM
        EXEC CICS ENDBR FILE('CARDDAT') END-EXEC
    END-IF

    IF navigation-direction = BACKWARD
        EXEC CICS STARTBR FILE('CARDDAT')
            RIDFLD(first-card-key)
            GTEQ
        END-EXEC
        PERFORM UNTIL line-count >= WS-MAX-SCREEN-LINES
            EXEC CICS READPREV FILE('CARDDAT')
                INTO(card-record)
                RIDFLD(card-key)
            END-EXEC
            IF RESP = ENDFILE
                SET start-of-file = TRUE
                EXIT PERFORM
            END-IF
            PERFORM 9500-FILTER-RECORDS
            IF record-passes-filter
                MOVE card-record TO screen-line(line-count)
                ADD 1 TO line-count
            END-IF
        END-PERFORM
        EXEC CICS ENDBR FILE('CARDDAT') END-EXEC
    END-IF

    IF line-count = 0
        MOVE 'NO RECORDS FOUND FOR THIS SEARCH CONDITION'
            TO WS-MESSAGE
    END-IF

    STORE first-card-key and last-card-key for page navigation
    ADD 1 TO WS-CA-SCREEN-NUM (or SUBTRACT 1 for backward)

9500-FILTER-RECORDS:
    IF account-filter-active
        IF card-account-id NOT = filter-account-id
            SET record-passes-filter = FALSE
            EXIT PARAGRAPH
        END-IF
    END-IF
    IF card-filter-active
        IF card-number NOT = filter-card-number
            SET record-passes-filter = FALSE
            EXIT PARAGRAPH
        END-IF
    END-IF
    SET record-passes-filter = TRUE
```

### Decision Table

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

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 178 (page size), 1123-1411 (read forward/backward/filter)

```cobol
000178     05  WS-MAX-SCREEN-LINES        PIC 9(02) VALUE 7.
```

```cobol
001129 9000-READ-FORWARD.
001130     EXEC CICS STARTBR
001131         FILE('CARDDAT')
001132         RIDFLD(WS-LAST-CARD-KEY)
001133         KEYLENGTH(LENGTH OF WS-LAST-CARD-KEY)
001134         GTEQ
001135         RESP(WS-RESP-CD)
001136     END-EXEC.
001137
001138     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
001139         MOVE 'NO RECORDS FOUND FOR THIS SEARCH CONDITION'
001140             TO WS-MESSAGE
001141         GO TO 9000-READ-FORWARD-EXIT
001142     END-IF.
001143
001144     MOVE 0 TO WS-LINE-COUNT.
001145     PERFORM UNTIL WS-LINE-COUNT >= WS-MAX-SCREEN-LINES
001146         EXEC CICS READNEXT
001147             FILE('CARDDAT')
001148             INTO(CARD-RECORD)
001149             RIDFLD(WS-CARD-KEY)
001150             RESP(WS-RESP-CD)
001151         END-EXEC
001152         IF WS-RESP-CD = DFHRESP(ENDFILE)
001153             SET WS-END-OF-FILE TO TRUE
001154             EXIT PERFORM
001155         END-IF
001156         PERFORM 9500-FILTER-RECORDS
001157         IF WS-RECORD-PASSES-FILTER
001158             ADD 1 TO WS-LINE-COUNT
001159             MOVE CARD-RECORD TO WS-SCREEN-LINE(WS-LINE-COUNT)
001160         END-IF
001161     END-PERFORM.
001162
001163     EXEC CICS ENDBR FILE('CARDDAT') END-EXEC.
```

```cobol
001264 9100-READ-BACKWARD.
001265     EXEC CICS STARTBR
001266         FILE('CARDDAT')
001267         RIDFLD(WS-FIRST-CARD-KEY)
001268         KEYLENGTH(LENGTH OF WS-FIRST-CARD-KEY)
001269         GTEQ
001270         RESP(WS-RESP-CD)
001271     END-EXEC.
001272
001273     MOVE 0 TO WS-LINE-COUNT.
001274     PERFORM UNTIL WS-LINE-COUNT >= WS-MAX-SCREEN-LINES
001275         EXEC CICS READPREV
001276             FILE('CARDDAT')
001277             INTO(CARD-RECORD)
001278             RIDFLD(WS-CARD-KEY)
001279             RESP(WS-RESP-CD)
001280         END-EXEC
001281         IF WS-RESP-CD = DFHRESP(ENDFILE)
001282             SET WS-START-OF-FILE TO TRUE
001283             EXIT PERFORM
001284         END-IF
001285         PERFORM 9500-FILTER-RECORDS
001286         IF WS-RECORD-PASSES-FILTER
001287             ADD 1 TO WS-LINE-COUNT
001288             MOVE CARD-RECORD TO WS-SCREEN-LINE(WS-LINE-COUNT)
001289         END-IF
001290     END-PERFORM.
001291
001292     EXEC CICS ENDBR FILE('CARDDAT') END-EXEC.
```

```cobol
001382 9500-FILTER-RECORDS.
001383     SET WS-RECORD-PASSES-FILTER TO TRUE.
001384
001385     IF WS-ACCT-FILTER-ACTIVE
001386         IF CARD-ACCT-ID NOT = WS-FILTER-ACCOUNT-ID
001387             SET WS-RECORD-PASSES-FILTER TO FALSE
001388             GO TO 9500-FILTER-RECORDS-EXIT
001389         END-IF
001390     END-IF.
001391
001392     IF WS-CARD-FILTER-ACTIVE
001393         IF CARD-NUM NOT = WS-FILTER-CARD-NUM
001394             SET WS-RECORD-PASSES-FILTER TO FALSE
001395             GO TO 9500-FILTER-RECORDS-EXIT
001396         END-IF
001397     END-IF.
001398
001399 9500-FILTER-RECORDS-EXIT.
001400     EXIT.
```

## Acceptance Criteria

### Scenario 1: Display first page of cards without filters

```gherkin
GIVEN the card list screen is loaded for the first time
  AND no account or card number filter is provided
WHEN the system retrieves cards from the CARDDAT file
THEN up to 7 cards are displayed on the screen
  AND the page number is set to 1
  AND the first and last card keys on the page are stored for navigation
```

### Scenario 2: Navigate forward to next page

```gherkin
GIVEN the card list screen is displaying page 1 with 7 cards
  AND more records exist beyond the last displayed card
WHEN the user presses PF8 (page down)
THEN the next 7 cards are displayed
  AND the page number increments to 2
  AND the first and last card keys are updated
```

### Scenario 3: Navigate backward to previous page

```gherkin
GIVEN the card list screen is displaying page 2
WHEN the user presses PF7 (page up)
THEN the previous 7 cards are displayed
  AND the page number decrements to 1
```

### Scenario 4: Filter by account ID

```gherkin
GIVEN the user enters an account ID "12345678901" in the account filter field
WHEN the card list is retrieved
THEN only cards belonging to account "12345678901" are displayed
  AND cards from other accounts are excluded
```

### Scenario 5: Filter by card number

```gherkin
GIVEN the user enters a card number "4000123456789012" in the card filter field
WHEN the card list is retrieved
THEN only the card matching "4000123456789012" is displayed
```

### Scenario 6: No records match filter criteria

```gherkin
GIVEN the user enters an account ID that has no associated cards
WHEN the card list is retrieved
THEN the message "NO RECORDS FOUND FOR THIS SEARCH CONDITION" is displayed
  AND no card rows are shown on the screen
```

### Scenario 7: Combined account and card filter

```gherkin
GIVEN the user enters both an account ID and a card number filter
WHEN the card list is retrieved
THEN only cards matching BOTH the account ID AND the card number are displayed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for accessing payment account information | Card list display is gated by CICS terminal authentication; migrated system must enforce SCA before displaying card data |
| GDPR | Art. 15 | Data subject's right of access to personal data | The card list provides account holders and authorized staff with access to card records; filtering supports targeted data retrieval as required for access requests |

## Edge Cases

1. **Fewer than 7 records in dataset**: When the total number of cards (or filtered results) is fewer than 7, the screen displays only the available records. The ENDFILE response from READNEXT terminates the loop early. The migrated system must handle partial pages without padding empty rows with stale data.

2. **Backward navigation at first page**: When the user presses PF7 on the first page, the READPREV encounters ENDFILE immediately. The COBOL program sets WS-START-OF-FILE and keeps the current page displayed. The migrated system must not throw an error or navigate to a negative page number.

3. **Filter produces exactly one page of results**: When filtered results yield exactly 7 records, PF8 should attempt to read forward and discover ENDFILE, then display a message or disable further forward navigation. The COBOL program handles this by checking WS-END-OF-FILE after the read loop.

4. **VSAM file unavailable**: If the CARDDAT file is not available (CICS RESP = NOTOPEN or DISABLED), the COBOL program falls through to the error handler. The migrated system must handle database unavailability gracefully with an appropriate error message and audit log entry.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The pagination mechanism using STARTBR/READNEXT on VSAM keys needs validation to ensure the migrated SQL-based pagination produces identical ordering and page boundaries.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
