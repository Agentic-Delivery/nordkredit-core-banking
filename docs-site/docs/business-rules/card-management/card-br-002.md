---
id: "card-br-002"
title: "Card list selection validation"
domain: "card-management"
cobol_source: "COCRDLIC.cbl:77-82,1073-1115"
requirement_id: "CARD-BR-002"
regulations:
  - "FFFS 2014:5 Ch. 8 §4"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# CARD-BR-002: Card list selection validation

## Summary

The card list screen allows users to select a card for viewing or updating by entering an action code next to a card row. Only two action codes are valid: 'S' for viewing card details and 'U' for updating card information. The system enforces a single-selection constraint -- only one card may be selected at a time. Invalid action codes or multiple simultaneous selections produce specific error messages. Upon valid selection, the program transfers control to the appropriate downstream program (COCRDSLC for view, COCRDUPC for update) passing the selected card's account number and card number via COMMAREA.

## Business Logic

### Pseudocode

```
PERFORM 2250-EDIT-ARRAY:
    SET selection-count = 0
    SET selected-index = 0

    PERFORM VARYING idx FROM 1 BY 1
        UNTIL idx > WS-MAX-SCREEN-LINES
        IF action-code(idx) NOT = SPACES
            EVALUATE action-code(idx)
                WHEN 'S'
                WHEN 'U'
                    ADD 1 TO selection-count
                    SET selected-index = idx
                WHEN OTHER
                    MOVE 'INVALID ACTION CODE' TO WS-MESSAGE
                    SET input-error = TRUE
                    GO TO 2250-EXIT
            END-EVALUATE
        END-IF
    END-PERFORM.

    IF selection-count > 1
        MOVE 'PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE'
            TO WS-MESSAGE
        SET input-error = TRUE
        GO TO 2250-EXIT
    END-IF.

    IF selection-count = 1
        MOVE card-account(selected-index) TO COMMAREA-ACCT-ID
        MOVE card-number(selected-index) TO COMMAREA-CARD-NUM

        IF action-code(selected-index) = 'S'
            EXEC CICS XCTL PROGRAM('COCRDSLC')
                COMMAREA(WS-COMMAREA)
            END-EXEC
        ELSE
            EXEC CICS XCTL PROGRAM('COCRDUPC')
                COMMAREA(WS-COMMAREA)
            END-EXEC
        END-IF
    END-IF.

2250-EXIT.
    EXIT.
```

### Decision Table

| Number of Selections | Action Code | Outcome |
|---------------------|-------------|---------|
| 0                   | N/A         | No action taken, list remains displayed |
| 1                   | 'S'         | Transfer to COCRDSLC (card detail view) via XCTL |
| 1                   | 'U'         | Transfer to COCRDUPC (card update) via XCTL |
| 1                   | Other       | Error: "INVALID ACTION CODE" |
| >1                  | Any valid   | Error: "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE" |
| >1                  | Mix of valid/invalid | Error: "INVALID ACTION CODE" (invalid detected first) |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 77-82 (selection code definitions), 1073-1115 (validation logic), 517-569 (XCTL transfers)

```cobol
000077     05  WS-ACTION-CODES.
000078         10  WS-ACTION-VIEW         PIC X(01) VALUE 'S'.
000079         10  WS-ACTION-UPDATE       PIC X(01) VALUE 'U'.
000080     05  WS-SELECTION-COUNT         PIC 9(02) VALUE 0.
000081     05  WS-SELECTED-INDEX          PIC 9(02) VALUE 0.
000082     05  WS-INPUT-ERROR-FLAG        PIC X(01) VALUE 'N'.
```

```cobol
001073 2250-EDIT-ARRAY.
001074     MOVE 0 TO WS-SELECTION-COUNT.
001075     MOVE 0 TO WS-SELECTED-INDEX.
001076
001077     PERFORM VARYING WS-IDX FROM 1 BY 1
001078         UNTIL WS-IDX > WS-MAX-SCREEN-LINES
001079         IF WS-ACTION-CODE(WS-IDX) NOT = SPACES
001080             EVALUATE WS-ACTION-CODE(WS-IDX)
001081                 WHEN 'S'
001082                 WHEN 'U'
001083                     ADD 1 TO WS-SELECTION-COUNT
001084                     MOVE WS-IDX TO WS-SELECTED-INDEX
001085                 WHEN OTHER
001086                     MOVE 'INVALID ACTION CODE'
001087                         TO WS-MESSAGE
001088                     SET WS-INPUT-ERROR TO TRUE
001089                     GO TO 2250-EDIT-ARRAY-EXIT
001090             END-EVALUATE
001091         END-IF
001092     END-PERFORM.
001093
001094     IF WS-SELECTION-COUNT > 1
001095         MOVE 'PLEASE SELECT ONLY ONE RECORD TO VIEW'
001096           ' OR UPDATE' TO WS-MESSAGE
001097         SET WS-INPUT-ERROR TO TRUE
001098         GO TO 2250-EDIT-ARRAY-EXIT
001099     END-IF.
001100
001101     IF WS-SELECTION-COUNT = 1
001102         MOVE WS-CARD-ACCT(WS-SELECTED-INDEX)
001103             TO WS-CA-ACCT-ID
001104         MOVE WS-CARD-NUM(WS-SELECTED-INDEX)
001105             TO WS-CA-CARD-NUM
001106     END-IF.
001107
001108 2250-EDIT-ARRAY-EXIT.
001109     EXIT.
```

```cobol
000517     IF WS-ACTION-CODE(WS-SELECTED-INDEX) = 'S'
000518         EXEC CICS XCTL
000519             PROGRAM('COCRDSLC')
000520             COMMAREA(WS-COMMAREA)
000521             LENGTH(LENGTH OF WS-COMMAREA)
000522             RESP(WS-RESP-CD)
000523         END-EXEC
000524         IF WS-RESP-CD NOT = DFHRESP(NORMAL)
000525             MOVE 'TRANSFER TO VIEW PROGRAM FAILED'
000526                 TO WS-MESSAGE
000527         END-IF
000541     END-IF.
000542
000545     IF WS-ACTION-CODE(WS-SELECTED-INDEX) = 'U'
000546         EXEC CICS XCTL
000547             PROGRAM('COCRDUPC')
000548             COMMAREA(WS-COMMAREA)
000549             LENGTH(LENGTH OF WS-COMMAREA)
000550             RESP(WS-RESP-CD)
000551         END-EXEC
000552         IF WS-RESP-CD NOT = DFHRESP(NORMAL)
000553             MOVE 'TRANSFER TO UPDATE PROGRAM FAILED'
000554                 TO WS-MESSAGE
000555         END-IF
000569     END-IF.
```

## Acceptance Criteria

### Scenario 1: Select a card for viewing

```gherkin
GIVEN the card list displays 5 cards
  AND the user enters 'S' next to the 3rd card row
WHEN the selection is validated
THEN the system transfers control to the card detail view program (COCRDSLC)
  AND the selected card's account number and card number are passed via COMMAREA
```

### Scenario 2: Select a card for updating

```gherkin
GIVEN the card list displays 5 cards
  AND the user enters 'U' next to the 2nd card row
WHEN the selection is validated
THEN the system transfers control to the card update program (COCRDUPC)
  AND the selected card's account number and card number are passed via COMMAREA
```

### Scenario 3: Multiple selections rejected

```gherkin
GIVEN the card list displays 5 cards
  AND the user enters 'S' next to the 1st card row
  AND the user enters 'U' next to the 4th card row
WHEN the selection is validated
THEN the error message "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE" is displayed
  AND no program transfer occurs
```

### Scenario 4: Invalid action code rejected

```gherkin
GIVEN the card list displays 5 cards
  AND the user enters 'X' next to the 2nd card row
WHEN the selection is validated
THEN the error message "INVALID ACTION CODE" is displayed
  AND no program transfer occurs
```

### Scenario 5: No selection made

```gherkin
GIVEN the card list displays 5 cards
  AND the user does not enter any action code
WHEN the screen is submitted
THEN the card list remains displayed without change
  AND no error message is shown
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 §4 | Internal controls must ensure that only authorized operations are performed | The single-selection constraint and action code validation prevent unintended operations; only 'S' (view) and 'U' (update) are permitted |
| PSD2 | Art. 97 | Strong customer authentication for payment account access | Selection validation ensures the correct card context is passed to downstream programs that enforce SCA; prevents parameter tampering through COMMAREA integrity |

## Edge Cases

1. **Empty list with selection attempt**: If the card list has no records displayed (e.g., after a filter returns zero results), but the user enters an action code in a row position, the action code field corresponds to an empty row. The COBOL code checks action codes only for populated rows. The migrated system must not allow selection of empty/placeholder rows.

2. **Invalid action code before valid ones**: The COBOL validation loop exits immediately upon encountering the first invalid action code (GO TO 2250-EDIT-ARRAY-EXIT). This means if an invalid code appears in row 2 and a valid 'S' appears in row 5, the invalid code error is reported first. The migrated system should preserve this fail-fast behavior.

3. **Case sensitivity of action codes**: The COBOL program checks for uppercase 'S' and 'U' only. Lowercase 's' or 'u' would be treated as invalid action codes. The migrated system should decide whether to accept lowercase input (with uppercase conversion) or maintain strict uppercase-only validation. This requires domain expert confirmation.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Need to confirm whether the migrated system should accept lowercase action codes ('s', 'u') or maintain strict uppercase-only validation as in the original COBOL. Also need to verify that the COMMAREA data contract between list and detail/update programs is fully documented.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
