---
id: "card-br-007"
title: "Card update workflow state machine"
domain: "card-management"
cobol_source: "COCRDUPC.cbl:275-290,429-543,948-1027"
requirement_id: "CARD-BR-007"
regulations:
  - "PSD2 Art. 97 — Strong Customer Authentication"
  - "FFFS 2014:5 Ch. 8 §4 — Operational risk management"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# CARD-BR-007: Card update workflow state machine

## Summary

The card update program (COCRDUPC) implements a multi-step state machine to control the card modification workflow. A card update progresses through distinct states: details not fetched, show details, changes made (with sub-states for validation errors, awaiting confirmation, confirmed and done, or failed). Each state determines which fields are editable, which PF keys are active, and what action the system takes on the next user interaction. This prevents accidental data modification by requiring explicit confirmation before committing changes.

## Business Logic

### Pseudocode

```
DEFINE states:
    DETAILS-NOT-FETCHED  = (LOW-VALUES, SPACES)  -- Initial state
    SHOW-DETAILS         = 'S'                    -- Card data displayed
    CHANGES-NOT-OK       = 'E'                    -- Validation errors
    CHANGES-OK-NOT-CONFIRMED = 'N'                -- Validated, awaiting F5
    CHANGES-OKAYED-AND-DONE  = 'C'                -- Successfully saved
    CHANGES-OKAYED-LOCK-ERROR = 'L'               -- Lock failed
    CHANGES-OKAYED-BUT-FAILED = 'F'               -- Write failed

EVALUATE current-state:
    WHEN DETAILS-NOT-FETCHED AND PGM-ENTER
        -- Fresh entry: prompt for search keys
        SEND empty form
        SET state = DETAILS-NOT-FETCHED

    WHEN PGM-ENTER AND from-list-program
    WHEN PFK12 AND from-list-program
        -- Arriving from card list with pre-validated keys
        READ card data by card number
        SET state = SHOW-DETAILS
        SEND card details form (search fields protected)

    WHEN SHOW-DETAILS
        -- User has seen data, process their changes
        VALIDATE changed fields
        IF validation-errors
            SET state = CHANGES-NOT-OK
        ELSE IF no-changes-detected
            KEEP state = SHOW-DETAILS
        ELSE
            SET state = CHANGES-OK-NOT-CONFIRMED
        END-IF

    WHEN CHANGES-OK-NOT-CONFIRMED AND PFK05
        -- User confirms save
        PERFORM write-processing
        EVALUATE write-result:
            WHEN lock-failed
                SET state = CHANGES-OKAYED-LOCK-ERROR
            WHEN write-failed
                SET state = CHANGES-OKAYED-BUT-FAILED
            WHEN data-changed-by-another-user
                SET state = SHOW-DETAILS (with refreshed data)
            WHEN success
                SET state = CHANGES-OKAYED-AND-DONE
        END-EVALUATE

    WHEN CHANGES-OKAYED-AND-DONE
    WHEN CHANGES-FAILED
        -- Reset for next operation
        INITIALIZE search keys
        SET state = DETAILS-NOT-FETCHED
END-EVALUATE
```

### Decision Table

| Current State | User Action | Validation Result | Next State |
|--------------|-------------|-------------------|------------|
| DETAILS-NOT-FETCHED | Enter with keys | N/A | SHOW-DETAILS |
| DETAILS-NOT-FETCHED | Enter without keys | N/A | DETAILS-NOT-FETCHED (error msg) |
| SHOW-DETAILS | Enter with changes | Valid | CHANGES-OK-NOT-CONFIRMED |
| SHOW-DETAILS | Enter with changes | Invalid | CHANGES-NOT-OK |
| SHOW-DETAILS | Enter with no changes | N/A | SHOW-DETAILS (no-change msg) |
| CHANGES-NOT-OK | Enter with corrections | Valid | CHANGES-OK-NOT-CONFIRMED |
| CHANGES-NOT-OK | Enter with corrections | Invalid | CHANGES-NOT-OK |
| CHANGES-OK-NOT-CONFIRMED | PF5 (confirm) | Write succeeds | CHANGES-OKAYED-AND-DONE |
| CHANGES-OK-NOT-CONFIRMED | PF5 (confirm) | Lock failed | CHANGES-OKAYED-LOCK-ERROR |
| CHANGES-OK-NOT-CONFIRMED | PF5 (confirm) | Data changed by other | SHOW-DETAILS (refreshed) |
| CHANGES-OK-NOT-CONFIRMED | Enter (no confirm) | N/A | CHANGES-OK-NOT-CONFIRMED |
| CHANGES-OKAYED-AND-DONE | Enter | N/A | DETAILS-NOT-FETCHED |
| CHANGES-FAILED | Enter | N/A | DETAILS-NOT-FETCHED |
| Any state | PF3 | N/A | Exit to caller/menu |

## Source COBOL Reference

**Program:** `COCRDUPC.cbl`
**Lines:** 275-290 (state variable definitions), 429-543 (main EVALUATE), 948-1027 (action decisions)

State variable definition:
```cobol
000276             10 CCUP-CHANGE-ACTION      PIC X(1)
000277                                        VALUE LOW-VALUES.
000278                88 CCUP-DETAILS-NOT-FETCHED
000279                                        VALUES LOW-VALUES, SPACES.
000280                88 CCUP-SHOW-DETAILS    VALUE 'S'.
000281                88 CCUP-CHANGES-MADE    VALUES 'E', 'N', 'C', 'L', 'F'.
000282                88 CCUP-CHANGES-NOT-OK  VALUE 'E'.
000283                88 CCUP-CHANGES-OK-NOT-CONFIRMED VALUE 'N'.
000284                88 CCUP-CHANGES-OKAYED-AND-DONE  VALUE 'C'.
000285                88 CCUP-CHANGES-FAILED  VALUES 'L', 'F'.
000286                88 CCUP-CHANGES-OKAYED-LOCK-ERROR VALUE 'L'.
000287                88 CCUP-CHANGES-OKAYED-BUT-FAILED VALUE 'F'.
```

Valid PF key processing:
```cobol
000413           SET PFK-INVALID TO TRUE
000414           IF CCARD-AID-ENTER OR
000415              CCARD-AID-PFK03 OR
000416              (CCARD-AID-PFK05 AND CCUP-CHANGES-OK-NOT-CONFIRMED)
000417                              OR
000418              (CCARD-AID-PFK12 AND NOT CCUP-DETAILS-NOT-FETCHED)
000419              SET PFK-VALID TO TRUE
000420           END-IF
```

Action decision logic:
```cobol
000948       2000-DECIDE-ACTION.
000949           EVALUATE TRUE
000988              WHEN CCUP-CHANGES-OK-NOT-CONFIRMED
000989               AND CCARD-AID-PFK05
000990                 PERFORM 9200-WRITE-PROCESSING
000991                    THRU 9200-WRITE-PROCESSING-EXIT
000992                 EVALUATE TRUE
000993                    WHEN COULD-NOT-LOCK-FOR-UPDATE
000994                         SET CCUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE
000995                    WHEN LOCKED-BUT-UPDATE-FAILED
000996                       SET CCUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
000997                    WHEN DATA-WAS-CHANGED-BEFORE-UPDATE
000998                        SET CCUP-SHOW-DETAILS            TO TRUE
000999                    WHEN OTHER
001000                       SET CCUP-CHANGES-OKAYED-AND-DONE   TO TRUE
001001                 END-EVALUATE
```

## Acceptance Criteria

### Scenario 1: Fresh entry prompts for search criteria

```gherkin
GIVEN a user navigates to the card update screen from the main menu
  AND no card data has been fetched
WHEN the screen is displayed
THEN the account number and card number fields are editable
  AND the card detail fields (name, status, expiry) are protected
  AND the message "Please enter Account and Card Number" is displayed
```

### Scenario 2: Arriving from card list with pre-selected card

```gherkin
GIVEN a user selects 'U' on a card in the card list screen
WHEN the card update screen is loaded
THEN the card data is fetched using the passed account and card number
  AND all card detail fields are displayed
  AND the account and card number fields are protected (read-only)
  AND the name, status, and expiry fields are editable
```

### Scenario 3: Valid changes require explicit confirmation

```gherkin
GIVEN a user has modified the card name from "JOHN DOE" to "JANE DOE"
  AND all validation rules pass
WHEN the user presses Enter
THEN the message "Changes validated. Press F5 to save" is displayed
  AND all fields become protected (read-only)
  AND the changes are NOT yet committed to the database
```

### Scenario 4: Confirmed save succeeds

```gherkin
GIVEN the changes have been validated and the user sees the confirmation prompt
WHEN the user presses PF5
THEN the card record is updated in the CARDDAT file
  AND the message "Changes committed to database" is displayed
  AND the screen resets for the next operation
```

### Scenario 5: Save fails due to concurrent modification

```gherkin
GIVEN the changes have been validated
  AND another user has modified the same card record since it was loaded
WHEN the user presses PF5 to confirm
THEN the message "Record changed by some one else. Please review" is displayed
  AND the screen shows the refreshed data from the database
  AND the user can make new changes based on the updated data
```

### Scenario 6: PF12 cancels changes and reloads original data

```gherkin
GIVEN the user has made changes to card fields
  AND the update originated from the card list screen
WHEN the user presses PF12
THEN the original card data is re-read from the database
  AND the screen displays the original values
  AND any unsaved changes are discarded
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for payment instrument management | The multi-step confirmation workflow (view → edit → validate → confirm) ensures intentional modification of payment card data |
| FFFS 2014:5 | Ch. 8 §4 | Operational risk management | State machine prevents partial or accidental updates; each state transition is explicit and auditable |

## Edge Cases

1. **Session timeout between validation and confirmation**: If the CICS transaction times out after validation but before PF5 confirmation, the state is lost. The migrated system should implement session-aware state management with appropriate timeout handling.

2. **PF5 pressed in wrong state**: PF5 is only accepted when the state is CHANGES-OK-NOT-CONFIRMED. In all other states, PF5 is treated as an invalid key and remapped to Enter. The migrated system must enforce this state check.

3. **Multiple rapid confirmations**: If PF5 is pressed twice quickly, the second attempt should encounter an already-committed record. The optimistic concurrency check (CARD-BR-008) prevents double-write.

4. **Navigation away and back**: If the user presses PF3 to exit and then re-enters the update screen, the state resets to DETAILS-NOT-FETCHED. No stale state is carried over.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The state machine must be faithfully reproduced in the migrated system. Consider whether a more explicit state pattern (e.g., a State design pattern) would improve maintainability while preserving identical behavior.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
