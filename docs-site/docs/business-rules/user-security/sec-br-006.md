---
id: "sec-br-006"
title: "Data modification authorization and confirmation workflow"
domain: "user-security"
cobol_source: "COCRDUPC.cbl:429-543,COCRDUPC.cbl:948-1027,COCRDUPC.cbl:988-1001,COCRDUPC.cbl:469-476"
requirement_id: "SEC-BR-006"
regulations:
  - "PSD2 Art. 97"
  - "PSD2 Art. 98"
  - "FFFS 2014:5 Ch. 8 §4"
  - "FFFS 2014:5 Ch. 4 §3"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# SEC-BR-006: Data modification authorization and confirmation workflow

## Summary

The card update program (COCRDUPC) implements a multi-step authorization workflow for data modifications. Changes cannot be saved directly — they must pass through a validation gate, a user confirmation step (PF5), and a CICS SYNCPOINT (transaction commit) before being written to the VSAM file. This three-phase pattern (validate → confirm → commit) ensures that data modifications are intentional and verified before persistence. The program also implements optimistic concurrency control, re-reading the record before update to detect intervening changes, and uses CICS SYNCPOINT to make the update atomic and recoverable.

## Business Logic

### Pseudocode

```
MODIFICATION WORKFLOW STATE MACHINE:

State 1: DETAILS-NOT-FETCHED
    -- User enters account and card number
    -- Program fetches card record from VSAM
    -- Transition to: SHOW-DETAILS

State 2: SHOW-DETAILS
    -- Card data displayed for editing
    -- User modifies fields (name, status, expiry)
    -- Program validates all changed fields
    -- IF validation passes:
        Transition to: CHANGES-OK-NOT-CONFIRMED
    -- IF validation fails:
        Transition to: CHANGES-NOT-OK (stay on form)

State 3: CHANGES-OK-NOT-CONFIRMED
    -- Validation passed, user sees "Press F5 to save"
    -- PF5 accepted ONLY in this state
    -- IF user presses PF5:
        PERFORM 9200-WRITE-PROCESSING
        -- Lock record for update (CICS READ UPDATE)
        -- Re-read to check for concurrent changes
        -- IF data changed by another user:
            Transition to: SHOW-DETAILS (re-fetch)
        -- IF lock failed:
            Transition to: CHANGES-OKAYED-LOCK-ERROR
        -- IF update failed:
            Transition to: CHANGES-OKAYED-BUT-FAILED
        -- IF success:
            EXEC CICS SYNCPOINT -- Commit transaction
            Transition to: CHANGES-OKAYED-AND-DONE
    -- IF user presses ENTER (not PF5):
        Stay in current state (re-display confirmation prompt)

State 4: CHANGES-OKAYED-AND-DONE
    -- Success message displayed
    -- Program resets search keys
    -- XCTL back to calling program (card list)

State 5: CHANGES-FAILED (lock error or update error)
    -- Failure message displayed
    -- Program resets search keys
    -- User can retry with fresh search
```

### State Transition Diagram

```
[DETAILS-NOT-FETCHED] --fetch--> [SHOW-DETAILS]
[SHOW-DETAILS] --valid changes--> [CHANGES-OK-NOT-CONFIRMED]
[SHOW-DETAILS] --invalid changes--> [CHANGES-NOT-OK] --> [SHOW-DETAILS]
[CHANGES-OK-NOT-CONFIRMED] --PF5--> [WRITE-PROCESSING]
[WRITE-PROCESSING] --success--> [CHANGES-OKAYED-AND-DONE] --XCTL--> [CALLER]
[WRITE-PROCESSING] --lock-error--> [CHANGES-FAILED] --> [DETAILS-NOT-FETCHED]
[WRITE-PROCESSING] --concurrent-change--> [SHOW-DETAILS] (re-fetch)
```

## Source COBOL Reference

**Program:** `COCRDUPC.cbl`
**Lines:** 429-543 (Main EVALUATE dispatch — state-driven action routing)

```cobol
000429         EVALUATE TRUE
000435             WHEN CCARD-AID-PFK03
000436             WHEN (CCUP-CHANGES-OKAYED-AND-DONE
000437              AND  CDEMO-LAST-MAPSET   EQUAL LIT-CCLISTMAPSET)
000438             WHEN (CCUP-CHANGES-FAILED
000439              AND  CDEMO-LAST-MAPSET   EQUAL LIT-CCLISTMAPSET)
000440                 -- EXIT: transfer back to caller
000441                 ...
000482             WHEN CDEMO-PGM-ENTER
000483              AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM
000484                 -- FETCH: arrived from card list with keys
000485                 ...
000502             WHEN CCUP-DETAILS-NOT-FETCHED
000503              AND CDEMO-PGM-ENTER
000504                 -- PROMPT: ask user for search keys
000505                 ...
000535             WHEN OTHER
000536                 -- PROCESS: validate input and decide action
000537                 PERFORM 1000-PROCESS-INPUTS
000538                 PERFORM 2000-DECIDE-ACTION
000539                 PERFORM 3000-SEND-MAP
000543         END-EVALUATE
```

**Lines:** 948-1001 (State-driven action decision — 2000-DECIDE-ACTION)

```cobol
000948      EVALUATE TRUE
000954         WHEN CCUP-DETAILS-NOT-FETCHED
000958         WHEN CCARD-AID-PFK12
000959            -- Cancel/re-fetch
000960            ...
000971         WHEN CCUP-SHOW-DETAILS
000972            IF INPUT-ERROR OR NO-CHANGES-DETECTED
000973               CONTINUE
000974            ELSE
000975               SET CCUP-CHANGES-OK-NOT-CONFIRMED TO TRUE
000976            END-IF
000988         WHEN CCUP-CHANGES-OK-NOT-CONFIRMED
000989          AND CCARD-AID-PFK05
000990            -- SAVE: write changes to VSAM
000991            PERFORM 9200-WRITE-PROCESSING
000992            EVALUATE TRUE
000993               WHEN COULD-NOT-LOCK-FOR-UPDATE
000994                  SET CCUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE
000995               WHEN LOCKED-BUT-UPDATE-FAILED
000996                  SET CCUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
000997               WHEN DATA-WAS-CHANGED-BEFORE-UPDATE
000998                  SET CCUP-SHOW-DETAILS TO TRUE
000999               WHEN OTHER
001000                  SET CCUP-CHANGES-OKAYED-AND-DONE TO TRUE
001001            END-EVALUATE
```

**Lines:** 469-476 (SYNCPOINT before exit — atomic commit)

```cobol
000469               EXEC CICS
000470                    SYNCPOINT
000471               END-EXEC
000472 *
000473               EXEC CICS XCTL
000474                    PROGRAM (CDEMO-TO-PROGRAM)
000475                    COMMAREA(CARDDEMO-COMMAREA)
000476               END-EXEC
```

## Acceptance Criteria

### Scenario 1: Changes require explicit confirmation before save

```gherkin
GIVEN the user has modified card fields on the update screen
  AND all field validations have passed
WHEN the state transitions to CHANGES-OK-NOT-CONFIRMED
THEN the message "Changes validated. Press F5 to save" is displayed
  AND the data fields are protected (read-only)
  AND the user must press PF5 to commit the changes
```

### Scenario 2: Invalid changes are rejected before confirmation

```gherkin
GIVEN the user has modified card fields with invalid data
WHEN field validation runs (1200-EDIT-MAP-INPUTS)
THEN the state remains at CHANGES-NOT-OK
  AND the specific validation error is displayed
  AND the user must correct the error before proceeding
```

### Scenario 3: Concurrent modification detected

```gherkin
GIVEN the user has confirmed changes with PF5
  AND another user has modified the same card record in between
WHEN the write processing (9200-WRITE-PROCESSING) re-reads the record
THEN DATA-WAS-CHANGED-BEFORE-UPDATE is detected
  AND the state reverts to SHOW-DETAILS with the updated data
  AND the user is informed that the data has changed
```

### Scenario 4: Lock failure prevents update

```gherkin
GIVEN the user has confirmed changes with PF5
  AND the record is currently locked by another CICS transaction
WHEN the program attempts CICS READ UPDATE
THEN COULD-NOT-LOCK-FOR-UPDATE is set
  AND the state transitions to CHANGES-FAILED
  AND the message "Changes unsuccessful. Please try again" is displayed
```

### Scenario 5: Successful update is committed atomically

```gherkin
GIVEN the user has confirmed changes with PF5
  AND the record lock was obtained successfully
  AND no concurrent modifications were detected
WHEN the VSAM REWRITE succeeds
THEN EXEC CICS SYNCPOINT commits the transaction
  AND CHANGES-OKAYED-AND-DONE is set
  AND the message "Changes committed to database" is displayed
  AND the program returns to the calling screen
```

### Scenario 6: PF5 is rejected outside confirmation state

```gherkin
GIVEN the card update screen is in SHOW-DETAILS state
  AND changes have NOT been validated yet
WHEN the user presses PF5
THEN the PF key validation rejects PF5 (not in allowed set for this state)
  AND the key is normalized to ENTER
  AND no save operation is performed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for initiating payment transactions | The validate-confirm-commit workflow ensures card data changes are intentional; PF5 confirmation acts as an explicit authorization step |
| PSD2 | Art. 98 | Dynamic linking — authentication must be linked to the specific transaction | The COMMAREA carries the specific card number and account being modified; the PF5 confirmation is contextually linked to the validated changes |
| FFFS 2014:5 | Ch. 8 §4 | Adequate internal controls for data integrity | Multi-step workflow prevents accidental data modification; optimistic concurrency prevents lost updates from concurrent access |
| FFFS 2014:5 | Ch. 4 §3 | Operational risk management | CICS SYNCPOINT ensures atomicity; lock failure and concurrent modification detection prevent data corruption |

## Edge Cases

1. **No re-authentication on confirmation**: The PF5 confirmation step does not require the user to re-enter credentials. PSD2 SCA may require re-authentication for sensitive data changes. The migrated system should evaluate whether card data modifications require step-up authentication.

2. **SYNCPOINT placement**: The CICS SYNCPOINT is executed before the XCTL to the calling program (line 469-471), not immediately after the REWRITE. If the XCTL fails, the SYNCPOINT has already committed. The migrated system should use database transactions that commit only after the full operation succeeds.

3. **No audit log of modifications**: The COBOL program does not write an audit trail of what was changed, by whom, or when. CICS journal logs may capture this at the system level. The migrated system must implement application-level audit logging for all data modifications (GDPR Art. 5(2) accountability principle).

4. **State stored in COMMAREA only**: The workflow state (CCUP-DETAILS-NOT-FETCHED, CCUP-CHANGES-OK-NOT-CONFIRMED, etc.) is stored in the COMMAREA, which is passed between transaction invocations. If the session is lost (terminal disconnect), the state is lost and changes are discarded. The migrated system should persist draft changes if needed for data loss prevention.

## Domain Expert Notes

- **null** — Awaiting domain expert review. Key questions: (1) Does the CICS journal log capture the before/after values of card updates for audit purposes? (2) Is the PF5 confirmation considered sufficient authorization, or are there additional approval workflows for certain types of changes (e.g., status changes)? (3) What is the CICS DTIMOUT (transaction timeout) for the update transaction — how long can a user hold a record locked?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
