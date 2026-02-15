---
id: "acct-br-004"
title: "Account update workflow and state machine"
domain: "account-management"
cobol_source: "COACTUPC.cbl:652-668"
requirement_id: "ACCT-BR-004"
regulations:
  - "PSD2 Art. 97"
  - "FSA FFFS 2014:5 Ch. 4 S3"
  - "DORA Art. 9"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-004: Account update workflow and state machine

## Summary

The account update program (COACTUPC) implements a multi-step conversational CICS transaction (CAUP) using a state machine stored in the COMMAREA. The field ACUP-CHANGE-ACTION drives the workflow through distinct states governing the entire account update lifecycle: initial entry, data display, field editing with validation, confirmation gating, and final write with success or failure outcomes. The COMMAREA (2000 bytes) persists all old values (snapshot), new values, and state flags across CICS pseudo-conversational cycles. PF key handling is state-dependent, with only contextually valid keys producing actions and all invalid keys silently remapped to ENTER. An unexpected state causes ABEND with code '9999' as a safety net against state corruption.

## Business Logic

### Pseudocode

```
PERFORM 9000-MAIN-ORCHESTRATION:
    EVALUATE TRUE
        WHEN ACUP-DETAILS-NOT-FETCHED
            -- Initial state: only Account ID field is editable
            IF ENTER pressed
                VALIDATE account-id-input
                IF valid
                    PERFORM 9000-READ-ACCT (three-file read chain)
                    IF read-success
                        PERFORM 9500-STORE-FETCHED-DATA (snapshot)
                        SET ACUP-CHANGE-ACTION = 'S'
                    ELSE
                        DISPLAY error message, stay in initial state
                    END-IF
                END-IF
            END-IF
            IF PF3 pressed
                EXIT to calling program (EXEC CICS XCTL)
            END-IF

        WHEN ACUP-SHOW-DETAILS ('S')
            -- All fields editable except Account ID, Customer ID, Country
            IF ENTER pressed
                PERFORM 1700-CHECK-CHANGES (compare old vs new)
                IF no-changes-detected
                    DISPLAY "No change detected with respect to values fetched."
                    STAY in state 'S'
                ELSE
                    PERFORM 2000-VALIDATE-ALL-INPUTS
                    IF validation-errors
                        SET ACUP-CHANGE-ACTION = 'E'
                        DISPLAY field-level error messages
                    ELSE
                        SET ACUP-CHANGE-ACTION = 'N'
                        PROTECT all fields (read-only)
                        HIGHLIGHT PF5 and PF12
                        DISPLAY "Press PF5 to confirm, PF12 to cancel"
                    END-IF
                END-IF
            END-IF
            IF PF12 pressed
                PERFORM 9000-READ-ACCT (re-read fresh data)
                SET ACUP-CHANGE-ACTION = 'S'
            END-IF
            IF PF3 pressed
                EXIT to calling program
            END-IF

        WHEN ACUP-CHANGES-NOT-OK ('E')
            -- Fields editable, error messages visible
            IF ENTER pressed
                PERFORM 2000-VALIDATE-ALL-INPUTS
                IF validation-errors
                    STAY in state 'E'
                    DISPLAY updated field-level errors
                ELSE
                    SET ACUP-CHANGE-ACTION = 'N'
                    PROTECT all fields
                END-IF
            END-IF
            IF PF12 pressed
                PERFORM 9000-READ-ACCT (re-read, discard edits)
                SET ACUP-CHANGE-ACTION = 'S'
            END-IF
            IF PF3 pressed
                EXIT to calling program
            END-IF

        WHEN ACUP-CHANGES-OK-NOT-CONFIRMED ('N')
            -- All fields protected, awaiting PF5 confirmation
            IF PF5 pressed
                PERFORM 3900-WRITE-PROCESSING
                EVALUATE TRUE
                    WHEN write-success
                        SET ACUP-CHANGE-ACTION = 'C'
                        DISPLAY "Update successful"
                    WHEN concurrent-change-detected
                        SET ACUP-CHANGE-ACTION = 'S'
                        DISPLAY "Record changed by some one else. Please review"
                    WHEN lock-error
                        SET ACUP-CHANGE-ACTION = 'L'
                        DISPLAY "Unable to lock record for update"
                    WHEN write-error
                        SET ACUP-CHANGE-ACTION = 'F'
                        DISPLAY "Update failed due to write error"
                END-EVALUATE
            END-IF
            IF PF12 pressed
                PERFORM 9000-READ-ACCT (cancel, re-read)
                SET ACUP-CHANGE-ACTION = 'S'
            END-IF
            IF PF3 pressed
                EXIT to calling program
            END-IF

        WHEN ACUP-CHANGES-OKAYED-AND-DONE ('C')
        WHEN ACUP-CHANGES-FAILED ('L', 'F')
            -- All fields protected, terminal state
            IF any-key pressed
                RESET to initial state (ACUP-DETAILS-NOT-FETCHED)
                CLEAR all fields
            END-IF

        WHEN OTHER
            -- Unexpected state: safety net
            EXEC CICS ABEND ABCODE('9999') END-EXEC

    END-EVALUATE

1700-CHECK-CHANGES:
    -- Pre-validation change detection
    PERFORM VARYING field-idx FROM 1 BY 1
        UNTIL all-fields-compared
        COMPARE FUNCTION UPPER-CASE(FUNCTION TRIM(old-value(field-idx)))
           WITH FUNCTION UPPER-CASE(FUNCTION TRIM(new-value(field-idx)))
        IF NOT EQUAL
            SET changes-detected = TRUE
            EXIT PERFORM
        END-IF
    END-PERFORM
    IF NOT changes-detected
        MOVE "No change detected with respect to values fetched."
            TO WS-MESSAGE
    END-IF
```

### State Diagram

```
                          +---------------------+
                          | INITIAL             |
                          | (LOW-VALUES/SPACES) |
                          +---------------------+
                                   |
                          ENTER + valid account ID
                          + successful read
                                   |
                                   v
                          +---------------------+
            +------------>| SHOW DETAILS ('S')  |<-----------+
            |             +---------------------+            |
            |                 |           |                   |
            |          ENTER +        ENTER +                 |
            |          changes +      changes +               |
            |          valid         errors                   |
            |                 |           |                   |
            |                 v           v                   |
            |    +-----------------+  +------------------+   |
            |    | AWAITING        |  | ERRORS ('E')     |   |
            |    | CONFIRM ('N')   |  +------------------+   |
            |    +-----------------+      |                   |
            |         |    |         ENTER + fix errors       |
            |      PF5 +  PF12            |                   |
            |      write   cancel    -----+                   |
            |         |    |                                   |
            |         |    +----------------------------------+
            |         |
            |         v
            |    +-------------------+
            |    | SUCCESS ('C')     |   concurrent change
            |    | LOCK ERROR ('L')  |---------->--------------+
            |    | WRITE ERROR ('F') |
            |    +-------------------+
            |         |
            |    any key -> reset
            |         |
            +---------+ (back to INITIAL)

    PF3 from any state -> EXIT to calling program
    PF12 from S/E/N -> re-read data -> SHOW DETAILS ('S')
    Unexpected state -> ABEND '9999'
```

### PF Key Decision Table

| PF Key | Initial State | Show Details ('S') | Errors ('E') | Awaiting Confirm ('N') | Success/Failure ('C'/'L'/'F') |
|--------|--------------|-------------------|-------------|----------------------|------------------------------|
| ENTER  | Fetch account data | Validate changes | Re-validate | Ignored (no effect) | Reset to initial |
| PF3    | Exit program | Exit program | Exit program | Exit program | Exit program |
| PF5    | Remap to ENTER | Remap to ENTER | Remap to ENTER | Confirm and write | Remap to ENTER |
| PF12   | Remap to ENTER | Cancel, re-read | Cancel, re-read | Cancel, re-read | Remap to ENTER |
| Other  | Remap to ENTER | Remap to ENTER | Remap to ENTER | Remap to ENTER | Remap to ENTER |

### UI Field Editability by State

| State | Account ID | Customer ID | Country | Other Fields | PF5 Highlighted | PF12 Highlighted |
|-------|-----------|-------------|---------|-------------|----------------|-----------------|
| Initial | Editable | N/A | N/A | N/A | No | No |
| Show Details ('S') | Protected | Protected | Protected | Editable | No | Yes |
| Errors ('E') | Protected | Protected | Protected | Editable | No | Yes |
| Awaiting Confirm ('N') | Protected | Protected | Protected | Protected | Yes | Yes |
| Success ('C') | Protected | Protected | Protected | Protected | No | No |
| Failure ('L'/'F') | Protected | Protected | Protected | Protected | No | No |

## Source COBOL Reference

**Program:** `COACTUPC.cbl`
**Lines:** 652-668 (state definitions), 858-1023 (main orchestration), 2562-2644 (decision engine), 1681-1779 (change detection)

### State variable definitions (lines 652-668)

```cobol
000652     10 ACUP-CHANGE-ACTION                     PIC X(1)
000653                                               VALUE LOW-VALUES.
000654         88 ACUP-DETAILS-NOT-FETCHED            VALUES LOW-VALUES,
000655                                                SPACES.
000656         88 ACUP-SHOW-DETAILS                   VALUE 'S'.
000657         88 ACUP-CHANGES-MADE                   VALUES 'E', 'N',
000658                                                'C', 'L', 'F'.
000659         88 ACUP-CHANGES-NOT-OK                 VALUE 'E'.
000660         88 ACUP-CHANGES-OK-NOT-CONFIRMED       VALUE 'N'.
000661         88 ACUP-CHANGES-OKAYED-AND-DONE        VALUE 'C'.
000662         88 ACUP-CHANGES-FAILED                 VALUES 'L', 'F'.
000663         88 ACUP-CHANGES-OKAYED-LOCK-ERROR      VALUE 'L'.
000664         88 ACUP-CHANGES-OKAYED-BUT-FAILED      VALUE 'F'.
```

### Main orchestration dispatch (lines 858-1023)

```cobol
000858 2000-DECIDE-PROCESSING.
000859     EVALUATE TRUE
000860         WHEN ACUP-DETAILS-NOT-FETCHED
000861             PERFORM 2100-PROCESS-INITIAL-STATE
000862         WHEN ACUP-SHOW-DETAILS
000863             PERFORM 2200-PROCESS-SHOW-DETAILS
000864         WHEN ACUP-CHANGES-NOT-OK
000865             PERFORM 2300-PROCESS-CHANGES-NOT-OK
000866         WHEN ACUP-CHANGES-OK-NOT-CONFIRMED
000867             PERFORM 2400-PROCESS-CONFIRM-STATE
000868         WHEN ACUP-CHANGES-OKAYED-AND-DONE
000869         WHEN ACUP-CHANGES-FAILED
000870             PERFORM 2500-PROCESS-TERMINAL-STATE
000871         WHEN OTHER
000872             EXEC CICS ABEND
000873                 ABCODE('9999')
000874             END-EXEC
000875     END-EVALUATE.
```

### Decision engine - PF key handling (lines 2562-2644)

```cobol
002562 2550-PF-KEY-HANDLING.
002563     EVALUATE EIBAID
002564         WHEN DFHENTER
002565             CONTINUE
002566         WHEN DFHPF3
002567             PERFORM 9800-RETURN-TO-PREV-SCREEN
002568         WHEN DFHPF5
002569             IF ACUP-CHANGES-OK-NOT-CONFIRMED
002570                 PERFORM 3900-WRITE-PROCESSING
002571             ELSE
002572                 CONTINUE
002573             END-IF
002574         WHEN DFHPF12
002575             IF NOT ACUP-DETAILS-NOT-FETCHED
002576                 PERFORM 9000-READ-ACCT
002577                 SET ACUP-SHOW-DETAILS TO TRUE
002578             ELSE
002579                 CONTINUE
002580             END-IF
002581         WHEN OTHER
002582             CONTINUE
002583     END-EVALUATE.
```

### Pre-validation change detection (lines 1681-1779)

```cobol
001681 1700-CHECK-FOR-CHANGES.
001682     SET WS-NO-CHANGES TO TRUE.
001683     IF FUNCTION UPPER-CASE(FUNCTION TRIM(
001684         ACUP-NEW-ACTIVE-STATUS))
001685         NOT = FUNCTION UPPER-CASE(FUNCTION TRIM(
001686         ACUP-OLD-ACTIVE-STATUS))
001687         SET WS-CHANGES-DETECTED TO TRUE
001688     END-IF.
001689
001690     IF FUNCTION UPPER-CASE(FUNCTION TRIM(
001691         ACUP-NEW-FIRST-NAME))
001692         NOT = FUNCTION UPPER-CASE(FUNCTION TRIM(
001693         ACUP-OLD-FIRST-NAME))
001694         SET WS-CHANGES-DETECTED TO TRUE
001695     END-IF.
001770
001771     IF WS-NO-CHANGES
001772         MOVE 'No change detected with respect to '
001773           'values fetched.'
001774             TO WS-MESSAGE
001775     END-IF.
001779 1700-EXIT.
001780     EXIT.
```

### COMMAREA structure

```cobol
000640     05 WS-THIS-PROGCOMMAREA.
000641         10 ACUP-OLD-ACCT-DATA.
000642             15 ACUP-OLD-ACTIVE-STATUS       PIC X(1).
000643             15 ACUP-OLD-CURR-BAL             PIC S9(10)V99.
000644             ...
000645         10 ACUP-NEW-ACCT-DATA.
000646             15 ACUP-NEW-ACTIVE-STATUS       PIC X(1).
000647             15 ACUP-NEW-CURR-BAL             PIC S9(10)V99.
000648             ...
000649         10 ACUP-OLD-CUST-DATA.
000650             ...
000651         10 ACUP-NEW-CUST-DATA.
000652             ...
```

## Acceptance Criteria

### Scenario 1: Initial entry fetches account data

```gherkin
GIVEN the account update screen is loaded for the first time
  AND no account data has been fetched (state = INITIAL)
WHEN the user enters a valid account ID "12345678901" and presses ENTER
THEN the system retrieves account, customer, and card cross-reference data
  AND all account and customer fields are displayed
  AND the Account ID, Customer ID, and Country fields are protected (non-editable)
  AND the state transitions to SHOW DETAILS ('S')
```

### Scenario 2: Validation errors transition to error state

```gherkin
GIVEN the account update screen is in SHOW DETAILS state ('S')
  AND the user modifies the credit limit to a non-numeric value
WHEN the user presses ENTER
THEN field-level validation errors are displayed
  AND the state transitions to ERRORS ('E')
  AND the erroneous fields are highlighted
  AND the user can correct the values
```

### Scenario 3: Valid changes transition to confirmation state

```gherkin
GIVEN the account update screen is in SHOW DETAILS state ('S')
  AND the user changes the account active status from 'Y' to 'N'
WHEN the user presses ENTER
  AND all field validations pass
THEN all fields become protected (read-only)
  AND the PF5 (confirm) and PF12 (cancel) keys are highlighted
  AND the message "Press PF5 to confirm, PF12 to cancel" is displayed
  AND the state transitions to AWAITING CONFIRMATION ('N')
```

### Scenario 4: PF5 confirms and writes successfully

```gherkin
GIVEN the account update screen is in AWAITING CONFIRMATION state ('N')
WHEN the user presses PF5
  AND no concurrent changes have been made by another user
  AND the account and customer records are successfully locked and written
THEN the state transitions to SUCCESS ('C')
  AND the message "Update successful" is displayed
  AND all fields remain protected
```

### Scenario 5: PF12 cancels changes and re-reads data

```gherkin
GIVEN the account update screen is in AWAITING CONFIRMATION state ('N')
  AND the user has pending changes
WHEN the user presses PF12
THEN the pending changes are discarded
  AND the account data is re-read from the files
  AND the state transitions back to SHOW DETAILS ('S')
  AND the fields display the current stored values
```

### Scenario 6: No changes detected shows informational message

```gherkin
GIVEN the account update screen is in SHOW DETAILS state ('S')
  AND the user has not modified any field values
WHEN the user presses ENTER
THEN the message "No change detected with respect to values fetched." is displayed
  AND the state remains at SHOW DETAILS ('S')
  AND no validation or write processing occurs
```

### Scenario 7: PF5 ignored in non-confirmation states

```gherkin
GIVEN the account update screen is in SHOW DETAILS state ('S')
WHEN the user presses PF5
THEN PF5 is silently remapped to ENTER behavior
  AND the system processes the action as if ENTER was pressed
```

### Scenario 8: Unexpected state triggers ABEND

```gherkin
GIVEN the COMMAREA state variable contains an unexpected value (e.g., 'Z')
WHEN the orchestration evaluates the current state
THEN the system triggers ABEND with code '9999'
  AND the transaction is terminated
```

### Scenario 9: Terminal states reset on any key

```gherkin
GIVEN the account update screen is in SUCCESS state ('C')
WHEN the user presses any key
THEN the state resets to INITIAL
  AND all fields are cleared
  AND the Account ID field becomes editable
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for accessing and modifying payment account information | The multi-step confirmation workflow (edit -> validate -> confirm via PF5) implements a deliberate user action gate before any account modification is persisted; the migrated system must enforce SCA before allowing access to the update screen |
| FSA FFFS 2014:5 | Ch. 4 S3 | Operational risk management requires controlled change processes for account data | The state machine enforces a structured workflow preventing accidental writes: changes must pass validation and explicit confirmation before persistence; the confirmation state ('N') with PF5 requirement ensures deliberate intent |
| DORA | Art. 9 | ICT risk management framework must include protection and prevention measures | The ABEND '9999' safety net for unexpected states, combined with the COMMAREA-based state persistence across pseudo-conversational cycles, provides resilience against state corruption; the migrated system must implement equivalent state validation and fail-safe mechanisms |

## Edge Cases

1. **COMMAREA corruption across pseudo-conversational cycles**: In the CICS pseudo-conversational model, the COMMAREA is passed back and forth between SEND MAP and RECEIVE MAP operations. If the COMMAREA is truncated or corrupted (e.g., due to a CICS region issue), the state variable may contain an unexpected value. The ABEND '9999' safety net catches this case. The migrated system must implement equivalent state validation, potentially using session state checksums or versioning.

2. **User abandons session mid-workflow**: If the user's CICS terminal session is terminated while in state 'N' (awaiting confirmation), no write occurs because PF5 was never pressed. The CICS pseudo-conversational model inherently supports this -- no resources are held between interactions. The migrated system must ensure that pending changes in the confirmation state do not persist beyond the session lifetime (e.g., no server-side draft state that survives session timeout).

3. **PF key remapping behavior**: All unrecognized PF keys are silently treated as ENTER. This means a user pressing PF7, PF8, or any other undefined key will trigger the same logic as ENTER. The migrated system must decide whether to preserve this permissive behavior or restrict to only documented keys, logging unexpected key presses for audit purposes.

4. **Rapid key presses during state transitions**: In CICS, the pseudo-conversational model serializes interactions -- each SEND/RECEIVE cycle is atomic. The migrated system using HTTP requests must guard against double-submission (e.g., user pressing PF5 twice rapidly) which could result in duplicate writes if the first request succeeds but the response is delayed.

5. **Protected field enforcement**: The COBOL program uses BMS map attributes to protect fields. If the BMS map definition is inconsistent with the program logic, a field could be editable when it should be protected. The migrated system should enforce field editability at the API level (server-side validation) rather than relying solely on UI controls.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The state machine is critical to maintaining update integrity and must be validated against the actual mainframe runtime behavior. Specific areas needing confirmation: (1) whether the COMMAREA size of 2000 bytes is sufficient for all account and customer snapshot fields, (2) whether the ABEND '9999' code has a recovery procedure in the CICS region, and (3) whether any additional states were added in later maintenance patches not reflected in the source listing.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
