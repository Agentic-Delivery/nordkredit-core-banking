---
id: "sec-br-007"
title: "Abend handling and secure error management"
domain: "user-security"
cobol_source: "COCRDLIC.cbl:263-271,COCRDLIC.cbl:578-598,COCRDSLC.cbl:250-252,COCRDSLC.cbl:857-878,COCRDUPC.cbl:370-372,COCRDUPC.cbl:1019-1026"
requirement_id: "SEC-BR-007"
regulations:
  - "FFFS 2014:5 Ch. 8 §4"
  - "DORA Art. 11"
  - "GDPR Art. 5(1)(f)"
  - "FFFS 2014:5 Ch. 4 §3"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# SEC-BR-007: Abend handling and secure error management

## Summary

All three CardDemo programs implement CICS abend (abnormal end) handling via the `EXEC CICS HANDLE ABEND` mechanism. When an unexpected error occurs, the programs display a structured error message containing the culprit program name and an abend code, then terminate the transaction with a CICS ABEND. Additionally, each program handles unexpected state scenarios in the main EVALUATE dispatch by performing the abend routine with a descriptive error code. File operation errors (VSAM READ/REWRITE failures) produce structured error messages with the operation name, file name, and CICS RESP/RESP2 codes. This error handling approach ensures that failures are captured, reported, and do not silently corrupt data or leave the system in an inconsistent state.

## Business Logic

### Pseudocode

```
ABEND HANDLER REGISTRATION (at program start):
    EXEC CICS HANDLE ABEND
        LABEL(ABEND-ROUTINE)
    END-EXEC

ABEND ROUTINE (invoked on unexpected error):
    IF abend-message = LOW-VALUES
        SET abend-message = 'UNEXPECTED ABEND OCCURRED.'
    END-IF
    SET abend-culprit = current-program-name

    -- Display error to terminal
    EXEC CICS SEND
        FROM(ABEND-DATA)
        LENGTH(LENGTH OF ABEND-DATA)
        NOHANDLE
    END-EXEC

    -- Cancel handler to prevent recursive abends
    EXEC CICS HANDLE ABEND CANCEL END-EXEC

    -- Terminate with abend code
    EXEC CICS ABEND ABCODE('9999') END-EXEC

UNEXPECTED STATE HANDLING (in EVALUATE dispatch):
    WHEN OTHER
        SET abend-culprit = current-program-name
        SET abend-code = '0001'
        SET abend-reason = SPACES
        SET abend-message = 'UNEXPECTED DATA SCENARIO'
        PERFORM ABEND-ROUTINE

FILE ERROR HANDLING (on VSAM operations):
    EVALUATE RESP-CODE
        WHEN DFHRESP(NORMAL)
            -- Success path
        WHEN DFHRESP(NOTFND)
            -- Record not found — user error
            SET INPUT-ERROR TO TRUE
            SET error-message = 'Did not find...'
        WHEN OTHER
            -- System error — construct detailed error
            MOVE 'READ'/'REWRITE' TO ERROR-OPNAME
            MOVE file-name TO ERROR-FILE
            MOVE resp-code TO ERROR-RESP
            MOVE resp2-code TO ERROR-RESP2
            MOVE file-error-message TO return-message
    END-EVALUATE
```

### Error Classification

| Error Type | Handling | User Message | System Action |
|-----------|----------|-------------|---------------|
| Unexpected abend | ABEND-ROUTINE | 'UNEXPECTED ABEND OCCURRED.' | Display error, CICS ABEND '9999' |
| Unexpected state | ABEND-ROUTINE | 'UNEXPECTED DATA SCENARIO' | Display error, CICS ABEND '0001' |
| Record not found | INPUT-ERROR flag | Domain-specific message | Redisplay form with error |
| File system error | Error message construction | 'File Error: READ on CARDDAT returned RESP X, RESP2 Y' | Redisplay form with error |
| Lock failure | State transition | 'Changes unsuccessful. Please try again' | Reset to search state |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 263-271 (Abend handler registration)

```cobol
000263         EXEC CICS HANDLE ABEND
000264                   LABEL(ABEND-ROUTINE)
000265         END-EXEC
```

**Lines:** 578-598 (Abend routine implementation)

```cobol
000578  ABEND-ROUTINE.
000579
000580      IF ABEND-MSG EQUAL LOW-VALUES
000581         MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG
000582      END-IF
000583
000584      MOVE LIT-THISPGM       TO ABEND-CULPRIT
000585
000586      EXEC CICS SEND
000587                   FROM (ABEND-DATA)
000588                   LENGTH(LENGTH OF ABEND-DATA)
000589                   NOHANDLE
000590      END-EXEC
000591
000592      EXEC CICS HANDLE ABEND
000593           CANCEL
000594      END-EXEC
000595
000596      EXEC CICS ABEND
000597           ABCODE('9999')
000598      END-EXEC
```

**Program:** `COCRDSLC.cbl`
**Lines:** 250-252 (Abend handler registration)

```cobol
000250         EXEC CICS HANDLE ABEND
000251                   LABEL(ABEND-ROUTINE)
000252         END-EXEC
```

**Lines:** 373-379 (Unexpected state in card detail dispatch)

```cobol
000373             WHEN OTHER
000374                  MOVE LIT-THISPGM    TO ABEND-CULPRIT
000375                  MOVE '0001'         TO ABEND-CODE
000376                  MOVE SPACES         TO ABEND-REASON
000377                  MOVE 'UNEXPECTED DATA SCENARIO'
000378                                      TO WS-RETURN-MSG
000379                  PERFORM SEND-PLAIN-TEXT
```

**Lines:** 857-878 (Abend routine in card detail)

```cobol
000857      ABEND-ROUTINE.
000858
000859          IF ABEND-MSG EQUAL LOW-VALUES
000860             MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG
000861          END-IF
000862
000863          MOVE LIT-THISPGM       TO ABEND-CULPRIT
000864
000865          EXEC CICS SEND
000866                       FROM (ABEND-DATA)
000867                       LENGTH(LENGTH OF ABEND-DATA)
000868                       NOHANDLE
000869          END-EXEC
000870
000871          EXEC CICS HANDLE ABEND
000872               CANCEL
000873          END-EXEC
000874
000875          EXEC CICS ABEND
000876               ABCODE('9999')
000877          END-EXEC
```

**Program:** `COCRDUPC.cbl`
**Lines:** 1019-1026 (Unexpected state triggers abend in update)

```cobol
001019             WHEN OTHER
001020                  MOVE LIT-THISPGM    TO ABEND-CULPRIT
001021                  MOVE '0001'         TO ABEND-CODE
001022                  MOVE SPACES         TO ABEND-REASON
001023                  MOVE 'UNEXPECTED DATA SCENARIO'
001024                                      TO ABEND-MSG
001025                  PERFORM ABEND-ROUTINE
001026                     THRU ABEND-ROUTINE-EXIT
```

### File Error Message Structure

```cobol
       05  WS-FILE-ERROR-MESSAGE.
           10  FILLER          PIC X(12) VALUE 'File Error: '.
           10  ERROR-OPNAME    PIC X(8)  VALUE SPACES.
           10  FILLER          PIC X(4)  VALUE ' on '.
           10  ERROR-FILE      PIC X(9)  VALUE SPACES.
           10  FILLER          PIC X(15) VALUE ' returned RESP '.
           10  ERROR-RESP      PIC X(10) VALUE SPACES.
           10  FILLER          PIC X(7)  VALUE ',RESP2 '.
           10  ERROR-RESP2     PIC X(10) VALUE SPACES.
```

## Acceptance Criteria

### Scenario 1: Unexpected abend is caught and reported

```gherkin
GIVEN a CICS program encounters an unexpected runtime error
WHEN the HANDLE ABEND label is triggered
THEN the error message "UNEXPECTED ABEND OCCURRED." is displayed
  AND the culprit program name is included in the message
  AND the HANDLE ABEND is cancelled to prevent recursive abends
  AND the transaction is terminated with ABCODE '9999'
```

### Scenario 2: Unexpected state triggers controlled abend

```gherkin
GIVEN the program's EVALUATE dispatch encounters an unhandled state
WHEN the WHEN OTHER clause is reached
THEN ABEND-CODE is set to '0001'
  AND ABEND-MSG is set to 'UNEXPECTED DATA SCENARIO'
  AND the ABEND-ROUTINE is performed
  AND the transaction is terminated cleanly
```

### Scenario 3: File error produces structured message

```gherkin
GIVEN a CICS READ operation on file CARDDAT fails
  AND the RESP code is not NORMAL or NOTFND
WHEN the error is evaluated
THEN the error message includes: operation name ('READ'), file name ('CARDDAT'), RESP code, and RESP2 code
  AND the structured message is displayed to the user
  AND no partial data is shown
```

### Scenario 4: Record not found is handled gracefully

```gherkin
GIVEN the user searches for an account/card combination
  AND the VSAM file does not contain a matching record
WHEN DFHRESP(NOTFND) is returned
THEN INPUT-ERROR is set to TRUE
  AND the message "Did not find cards for this search condition" is displayed
  AND the user can correct their search and retry
  AND no abend occurs
```

### Scenario 5: Abend handler cancelled before ABEND

```gherkin
GIVEN the ABEND-ROUTINE has been triggered
WHEN the routine executes
THEN EXEC CICS HANDLE ABEND CANCEL is called before EXEC CICS ABEND
  AND this prevents infinite recursion if the ABEND itself triggers an error
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 §4 | Internal controls must ensure integrity and availability of information systems | Abend handling prevents uncontrolled failures; structured error messages aid in incident investigation |
| DORA | Art. 11 | ICT systems must have detection mechanisms for anomalous activities and logging capabilities | Abend codes ('9999', '0001') and structured file error messages provide forensic data for incident analysis |
| GDPR | Art. 5(1)(f) | Integrity and confidentiality of personal data processing | Controlled abend prevents partial data display or data corruption when errors occur during processing |
| FFFS 2014:5 | Ch. 4 §3 | Operational risk management including error handling and recovery | Explicit handling of VSAM errors (NOTFND vs system errors), state errors, and unexpected abends ensures the system fails safely |

## Edge Cases

1. **Error messages expose system internals**: The file error message includes the CICS RESP/RESP2 codes, the operation name, and the VSAM file name. This information could aid an attacker in understanding the system architecture. The migrated system should log detailed errors server-side but present generic messages to users.

2. **No automatic recovery**: After an abend, the transaction is terminated and the user must restart their workflow from scratch. There is no session persistence or recovery mechanism. The migrated system should consider idempotent operations and draft state saving.

3. **NOHANDLE on error SEND**: The SEND in the abend routine uses NOHANDLE, meaning if the SEND itself fails (e.g., terminal disconnected), the error is silently ignored and the ABEND proceeds. This is a defensive pattern to ensure the ABEND always executes. The migrated system should log errors to a persistent store regardless of the client connection state.

4. **Card detail uses SEND-PLAIN-TEXT for unexpected state**: The card detail program (COCRDSLC) uses SEND-PLAIN-TEXT instead of ABEND-ROUTINE for the unexpected state case (line 379). This means the program does not abend — it displays text and returns. The migrated system should consistently handle unexpected states with appropriate HTTP error codes (500 Internal Server Error).

## Domain Expert Notes

- **null** — Awaiting domain expert review. Key questions: (1) Are CICS abend codes ('9999', '0001') tracked in the mainframe's error management system, and what operational procedures are triggered? (2) Is there a CICS auxiliary trace or journal that captures abend events for later analysis? (3) What is the current SLA for resolving CICS abends in the card management subsystem?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
