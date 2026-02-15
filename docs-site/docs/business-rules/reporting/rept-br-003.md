---
id: "rept-br-003"
title: "Report job submission via transient data queue"
domain: "reporting"
cobol_source: "CORPT00C.cbl:462-535"
requirement_id: "REPT-BR-003"
regulations:
  - "FFFS 2014:5 Ch. 8 §4 (internal controls - job submission audit)"
  - "DORA Art. 11 (ICT operations)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# REPT-BR-003: Report job submission via transient data queue

## Summary

After the date range is determined and validated, the report generation process requires explicit user confirmation before submitting the batch report job. The user must enter 'Y' or 'y' to confirm; 'N' or 'n' cancels the operation; any other input produces an error. Upon confirmation, the program constructs a JCL job stream from a pre-built template (JOB-DATA area, lines 81-128) containing parameterized start and end dates. The JCL records are written sequentially to the CICS transient data queue (TDQ) named 'JOBS' using EXEC CICS WRITEQ TD, iterating through 80-byte JOB-LINES records until an '/*EOF' sentinel or spaces are encountered. The TDQ write operation includes error handling that checks the RESP code and displays an error if the response is not NORMAL. On successful submission, a confirmation message is displayed indicating the report type and that it has been submitted for printing.

## Business Logic

### Pseudocode

```
FUNCTION SubmitReportJob(reportType, startDate, endDate, confirmInput):

    -- Step 1: Confirmation prompt
    IF confirmInput IS EMPTY THEN
        DISPLAY "Press Y to confirm, N to cancel"
        SET cursor to confirmation field
        RETURN AWAITING_CONFIRMATION
    END IF

    -- Step 2: Confirmation validation
    IF confirmInput = 'Y' OR confirmInput = 'y' THEN
        -- Proceed to job submission
    ELSE IF confirmInput = 'N' OR confirmInput = 'n' THEN
        CLEAR confirmation field
        DISPLAY "Report cancelled"
        RETURN CANCELLED
    ELSE
        DISPLAY "Invalid input - enter Y or N"
        SET cursor to confirmation field
        RETURN VALIDATION_ERROR
    END IF

    -- Step 3: Construct JCL from template
    jobData = LOAD_JCL_TEMPLATE("TRNRPT00")
    SET jobData.PARM-START-DATE = startDate
    SET jobData.PARM-END-DATE = endDate

    -- Step 4: Write JCL records to TDQ
    FOR EACH line IN jobData.JOB-LINES:
        IF line = '/*EOF' OR line = SPACES THEN
            EXIT LOOP
        END IF
        WRITEQ TD QUEUE('JOBS') FROM(line) LENGTH(80)
        IF RESP != NORMAL THEN
            DISPLAY "Error submitting report job: " + RESP
            RETURN SUBMISSION_ERROR
        END IF
    END FOR

    -- Step 5: Success message
    DISPLAY reportType + " report submitted for printing ..."
    RETURN SUCCESS

END FUNCTION
```

### Decision Table

| Confirmation Input | Action | Result |
|-------------------|--------|--------|
| Empty/blank | Prompt user for confirmation | Awaiting input |
| 'Y' or 'y' | Construct JCL and write to TDQ | Job submitted (if no TDQ error) |
| 'N' or 'n' | Cancel report generation | Report cancelled |
| Any other value | Display error | "Invalid input - enter Y or N" |
| TDQ write RESP != NORMAL | Display TDQ error | Submission failed |

## Source COBOL Reference

**Program:** `CORPT00C.cbl`
**Lines:** 462-535 (Job submission and confirmation handling)

JCL template definition (lines 81-128):
```cobol
000081 01  JOB-DATA.
000082     05  JOB-LINES.
000083         10  FILLER PIC X(80) VALUE
000084             '//TRNRPT00 JOB ACCT#,TRNRPT00,MSGCLASS=X,      '.
000085         10  FILLER PIC X(80) VALUE
000086             '//         CLASS=A,NOTIFY=&SYSUID                '.
000087         10  FILLER PIC X(80) VALUE
000088             '//JOBLIB   DD DSN=&&PROCLIB,DISP=SHR             '.
000089         10  FILLER PIC X(80) VALUE
000090             '//STEP10   EXEC PROC=TRANREPT                    '.
000091         10  FILLER PIC X(80) VALUE
000092             '//SYMNAMES DD *                                   '.
000093         10  FILLER PIC X(80) VALUE
000094             '  TRAN-CARD-NUM,263,16,ZD                        '.
000095         10  FILLER PIC X(80) VALUE
000096             '  TRAN-PROC-DT,305,10,CH                         '.
000097         10  FILLER PIC X(80) VALUE
000098             '/*                                                '.
000099         10  FILLER PIC X(80) VALUE
000100             '//DATEPARM DD *                                   '.
000101         10  WS-JCL-START  PIC X(80).
000102         10  WS-JCL-END    PIC X(80).
000103         10  FILLER PIC X(80) VALUE
000104             '/*                                                '.
000105         10  FILLER PIC X(80) VALUE
000106             '/*EOF                                             '.
```

Date parameter injection (lines 103-122):
```cobol
000103     MOVE SPACES TO WS-JCL-START
000104     STRING 'PARM-START-DATE,'''
000105            WS-START-DATE
000106            ''''
000107            DELIMITED BY SIZE INTO WS-JCL-START
000108     MOVE SPACES TO WS-JCL-END
000109     STRING 'PARM-END-DATE,'''
000110            WS-END-DATE
000111            ''''
000112            DELIMITED BY SIZE INTO WS-JCL-END
```

Confirmation prompt (lines 464-474):
```cobol
000464     IF CONFIRMI OF CORPT0AI = SPACES
000465         OR CONFIRMI OF CORPT0AI = LOW-VALUES
000466         MOVE 'Press Y to confirm, N to cancel'
000467             TO WS-ERROR-MSG
000468         MOVE -1 TO CONFIRML OF CORPT0AI
000469         MOVE 'N' TO WS-VALID-INPUT
000470         GO TO SEND-REPORT-SCREEN
000471     END-IF
```

Confirmation validation (lines 477-494):
```cobol
000477     EVALUATE CONFIRMI OF CORPT0AI
000478         WHEN 'Y'
000479         WHEN 'y'
000480             CONTINUE
000481         WHEN 'N'
000482         WHEN 'n'
000483             MOVE SPACES TO CONFIRMI OF CORPT0AO
000484             MOVE 'Report cancelled'
000485                 TO WS-ERROR-MSG
000486             MOVE -1 TO MONTHLYL OF CORPT0AI
000487             MOVE 'N' TO WS-VALID-INPUT
000488             GO TO SEND-REPORT-SCREEN
000489         WHEN OTHER
000490             MOVE 'Invalid input - enter Y or N'
000491                 TO WS-ERROR-MSG
000492             MOVE -1 TO CONFIRML OF CORPT0AI
000493             MOVE 'N' TO WS-VALID-INPUT
000494             GO TO SEND-REPORT-SCREEN
```

TDQ write loop (lines 496-509):
```cobol
000496     PERFORM VARYING WS-JCL-IDX FROM 1 BY 1
000497         UNTIL WS-JCL-IDX > 20
000498         OR JOB-LINE(WS-JCL-IDX) = '/*EOF'
000499         OR JOB-LINE(WS-JCL-IDX) = SPACES
000500         EXEC CICS WRITEQ TD
000501             QUEUE('JOBS')
000502             FROM(JOB-LINE(WS-JCL-IDX))
000503             LENGTH(80)
000504             RESP(WS-RESP-CODE)
000505         END-EXEC
000506         IF WS-RESP-CODE NOT = DFHRESP(NORMAL)
000507             GO TO TDQ-ERROR
000508         END-IF
000509     END-PERFORM
```

TDQ error handling (lines 515-535):
```cobol
000515 TDQ-ERROR.
000516     MOVE 'Error submitting report job'
000517         TO WS-ERROR-MSG
000518     STRING 'TDQ RESP: '
000519            WS-RESP-CODE
000520            DELIMITED BY SIZE INTO WS-ERROR-DETAIL
000521     MOVE -1 TO CONFIRML OF CORPT0AI
000522     MOVE 'N' TO WS-VALID-INPUT
000523     GO TO SEND-REPORT-SCREEN.
```

Success message (lines 445-456):
```cobol
000445     STRING WS-REPORT-TYPE-DESC
000446            ' report submitted for printing ...'
000447            DELIMITED BY SIZE INTO WS-SUCCESS-MSG
000448     MOVE WS-SUCCESS-MSG TO INFOMSG OF CORPT0AO
000449     MOVE SPACES TO CONFIRMI OF CORPT0AO
```

## Acceptance Criteria

### Scenario 1: User confirms report submission

```gherkin
Given a valid date range has been determined
And the user is prompted for confirmation
When the user enters "Y"
Then the JCL job is constructed with the validated date range
And the job records are written to the transient data queue
And a success message is displayed indicating the report was submitted
```

### Scenario 2: User cancels report submission

```gherkin
Given a valid date range has been determined
And the user is prompted for confirmation
When the user enters "N"
Then the report generation is cancelled
And the confirmation field is cleared
And a "Report cancelled" message is displayed
```

### Scenario 3: Invalid confirmation input

```gherkin
Given a valid date range has been determined
And the user is prompted for confirmation
When the user enters "X"
Then the system displays "Invalid input - enter Y or N"
And the cursor is positioned on the confirmation field
```

### Scenario 4: TDQ write failure

```gherkin
Given the user has confirmed report submission
When the system writes JCL records to the TDQ
And the TDQ write returns a non-NORMAL response code
Then the system displays "Error submitting report job"
And the RESP code is included in the error detail
```

### Scenario 5: Date parameters injected into JCL template

```gherkin
Given the user selected a Monthly report with start date "2025-03-01" and end date "2025-03-31"
When the JCL template is constructed
Then the PARM-START-DATE line contains "2025-03-01"
And the PARM-END-DATE line contains "2025-03-31"
And the JCL records terminate at the "/*EOF" sentinel
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 §4 | Internal controls must include procedures for authorization of transactions and operations | User confirmation step before job submission provides an explicit authorization control, ensuring report generation is intentional and auditable |
| DORA | Art. 11 | ICT operations management including batch processing and job scheduling | Structured job submission via TDQ with error handling ensures reliable batch job initiation with failure detection, supporting operational resilience of the reporting pipeline |

## Edge Cases

1. **TDQ 'JOBS' not defined**: If the CICS TDQ named 'JOBS' is not defined in the CICS TDQ table, the WRITEQ TD will return a QIDERR response. The .NET equivalent (e.g., Azure Service Bus queue) must handle queue-not-found scenarios.
2. **JCL template exceeds 20 lines**: The PERFORM loop iterates up to index 20. If the JCL template grows beyond 20 lines, records will be silently truncated. The .NET implementation should not have this artificial limit but should document the constraint.
3. **Partial TDQ write failure**: If the TDQ write fails mid-stream (e.g., after 5 of 12 records), partial JCL may exist on the queue. The COBOL program does not roll back partial writes. The .NET implementation should consider transactional queue writes.
4. **Confirmation field case sensitivity**: Only 'Y'/'y' and 'N'/'n' are accepted. Uppercase and lowercase are handled explicitly via EVALUATE. No trimming of whitespace is performed on the confirmation input.
5. **Concurrent job submission**: Multiple users could submit report jobs simultaneously via the same TDQ. The CICS TDQ handles serialization, but the .NET replacement must ensure equivalent concurrency control.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The CICS TDQ 'JOBS' is a critical integration point -- confirm which downstream system reads from this queue and initiates the JES2 batch job. This mapping is essential for designing the Azure equivalent (likely Azure Service Bus triggering an Azure Function or Azure Batch job).

---
**Template version:** 1.0
**Last updated:** 2026-02-15
