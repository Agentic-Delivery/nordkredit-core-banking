---
id: "rpt-br-006"
title: "DORA incident reporting and ICT risk management"
domain: "reporting"
cobol_source: "CBTRN01C.cbl:1-489 (error handling pattern), CBTRN02C.cbl:1-723 (error handling pattern), CBTRN03C.cbl:1-650 (error handling pattern)"
requirement_id: "RPT-BR-006"
regulations:
  - "DORA Art. 11"
  - "DORA Art. 17"
  - "DORA Art. 19"
  - "FSA FFFS 2014:5 Ch. 7"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-006: DORA incident reporting and ICT risk management

## Summary

The Digital Operational Resilience Act (DORA, Regulation (EU) 2022/2554) requires NordKredit AB to classify, report, and manage ICT-related incidents. The current mainframe system handles operational incidents through ABEND codes (program termination with error codes), DISPLAY statements (console logging), and return codes (JCL step flow control). When a batch program fails (CBTRN01C/02C/03C), it ABENDs with code 999, writes a DISPLAY message, and the JCL COND parameter prevents subsequent steps from running. The migrated system must transform this basic error handling into structured DORA-compliant incident reporting with classification, notification timelines, and root cause analysis. Extracted from the error handling patterns in CBTRN01C, CBTRN02C, and CBTRN03C, and DORA regulatory requirements.

## Business Logic

### Pseudocode

```
DORA INCIDENT REPORTING FRAMEWORK:

    INCIDENT CLASSIFICATION (DORA Art. 17):
        FOR EACH operational incident:
            Evaluate impact criteria:
                - Number of clients affected
                - Duration of service disruption
                - Geographic spread
                - Data losses
                - Economic impact
                - Criticality of services affected

            CLASSIFY severity:
                MAJOR: Meets two or more of the following:
                    - Affects > 10% of clients (> 5,000 for NordKredit)
                    - Service unavailable > 2 hours during business hours
                    - Data integrity breach
                    - Financial loss > 100,000 SEK
                SIGNIFICANT: Meets one of the above criteria
                MINOR: Below all thresholds

    NOTIFICATION TIMELINES (DORA Art. 19):
        IF severity = MAJOR:
            Initial notification to FSA: within 4 hours of classification
            Intermediate report: within 72 hours
            Final report: within 1 month of resolution
        IF severity = SIGNIFICANT:
            Report in next regular reporting cycle
        IF severity = MINOR:
            Log internally, no external reporting required

    CURRENT MAINFRAME ERROR HANDLING:
        CBTRN01C (Verification step):
            ON file-open-error:
                DISPLAY 'ERROR OPENING FILE [filename]'
                PERFORM ABEND-ROUTINE (code 999)
            ON read-error:
                DISPLAY 'ERROR READING RECORD'
                PERFORM ABEND-ROUTINE (code 999)

        CBTRN02C (Posting step):
            ON file-io-error:
                DISPLAY 'ERROR ON FILE OPERATION'
                PERFORM ABEND-ROUTINE (code 999)
            ON validation-failure:
                WRITE reject record to DALYREJS
                SET RETURN-CODE = 4 (warning, continue)

        CBTRN03C (Report step):
            ON file-io-error:
                DISPLAY 'ERROR ON FILE OPERATION'
                PERFORM ABEND-ROUTINE (code 999)
            ON lookup-failure (XREF/TYPE/CATEGORY):
                DISPLAY 'LOOKUP ERROR FOR [key]'
                PERFORM ABEND-ROUTINE (code 999)
            ON dateparm-read-error:
                SET APPL-RESULT = 12 or 16
                PERFORM ABEND-ROUTINE (code 999)

    MIGRATED INCIDENT CAPTURE:
        FOR EACH error/exception in the migrated system:
            1. Capture structured incident record:
                incident-id (GUID)
                timestamp (UTC)
                service-name (e.g., "DailyBatchPipeline")
                step (e.g., "CBTRN03C-equivalent")
                error-type (file-io, lookup-failure, validation, timeout)
                error-detail (exception message, stack trace)
                affected-records (count of records processed/pending)
                recovery-action (retry, skip, abort)

            2. Classify per DORA criteria (see above)

            3. IF classification = MAJOR:
                Generate initial notification report
                Trigger alert to IT operations and compliance
                Start 4-hour notification timer

            4. Log to incident register (DORA Art. 17 requirement)
```

### ABEND Code Mapping

| COBOL ABEND | Current Handling | DORA Classification | Migrated Handling |
|---|---|---|---|
| 999 (file open) | DISPLAY + terminate | Major (batch window SLA breach) | Structured exception + retry + incident report |
| 999 (read error) | DISPLAY + terminate | Depends on scope | Structured exception + skip/retry + incident report |
| 999 (lookup fail) | DISPLAY + terminate | Minor (data quality) | Log + continue with placeholder + data quality alert |
| RC=4 (rejects) | DALYREJS + continue | Minor (expected behavior) | Structured logging + monitoring threshold |
| RC=0 (success) | Continue | N/A | Success logging + SLA compliance record |

### Incident Report Structure

| Field | Description | Source |
|---|---|---|
| Incident ID | Unique identifier | Generated (GUID) |
| Detection timestamp | When the incident was detected | System clock (UTC) |
| Classification | Major / Significant / Minor | DORA Art. 17 criteria |
| Service affected | Which system component | Service identifier |
| Impact description | Business impact narrative | Automated + manual |
| Clients affected | Number and percentage | Account/transaction count |
| Duration | Time from detection to resolution | Timer |
| Root cause | Technical cause analysis | Post-incident |
| Recovery actions | Steps taken to resolve | Operator actions |
| Regulatory notification | Sent to FSA (if Major) | Compliance team |

## Source COBOL Reference

**Programs:** `CBTRN01C.cbl` (489 lines), `CBTRN02C.cbl` (723 lines), `CBTRN03C.cbl` (650 lines)

CBTRN01C entry and error display:
```cobol
000155           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN01C'.
000187           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN01C'.
```

CBTRN02C return code for partial failure:
```cobol
000230           IF WS-REJECT-COUNT > 0
000231              MOVE 4 TO RETURN-CODE
000232           END-IF
```

CBTRN03C date parameter error handling:
```cobol
000220       0550-DATEPARM-READ.
000221           READ DATE-PARMS-FILE INTO WS-DATEPARM-RECORD
000222           EVALUATE DATEPARM-STATUS
000223             WHEN '00'
000224                 MOVE 0 TO APPL-RESULT
000225             WHEN '10'
000226                 MOVE 16 TO APPL-RESULT
000227             WHEN OTHER
000228                 MOVE 12 TO APPL-RESULT
000229           END-EVALUATE
```

The COBOL error handling pattern across all three programs is:
1. Check return status after each file operation
2. DISPLAY an error message to console (SYSOUT)
3. ABEND with code 999 (terminates the program)
4. JCL COND parameter prevents subsequent steps from running

This is the baseline that must be transformed into DORA-compliant incident management.

## Acceptance Criteria

### Scenario 1: Batch pipeline ABEND generates incident report

```gherkin
GIVEN the daily batch pipeline is running
  AND CBTRN03C encounters a file I/O error and would ABEND with code 999
WHEN the migrated system catches the equivalent exception
THEN a structured incident record is created with:
  | Field | Value |
  | Service | DailyBatchPipeline |
  | Step | ReportGeneration |
  | Error type | file-io |
  | Severity | To be classified per DORA criteria |
  AND the incident is logged to the incident register
```

### Scenario 2: Major incident triggers 4-hour notification

```gherkin
GIVEN a batch pipeline failure occurs during the nightly window
  AND the failure prevents all daily reports from being generated
  AND this affects the availability of regulatory reports for > 50,000 accounts
WHEN the incident is classified as MAJOR per DORA Art. 17
THEN an initial notification is prepared for FSA submission within 4 hours
  AND IT operations and the compliance team are alerted immediately
  AND a 72-hour intermediate report timer starts
```

### Scenario 3: Minor data quality incident logged only

```gherkin
GIVEN a transaction references a type code not found in TRANTYPE
  AND only 1 transaction record is affected
WHEN the incident is classified per DORA criteria
THEN the severity is classified as MINOR
  AND the incident is logged internally to the incident register
  AND no external notification to FSA is required
  AND a data quality alert is raised for the operations team
```

### Scenario 4: SLA breach detection

```gherkin
GIVEN the nightly batch window SLA requires completion by 06:00
  AND the batch pipeline has not completed by 05:45
WHEN the SLA monitoring detects the approaching deadline
THEN a warning alert is raised to IT operations
  AND if the pipeline has not completed by 06:00, an SLA breach incident is created
  AND the incident is classified per DORA criteria based on downstream impact
```

### Scenario 5: Incident register maintenance

```gherkin
GIVEN incidents have been recorded throughout the year
WHEN the annual DORA compliance review is performed
THEN the incident register contains all incidents with:
  - Classification (Major/Significant/Minor)
  - Detection and resolution timestamps
  - Root cause analysis (for Major and Significant)
  - Recovery actions taken
  - Notification records (for Major incidents)
  AND the register is available for FSA inspection
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| DORA | Art. 11 | ICT risk management framework including incident detection and response | Structured incident capture replaces COBOL ABEND/DISPLAY with classified, traceable incident records |
| DORA | Art. 17 | ICT-related incident classification based on impact criteria | Multi-dimensional classification (clients, duration, data, financial) determines severity |
| DORA | Art. 19 | Reporting of major ICT-related incidents to competent authorities | Major incidents trigger 4-hour initial notification, 72-hour intermediate, and 1-month final reports to FSA |
| FSA FFFS 2014:5 | Ch. 7 | Adequate operational risk management and incident handling | Incident register and classification framework satisfy FSA governance requirements |

## Edge Cases

1. **Cascading failures**: A single file I/O error in CBTRN01C can prevent all three pipeline steps from running. The incident should be classified based on the total downstream impact (all reports unavailable), not just the initial error.

2. **Partial batch completion**: If CBTRN02C completes but CBTRN03C fails, transactions are posted but the daily report is not generated. The incident classification must consider that financial transactions were processed successfully but the audit trail report is missing.

3. **Repeated minor incidents**: A pattern of recurring minor incidents (e.g., daily lookup failures for the same type code) may indicate a systemic issue that should be escalated to SIGNIFICANT even if each individual incident is MINOR.

4. **Incident during maintenance window**: Planned maintenance may cause expected failures. The incident reporting framework must distinguish between planned downtime and unplanned incidents to avoid false DORA notifications.

5. **Multi-tenant impact**: If the batch system processes multiple entity portfolios, an incident affecting one entity may be MINOR overall but MAJOR for that specific entity. Classification must consider entity-level impact.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) What is the current incident management process when a batch ABEND occurs — is there an automated alerting system or is it manual console monitoring? (2) Has NordKredit already established DORA incident classification thresholds, or do these need to be defined as part of the migration? (3) What is the current process for notifying FSA of operational incidents? (4) Does the mainframe system have any existing incident logging beyond SYSOUT DISPLAY messages? (5) What are the agreed SLA thresholds for batch window completion and report availability? (6) Is there a DORA implementation timeline that the migration must align with?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
