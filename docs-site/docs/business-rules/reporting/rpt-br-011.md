---
id: "rpt-br-011"
title: "Regulatory report submission and audit trail"
domain: "reporting"
cobol_source: "CBTRN03C.cbl:1-650 (report generation output), CBTRN02C.cbl:1-723 (source data pipeline)"
requirement_id: "RPT-BR-011"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 7"
  - "AML 2017:11 Para. 3"
  - "DORA Art. 19"
  - "EBA Outsourcing Guidelines"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-011: Regulatory report submission and audit trail

## Summary

NordKredit AB must submit regulatory reports to the Swedish Financial Supervisory Authority (Finansinspektionen, FSA) and other authorities (Finanspolisen for SARs, EBA for outsourcing) in prescribed formats and within defined deadlines. The current mainframe system generates data extracts via batch programs (CBTRN03C pattern), which are then manually reformatted by the compliance team for submission through FSA's electronic reporting portal. The migrated system must automate the end-to-end submission workflow: data extraction, format conversion (to XBRL, CSV, or XML as required by each report type), pre-submission validation, submission, and acknowledgement tracking. Every step must be logged in an immutable audit trail for regulatory inspection. Extracted from the CBTRN03C report generation framework, FSA submission requirements, and DORA Art. 19 incident reporting obligations.

## Business Logic

### Pseudocode

```
REGULATORY SUBMISSION WORKFLOW:

    STEP 1 — DATA EXTRACTION:
        Trigger: Batch schedule (RPT-BR-008) or ad-hoc request
        Execute report generation per RPT-BR-001 (FSA), RPT-BR-002 (AML)
        Output: Internal data extract (structured dataset)
        Audit log: extraction-started, extraction-completed, record-count, data-hash

    STEP 2 — RECONCILIATION CHECK:
        Execute reconciliation per RPT-BR-010
        IF reconciliation fails:
            HALT submission process
            Alert compliance team
            Audit log: reconciliation-failed, discrepancy-details
            EXIT (do not submit unreconciled data)
        END-IF
        Audit log: reconciliation-passed, check-results

    STEP 3 — FORMAT CONVERSION:
        Convert internal extract to submission format:

        FSA REPORTS:
            Monthly LCR: XBRL (eXtensible Business Reporting Language)
            Quarterly capital adequacy: XBRL
            Annual financial statements: XBRL + PDF
            Ad-hoc material events: FSA prescribed form (XML)

        AML REPORTS:
            Suspicious Activity Reports: goAML format (Finanspolisen)
            Threshold reports: Structured XML per Finanspolisen schema

        DORA INCIDENT REPORTS:
            Initial notification: FSA DORA reporting template (Art. 19)
            Intermediate report: FSA DORA template
            Final report: FSA DORA template

        EBA REPORTS:
            Outsourcing register: EBA prescribed CSV template

        Audit log: format-conversion-completed, output-format, output-hash

    STEP 4 — PRE-SUBMISSION VALIDATION:
        Validate formatted report against submission schema:
            XBRL: Validate against FSA taxonomy
            XML: Validate against XSD schema
            CSV: Validate field count, data types, required fields
        IF validation fails:
            HALT submission
            Generate validation error report
            Audit log: validation-failed, error-details
            EXIT
        END-IF
        Audit log: validation-passed

    STEP 5 — SUBMISSION:
        Submit to regulatory authority:
            FSA: Via FSA electronic reporting portal (API or upload)
            Finanspolisen: Via goAML portal
            EBA: Via EBA reporting portal

        Record submission details:
            submission-id (from authority acknowledgement)
            submission-timestamp
            submitted-by (system or operator ID)
            authority-reference-number (if assigned)

        Audit log: submission-completed, authority-ack, reference-number

    STEP 6 — ACKNOWLEDGEMENT TRACKING:
        Monitor for authority acknowledgement:
            FSA: Acknowledge within 24 hours (typically)
            Finanspolisen: SAR acknowledged immediately
        IF rejection received:
            Log rejection reason
            Alert compliance team
            Initiate correction workflow
            Audit log: submission-rejected, rejection-reason
        IF acknowledged:
            Audit log: submission-acknowledged, authority-ack-id

    MAINFRAME CURRENT PROCESS:
        CBTRN03C generates fixed-width data extract
        Compliance team manually:
            1. Downloads extract from mainframe output queue (JES spool)
            2. Opens in spreadsheet / conversion tool
            3. Reformats to FSA submission template
            4. Uploads to FSA portal manually
            5. Records submission in compliance log (paper/spreadsheet)
        This manual process is error-prone and lacks traceability
```

### Submission Calendar

| Report | Frequency | Deadline | Authority | Format |
|---|---|---|---|---|
| Liquidity coverage (LCR) | Monthly | Day 15 of M+1 | FSA | XBRL |
| Large exposures | Monthly (if triggered) | Day 15 of M+1 | FSA | XBRL |
| Capital adequacy | Quarterly | Day 30 of Q+1 | FSA | XBRL |
| Profit and loss | Quarterly | Day 30 of Q+1 | FSA | XBRL |
| Annual financial statements | Annually | March 31 of Y+1 | FSA | XBRL + PDF |
| Governance report | Annually | March 31 of Y+1 | FSA | PDF |
| Outsourcing register | Annually | March 31 of Y+1 | EBA via FSA | CSV |
| SAR filings | As needed | Within 24 hours of detection | Finanspolisen | goAML XML |
| AML threshold reports | Daily | Next business day | Finanspolisen | goAML XML |
| DORA major incident initial | As needed | Within 4 hours | FSA | DORA template |
| DORA intermediate report | As needed | Within 72 hours | FSA | DORA template |
| DORA final report | As needed | Within 1 month | FSA | DORA template |

### Audit Trail Structure

| Field | Description | Type |
|---|---|---|
| Audit ID | Unique identifier for audit entry | GUID |
| Timestamp | When the event occurred | DateTime (UTC) |
| Report ID | Identifier for the report being submitted | String |
| Reporting period | Period covered by the report | Date range |
| Event type | Step in the workflow | Enum (see above) |
| Actor | System or operator performing the action | String |
| Status | Success or failure | Enum |
| Details | Additional context (hash, error, reference) | JSON |
| Authority | Regulatory body receiving the report | String |
| Immutable | Audit records cannot be modified or deleted | Boolean (always true) |

### Decision Table — Submission Failure Response

| Failure Point | Impact | Response | Deadline Risk |
|---|---|---|---|
| Reconciliation break | Cannot validate data accuracy | Block submission, investigate | High — deadline countdown continues |
| Format conversion error | Cannot produce submission file | Fix template/conversion, retry | Medium — retry usually fast |
| Schema validation failure | Submission would be rejected | Fix data/format, re-validate | Medium — fix and retry |
| Portal connectivity failure | Cannot submit | Retry with backoff, escalate | High — may need manual fallback |
| Authority rejection | Submission not accepted | Correct data, resubmit | Critical — new deadline may apply |

## Source COBOL Reference

**Programs:** `CBTRN03C.cbl` (650 lines — report output generation), `CBTRN02C.cbl` (723 lines — source transaction data)

The mainframe report generation creates the data extract that feeds the submission process:

```cobol
000084       FD  REPORT-FILE.
000085       01 FD-REPTFILE-REC       PIC X(133).
```

CBTRN03C writes 133-character fixed-width records to TRANREPT. In the current process, this output is the starting point for manual reformatting to FSA submission format. The migrated system eliminates this manual step by generating submission-format output directly.

Report header with date range (the reporting period for regulatory submissions):
```cobol
000277              MOVE WS-START-DATE TO REPT-START-DATE
000278              MOVE WS-END-DATE TO REPT-END-DATE
```

The date range parameters that define the reporting period are read from DATEPARM (line 221) and written to the report header. In the migrated system, these parameters define the regulatory reporting period for the submission audit trail.

**Note:** The current mainframe system does not include automated regulatory submission. The COBOL programs produce data extracts; submission is a manual compliance process. The migrated system must automate this end-to-end workflow with full audit trail.

## Acceptance Criteria

### Scenario 1: End-to-end FSA quarterly submission

```gherkin
GIVEN the Q4 2025 capital adequacy report data has been extracted
  AND reconciliation checks have passed
WHEN the submission workflow executes
THEN the data is converted to XBRL format per FSA taxonomy
  AND the XBRL is validated against the FSA schema
  AND the report is submitted to the FSA electronic portal
  AND an acknowledgement reference number is recorded
  AND every step is logged in the immutable audit trail
```

### Scenario 2: SAR filing within 24-hour deadline

```gherkin
GIVEN the AML screening batch (RPT-BR-002) has flagged a suspicious transaction
  AND the compliance officer has reviewed and confirmed the SAR
WHEN the SAR filing is initiated
THEN the SAR is formatted in goAML XML format
  AND the SAR is submitted to Finanspolisen within 24 hours of detection
  AND the submission timestamp and Finanspolisen reference are recorded
  AND the audit trail links the SAR to the original AML screening report
```

### Scenario 3: Submission blocked by reconciliation failure

```gherkin
GIVEN a monthly LCR report has been generated
  AND the reconciliation check (RPT-BR-010) detects a material discrepancy
WHEN the submission workflow reaches the reconciliation step
THEN the submission is halted
  AND the compliance team is alerted with discrepancy details
  AND the audit trail records the reconciliation failure
  AND the report is not submitted until the discrepancy is resolved
```

### Scenario 4: DORA major incident initial notification

```gherkin
GIVEN a major ICT incident has been classified per RPT-BR-006
  AND the 4-hour notification deadline is approaching
WHEN the DORA incident report is generated
THEN the initial notification is formatted per FSA DORA template
  AND the notification is submitted to FSA within 4 hours of classification
  AND the 72-hour intermediate report deadline is scheduled
  AND the audit trail records the notification with FSA reference
```

### Scenario 5: Audit trail completeness for regulatory inspection

```gherkin
GIVEN an FSA inspector requests the audit trail for Q3 2025 regulatory submissions
WHEN the audit trail is queried for reporting period Q3 2025
THEN the trail shows every step for each submission:
  | Step | Timestamp | Status | Details |
  | Extraction | 2025-10-01 22:30 | Success | 50,000 records, hash: abc123 |
  | Reconciliation | 2025-10-01 23:00 | Passed | 0 discrepancies |
  | Format conversion | 2025-10-01 23:15 | Success | XBRL, hash: def456 |
  | Validation | 2025-10-01 23:20 | Passed | FSA taxonomy v2.1 |
  | Submission | 2025-10-01 23:25 | Success | FSA ref: FSA-2025-Q3-001 |
  | Acknowledgement | 2025-10-02 10:00 | Accepted | FSA ack: ACK-2025-001 |
  AND the audit records are immutable and tamper-evident
```

### Scenario 6: Authority rejection and resubmission

```gherkin
GIVEN an FSA quarterly report was submitted
  AND the FSA rejects the submission due to a field format error
WHEN the rejection notification is received
THEN the rejection reason is logged in the audit trail
  AND the compliance team is alerted
  AND the corrected report goes through the full workflow again
  AND both the original rejected submission and the corrected resubmission are retained
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 3 | Accurate and complete accounting records with audit trail | Immutable audit trail records every step of the report generation and submission process |
| FSA FFFS 2014:5 | Ch. 7 | Timely and accurate regulatory reporting | Automated submission workflow with deadline monitoring and pre-submission validation |
| AML 2017:11 | Para. 3 | Suspicious activity reports filed with Finanspolisen | Automated SAR formatting and submission via goAML with 24-hour deadline tracking |
| DORA | Art. 19 | Major ICT incident reporting to competent authorities | Structured DORA incident report submission with 4-hour/72-hour/1-month timelines |
| EBA | Outsourcing Guidelines Sec. 13 | Annual outsourcing register reporting | Automated annual outsourcing register submission in EBA prescribed format |

## Edge Cases

1. **Portal unavailability during deadline**: If the FSA electronic portal is unavailable near a submission deadline, the system must support alternative submission channels (e.g., secure email to FSA contact) and document the portal outage in the audit trail.

2. **XBRL taxonomy version changes**: The FSA periodically updates the XBRL taxonomy for regulatory reports. The system must support configurable taxonomy versions and validate against the correct version for each reporting period. Reports generated with an outdated taxonomy must be blocked from submission.

3. **Dual submission requirements**: Some reports may need to be submitted to both FSA and EBA (e.g., capital adequacy under CRD/CRR). The audit trail must track each submission independently.

4. **Restatement submissions**: If previously submitted data is found to be incorrect, a restatement must be submitted. The restatement must reference the original submission and explain the correction. Both the original and restatement are retained in the audit trail.

5. **Ad-hoc material event timing**: Material event notifications (e.g., significant loss, fraud discovery) have no fixed calendar deadline — they must be submitted "without delay." The system must support immediate report generation and submission outside the regular batch schedule.

6. **Operator override**: In exceptional circumstances, a compliance officer may need to override a reconciliation block and submit a report with a known discrepancy (e.g., when the deadline is imminent and the discrepancy is immaterial). The override must be explicitly authorized, logged in the audit trail with justification, and flagged for post-submission review.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) What is the current manual process for converting CBTRN03C output to FSA submission format? (2) Does NordKredit use the FSA electronic reporting portal or another submission channel? (3) What XBRL taxonomy version is currently used for FSA submissions? (4) How are SAR filings currently prepared and submitted to Finanspolisen? (5) What is the current audit trail mechanism for regulatory submissions (paper log, spreadsheet, database)? (6) Has NordKredit implemented DORA Art. 19 incident reporting yet, or is this new with the migration?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
