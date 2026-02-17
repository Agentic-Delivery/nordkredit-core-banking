---
id: "rpt-br-008"
title: "Batch reporting SLA and scheduling rules"
domain: "reporting"
cobol_source: "CBTRN01C.cbl:1-489, CBTRN02C.cbl:1-723, CBTRN03C.cbl:1-650 (pipeline sequence)"
requirement_id: "RPT-BR-008"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "AML 2017:11 Para. 3"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-008: Batch reporting SLA and scheduling rules

## Summary

The mainframe nightly batch window orchestrates all report generation with strict service level agreements (SLAs) tied to regulatory deadlines and business operations. The three-step transaction processing pipeline (CBTRN01C, CBTRN02C, CBTRN03C) must complete by 06:00 to ensure reports are available before business hours. Additional reporting jobs — AML screening (RPT-BR-002), management reports (RPT-BR-005), and monthly statement generation (RPT-BR-003) — have their own SLAs that depend on the core pipeline completing first. The JCL job scheduler enforces sequencing via COND parameters and job dependencies. The migrated system must preserve these SLA constraints using Azure Functions timer triggers and orchestration (Durable Functions). Extracted from the batch pipeline framework (TRN-BR-009) and regulatory SLA requirements.

## Business Logic

### Pseudocode

```
NIGHTLY BATCH SCHEDULE:

    BATCH WINDOW: 22:00 (day D) to 06:00 (day D+1)
    HARD DEADLINE: 06:00 — all critical reports must be complete

    PHASE 1 — TRANSACTION PROCESSING (22:00 - 02:00 target):
        JOB: TRANPROC
            Step 1: CBTRN01C (Verification)
                COND: None (always runs)
                Typical duration: 15-30 minutes
            Step 2: CBTRN02C (Posting)
                COND: (4,LT,VERIFY) — runs if step 1 RC <= 4
                Typical duration: 30-60 minutes
            Step 3: CBTRN03C (Daily Report)
                COND: (4,LT,POST) — runs if step 2 RC <= 4
                Typical duration: 15-30 minutes
        Total: 60-120 minutes

    PHASE 2 — COMPLIANCE REPORTING (02:00 - 04:00 target):
        JOB: AMLSCR (depends on TRANPROC completion)
            AML/KYC screening batch (RPT-BR-002)
            Duration: 30-60 minutes

    PHASE 3 — MANAGEMENT REPORTING (04:00 - 06:00 target):
        JOB: MGMTRPT (depends on TRANPROC completion)
            Daily operations summary
            Portfolio balance report
            Delinquency aging report
            Duration: 30-60 minutes

    MONTHLY ADDITIONS (day 1-3 of each month):
        JOB: STMTGEN (depends on billing cycle close)
            Customer statement generation (RPT-BR-003)
            Duration: 2-4 hours (runs in parallel with nightly jobs)
            SLA: Complete by day 3

    QUARTERLY ADDITIONS:
        JOB: FSARPT
            FSA regulatory report data extract (RPT-BR-001)
            Duration: 1-2 hours
            SLA: Complete by day 30 of quarter+1

    JCL SEQUENCING RULES:
        COND=(4,LT,stepname):
            IF stepname RC > 4: skip this step
            IF stepname RC <= 4: run this step
            ABEND (RC=999): skip all subsequent steps

        Job dependency:
            AMLSCR depends on TRANPROC (cannot start until TRANPROC completes)
            MGMTRPT depends on TRANPROC (can run parallel with AMLSCR)
            STMTGEN depends on billing cycle close (independent of nightly chain)
```

### SLA Matrix

| Report | SLA Deadline | Dependency | Regulatory Driver |
|---|---|---|---|
| Daily transaction report (CBTRN03C) | 06:00 daily | CBTRN02C completion | FSA FFFS 2014:5 Ch. 7 |
| AML screening report | 06:00 daily | TRANPROC job completion | AML 2017:11 Para. 3 |
| Daily management reports | 08:00 daily | TRANPROC job completion | FSA FFFS 2014:5 Ch. 7 |
| Customer statements | Day 3 of M+1 | Billing cycle close | PSD2 Art. 45/57 |
| FSA quarterly reports | Day 30 of Q+1 | Quarter-end data | FSA FFFS 2014:5 Ch. 7 |
| FSA annual reports | March 31 of Y+1 | Year-end data | FSA FFFS 2014:5 Ch. 3 |

### Decision Table — Batch Failure Handling

| Failure Point | Impact | Recovery | SLA Risk |
|---|---|---|---|
| CBTRN01C ABEND | All subsequent steps skip | Fix data, restart full pipeline | High — entire pipeline delayed |
| CBTRN02C ABEND | Posting incomplete, report skipped | Manual reconciliation, restart from step 2 | Critical — partial postings |
| CBTRN02C RC=4 | Some rejects, report still runs | Review rejects in morning | Low — normal operation |
| CBTRN03C ABEND | Report not generated, postings OK | Fix issue, restart step 3 only | Medium — report delayed |
| AML screening failure | Screening incomplete | Restart AML job | High — regulatory requirement |
| Statement batch timeout | Partial statements | Restart for remaining accounts | Medium — within 3-day window |

### Azure Migration Architecture

```
Current (Mainframe JCL):
    JOB SCHEDULE → CBTRN01C → CBTRN02C → CBTRN03C → AMLSCR → MGMTRPT

Migrated (Azure):
    Timer Trigger (22:00)
        → Durable Functions Orchestrator
            → Activity: TransactionVerification  (CBTRN01C equivalent)
            → Activity: TransactionPosting        (CBTRN02C equivalent)
            → Fan-out:
                → Activity: DailyTransactionReport (CBTRN03C equivalent)
                → Activity: AmlScreening           (AMLSCR equivalent)
                → Activity: ManagementReports      (MGMTRPT equivalent)
            → Monitor: SLA compliance check at 05:30 (alert if incomplete)

    Timer Trigger (Day 1, 22:00 — monthly):
        → Durable Functions Orchestrator
            → Activity: StatementGeneration (STMTGEN equivalent)
            → Monitor: Day 3 SLA check

    Timer Trigger (Quarter+1 Day 1 — quarterly):
        → Durable Functions Orchestrator
            → Activity: FsaReportGeneration (FSARPT equivalent)
```

## Source COBOL Reference

**Programs:** `CBTRN01C.cbl` (489 lines), `CBTRN02C.cbl` (723 lines), `CBTRN03C.cbl` (650 lines)

JCL step sequencing (from TRN-BR-009):
```jcl
//VERIFY   EXEC PGM=CBTRN01C
//POST     EXEC PGM=CBTRN02C,COND=(4,LT,VERIFY)
//REPORT   EXEC PGM=CBTRN03C,COND=(4,LT,POST)
```

The `COND=(4,LT,POST)` parameter means: if the return code from the POST step is greater than 4, skip the REPORT step. This ensures reports are only generated when posting has succeeded or completed with warnings (RC=4).

CBTRN02C return code setting:
```cobol
000230           IF WS-REJECT-COUNT > 0
000231              MOVE 4 TO RETURN-CODE
000232           END-IF
000234           GOBACK.
```

CBTRN01C execution boundaries:
```cobol
000155           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN01C'.
000187           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN01C'.
000189           GOBACK.
```

## Acceptance Criteria

### Scenario 1: Full pipeline completes within SLA

```gherkin
GIVEN the nightly batch window starts at 22:00
  AND the daily transaction file contains 50,000 transactions
WHEN the three-step pipeline executes successfully
THEN CBTRN01C (verification) completes within 30 minutes
  AND CBTRN02C (posting) completes within 60 minutes
  AND CBTRN03C (reporting) completes within 30 minutes
  AND the total pipeline completes before 06:00
  AND all downstream reports can begin generation
```

### Scenario 2: Pipeline failure prevents downstream reports

```gherkin
GIVEN CBTRN02C ABENDs with code 999 during transaction posting
WHEN the JCL scheduler evaluates step 3 (CBTRN03C)
THEN step 3 is skipped (COND parameter prevents execution)
  AND the AML screening job does not start (depends on TRANPROC completion status)
  AND an SLA breach alert is raised for the 06:00 deadline
  AND a DORA incident record is created (RPT-BR-006)
```

### Scenario 3: Partial rejection allows pipeline continuation

```gherkin
GIVEN CBTRN02C processes 50,000 transactions
  AND 127 transactions fail validation and are written to DALYREJS
WHEN CBTRN02C completes with RC=4 (warning — some rejects)
THEN CBTRN03C (daily report) executes normally
  AND the report includes only the 49,873 successfully posted transactions
  AND the reject file is available for morning review
```

### Scenario 4: Monthly statement SLA compliance

```gherkin
GIVEN the billing cycle for January 2026 closes on January 31
WHEN the statement generation batch starts on February 1
THEN all customer statements are generated by February 3
  AND the batch processes accounts in parallel where possible
  AND an SLA warning is raised if generation has not started by February 2
```

### Scenario 5: SLA monitoring and alerting

```gherkin
GIVEN the nightly pipeline has been running since 22:00
  AND it is now 05:30
  AND CBTRN03C has not yet started
WHEN the SLA monitor checks pipeline progress
THEN a warning alert is raised to IT operations
  AND the alert includes the current pipeline status and estimated completion time
  AND if completion is projected after 06:00, a pre-breach escalation is triggered
```

### Scenario 6: Azure orchestrator retry on transient failure

```gherkin
GIVEN the migrated pipeline runs as a Durable Functions orchestration
  AND the TransactionPosting activity encounters a transient Azure SQL error
WHEN the orchestrator detects the failure
THEN the activity is retried with exponential backoff (max 3 retries)
  AND if all retries fail, the orchestrator records the failure and halts
  AND the SLA monitoring timer is checked for 06:00 deadline risk
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 7 | Adequate batch processing controls and timely reporting | SLA-driven scheduling ensures reports are generated within regulatory deadlines |
| AML 2017:11 | Para. 3 | Nightly AML screening must be completed timely | AML screening SLA (06:00) ensures screening results are available before business hours |
| DORA | Art. 11 | ICT operational resilience including batch processing continuity | SLA monitoring, failure alerting, and recovery procedures support operational resilience |

## Edge Cases

1. **Daylight saving time transitions**: The batch window 22:00-06:00 must handle CET/CEST transitions. On the spring forward night (losing 1 hour), the batch window is effectively shorter. On the fall back night (gaining 1 hour), there is more time. The scheduler must use UTC internally and convert for display.

2. **End-of-month and quarter overlap**: On March 31, the system must generate: (a) nightly daily report, (b) monthly statements for March, (c) Q1 FSA quarterly report extract. These must be prioritized and sequenced correctly. The daily pipeline always takes priority.

3. **Large volume days**: Year-end, Black Friday, or other high-volume days may significantly increase processing time. The batch system must scale (in Azure: auto-scale Azure Functions) to maintain SLA compliance on peak days.

4. **Manual restart after failure**: If the pipeline fails at step 2 and is restarted, the restart must resume from the failed step, not repeat step 1. CBTRN01C is advisory (read-only), so re-running it is safe but wastes time. The migrated system should support step-level restart.

5. **Holiday schedules**: On Swedish public holidays (e.g., Midsommar, Jul), transaction volumes may be lower but regulatory deadlines may still apply. The batch schedule must run regardless of holidays unless explicitly configured otherwise.

6. **Parallel vs. sequential trade-offs**: In the migrated Azure system, report generation (CBTRN03C equivalent) and AML screening can run in parallel (fan-out) since they both read from TRANSACT. However, they must not run until CBTRN02C has completed writing to TRANSACT. The orchestrator must enforce this dependency.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) What is the exact batch window (are 22:00 and 06:00 the actual times)? (2) What is the current JCL job scheduling tool (CA-7, TWS, Control-M)? (3) Are there other batch jobs in the nightly window beyond the transaction pipeline? (4) What is the escalation procedure when the 06:00 SLA is breached? (5) How is the monthly statement batch currently scheduled — does it run in the regular nightly window or as a separate job? (6) What are the peak transaction volumes (Black Friday, year-end) and how long does the pipeline take on peak days?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
