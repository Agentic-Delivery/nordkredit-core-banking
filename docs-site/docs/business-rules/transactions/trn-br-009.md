---
id: "trn-br-009"
title: "Daily batch transaction processing pipeline"
domain: "transactions"
cobol_source: "CBTRN01C.cbl:1-489,CBTRN02C.cbl:1-723,CBTRN03C.cbl:1-650"
requirement_id: "TRN-BR-009"
regulations:
  - "PSD2 Art. 64"
  - "FSA FFFS 2014:5"
  - "AML 2017:11"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# TRN-BR-009: Daily batch transaction processing pipeline

## Summary

The daily batch transaction processing pipeline consists of three sequential programs (CBTRN01C, CBTRN02C, CBTRN03C) that form a JCL job step sequence. Step 1 (CBTRN01C) verifies card numbers and account references. Step 2 (CBTRN02C) validates business rules, posts valid transactions, updates balances, and writes rejects. Step 3 (CBTRN03C) generates the daily transaction report. This pipeline is the backbone of the end-of-day batch processing cycle and must complete within the nightly batch window SLA. Extracted from `CBTRN01C.cbl`, `CBTRN02C.cbl`, and `CBTRN03C.cbl`.

## Business Logic

### Pipeline Sequence

```
Step 1: CBTRN01C — Verification
    Input:  DALYTRAN (daily transactions)
    Reads:  XREFFILE (card cross-reference)
            ACCTFILE (account master)
    Output: Console log (verification results)
    Errors: ABEND 999 on file I/O errors
    Status: No output file — verification only

Step 2: CBTRN02C — Validation & Posting
    Input:  DALYTRAN (daily transactions)
    Reads:  XREFFILE (card cross-reference)
    Updates: ACCTFILE (account balances)
             TCATBALF (category balances)
    Output: TRANSACT (posted transactions)
            DALYREJS (rejected transactions)
    Errors: ABEND 999 on file I/O errors
    Return: RC=0 (all posted), RC=4 (some rejected)

Step 3: CBTRN03C — Reporting
    Input:  TRANSACT (posted transactions)
            DATEPARM (date range parameters)
    Reads:  CARDXREF (card cross-reference)
            TRANTYPE (type descriptions)
            TRANCATG (category descriptions)
    Output: TRANREPT (transaction detail report)
    Errors: ABEND 999 on file I/O errors or lookup failures
```

### Pipeline Data Flow

```
External    ┌──────────┐   ┌──────────┐   ┌──────────┐
Source  ──► │ CBTRN01C │──►│ CBTRN02C │──►│ CBTRN03C │──► Report
(DALYTRAN)  │ Verify   │   │ Post     │   │ Report   │   (TRANREPT)
            └────┬─────┘   └────┬─────┘   └──────────┘
                 │              │
                 │         ┌────┴─────┐
                 │         │ DALYREJS │  (Rejected)
                 │         └──────────┘
                 │
            Console Log    ACCTFILE ◄── balance updates
            (Verification) TCATBALF ◄── category balance updates
                           TRANSACT ◄── posted records
```

### JCL Job Flow (Conceptual)

```jcl
//TRANPROC JOB  ...
//*---------------------------------------------------
//* STEP 1: Verify card numbers and accounts
//*---------------------------------------------------
//VERIFY   EXEC PGM=CBTRN01C
//DALYTRAN DD DSN=PROD.DAILY.TRANS,DISP=SHR
//XREFFILE DD DSN=PROD.CARD.XREF,DISP=SHR
//ACCTFILE DD DSN=PROD.ACCOUNT.MASTER,DISP=SHR
//CUSTFILE DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR
//CARDFILE DD DSN=PROD.CARD.MASTER,DISP=SHR
//TRANFILE DD DSN=PROD.TRANS.MASTER,DISP=SHR
//*---------------------------------------------------
//* STEP 2: Validate, post, update balances
//*---------------------------------------------------
//POST     EXEC PGM=CBTRN02C,COND=(4,LT,VERIFY)
//DALYTRAN DD DSN=PROD.DAILY.TRANS,DISP=SHR
//TRANSACT DD DSN=PROD.TRANS.MASTER,DISP=OLD
//XREFFILE DD DSN=PROD.CARD.XREF,DISP=SHR
//DALYREJS DD DSN=PROD.DAILY.REJECTS,DISP=OLD
//ACCTFILE DD DSN=PROD.ACCOUNT.MASTER,DISP=OLD
//TCATBALF DD DSN=PROD.TCAT.BALANCE,DISP=OLD
//*---------------------------------------------------
//* STEP 3: Generate daily report
//*---------------------------------------------------
//REPORT   EXEC PGM=CBTRN03C,COND=(4,LT,POST)
//TRANFILE DD DSN=PROD.TRANS.MASTER,DISP=SHR
//CARDXREF DD DSN=PROD.CARD.XREF,DISP=SHR
//TRANTYPE DD DSN=PROD.TRAN.TYPE,DISP=SHR
//TRANCATG DD DSN=PROD.TRAN.CATG,DISP=SHR
//TRANREPT DD DSN=PROD.DAILY.REPORT,DISP=OLD
//DATEPARM DD DSN=PROD.DATE.PARMS,DISP=SHR
```

### Error Handling Strategy

| Program | Error Type | Response | Return Code |
|---|---|---|---|
| CBTRN01C | File open failure | DISPLAY error + ABEND 999 | N/A (abend) |
| CBTRN01C | Read error | DISPLAY error + ABEND 999 | N/A (abend) |
| CBTRN01C | Card not in XREF | DISPLAY warning, skip record | 0 (continue) |
| CBTRN02C | File I/O error | DISPLAY error + ABEND 999 | N/A (abend) |
| CBTRN02C | Validation failure | Write to DALYREJS | 4 (warning) |
| CBTRN02C | All records valid | N/A | 0 (success) |
| CBTRN03C | File I/O error | DISPLAY error + ABEND 999 | N/A (abend) |
| CBTRN03C | Lookup failure | DISPLAY error + ABEND 999 | N/A (abend) |
| CBTRN03C | Date parm missing | ABEND 999 | N/A (abend) |

### SLA Requirements

| Batch Window | Requirement | Validation |
|---|---|---|
| Nightly processing | Complete by 06:00 | All three steps must finish within window |
| Report generation | Available before business hours | CBTRN03C output must be complete |
| Reject handling | Available for morning review | DALYREJS file must be accessible |

## Source COBOL Reference

**Programs:** `CBTRN01C.cbl` (489 lines), `CBTRN02C.cbl` (723 lines), `CBTRN03C.cbl` (650 lines)

CBTRN01C entry/exit:
```cobol
000155           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN01C'.
000186           END-PERFORM.
000187           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN01C'.
000189           GOBACK.
```

CBTRN02C return code:
```cobol
000230           IF WS-REJECT-COUNT > 0
000231              MOVE 4 TO RETURN-CODE
000232           END-IF
000234           GOBACK.
```

CBTRN03C date parameter:
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

## Acceptance Criteria

### Scenario 1: Full pipeline success

GIVEN the daily transaction file contains only valid transactions
  AND all card numbers and accounts exist
  AND no transactions exceed credit limits or have expired accounts
WHEN the three-step pipeline executes
THEN CBTRN01C completes with RC=0
  AND CBTRN02C posts all transactions and completes with RC=0
  AND CBTRN03C generates the report
  AND the entire pipeline completes within the nightly batch window

### Scenario 2: Pipeline with rejections

GIVEN some daily transactions fail validation (invalid card, overlimit, expired)
WHEN the pipeline executes
THEN CBTRN01C completes with RC=0 (verification only)
  AND CBTRN02C posts valid transactions, writes rejects, and returns RC=4
  AND CBTRN03C generates the report for posted transactions only
  AND the reject file is available for morning review

### Scenario 3: Step 1 ABEND stops pipeline

GIVEN CBTRN01C encounters a file I/O error
WHEN the program ABENDs with code 999
THEN steps 2 and 3 do not execute (JCL COND parameter)
  AND operations are alerted for investigation

### Scenario 4: Date parameter drives report scope

GIVEN the DATEPARM file contains "2026-01-01 2026-01-31"
WHEN CBTRN03C generates the report
THEN only transactions with processing dates in January 2026 are included
  AND the report header displays the date range

### Scenario 5: Account balance atomicity

GIVEN CBTRN02C posts a transaction and updates the account balance
WHEN both operations complete
THEN the TRANSACT write and ACCTFILE REWRITE are consistent
  AND if either fails, the program ABENDs (preventing partial updates)

### Scenario 6: Pipeline ordering dependency

GIVEN the three programs run as sequential JCL steps
WHEN step 2 (CBTRN02C) runs
THEN it reads the same DALYTRAN file as step 1
  AND step 3 (CBTRN03C) reads the TRANSACT file that includes records posted by step 2

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 64 | End-to-end transaction integrity | Three-step pipeline ensures verification, validation, posting, and reporting |
| FSA FFFS 2014:5 | Ch. 7 | Batch processing controls and audit trail | Return codes, reject files, and reports provide complete batch audit trail |
| AML 2017:11 | Para. 3 | Transaction monitoring infrastructure | Daily batch processing is the foundation for nightly AML screening |
| DORA | Art. 11 | ICT operational resilience | ABEND handling with error codes supports monitoring and incident response |

## Edge Cases

1. **Pipeline restart after failure**: If CBTRN02C ABENDs mid-run, partial transactions have been posted and balances updated. There is no rollback mechanism. The operations team must manually reconcile. The migrated system should use database transactions for atomicity.

2. **Step 1 is advisory only**: CBTRN01C verifies cards and accounts but does not reject or filter records. Its output is console logging only. CBTRN02C re-performs the same card/account verification. Step 1 could be considered redundant but serves as an early warning system.

3. **Report includes all TRANSACT records**: CBTRN03C reads from the cumulative TRANSACT file, not just the day's postings. The date filter is essential to limit the report to the current period. Without the date parameter, it would report all historical transactions.

4. **Concurrent batch and online access**: The batch programs open files in I-O mode, which could conflict with CICS online programs (COTRN00C, COTRN01C, COTRN02C) if they run simultaneously. The batch window must be separate from online processing hours.

5. **JCL COND parameter**: The conceptual JCL uses `COND=(4,LT)` to skip subsequent steps if a previous step returns RC > 4. This means RC=4 (some rejects) still allows the report to run, but an ABEND stops everything.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS for migration: (1) Should the three batch steps be migrated as separate Azure Functions or a single orchestrated workflow (Durable Functions / Azure Logic Apps)? (2) What is the actual batch window SLA in hours/minutes? (3) Is CBTRN01C actually needed or can its verification be folded into CBTRN02C? (4) Should the migrated system implement transaction-level rollback or continue with the ABEND-on-error approach? (5) How should the date parameter file be replaced — Azure configuration, pipeline parameter, or scheduled trigger date?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
