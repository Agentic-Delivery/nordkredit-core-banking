---
title: Nightly Transaction Pipeline
sidebar_position: 3
---

# Nightly Transaction Pipeline

**JCL Job Chain:** CBTRN01C &rarr; CBTRN02C &rarr; CBTRN03C
**Schedule:** Daily at 01:00 UTC (configurable via `BatchSchedule:CronHour` / `BatchSchedule:CronMinute`)
**SLA:** Must complete by 06:00 UTC
**Azure Migration:** `DailyBatchOrchestrator` in `NordKredit.Functions`
**Regulation:** FFFS 2014:5 Ch.4 &sect;3 (operational risk), DORA Art.11 (ICT risk management)

---

## Overview

The nightly transaction pipeline is the primary batch job chain on the mainframe. It processes the daily transaction file through three sequential COBOL programs: card verification, credit validation and posting, and report generation. On the mainframe, a JCL job schedule triggers the chain nightly; on Azure, a `Worker` (BackgroundService) replaces the JCL scheduler.

---

## Pipeline Steps

### Step 1: Card Verification (CBTRN01C)

| Attribute | Value |
|-----------|-------|
| **COBOL Program** | `CBTRN01C.cbl` (494 lines) |
| **COBOL Source Lines** | Lines 154-250 (PROCEDURE DIVISION) |
| **Azure Implementation** | `CardVerificationFunction` (`ICardVerificationStep`) |
| **Business Rule** | TRN-BR-005 |
| **Regulation** | PSD2 Art.97 (SCA), AML/KYC |

**Input files (mainframe):**

| DD Name | Dataset Type | Description |
|---------|-------------|-------------|
| DALYTRAN | Sequential (QSAM) | Daily transaction file |
| CUSTFILE | Indexed (VSAM KSDS) | Customer master file |
| XREFFILE | Indexed (VSAM KSDS) | Card-to-account cross-reference |
| CARDFILE | Indexed (VSAM KSDS) | Card master file |
| ACCTFILE | Indexed (VSAM KSDS) | Account master file |
| TRANFILE | Indexed (VSAM KSDS) | Transaction history file |

**Output files (mainframe):**
- Writes to SYSOUT (DISPLAY statements) only; no output file is produced.

**Processing logic:**

1. Open all input files (abend on any open failure)
2. Read daily transaction file sequentially
3. For each transaction:
   - Look up card number in XREFFILE to find account ID
   - If card not found: log "CARD NUMBER ... COULD NOT BE VERIFIED" and skip
   - If card found: read account record from ACCTFILE
   - If account not found: log "ACCOUNT ... NOT FOUND"
4. Close all files
5. Return code 0 (advisory-only step)

**Error handling:**
- File open failure &rarr; ABEND (CEE3ABD, code 999)
- File read error (non-EOF) &rarr; ABEND
- Missing card/account &rarr; DISPLAY warning, continue processing

**Azure mapping:** `CardVerificationResult` contains `VerifiedCount`, `FailedCount`, and a list of `VerifiedTransaction` records. Only verified transactions are passed to Step 2.

---

### Step 2: Credit Validation and Transaction Posting (CBTRN02C)

| Attribute | Value |
|-----------|-------|
| **COBOL Program** | `CBTRN02C.cbl` (731 lines) |
| **COBOL Source Lines** | Lines 370-422 (validation), 424-579 (posting) |
| **Azure Implementation** | `TransactionCreditValidationFunction` + `TransactionPostingFunction` |
| **Business Rules** | TRN-BR-006 (validation), TRN-BR-007/008 (posting) |
| **Regulation** | PSD2 Art.97, FFFS 2014:5 Ch.3/Ch.16, EBA creditworthiness |

**Input files (mainframe):**

| DD Name | Dataset Type | Access | Description |
|---------|-------------|--------|-------------|
| DALYTRAN | Sequential (QSAM) | Input | Daily transaction file |
| XREFFILE | Indexed (VSAM KSDS) | Input | Card-to-account cross-reference |
| ACCTFILE | Indexed (VSAM KSDS) | I-O | Account master file (read + update) |
| TCATBALF | Indexed (VSAM KSDS) | I-O | Transaction category balance file (read + update/create) |

**Output files (mainframe):**

| DD Name | Dataset Type | Description |
|---------|-------------|-------------|
| TRANFILE | Indexed (VSAM KSDS) | Posted transaction records |
| DALYREJS | Sequential (QSAM) | Rejected transaction records with reason codes |

**Processing logic:**

1. **Validation phase** (lines 370-422):
   - Look up card number in XREFFILE &rarr; reject code 100 if not found
   - Look up account in ACCTFILE &rarr; reject code 101 if not found
   - Check credit limit: `ACCT-CREDIT-LIMIT >= (ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + TRAN-AMT)` &rarr; reject code 102 ("OVERLIMIT TRANSACTION")
   - Check expiration: `ACCT-EXPIRATION-DATE >= TRAN-ORIG-TS(1:10)` &rarr; reject code 103 ("TRANSACTION RECEIVED AFTER ACCT EXPIRATION")
   - Rejected transactions are written to DALYREJS with a validation trailer

2. **Posting phase** (lines 424-579):
   - Map daily transaction fields to permanent transaction record fields
   - Generate DB2-format processing timestamp
   - **Update TCATBAL** (category balance): read existing record; if not found, create new record; add transaction amount to category balance
   - **Update ACCTFILE** (account balance): add transaction amount to `ACCT-CURR-BAL`; credit/debit cycle tracking based on amount sign
   - **Write TRANFILE**: write completed transaction record

3. **Summary** (line 227-231):
   - Display transaction and reject counts
   - Set `RETURN-CODE = 4` if any rejections occurred

**Rejection reason codes:**

| Code | Description |
|------|-------------|
| 100 | Invalid card number (not in XREF) |
| 101 | Account record not found |
| 102 | Over credit limit |
| 103 | Transaction after account expiration |

**Error handling:**
- File open/read/write errors &rarr; ABEND (code 999)
- RETURN-CODE 4 &rarr; warnings (rejections exist but processing completed)
- RETURN-CODE 0 &rarr; all transactions posted successfully

**Azure mapping:** Split into two steps in the orchestrator:
- **Step 2a** (`TransactionCreditValidationFunction`): validates transactions, produces `TransactionCreditValidationResult` with `ValidCount`/`RejectedCount` and reason codes. Sets `HasWarnings = true` if rejections exist (replaces `RETURN-CODE = 4`).
- **Step 2b** (`TransactionPostingFunction`): posts valid transactions, updates category and account balances. Produces `TransactionPostingResult` with `PostedCount`/`SkippedCount`/`FailedCount`. Sets `HasErrors = true` if posting failures exist (replaces `RETURN-CODE = 8`).

---

### Step 3: Report Generation (CBTRN03C)

| Attribute | Value |
|-----------|-------|
| **COBOL Program** | `CBTRN03C.cbl` (649 lines) |
| **COBOL Source Lines** | Lines 159-373 (report logic) |
| **Azure Implementation** | `TransactionReportFunction` (`IReportGenerationStep`) |
| **Business Rule** | TRN-BR-009 |
| **Regulation** | FFFS 2014:5 Ch.7, PSD2 Art.94, AML 2017:11 |

**Input files (mainframe):**

| DD Name | Dataset Type | Description |
|---------|-------------|-------------|
| TRANFILE | Sequential (QSAM) | Posted transaction file (output from Step 2) |
| CARDXREF | Indexed (VSAM KSDS) | Card-to-account cross-reference |
| TRANTYPE | Indexed (VSAM KSDS) | Transaction type description lookup |
| TRANCATG | Indexed (VSAM KSDS) | Transaction category description lookup |
| DATEPARM | Sequential (QSAM) | Date range parameters (start date, end date) |

**Output files (mainframe):**

| DD Name | Dataset Type | Description |
|---------|-------------|-------------|
| TRANREPT | Sequential (QSAM) | Transaction detail report (133-character print lines) |

**Processing logic:**

1. Read date parameters (start date, end date) from DATEPARM file
2. Read transaction file sequentially
3. Filter transactions by processing timestamp within date range
4. For each qualifying transaction:
   - Look up card cross-reference for account ID
   - Look up transaction type description
   - Look up transaction category description
   - Write detail line to report
5. Pagination: 20 lines per page, with page totals
6. Group by card number, with account totals on card change
7. Grand total at end of report

**Report structure:**

```
TRANSACTION DETAIL REPORT            From: YYYY-MM-DD To: YYYY-MM-DD

Trans ID         Account     Type  Type Desc    Cat   Cat Desc     Source  Amount
---------------------------------------------------------------------------
XXXXXXXXXXXXXXXX 99999999999  XX   Description  9999  Description  XX    $999,999.99
...
                                                          PAGE TOTAL: $999,999.99
---------------------------------------------------------------------------
                                                       ACCOUNT TOTAL: $999,999.99
---------------------------------------------------------------------------
                                                         GRAND TOTAL: $999,999.99
```

**Error handling:**
- File open failure &rarr; ABEND (code 999)
- Invalid card XREF, transaction type, or category lookup &rarr; ABEND
- Date parameter file empty/error &rarr; ABEND

**Azure mapping:** `TransactionReportFunction` generates a `TransactionReportFunctionResult` containing `TotalTransactions`, `DetailLineCount`, `PageCount`, `AccountGroupCount`, and `GrandTotal`. Page size defaults to 20 (matching COBOL `WS-PAGE-SIZE`).

---

## VSAM-to-Azure SQL Dataset Mapping

| Mainframe Dataset (DD) | VSAM Type | Azure SQL Table | Entity Framework Entity |
|------------------------|-----------|-----------------|------------------------|
| DALYTRAN | Sequential | DailyTransactions | DailyTransaction |
| XREFFILE / CARDXREF | KSDS | CardCrossReferences | CardCrossReference |
| CUSTFILE | KSDS | Customers | Customer |
| CARDFILE | KSDS | Cards | Card |
| ACCTFILE | KSDS | Accounts | Account |
| TRANFILE | KSDS / Sequential | Transactions | Transaction |
| TCATBALF | KSDS | TransactionCategoryBalances | TransactionCategoryBalance |
| DALYREJS | Sequential | (logged, not persisted) | RejectedTransaction |
| TRANREPT | Sequential | (generated in memory) | TransactionReportFunctionResult |
| DATEPARM | Sequential | (configuration / parameters) | N/A |
| TRANTYPE | KSDS | TransactionTypes | TransactionType |
| TRANCATG | KSDS | TransactionCategories | TransactionCategory |

---

## Scheduling and SLA

| Attribute | Mainframe | Azure |
|-----------|-----------|-------|
| **Trigger** | JCL job schedule | `Worker` BackgroundService (timer) |
| **Schedule** | Nightly (defined in JCL) | 01:00 UTC (configurable) |
| **SLA deadline** | 06:00 CET | 06:00 UTC (hardcoded in `DailyBatchOrchestrator`) |
| **SLA monitoring** | Manual / JES2 | `IsSlaBreached()` &rarr; Application Insights alert |
| **Retry on failure** | Manual restart by operator | No automatic retry (operator intervention) |

---

## Error Handling and Recovery

### Mainframe (JCL/COBOL)

- **ABEND handling:** All three programs call `CEE3ABD` with abend code 999 on unrecoverable errors
- **Restart/recovery:** JCL restart from last successful step; no automatic checkpointing within programs
- **Return codes:** Step 2 sets `RETURN-CODE = 4` for warnings (rejections); other steps return 0 or ABEND
- **JCL COND parameter:** Subsequent steps check predecessor return codes to decide whether to execute

### Azure (DailyBatchOrchestrator)

- **Failure handling:** Unrecoverable exception halts the pipeline; `DailyBatchResult.FailedStep` identifies which step failed
- **Logging:** Structured logging via `ILogger` partial methods &rarr; Application Insights
- **Metrics:** `BatchMetricsService` tracks pipeline duration, transactions posted, and transactions rejected via OpenTelemetry
- **SLA breach:** Logged as `LogLevel.Error`; triggers Application Insights alert rule

---

## Telemetry

| Metric | Instrument | Description |
|--------|-----------|-------------|
| `NordKredit.Batch.BatchDuration` | Histogram | Pipeline execution time in seconds |
| `NordKredit.Batch.TransactionsPosted` | Counter | Count of successfully posted transactions |
| `NordKredit.Batch.TransactionsRejected` | Counter | Count of rejected transactions |

---

## Related Documents

- [ADR-001: Batch Pipeline Orchestrator Pattern](../Architecture/ADR-001-batch-pipeline-orchestrator.md)
- [UC-TRN-04: Daily Transaction Verification](../requirements/transactions/uc-trn-04-daily-verification.md)
- [UC-TRN-05: Post Daily Transactions](../requirements/transactions/uc-trn-05-daily-posting.md)
- [UC-TRN-06: Generate Daily Transaction Report](../requirements/transactions/uc-trn-06-daily-report.md)
- [TRN-BR-005: Daily Transaction Posting](../business-rules/transactions/trn-br-005-daily-transaction-posting.md)
