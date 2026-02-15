---
id: "PROC-TRANREPT-001"
title: "Transaction Report"
sidebar_position: 6
process_type: batch
domain: reporting
jcl_source: "TRANREPT.jcl"
cobol_programs:
  - "CBTRN03C.cbl"
schedule: "daily (after POSTTRAN)"
sla: "None specified"
status: extracted
target_implementation: Azure Functions
---

# Transaction Report

## Overview

The Transaction Report batch process produces a formatted daily transaction report. It first backs up the current transactions, then filters and sorts them by a date range (from PARM-START-DATE to PARM-END-DATE) and card number, and finally generates a formatted report with card lookups, transaction type descriptions, category descriptions, and running totals.

The report is used by operations and business teams to review daily transaction activity. It provides a human-readable summary of all posted transactions with enriched data from reference files (card cross-reference, transaction type codes, transaction category codes).

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** Daily
**Business owner:** Transaction Operations / Reporting Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP05R) | IDCAMS REPRO (via REPROC proc) | Backs up the current transaction VSAM file to a sequential GDG for report processing | TRANSACT.VSAM.KSDS | TRANSACT.BKUP GDG (+1) | Abort on REPRO failure |
| 2 (STEP05R) | SORT | Filters transactions by date range (PARM-START-DATE to PARM-END-DATE) and sorts the result by card number | TRANSACT.BKUP GDG (current) | TRANSACT.DALY GDG (+1) | Abort on sort failure; verify date parameters |
| 3 (STEP10R) | CBTRN03C.cbl | Produces formatted transaction report with card lookups from CARDXREF, transaction type descriptions from TRANTYPE, category descriptions from TRANCATG, and running totals per card and overall | TRANSACT.DALY GDG (+1), CARDXREF.VSAM.KSDS, TRANTYPE.VSAM.KSDS, TRANCATG.VSAM.KSDS, DATEPARM | TRANREPT GDG (+1) | Skip records with lookup failures; log to SYSPRINT |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-POSTTRAN-001 | Process | Transaction posting must complete so report includes all daily transactions |
| TRANSACT.VSAM.KSDS | Data | Transaction master file must contain current posted transactions |
| CARDXREF.VSAM.KSDS | Data | Card cross-reference data for card number lookups |
| TRANTYPE.VSAM.KSDS | Data | Transaction type reference file for type descriptions |
| TRANCATG.VSAM.KSDS | Data | Transaction category reference file for category descriptions |
| DATEPARM | Data | Date range parameters for filtering |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Operations review | Manual | Daily report reviewed by Transaction Operations team |
| Audit | Process | Reports retained for audit trail purposes |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | After POSTTRAN completes | Batch dependency chain |
| Completion deadline | None specified | Operational schedule |
| Maximum duration | Not constrained | Operational runbook |
| Retry policy | Re-run from step 1 | Operational runbook |
| Escalation | N/A (no strict SLA) | N/A |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TRANSACT.VSAM.KSDS | VSAM KSDS | 16-byte key, 350-byte records | Transaction master | Current posted transactions |
| CARDXREF.VSAM.KSDS | VSAM KSDS | Card cross-reference layout | Card management | Card number to account number mapping |
| TRANTYPE.VSAM.KSDS | VSAM KSDS | Transaction type layout | Reference data | Transaction type codes and descriptions |
| TRANCATG.VSAM.KSDS | VSAM KSDS | Transaction category layout | Reference data | Transaction category codes and descriptions |
| DATEPARM | Parameter file | Date range (start/end) | Job parameters | Defines the date range for transaction filtering |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANSACT.BKUP GDG (+1) | Sequential (GDG) | Fixed-length 350-byte records | Step 2 input / archival | Sequential backup of transactions |
| TRANSACT.DALY GDG (+1) | Sequential (GDG) | Fixed-length 350-byte records | Step 3 input | Date-filtered and sorted transactions |
| TRANREPT GDG (+1) | Sequential (GDG) | Fixed-length, LRECL=133 | Operations team, audit | Formatted transaction report with descriptions and totals |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| System error | REPRO failure (step 1) | Abort job | Verify VSAM accessibility, re-run from step 1 |
| System error | SORT failure (step 2) | Abort job | Verify input GDG and date parameters, re-run from step 1 |
| Data validation | Card not found in XREF | Skip record in report, log to SYSPRINT | Review card data; report will have missing card details |
| Data validation | Transaction type not found in TRANTYPE | Use default description in report | Review reference data |
| Data validation | Transaction category not found in TRANCATG | Use default description in report | Review reference data |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool for all 3 steps
2. If step 1 (REPRO) failed: verify TRANSACT.VSAM.KSDS is accessible, re-run from step 1
3. If step 2 (SORT) failed: verify date parameters are valid, re-run from step 1
4. If step 3 (report generation) failed: verify reference files (CARDXREF, TRANTYPE, TRANCATG) are available, re-run from step 3 using TRANSACT.DALY GDG

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (timer trigger)
**Trigger:** Timer (CRON) or orchestrated via Durable Functions
**CRON expression:** `0 30 1 * * *` (daily 01:30 UTC, after POSTTRAN)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL TRANREPT job | Azure Function `DailyTransactionReport` | Timer-triggered or orchestrated function |
| IDCAMS REPRO (step 1) | Eliminated -- query Azure SQL directly | No need for sequential backup for reporting |
| SORT filter/sort (step 2) | SQL WHERE clause with date range + ORDER BY card number | Database handles filtering and sorting |
| CBTRN03C.cbl (step 3) | C# `TransactionReportService` class | Generates formatted report |
| TRANTYPE.VSAM.KSDS | Azure SQL `TransactionTypes` table | Reference data for type descriptions |
| TRANCATG.VSAM.KSDS | Azure SQL `TransactionCategories` table | Reference data for category descriptions |
| TRANREPT GDG | Azure Blob Storage `transaction-reports/` container | Report output stored as file with date-based naming |
| DATEPARM | Azure Function parameters / app settings | Date range passed as configuration |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of report line items between mainframe TRANREPT output and Azure report output
- **Tolerance:** Exact match for financial amounts and running totals; formatting differences acceptable
- **Comparison frequency:** Every daily run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| RPT-BR-006 | PSD2 Art. 57 | Transaction reports provide detailed payment transaction information required by PSD2 |
| RPT-BR-007 | FFFS 2014:5 Ch. 6 | Transaction reports support orderly record-keeping for payment services |
| RPT-BR-008 | GDPR Art. 15 | Transaction reports may be used to fulfill data subject access requests |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
