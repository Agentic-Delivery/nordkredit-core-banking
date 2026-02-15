---
id: "PROC-PRTCATBL-001"
title: "Print Category Balance"
sidebar_position: 8
process_type: batch
domain: reporting
jcl_source: "PRTCATBL.jcl"
cobol_programs: []
schedule: "on-demand"
sla: "None"
status: extracted
target_implementation: Azure Functions
---

# Print Category Balance

## Overview

The Print Category Balance batch process produces a formatted report of transaction category balances. It first removes any previous report output, then backs up the current TCATBALF (Transaction Category Balance File) VSAM dataset to a GDG, and finally sorts and formats the data into a human-readable report with proper currency display.

The report is organized by account ID, transaction type, and category, providing a breakdown of balances across different transaction categories (purchases, cash advances, balance transfers, fees, etc.) for each account. The SORT OUTREC EDIT formatting ensures currency values are displayed with proper decimal placement.

This is an on-demand report used by operations and business teams for balance reconciliation and analysis. It has no strict SLA but depends on the TCATBALF file being populated by the POSTTRAN process.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand
**Business owner:** Billing Operations / Reporting Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (DELDEF) | IEFBR14 | Deletes the previous report file to prevent appending to stale data | TCATBALF.REPT (previous) | File deleted | Non-existence is not an error (DISP=(MOD,DELETE,DELETE)) |
| 2 (STEP05R) | IDCAMS REPRO (via REPROC proc) | Backs up the current TCATBALF.VSAM.KSDS to a sequential GDG for processing and archival | TCATBALF.VSAM.KSDS | TCATBALF.BKUP GDG (+1) | Abort on REPRO failure |
| 3 (STEP10R) | SORT | Sorts backup data by account ID, transaction type, and category; formats output with OUTREC EDIT for currency display (decimal editing) | TCATBALF.BKUP GDG (+1) | TCATBALF.REPT (LRECL=40, formatted report) | Abort on sort failure |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-POSTTRAN-001 | Process | TCATBALF must be populated with current category balances from transaction posting |
| TCATBALF.VSAM.KSDS | Data | Transaction category balance file must contain current data |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Operations review | Manual | Report reviewed by Billing Operations for balance reconciliation |
| Audit | Process | Report retained for audit and reconciliation purposes |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand (after POSTTRAN) | Operational request |
| Completion deadline | None | No strict SLA |
| Maximum duration | Not constrained | Operational runbook |
| Retry policy | Re-run from step 1 | Operational runbook |
| Escalation | N/A (no strict SLA) | N/A |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TCATBALF.VSAM.KSDS | VSAM KSDS | Category balance record layout | POSTTRAN process | Transaction category balances by account, type, and category |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TCATBALF.BKUP GDG (+1) | Sequential (GDG) | Category balance record layout | Archival / step 3 input | Sequential backup of category balance data |
| TCATBALF.REPT | Sequential | Fixed-length, LRECL=40 | Operations team | Formatted report sorted by account/type/category with currency display |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| System error | IEFBR14 delete failure (step 1) | Non-critical; previous file may not exist | Continue to next step |
| System error | REPRO failure (step 2) | Abort job | Verify TCATBALF.VSAM.KSDS accessibility, re-run from step 1 |
| System error | SORT failure (step 3) | Abort job | Verify backup GDG was created, re-run from step 3 |
| Data integrity | TCATBALF empty or not populated | Report will be empty | Verify POSTTRAN has run; re-run after POSTTRAN completes |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool for all 3 steps
2. If step 2 (REPRO) failed: verify TCATBALF.VSAM.KSDS is accessible, re-run from step 1
3. If step 3 (SORT) failed: verify TCATBALF.BKUP GDG (+1) exists, re-run from step 3
4. All steps are idempotent -- safe to re-run from step 1 at any point

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (HTTP trigger or timer trigger)
**Trigger:** HTTP (on-demand) or Timer (if scheduled)
**CRON expression:** N/A (on-demand)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL PRTCATBL job | Azure Function `CategoryBalanceReport` | HTTP-triggered for on-demand, or timer-triggered if scheduled |
| IEFBR14 delete (step 1) | Azure Blob Storage overwrite | New report replaces previous output |
| IDCAMS REPRO (step 2) | Azure SQL query / Blob Storage export | Query category balances directly from Azure SQL |
| SORT with OUTREC EDIT (step 3) | C# report formatting with `decimal.ToString("C")` | Currency formatting in C# replaces SORT EDIT masks |
| TCATBALF.VSAM.KSDS | Azure SQL `TransactionCategoryBalances` table | Category balance data |
| TCATBALF.BKUP GDG | Azure Blob Storage `category-balance-backups/` | Backup retained for audit |
| TCATBALF.REPT | Azure Blob Storage `category-balance-reports/` | Formatted report output (CSV or PDF) |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of category balance report figures between mainframe TCATBALF.REPT and Azure report output
- **Tolerance:** Exact match for all balance amounts
- **Comparison frequency:** Every run during parallel-run period (on-demand)
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| RPT-BR-009 | FFFS 2014:5 Ch. 3 | Balance reporting must accurately reflect all transaction categories |
| RPT-BR-010 | PSD2 Art. 57 | Payment account balance information must be available for review |
| RPT-BR-011 | FFFS 2014:5 Ch. 6 | Category balance records support orderly payment services record-keeping |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
