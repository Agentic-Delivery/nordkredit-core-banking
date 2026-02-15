---
id: "PROC-COMBTRAN-001"
title: "Combine Transactions"
sidebar_position: 2
process_type: batch
domain: transactions
jcl_source: "COMBTRAN.jcl"
cobol_programs:
  - "SORT"
  - "IDCAMS"
schedule: "daily (after POSTTRAN and TRANBKP)"
sla: "Before online cycle"
status: extracted
target_implementation: Azure Functions
---

# Combine Transactions

## Overview

The Combine Transactions batch process merges the backed-up posted transactions with system-generated transactions (such as interest and fee transactions from the interest calculation process) into a single consolidated transaction file. This two-step process first sorts and merges the two input GDG files by transaction ID, then loads the combined result into the transaction VSAM KSDS file, which serves as the master transaction store for the next online cycle.

This job runs after both POSTTRAN (transaction posting) and TRANBKP (transaction backup) have completed, and after INTCALC has generated system transactions. It ensures that the transaction master file is complete and ready for the next business day.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** Daily
**Business owner:** Transaction Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP05R) | SORT | Sorts and merges TRANSACT.BKUP(0) and SYSTRAN(0) by transaction ID into a combined sequential file | TRANSACT.BKUP GDG (current generation), SYSTRAN GDG (current generation) | TRANSACT.COMBINED GDG (+1) | Abort on sort failure; verify input GDG generations exist |
| 2 (STEP10) | IDCAMS REPRO | Loads the combined sequential file into the TRANSACT.VSAM.KSDS cluster | TRANSACT.COMBINED GDG (+1) | Updated TRANSACT.VSAM.KSDS | Abort on REPRO failure; VSAM must be empty (from TRANBKP) |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-POSTTRAN-001 | Process | Transaction posting must complete to generate current day transactions |
| PROC-TRANBKP-001 | Process | Transaction backup must complete (backs up and clears VSAM, creates TRANSACT.BKUP GDG) |
| PROC-INTCALC-001 | Process | Interest calculation must complete to generate SYSTRAN GDG with interest/fee transactions |
| TRANSACT.BKUP GDG (0) | Data | Current generation of backed-up posted transactions |
| SYSTRAN GDG (0) | Data | Current generation of system-generated interest/fee transactions |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Online cycle | System | The combined transaction file must be ready before the online banking cycle begins |
| PROC-CREASTMT-001 | Process | Statement generation reads the complete transaction file |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | After TRANBKP and INTCALC complete | Batch dependency chain |
| Completion deadline | Before online cycle opens | Business SLA |
| Maximum duration | Determined by transaction volume | Operational runbook |
| Retry policy | Re-run from step 1 (idempotent merge) | Operational runbook |
| Escalation | On-call Transaction Ops if job fails | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TRANSACT.BKUP GDG (0) | Sequential (GDG) | Fixed-length 350-byte records | TRANBKP process | Backed-up posted transactions from the current cycle |
| SYSTRAN GDG (0) | Sequential (GDG) | Fixed-length 350-byte records | INTCALC process | System-generated interest and fee transactions |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANSACT.COMBINED GDG (+1) | Sequential (GDG) | Fixed-length 350-byte records | Archival / audit | Combined and sorted sequential file (new generation) |
| TRANSACT.VSAM.KSDS | VSAM KSDS | 16-byte key, 350-byte records | Online cycle, CREASTMT | Reloaded transaction master with all transactions |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Data validation | Duplicate transaction IDs in merge | SORT will flag duplicates | Review upstream processes for ID generation issues |
| System error | SORT utility failure | Abort job at step 1 | Verify input GDG files, re-run from step 1 |
| System error | IDCAMS REPRO failure | Abort job at step 2 | Verify VSAM cluster is empty (TRANBKP), re-run from step 2 |
| Capacity | VSAM space exhaustion during REPRO | Abort with return code | Extend VSAM allocation, re-run from step 2 |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool for both steps
2. If step 1 (SORT) failed: verify both input GDG files exist and are valid, re-run from step 1
3. If step 2 (REPRO) failed: verify TRANSACT.VSAM.KSDS is empty (re-run TRANBKP if needed), verify COMBINED GDG was created, re-run from step 2
4. After successful recovery, verify record counts match expected totals (BKUP records + SYSTRAN records = COMBINED records)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (timer trigger)
**Trigger:** Orchestrated via Durable Functions (runs after TRANBKP and INTCALC complete)
**CRON expression:** N/A (event-driven via batch orchestrator)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL COMBTRAN job | Azure Function `CombineTransactions` | Orchestrated function |
| SORT (step 1) | C# `TransactionCombineService` class | LINQ-based merge and sort by transaction ID |
| IDCAMS REPRO (step 2) | C# bulk insert to Azure SQL | Entity Framework bulk operations or SqlBulkCopy |
| TRANSACT.BKUP GDG | Azure SQL archive table or Blob Storage | Backed-up transaction data |
| SYSTRAN GDG | Azure SQL staging table or Blob Storage | System-generated transactions from interest calculation |
| TRANSACT.VSAM.KSDS | Azure SQL `Transactions` table | Master transaction table |
| TRANSACT.COMBINED GDG | Azure Blob Storage archive | Combined file for audit trail |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of the combined transaction output between mainframe and Azure
- **Tolerance:** Exact match required for all transaction fields including amounts and IDs
- **Comparison frequency:** Every nightly run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| TXN-BR-005 | PSD2 Art. 64 | All payment transactions must be consolidated and available for the next business cycle |
| TXN-BR-006 | PSD2 Art. 78 | Transaction amounts must be preserved exactly through the merge process without modification |
| TXN-BR-007 | FFFS 2014:5 Ch. 6 | Complete transaction records must be maintained for regulatory audit |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
