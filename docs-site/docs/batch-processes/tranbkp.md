---
id: "PROC-TRANBKP-001"
title: "Transaction Backup"
sidebar_position: 5
process_type: batch
domain: transactions
jcl_source: "TRANBKP.jcl"
cobol_programs:
  - "IDCAMS"
schedule: "daily (after POSTTRAN and TRANREPT)"
sla: "Before COMBTRAN"
status: extracted
target_implementation: Azure Functions
---

# Transaction Backup

## Overview

The Transaction Backup batch process creates a sequential backup of the transaction VSAM master file, then deletes and redefines the VSAM cluster to prepare an empty transaction file for the next cycle. This three-step process is essential for the daily transaction lifecycle: it preserves the current day's posted transactions in a GDG backup, clears the VSAM file, and redefines it with the correct structure so that the COMBTRAN process can reload the merged transactions.

The backup GDG serves as both a recovery mechanism and an input to the COMBTRAN process, which merges backed-up transactions with system-generated transactions.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** Daily
**Business owner:** Transaction Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP05R) | IDCAMS REPRO (via REPROC proc) | Copies all records from TRANSACT.VSAM.KSDS to a new generation of the sequential backup GDG | TRANSACT.VSAM.KSDS | TRANSACT.BKUP GDG (+1) | Abort on REPRO failure; VSAM must be accessible |
| 2 (STEP05) | IDCAMS DELETE | Deletes the TRANSACT.VSAM.KSDS cluster and its alternate index (AIX) | TRANSACT.VSAM.KSDS | Cluster and AIX removed from catalog | Abort on delete failure |
| 3 (STEP10) | IDCAMS DEFINE | Redefines TRANSACT.VSAM.KSDS as an empty indexed cluster (16-byte key, 350-byte records) | None | Empty TRANSACT.VSAM.KSDS cluster | COND=(4,LT) -- skips if prior step had RC > 4 |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-POSTTRAN-001 | Process | Transaction posting must complete before backing up and clearing the file |
| PROC-TRANREPT-001 | Process | Transaction report must complete before the file is cleared |
| TRANSACT.VSAM.KSDS | Data | Transaction master file must contain all posted transactions |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| PROC-COMBTRAN-001 | Process | Uses TRANSACT.BKUP GDG as input for the merge; requires empty TRANSACT.VSAM.KSDS for reload |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | After POSTTRAN and TRANREPT complete | Batch dependency chain |
| Completion deadline | Before COMBTRAN starts | Batch dependency chain |
| Maximum duration | Determined by transaction volume | Operational runbook |
| Retry policy | Must verify backup integrity before retrying delete/define | Operational runbook |
| Escalation | On-call Transaction Ops on failure | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TRANSACT.VSAM.KSDS | VSAM KSDS | 16-byte key, 350-byte records | Transaction master | Current posted transactions to be backed up |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANSACT.BKUP GDG (+1) | Sequential (GDG) | Fixed-length 350-byte records | COMBTRAN process, archival | Sequential backup of all transactions |
| TRANSACT.VSAM.KSDS | VSAM KSDS | 16-byte key, 350-byte records (empty) | COMBTRAN process | Redefined empty cluster ready for reload |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| System error | REPRO failure (step 1) | Abort job | Verify VSAM accessibility, retry REPRO |
| System error | DELETE failure (step 2) | Abort job | Verify cluster exists, check for VSAM enqueue contention |
| System error | DEFINE failure (step 3) | Skip (COND=(4,LT)) | Manually define VSAM cluster; verify catalog |
| Data integrity | Backup file incomplete | Do not proceed to DELETE | Compare record counts between VSAM and backup GDG |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool for all 3 steps
2. If step 1 (REPRO) failed: verify TRANSACT.VSAM.KSDS is accessible, re-run from step 1
3. If step 2 (DELETE) failed: verify backup GDG (+1) was created with correct record count before retrying delete
4. If step 3 (DEFINE) failed: manually define the VSAM cluster using the standard definition parameters (16-byte key, 350-byte records)
5. CRITICAL: Never proceed to DELETE without confirming backup integrity -- data loss is unrecoverable

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (orchestrated)
**Trigger:** Orchestrated via Durable Functions (runs after POSTTRAN and TRANREPT complete)
**CRON expression:** N/A (event-driven via batch orchestrator)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL TRANBKP job | Azure Function `TransactionBackup` | Orchestrated function |
| IDCAMS REPRO (step 1) | Azure SQL archive query / Azure Blob Storage export | Copy transactions to archive table or blob |
| IDCAMS DELETE (step 2) | Azure SQL TRUNCATE or DELETE | Clear the active transactions table |
| IDCAMS DEFINE (step 3) | Not needed in Azure SQL | Table schema persists; TRUNCATE handles clearing |
| TRANSACT.BKUP GDG | Azure Blob Storage `transaction-backups/` container | Daily backup files with date-based naming |
| TRANSACT.VSAM.KSDS | Azure SQL `Transactions` table | Truncated and ready for reload |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of backup file contents between mainframe GDG and Azure Blob Storage backup
- **Tolerance:** Exact match required for all fields
- **Comparison frequency:** Every nightly run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| TXN-BR-008 | PSD2 Art. 64 | Transaction records must be preserved through the backup process without loss |
| TXN-BR-009 | FFFS 2014:5 Ch. 6 | Complete transaction audit trail must be maintained through backup and restore cycle |
| TXN-BR-010 | DORA Art. 11 | Backup and recovery procedures must ensure operational resilience of critical financial data |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
