---
id: "PROC-TCATBALF-001"
title: "Transaction Category Balance File Definition/Loading"
sidebar_position: 16
process_type: batch
domain: transactions
jcl_source: "TCATBALF.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database table
---

# Transaction Category Balance File Definition/Loading

## Overview

Defines and loads the Transaction Category Balance VSAM KSDS cluster. This is a data file management job that deletes any existing cluster, redefines it with the required key and record structure, and bulk-loads transaction category balance data from a flat file into the newly defined VSAM dataset.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The transaction category balance file stores aggregated balance information per transaction category, used for reporting and reconciliation purposes.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Transaction Processing / Finance Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (DELETE) | IDCAMS DELETE | Delete existing transaction category balance VSAM KSDS cluster | N/A | Cluster removed (or no-op if absent) | MAXCC tolerated for not-found conditions |
| 2 (DEFINE) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(17 0) and RECORDSIZE(50 50) | N/A | Empty VSAM KSDS cluster for transaction category balances | Abort on catalog or allocation failure |
| 3 (REPRO) | IDCAMS REPRO | Bulk-load transaction category balance records from TCATBALF.PS flat file into the VSAM KSDS | TCATBALF.PS (flat file) | Populated transaction category balance VSAM KSDS | Abort on format mismatch or key sequence error |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| TCATBALF.PS flat file | Data | Physical sequential file containing transaction category balance records must be available |
| PROC-TRANCATG-001 | Process | Transaction category file should be loaded first (balances reference categories) |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Transaction reporting batch jobs | Process | Report generation uses category balances for summary reporting |
| Reconciliation processes | Process | Reconciliation jobs compare category balances against transaction details |
| CICS online inquiry | System | Category balance inquiry transactions read from this cluster |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on data volume (typically small dataset) | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Transaction Processing team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TCATBALF.PS | Physical Sequential (PS) | Fixed-length 50-byte records | Data migration / extract | Flat file containing transaction category balance records for bulk load |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Transaction category balance VSAM KSDS | VSAM KSDS | KEYS(17 0), RECORDSIZE(50 50) | CICS online, batch programs | Transaction category balance data keyed on 17-byte composite key at offset 0 |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Cluster not found on DELETE | IDCAMS DELETE returns entry-not-found | Tolerated; proceed to DEFINE | No recovery needed |
| Catalog error on DEFINE | Duplicate entry or space unavailable | Abort job step | Resolve catalog issue; re-run from DELETE step |
| Data load error on REPRO | Record format mismatch, key sequence error, or input file unavailable | Abort job step | Verify input file format and availability; re-run from DELETE step |
| Duplicate key on REPRO | Input file contains duplicate composite keys | Abort REPRO step | Deduplicate input file; re-run from DELETE step |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step
2. If any step failed: resolve the root cause
3. Re-run entire job from the beginning (DELETE through REPRO) since VSAM cluster definition is atomic
4. Verify record count after successful load

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database table
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL TCATBALF job | EF Core migration + seed script | One-time setup or reload scenario |
| IDCAMS DELETE cluster | EF Core migration: `DROP TABLE IF EXISTS` or `TRUNCATE TABLE` | Idempotent table recreation |
| IDCAMS DEFINE CLUSTER (KEYS(17 0)) | EF Core migration: `CREATE TABLE` with primary key on composite key (17-char) | VSAM KSDS maps to SQL table with clustered primary key |
| IDCAMS REPRO from flat file | Azure Data Factory Copy Activity or EF Core seed | Flat file source from Azure Blob Storage |
| TCATBALF.PS flat file | Azure Blob Storage (CSV/fixed-width) | EBCDIC-to-Unicode conversion during migration |
| Transaction category balance VSAM KSDS | Azure SQL Database `TransactionCategoryBalances` table | RECORDSIZE(50 50) maps to table columns per copybook layout |

### Parallel-Run Considerations

- **Comparison strategy:** Record count and key-by-key field comparison between VSAM and Azure SQL
- **Tolerance:** Exact match required for balance amounts (financial data)
- **Comparison frequency:** After each reload during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| PAY-BR-010 | FFFS 2014:5 Ch. 5 | Transaction category balances must be accurate for financial reporting |
| PAY-BR-011 | Bokforingslagen (1999:1078) | Category balance records support bookkeeping reconciliation requirements |
