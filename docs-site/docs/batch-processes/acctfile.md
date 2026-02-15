---
id: "PROC-ACCTFILE-001"
title: "Account File Definition/Loading"
sidebar_position: 10
process_type: batch
domain: account-management
jcl_source: "ACCTFILE.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database seed script / Azure Data Factory pipeline
---

# Account File Definition/Loading

## Overview

Defines and loads the Account master VSAM KSDS cluster used by the core banking system. This is a data file management job that deletes any existing Account VSAM cluster, redefines it with the required key and record structure, and bulk-loads account data from a flat (physical sequential) file into the newly defined VSAM dataset.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The job uses IDCAMS utility exclusively (no COBOL programs) to manage VSAM cluster lifecycle.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Infrastructure Operations / Account Management Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP05) | IDCAMS DELETE | Delete existing ACCTDATA.VSAM.KSDS cluster. Condition code override: IF MAXCC LE 08 THEN SET MAXCC = 0 (tolerates cluster-not-found) | N/A | Cluster removed (or no-op if absent) | MAXCC <= 8 tolerated (cluster may not exist); MAXCC > 8 aborts job |
| 2 (STEP10) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(11 0) and RECORDSIZE(300 300), INDEXED | N/A | Empty VSAM KSDS cluster ACCTDATA.VSAM.KSDS | Abort on failure (allocation or catalog error) |
| 3 (STEP15) | IDCAMS REPRO | Bulk-load account records from flat file ACCTDATA.PS into the VSAM KSDS cluster | ACCTDATA.PS (flat file) | Populated ACCTDATA.VSAM.KSDS | Abort on failure; check record count mismatch |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| ACCTDATA.PS flat file | Data | Physical sequential file containing account records must be available on disk or tape |
| VSAM catalog | System | ICF catalog must be available for cluster definition |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS online transactions | System | CICS programs reading account data depend on the VSAM cluster being defined and loaded |
| PROC-CARDFILE-001 | Process | Card file cross-references account IDs that must exist in the account file |
| PROC-XREFFILE-001 | Process | Cross-reference file links cards to accounts in this cluster |
| Batch processes (interest calc, statements) | Process | All account-related batch jobs read from this VSAM cluster |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Infrastructure Operations team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| ACCTDATA.PS | Physical Sequential (PS) | Fixed-length 300-byte records | Data migration / extract | Flat file containing account master records for bulk load |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| ACCTDATA.VSAM.KSDS | VSAM KSDS | KEYS(11 0), RECORDSIZE(300 300), INDEXED | CICS online, batch programs | Account master data VSAM cluster keyed on 11-byte account ID at offset 0 |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Cluster not found on DELETE | IDCAMS DELETE returns MAXCC 8 (entry not found) | Tolerated; MAXCC set to 0 via IF MAXCC LE 08 THEN SET MAXCC = 0 | No recovery needed; proceed to DEFINE |
| Catalog error on DEFINE | Duplicate entry, space unavailable, or catalog offline | Abort job step | Resolve catalog issue; delete stale entry if needed; re-run from STEP05 |
| Data load error on REPRO | Record format mismatch, key sequence error, or input file unavailable | Abort job step | Verify input file format and availability; re-run from STEP05 |
| Duplicate key on REPRO | Input file contains duplicate account IDs | Abort REPRO step | Deduplicate input file; re-run from STEP05 |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step (STEP05, STEP10, STEP15)
2. If STEP05 failed with MAXCC > 8: investigate catalog or VSAM issue; resolve and re-run entire job
3. If STEP10 failed: check for existing cluster (may need manual DELETE); resolve allocation issue and re-run from STEP05
4. If STEP15 failed: verify input file ACCTDATA.PS integrity; re-run entire job from STEP05 (DEFINE requires empty cluster)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database seed script / Azure Data Factory pipeline
**Trigger:** Manual (on-demand) or Azure Data Factory pipeline trigger
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL ACCTFILE job | EF Core migration + seed script or Azure Data Factory pipeline | One-time setup or reload scenario |
| IDCAMS DELETE cluster | EF Core migration: `DROP TABLE IF EXISTS` or `TRUNCATE TABLE` | Idempotent table recreation |
| IDCAMS DEFINE CLUSTER (KEYS(11 0)) | EF Core migration: `CREATE TABLE` with primary key on AccountId (11-char) | VSAM KSDS maps to SQL table with clustered primary key |
| IDCAMS REPRO from flat file | Azure Data Factory Copy Activity or EF Core `DbContext.BulkInsert` | Flat file source from Azure Blob Storage |
| ACCTDATA.PS flat file | Azure Blob Storage (CSV/fixed-width) | EBCDIC-to-Unicode conversion during migration |
| ACCTDATA.VSAM.KSDS | Azure SQL Database `Accounts` table | RECORDSIZE(300 300) maps to table columns per copybook layout |

### Parallel-Run Considerations

- **Comparison strategy:** Record count comparison and key-by-key field validation between VSAM and Azure SQL
- **Tolerance:** Exact match required for all account fields (no calculation involved)
- **Comparison frequency:** After each reload during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| ACCT-BR-001 | FFFS 2014:5 Ch. 3 | Account master data must be accurate and complete for all banking operations |
| ACCT-BR-002 | GDPR Art. 5(1)(d) | Account data must be accurate and kept up to date |
