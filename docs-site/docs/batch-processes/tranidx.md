---
id: "PROC-TRANIDX-001"
title: "Transaction Alternate Index Definition/Build"
sidebar_position: 20
process_type: batch
domain: transactions
jcl_source: "TRANIDX.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/rebuild)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database non-clustered index
---

# Transaction Alternate Index Definition/Build

## Overview

Defines and builds the Alternate Index (AIX) on the Transaction VSAM KSDS cluster for the processed timestamp field. This is a data file management job that defines an AIX, creates the access PATH, and builds the index from existing base cluster records. Unlike PROC-TRANFILE-001 which handles the full transaction file lifecycle including the AIX, this job is dedicated to the AIX definition and rebuild only, operating on an already-populated transaction VSAM cluster.

This is an initialization job, not a recurring batch process. It is executed on-demand when the alternate index needs to be created or rebuilt independently of the base cluster reload. Use cases include initial AIX setup on an existing cluster, AIX rebuild after data corruption, or index maintenance.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup/rebuild)
**Business owner:** Transaction Processing / Database Administration Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP20) | IDCAMS DEFINE AIX | Define Alternate Index on TRANSACT.VSAM.KSDS for processed timestamp: KEYS(26 304), NONUNIQUEKEY | TRANSACT.VSAM.KSDS (base cluster, must exist) | AIX definition on processed timestamp at offset 304 | Abort on catalog error or if base cluster does not exist |
| 2 (STEP25) | IDCAMS DEFINE PATH | Define PATH to associate the AIX with the base cluster for read access | AIX definition from STEP20 | PATH linking AIX to TRANSACT.VSAM.KSDS base cluster | Abort on definition error |
| 3 (STEP30) | IDCAMS BLDINDEX | Build the alternate index by scanning all records in the base cluster | TRANSACT.VSAM.KSDS (base cluster) | Populated AIX with entries for all existing records | Abort on build failure (I/O error, key extraction error) |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| TRANSACT.VSAM.KSDS | Data | Transaction VSAM KSDS base cluster must exist and be populated (via PROC-TRANFILE-001 or equivalent) |
| CICS file closure | System | CICS files TRANSACT and CXACAIX should be closed before AIX rebuild to prevent access conflicts |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS timestamp-based queries | System | Online transaction inquiries by processed timestamp use this AIX via PATH |
| Transaction reporting batch jobs | Process | Report generation jobs that select transactions by date range use this AIX |
| End-of-day processing | Process | EOD processes that query recent transactions use timestamp-based access |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations or DBA |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on base cluster size (BLDINDEX scans all records) | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Database Administration / Transaction Processing team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TRANSACT.VSAM.KSDS | VSAM KSDS | KEYS(16 0), RECORDSIZE(350 350) | PROC-TRANFILE-001 | Existing transaction base cluster that the AIX will be built over |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Transaction AIX (CXACAIX) | VSAM AIX | KEYS(26 304), NONUNIQUEKEY | CICS online (via PATH), batch programs | Alternate index on 26-byte processed timestamp at offset 304 for time-based queries |
| Transaction PATH | VSAM PATH | N/A | CICS CXACAIX file definition | Access path linking timestamp AIX to base cluster |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Base cluster not found | TRANSACT.VSAM.KSDS does not exist | DEFINE AIX aborts | Run PROC-TRANFILE-001 to create and load base cluster first |
| AIX already exists | DEFINE AIX returns duplicate entry | Abort job step | Delete existing AIX manually or use PROC-TRANFILE-001 for full reload |
| PATH already exists | DEFINE PATH returns duplicate entry | Abort job step | Delete existing PATH manually; re-run from STEP20 |
| BLDINDEX failure | I/O error or key extraction error during index build | Abort job step | Verify base cluster integrity; delete AIX and PATH; re-run entire job |
| CICS access conflict | Base cluster open in CICS during BLDINDEX | BLDINDEX may fail or produce incomplete index | Close CICS files before running; re-run after closure |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step (STEP20, STEP25, STEP30)
2. If STEP20 (DEFINE AIX) failed: verify base cluster exists; resolve catalog issue; re-run
3. If STEP25 (DEFINE PATH) failed: resolve catalog issue; may need to delete AIX from STEP20; re-run entire job
4. If STEP30 (BLDINDEX) failed: AIX and PATH definitions are valid but index is incomplete; re-run BLDINDEX step only
5. After successful completion, reopen CICS files (CEMT SET FIL(TRANSACT) OPE, CEMT SET FIL(CXACAIX) OPE)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database non-clustered index
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL TRANIDX job | EF Core migration: `CREATE NONCLUSTERED INDEX` | Index creation as part of database migration |
| IDCAMS DEFINE AIX (KEYS(26 304)) | SQL `CREATE NONCLUSTERED INDEX IX_Transactions_ProcessedTimestamp ON Transactions(ProcessedTimestamp)` | NONUNIQUEKEY AIX maps to non-unique non-clustered index |
| IDCAMS DEFINE PATH | N/A | SQL indexes are accessed directly via query optimizer; no PATH concept needed |
| IDCAMS BLDINDEX | Automatic index population on CREATE INDEX | SQL Server builds index automatically from existing table data |
| CICS file close/open | N/A | Azure SQL handles concurrent access; online/offline index builds available |

### Parallel-Run Considerations

- **Comparison strategy:** Verify that timestamp-based queries return identical result sets from both VSAM (via AIX) and Azure SQL (via non-clustered index)
- **Tolerance:** Exact match required for query results; timestamp format alignment needed (COBOL timestamp to SQL datetime2)
- **Comparison frequency:** After each index rebuild during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| PAY-BR-040 | PSD2 Art. 87 | Transaction records must be queryable by timestamp for regulatory inquiries |
| PAY-BR-041 | FFFS 2014:5 Ch. 5 | Timely access to transaction history is required for supervisory reporting |
