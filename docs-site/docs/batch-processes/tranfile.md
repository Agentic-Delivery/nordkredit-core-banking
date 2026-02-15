---
id: "PROC-TRANFILE-001"
title: "Transaction File Definition/Loading"
sidebar_position: 13
process_type: batch
domain: transactions
jcl_source: "TRANFILE.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database with timestamp index
---

# Transaction File Definition/Loading

## Overview

Defines and loads the Transaction VSAM KSDS cluster along with an Alternate Index (AIX) on the processed timestamp field. This is a data file management job that coordinates with CICS to close file access, deletes and redefines the transaction cluster and AIX, bulk-loads transaction data from an initialization flat file, builds the alternate index, and reopens CICS file access.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The job manages the full lifecycle of the TRANSACT VSAM cluster and its timestamp-based AIX (CXACAIX) with CICS coordination.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility with CICS coordination)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Transaction Processing / Payments Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (Pre-step) | SDSF/CEMT | Close CICS file definitions: CEMT SET FIL(TRANSACT) CLO and CEMT SET FIL(CXACAIX) CLO | N/A | CICS files closed for exclusive VSAM access | Manual verification required; CICS must be active |
| 2 (DELETE) | IDCAMS DELETE | Delete existing transaction VSAM KSDS cluster and AIX | N/A | Cluster and AIX removed (or no-op if absent) | MAXCC tolerated for not-found conditions |
| 3 (DEFINE) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(16 0) and RECORDSIZE(350 350) | N/A | Empty VSAM KSDS cluster for transaction data | Abort on catalog or allocation failure |
| 4 (REPRO) | IDCAMS REPRO | Bulk-load transaction records from DALYTRAN.PS.INIT flat file into the VSAM KSDS | DALYTRAN.PS.INIT (flat file) | Populated transaction VSAM KSDS | Abort on format mismatch or key sequence error |
| 5 (DEFINE AIX) | IDCAMS DEFINE AIX | Define Alternate Index on processed timestamp field: KEYS(26 304), NONUNIQUEKEY | Transaction VSAM KSDS (base cluster) | AIX definition on timestamp at offset 304 | Abort on catalog error |
| 6 (DEFINE PATH) | IDCAMS DEFINE PATH | Define PATH to associate AIX with base cluster for access | AIX definition | PATH linking AIX to base cluster | Abort on definition error |
| 7 (BLDINDEX) | IDCAMS BLDINDEX | Build the alternate index from existing base cluster records | Transaction VSAM KSDS | Populated AIX | Abort on build failure |
| 8 (Post-step) | SDSF/CEMT | Reopen CICS file definitions: CEMT SET FIL(TRANSACT) OPE and CEMT SET FIL(CXACAIX) OPE | N/A | CICS files reopened for online access | Manual verification required |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| DALYTRAN.PS.INIT flat file | Data | Physical sequential initialization file containing transaction records must be available |
| CICS region | System | CICS must be active for file close/open commands |
| PROC-ACCTFILE-001 | Process | Account file should be loaded first (transactions reference account IDs) |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS transaction processing | System | Online transaction inquiry and posting depend on this VSAM cluster |
| PROC-TRANIDX-001 | Process | Transaction alternate index rebuild job operates on this cluster |
| Transaction reporting batch jobs | Process | Report generation reads from this cluster and its AIX |
| PROC-TCATBALF-001 | Process | Transaction category balance file references transactions in this cluster |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Transaction Processing and CICS support team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| DALYTRAN.PS.INIT | Physical Sequential (PS) | Fixed-length 350-byte records | Data migration / extract | Initialization flat file containing transaction records for bulk load |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Transaction VSAM KSDS (TRANSACT) | VSAM KSDS | KEYS(16 0), RECORDSIZE(350 350) | CICS online, batch programs | Transaction data keyed on 16-byte transaction ID at offset 0 |
| Transaction AIX (CXACAIX) | VSAM AIX | KEYS(26 304), NONUNIQUEKEY | CICS online (via PATH) | Alternate index on 26-byte processed timestamp at offset 304 for time-based queries |
| Transaction PATH | VSAM PATH | N/A | CICS CXACAIX file definition | Access path linking timestamp AIX to base cluster |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| CICS file close failure | CEMT SET FIL CLO fails (file in use or CICS down) | Cannot proceed with VSAM operations | Wait for active transactions to complete; retry CEMT command |
| Cluster not found on DELETE | IDCAMS DELETE returns entry-not-found | Tolerated; proceed to DEFINE | No recovery needed |
| Catalog error on DEFINE | Duplicate entry or space unavailable | Abort job step | Resolve catalog issue; re-run from DELETE step |
| Data load error on REPRO | Record format mismatch, key sequence error, or input file unavailable | Abort job step | Verify input file format and availability; re-run from DELETE step |
| AIX build failure | BLDINDEX encounters key extraction or sequence errors | Abort job step | Verify base cluster data integrity; re-run BLDINDEX step |
| CICS file reopen failure | CEMT SET FIL OPE fails | Online transactions unavailable | Manually issue CEMT command; escalate to CICS support |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step
2. If any step failed: resolve the root cause
3. Re-run entire job from the beginning (CICS close, DELETE through BLDINDEX, CICS open) since VSAM cluster definition is atomic
4. After successful completion, verify CICS file status with CEMT INQ FIL(TRANSACT) and CEMT INQ FIL(CXACAIX)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database with timestamp index
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL TRANFILE job | EF Core migration + seed script | One-time setup or reload scenario |
| CICS file close/open (TRANSACT, CXACAIX) | N/A | Not needed; Azure SQL handles concurrent access natively |
| IDCAMS DELETE cluster + AIX | EF Core migration: `DROP TABLE IF EXISTS` | Single table replaces cluster + AIX + PATH |
| IDCAMS DEFINE CLUSTER (KEYS(16 0)) | EF Core migration: `CREATE TABLE` with primary key on TransactionId (16-char) | VSAM KSDS maps to SQL table with clustered primary key |
| IDCAMS DEFINE AIX (KEYS(26 304)) | SQL non-clustered index on ProcessedTimestamp column | NONUNIQUEKEY AIX maps to non-unique non-clustered index |
| IDCAMS DEFINE PATH | N/A | SQL indexes are accessed directly; no PATH concept needed |
| IDCAMS BLDINDEX | Automatic index maintenance | SQL Server maintains indexes automatically on insert |
| IDCAMS REPRO from flat file | Azure Data Factory Copy Activity or EF Core seed | Flat file source from Azure Blob Storage |
| DALYTRAN.PS.INIT flat file | Azure Blob Storage (CSV/fixed-width) | EBCDIC-to-Unicode conversion during migration |

### Parallel-Run Considerations

- **Comparison strategy:** Record count and key-by-key field comparison between VSAM and Azure SQL
- **Tolerance:** Exact match required for all transaction fields; timestamp precision may require alignment (COBOL timestamp format to SQL datetime2)
- **Comparison frequency:** After each reload during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| PAY-BR-001 | PSD2 Art. 87 | Transaction records must be accurately maintained and accessible |
| PAY-BR-002 | FFFS 2014:5 Ch. 5 | Transaction data integrity must be ensured for financial reporting |
| PAY-BR-003 | Bokforingslagen (1999:1078) | Transaction records must be retained per Swedish bookkeeping law |
