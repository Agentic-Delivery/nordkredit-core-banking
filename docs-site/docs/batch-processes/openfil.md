---
id: "PROC-OPENFIL-001"
title: "Open VSAM Files in CICS"
sidebar_position: 21
process_type: batch
domain: account-management
jcl_source: "OPENFIL.jcl"
cobol_programs: []
schedule: "on-demand (after file reload)"
sla: "N/A"
status: extracted
target_implementation: Not applicable in Azure
---

# Open VSAM Files in CICS

## Overview

The Open VSAM Files process opens the required VSAM files in the CICS region so that online transaction processing can access them. This utility job is executed after batch processing completes and files have been reloaded or updated. It is the counterpart to the CLOSEFIL job and together they form the batch window file management pair: CLOSEFIL closes files before batch, OPENFIL reopens them after batch.

Five VSAM files are opened in the CICS region: TRANSACT (transaction master), CCXREF (card cross-reference), ACCTDAT (account data), CXACAIX (card-account alternate index), and USRSEC (user security). All five files must be successfully opened before online CICS transactions can resume.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (CICS utility)
**Frequency:** On-demand (after file reload / end of batch window)
**Business owner:** Systems Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | SDSF (CICS SET FILE OPEN) | Opens 5 VSAM files in the CICS region: TRANSACT, CCXREF, ACCTDAT, CXACAIX, USRSEC | CICS region file definitions | Files available for CICS online access | Job fails if CICS region is not active or file definitions are missing |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| Batch processing window | Process | All batch jobs that modify VSAM files must have completed |
| CLOSEFIL | Process | Files were closed by CLOSEFIL before the batch window began |
| File reload/update jobs | Process | Any IDCAMS DEFINE/REPRO or data loading jobs must have completed |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS online transactions | System | All CICS transactions (card inquiry, account updates, user sign-on) depend on these files being open |
| Online Banking | System | Customer-facing operations require CICS file access |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | After batch window completion | Operational schedule |
| Completion deadline | N/A (utility job, completes in seconds) | N/A |
| Maximum duration | Less than 1 minute | Operational expectation |
| Retry policy | Re-run after verifying CICS region status | Operational runbook |
| Escalation | On-call systems operator if files fail to open | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CICS File Definitions | CICS CSD | Resource definitions | CICS CSD (CBADMCDJ) | File resource definitions in the CICS system definition |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANSACT | VSAM KSDS | Transaction record layout | CICS online transactions | Transaction master file, now open for online access |
| CCXREF | VSAM KSDS | Card cross-reference layout | CICS online transactions | Card-to-account cross-reference file |
| ACCTDAT | VSAM KSDS | Account record layout | CICS online transactions | Account master data file |
| CXACAIX | VSAM AIX | Alternate index layout | CICS online transactions | Card-account alternate index for reverse lookups |
| USRSEC | VSAM KSDS | User security record layout | CICS sign-on transactions | User security file for authentication |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| System error | CICS region not active | Job fails with non-zero return code | Start CICS region, then re-run OPENFIL |
| Configuration error | File definition missing from CSD | Job fails for that file | Run CBADMCDJ to define missing resources, then re-run |
| File error | VSAM file damaged or missing | CICS reports file error on open | Restore file from backup or re-run file definition/load jobs, then re-run OPENFIL |

### Restart/Recovery Procedure

1. Check CICS region status (is it active and accepting commands)
2. Verify all 5 VSAM files exist and are not damaged (IDCAMS LISTCAT)
3. Verify file definitions exist in CICS CSD
4. Re-run OPENFIL (idempotent; opening an already-open file is a no-op)

## Target Azure Implementation Notes

### Architecture

**Target service:** Not applicable in Azure
**Trigger:** N/A
**CRON expression:** N/A

This job has no equivalent in the Azure target architecture. In the mainframe environment, CICS requires explicit file open/close operations to manage concurrent access between batch and online processing. In Azure, Azure SQL Database handles concurrent access natively through connection pooling and transaction isolation levels. Files (now database tables) are always accessible to both batch (Azure Functions) and online (Azure App Service) workloads simultaneously.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| OPENFIL JCL job | No equivalent needed | Azure SQL handles concurrent access natively |
| SDSF CICS SET FILE OPEN | N/A | Connection pooling replaces file open/close semantics |
| VSAM file open state | Azure SQL connection pool | Always available; no explicit open/close required |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable (infrastructure utility, no business data output)
- **Tolerance:** N/A
- **Comparison frequency:** N/A
- During parallel-run, this job continues to run on the mainframe to support CICS operations until cutover

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | N/A | This is an infrastructure utility job with no direct regulatory requirements. Indirectly supports all online transaction processing which is subject to PSD2 and FFFS 2014:5 availability requirements. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
