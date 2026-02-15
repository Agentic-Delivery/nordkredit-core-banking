---
id: "PROC-CLOSEFIL-001"
title: "Close VSAM Files in CICS"
sidebar_position: 22
process_type: batch
domain: account-management
jcl_source: "CLOSEFIL.jcl"
cobol_programs: []
schedule: "on-demand (before batch window)"
sla: "N/A"
status: extracted
target_implementation: Not applicable in Azure
---

# Close VSAM Files in CICS

## Overview

The Close VSAM Files process closes VSAM files in the CICS region to release exclusive access for batch processing. This utility job is executed at the start of the nightly batch window, before any batch jobs that modify VSAM files can run. It is the counterpart to the OPENFIL job and together they form the batch window file management pair.

Five VSAM files are closed in the CICS region: TRANSACT (transaction master), CCXREF (card cross-reference), ACCTDAT (account data), CXACAIX (card-account alternate index), and USRSEC (user security). Closing these files prevents CICS online transactions from accessing them while batch jobs perform updates, avoiding data corruption from concurrent access.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (CICS utility)
**Frequency:** On-demand (before batch window)
**Business owner:** Systems Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | SDSF (CICS SET FILE CLOSE) | Closes 5 VSAM files in the CICS region: TRANSACT, CCXREF, ACCTDAT, CXACAIX, USRSEC | CICS region file definitions | Files closed and unavailable for CICS online access | Job fails if CICS region is not active |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| Online transaction window close | Process | All online transactions should be drained before closing files |
| Operations schedule | Process | Batch window start time reached |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| POSTTRAN | Process | Transaction posting batch job requires exclusive file access |
| File definition/loading jobs | Process | Any IDCAMS DEFINE/REPRO jobs that recreate or reload VSAM files |
| All nightly batch jobs | Process | Batch jobs that read/write VSAM files depend on files being closed to CICS |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | Beginning of nightly batch window | Operational schedule |
| Completion deadline | N/A (utility job, completes in seconds) | N/A |
| Maximum duration | Less than 1 minute | Operational expectation |
| Retry policy | Re-run after verifying CICS region status | Operational runbook |
| Escalation | On-call systems operator if files fail to close | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CICS File Definitions | CICS CSD | Resource definitions | CICS CSD (CBADMCDJ) | File resource definitions in the CICS system definition |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| N/A | N/A | N/A | N/A | No data output; this job changes CICS file state from open to closed |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| System error | CICS region not active | Job fails with non-zero return code | Start CICS region, then re-run CLOSEFIL |
| Contention | Active transactions still using files | Close may be deferred by CICS | Wait for active transactions to complete, then re-run |
| Configuration error | File definition missing from CSD | Job fails for that file | Run CBADMCDJ to define missing resources, then re-run |

### Restart/Recovery Procedure

1. Check CICS region status (is it active and accepting commands)
2. Verify no active transactions are using the files (CICS INQUIRE FILE)
3. Re-run CLOSEFIL (idempotent; closing an already-closed file is a no-op)
4. Verify all 5 files show CLOSED status before proceeding with batch jobs

## Target Azure Implementation Notes

### Architecture

**Target service:** Not applicable in Azure
**Trigger:** N/A
**CRON expression:** N/A

This job has no equivalent in the Azure target architecture. The mainframe requires explicit file close/open operations to manage the transition between online (CICS) and batch processing modes. In Azure, Azure SQL Database supports concurrent access from multiple services natively. Connection pooling in Azure App Service and Azure Functions handles access management transparently, eliminating the need for explicit file close operations before batch runs.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| CLOSEFIL JCL job | No equivalent needed | Azure SQL handles concurrent access natively |
| SDSF CICS SET FILE CLOSE | N/A | Connection pooling replaces file open/close semantics |
| Batch window file exclusion | Azure SQL transaction isolation | Row-level locking replaces file-level exclusion |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable (infrastructure utility, no business data output)
- **Tolerance:** N/A
- **Comparison frequency:** N/A
- During parallel-run, this job continues to run on the mainframe to support the batch window until cutover

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | N/A | This is an infrastructure utility job with no direct regulatory requirements. Indirectly supports batch processing integrity which is subject to FFFS 2014:5 orderly processing requirements. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
