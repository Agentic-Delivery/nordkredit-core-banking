---
id: "PROC-ESDSRRDS-001"
title: "ESDS/RRDS File Operations"
sidebar_position: 27
process_type: batch
domain: account-management
jcl_source: "ESDSRRDS.jcl"
cobol_programs: []
schedule: "on-demand (demo/testing)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database
---

# ESDS/RRDS File Operations

## Overview

The ESDS/RRDS File Operations job demonstrates the creation and population of VSAM Entry-Sequenced Data Set (ESDS) and Relative Record Data Set (RRDS) files using the same user security data that DUSRSECJ loads into a KSDS. This job exists primarily for demonstration and testing purposes, showing how the same data can be stored in different VSAM file organizations.

The job creates a flat file from inline user data (same 10 users as DUSRSECJ: 5 admin, 5 regular), then defines and loads both an ESDS (NONINDEXED) cluster and an RRDS (NUMBERED) cluster. ESDS stores records in arrival sequence with no key-based access, while RRDS stores records by relative record number for direct positional access.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IEFBR14/IEBGENER/IDCAMS utilities)
**Frequency:** On-demand (demonstration/testing only)
**Business owner:** Systems Development Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (PREDEL) | IEFBR14 | Delete previous flat file (catalog cleanup) | Catalog entry for PS file | PS file deleted | Normal if file does not exist |
| 2 (STEP01) | IEBGENER | Create sequential (PS) file from in-stream user data (5 admin + 5 regular users) | In-stream data (SYSIN) | User data PS file (LRECL=80) | Job fails if output allocation fails |
| 3 (STEP02) | IDCAMS | DELETE existing ESDS cluster then DEFINE CLUSTER as NONINDEXED (ESDS) | IDCAMS control statements | Empty ESDS cluster | DELETE failure ignored; DEFINE failure aborts |
| 4 (STEP03) | IDCAMS | REPRO from PS file to ESDS cluster (load user records in arrival sequence) | User data PS file | Populated ESDS cluster | Fails on I/O errors |
| 5 (STEP04) | IDCAMS | DELETE existing RRDS cluster then DEFINE CLUSTER as NUMBERED (RRDS) | IDCAMS control statements | Empty RRDS cluster | DELETE failure ignored; DEFINE failure aborts |
| 6 (STEP05) | IDCAMS | REPRO from PS file to RRDS cluster (load user records by relative record number) | User data PS file | Populated RRDS cluster | Fails on I/O errors |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| None | N/A | Self-contained demo/test job with inline data |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| None | N/A | Demo/test job with no production dependencies |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Development/testing schedule |
| Completion deadline | N/A (demo/test utility) | N/A |
| Maximum duration | Less than 1 minute | Operational expectation |
| Retry policy | Re-run (fully idempotent) | N/A |
| Escalation | N/A (non-production job) | N/A |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| In-stream user data | JCL SYSIN | Fixed 80-byte records | JCL inline data | 10 user records (same data as DUSRSECJ): 5 admin, 5 regular users |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| User data PS file | Sequential (PS) | LRECL=80, fixed-length | Steps 03 and 05 (REPRO input) | Intermediate flat file of user records |
| USRSEC ESDS cluster | VSAM ESDS (NONINDEXED) | 80-byte records, arrival-sequenced | Demo/testing | User security data in entry-sequenced format (no key access) |
| USRSEC RRDS cluster | VSAM RRDS (NUMBERED) | 80-byte records, relative record access | Demo/testing | User security data in relative record format (positional access) |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Expected condition | PS file does not exist on PREDEL | IEFBR14 completes normally | No action needed |
| Expected condition | ESDS/RRDS cluster does not exist on DELETE | IDCAMS continues to DEFINE | No action needed |
| System error | Catalog or volume error on DEFINE | Step fails | Verify catalog and volume availability, re-run |
| Data error | REPRO failure (I/O error) | Step fails | Verify source PS file integrity, re-run |

### Restart/Recovery Procedure

1. Check return codes for each step
2. Job is fully idempotent: re-running deletes and recreates everything from scratch
3. Since this is a demo/test job, restart is simply re-running the entire job

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database
**Trigger:** N/A
**CRON expression:** N/A

The VSAM file organization concepts (ESDS, RRDS, KSDS) do not have direct equivalents in Azure SQL Database. All three VSAM organizations store the same logical data; the difference is in access method (sequential, positional, key-based). In Azure SQL Database, a single table with appropriate indexes supports all access patterns:

- **KSDS equivalent:** Primary key index (key-based access)
- **ESDS equivalent:** Clustered index on insertion order / identity column (arrival sequence)
- **RRDS equivalent:** Identity column or row number (positional access)

Since this is a demo/test job, it has no direct migration target. The concepts it demonstrates are useful for understanding VSAM file types during the migration of production jobs that use ESDS or RRDS files.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| VSAM ESDS (NONINDEXED) | Azure SQL table with identity column | Arrival-sequence access via auto-increment ID |
| VSAM RRDS (NUMBERED) | Azure SQL table with identity column | Relative record access via row number |
| VSAM KSDS (INDEXED) | Azure SQL table with primary key | Key-based access via primary key index |
| IDCAMS DEFINE CLUSTER | EF Core migration | Schema managed through code-first migrations |
| IDCAMS REPRO | EF Core seed data or SQL INSERT | Data loading via application code |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable (demo/test job, not part of production processing)
- **Tolerance:** N/A
- **Comparison frequency:** N/A

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | N/A | Demo/test job with no regulatory requirements. Contains same user data as DUSRSECJ; same GDPR Art. 32 security concerns apply to any environment where this data exists. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
