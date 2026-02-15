---
id: "PROC-DEFCUST-001"
title: "Define Customer File (Alternate)"
sidebar_position: 23
process_type: batch
domain: account-management
jcl_source: "DEFCUST.jcl"
cobol_programs: []
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database
---

# Define Customer File (Alternate)

## Overview

The Define Customer File job creates (or recreates) a VSAM KSDS cluster for customer data in an alternate environment. This is a setup/maintenance utility that uses IDCAMS to delete and redefine the customer data cluster. The job uses a different DSN prefix (`AWS.CCDA.CUSTDATA`) compared to the primary environment (`AWS.M2.CARDDEMO`), indicating this is for an alternate or secondary environment (possibly a test, staging, or migration target environment).

The cluster is defined with a 10-byte key starting at position 0, fixed record size of 500 bytes, and SHAREOPTIONS(1 4) allowing single-region read/write with cross-region read access.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup or environment refresh)
**Business owner:** Systems Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | IDCAMS | DELETE the existing customer data cluster AWS.CCDA.CUSTDATA.CLUSTER | Catalog entry | Cluster deleted from catalog | If cluster does not exist (RC=8), processing continues |
| 2 | IDCAMS | DEFINE CLUSTER for AWS.CCDA.CUSTDATA.CLUSTER with KEYS(10 0), RECORDSIZE(500 500), SHAREOPTIONS(1 4) | IDCAMS control statements | New empty VSAM KSDS cluster | Job fails if volume or catalog errors occur |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CLOSEFIL | Process | If the file is in use by CICS, it must be closed first |
| DASD volume availability | Infrastructure | Sufficient disk space on target volume |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Customer data load | Process | After definition, customer data must be loaded (IDCAMS REPRO or application load) |
| OPENFIL | Process | Once loaded, the file must be opened in CICS for online access |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Operational schedule |
| Completion deadline | N/A (utility job) | N/A |
| Maximum duration | Less than 1 minute | Operational expectation |
| Retry policy | Re-run after resolving catalog or volume errors | Operational runbook |
| Escalation | Storage administrator if volume allocation fails | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| IDCAMS control statements | In-stream (SYSIN) | IDCAMS commands | JCL | DELETE and DEFINE commands for the customer cluster |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| AWS.CCDA.CUSTDATA.CLUSTER | VSAM KSDS | KEYS(10 0), RECORDSIZE(500 500) | Customer data load process, CICS transactions | Empty VSAM KSDS cluster ready for data loading |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Expected condition | Cluster does not exist on DELETE (RC=8) | Continue processing (IF MAXCC=8 THEN SET MAXCC=0) | No action needed; this is normal for first-time setup |
| System error | Catalog error on DEFINE | Job fails | Verify catalog and volume availability, re-run |
| Capacity | Insufficient DASD space | DEFINE fails | Free space on target volume or specify alternate volume, re-run |

### Restart/Recovery Procedure

1. Check IDCAMS return codes in job output
2. If DELETE failed with RC other than 8, investigate catalog issues
3. If DEFINE failed, verify volume space and catalog health
4. Re-run job (idempotent: DELETE + DEFINE sequence handles pre-existing clusters)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database
**Trigger:** N/A (one-time schema setup via EF Core migrations)
**CRON expression:** N/A

In the Azure target, this VSAM cluster definition translates to a database table schema definition. The customer data table will be created and managed through Entity Framework Core migrations, eliminating the need for a separate file definition job.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| IDCAMS DELETE/DEFINE | EF Core migration (CreateTable) | Schema managed through code-first migrations |
| VSAM KSDS cluster | Azure SQL `Customers` table | 10-byte key maps to primary key column |
| RECORDSIZE(500 500) | Table columns | Fixed 500-byte record mapped to typed columns per copybook layout |
| SHAREOPTIONS(1 4) | Azure SQL connection pooling | Concurrent access handled natively |
| Alternate environment (AWS.CCDA prefix) | Azure SQL alternate schema or database | Separate database for staging/test environment |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable (infrastructure utility, creates empty file)
- **Tolerance:** N/A
- **Comparison frequency:** N/A
- Schema equivalence validated during data migration testing

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | GDPR Art. 5(1)(f) | Customer data storage must ensure appropriate security; VSAM SHAREOPTIONS and Azure SQL access controls must be equivalent |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
