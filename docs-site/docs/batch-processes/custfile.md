---
id: "PROC-CUSTFILE-001"
title: "Customer File Definition/Loading"
sidebar_position: 12
process_type: batch
domain: account-management
jcl_source: "CUSTFILE.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database
---

# Customer File Definition/Loading

## Overview

Defines and loads the Customer master VSAM KSDS cluster used by the core banking system. This is a data file management job that coordinates with CICS to close file access, deletes and redefines the customer VSAM cluster, bulk-loads customer data from a flat file, and reopens CICS file access.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The job manages the full lifecycle of the CUSTDAT VSAM cluster with CICS coordination.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility with CICS coordination)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Account Management / Customer Data Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (Pre-step) | SDSF/CEMT | Close CICS file definition: CEMT SET FIL(CUSTDAT) CLO | N/A | CICS file closed for exclusive VSAM access | Manual verification required; CICS must be active |
| 2 (DELETE) | IDCAMS DELETE | Delete existing customer data VSAM KSDS cluster | N/A | Cluster removed (or no-op if absent) | MAXCC tolerated for not-found conditions |
| 3 (DEFINE) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(9 0) and RECORDSIZE(500 500) | N/A | Empty VSAM KSDS cluster for customer data | Abort on catalog or allocation failure |
| 4 (REPRO) | IDCAMS REPRO | Bulk-load customer records from flat file into the VSAM KSDS | Customer flat file (PS) | Populated customer data VSAM KSDS | Abort on format mismatch or key sequence error |
| 5 (Post-step) | SDSF/CEMT | Reopen CICS file definition: CEMT SET FIL(CUSTDAT) OPE | N/A | CICS file reopened for online access | Manual verification required |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| Customer flat file (PS) | Data | Physical sequential file containing customer records must be available |
| CICS region | System | CICS must be active for file close/open commands |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS customer inquiry transactions | System | Online customer lookup and update transactions depend on this VSAM cluster |
| PROC-ACCTFILE-001 | Process | Account file references customer IDs from this cluster |
| KYC/AML batch processes | Process | AML screening jobs read customer data from this cluster |
| Statement generation | Process | Monthly statements use customer name and address data |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Customer Data Management and CICS support team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| Customer flat file | Physical Sequential (PS) | Fixed-length 500-byte records | Data migration / extract | Flat file containing customer master records for bulk load |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Customer data VSAM KSDS (CUSTDAT) | VSAM KSDS | KEYS(9 0), RECORDSIZE(500 500) | CICS online, batch programs | Customer master data keyed on 9-byte customer ID at offset 0 |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| CICS file close failure | CEMT SET FIL(CUSTDAT) CLO fails (file in use or CICS down) | Cannot proceed with VSAM operations | Wait for active transactions to complete; retry CEMT command |
| Cluster not found on DELETE | IDCAMS DELETE returns entry-not-found | Tolerated; proceed to DEFINE | No recovery needed |
| Catalog error on DEFINE | Duplicate entry or space unavailable | Abort job step | Resolve catalog issue; re-run from DELETE step |
| Data load error on REPRO | Record format mismatch or input file unavailable | Abort job step | Verify input file format and availability; re-run from DELETE step |
| CICS file reopen failure | CEMT SET FIL(CUSTDAT) OPE fails | Online transactions unavailable | Manually issue CEMT command; escalate to CICS support |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step
2. If any step failed: resolve the root cause
3. Re-run entire job from the beginning (CICS close, DELETE through REPRO, CICS open) since VSAM cluster definition is atomic
4. After successful completion, verify CICS file status with CEMT INQ FIL(CUSTDAT)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL CUSTFILE job | EF Core migration + seed script | One-time setup or reload scenario |
| CICS file close/open (CUSTDAT) | N/A | Not needed; Azure SQL handles concurrent access natively |
| IDCAMS DELETE cluster | EF Core migration: `DROP TABLE IF EXISTS` or `TRUNCATE TABLE` | Idempotent table recreation |
| IDCAMS DEFINE CLUSTER (KEYS(9 0)) | EF Core migration: `CREATE TABLE` with primary key on CustomerId (9-char) | VSAM KSDS maps to SQL table with clustered primary key |
| IDCAMS REPRO from flat file | Azure Data Factory Copy Activity or EF Core seed | Flat file source from Azure Blob Storage |
| Customer flat file (PS) | Azure Blob Storage (CSV/fixed-width) | EBCDIC-to-Unicode conversion during migration; GDPR considerations for PII |
| Customer VSAM KSDS (CUSTDAT) | Azure SQL Database `Customers` table | RECORDSIZE(500 500) maps to table columns per copybook layout |

### Parallel-Run Considerations

- **Comparison strategy:** Record count and key-by-key field comparison between VSAM and Azure SQL
- **Tolerance:** Exact match required for all customer fields
- **Comparison frequency:** After each reload during parallel-run period
- **GDPR note:** Customer PII comparison must occur within EU data boundaries; no data export outside EU

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| ACCT-BR-010 | GDPR Art. 5(1)(d) | Customer data must be accurate and kept up to date |
| ACCT-BR-011 | GDPR Art. 17 | Customer data deletion (right to erasure) must be supported |
| ACCT-BR-012 | AML/KYC (FFFS 2017:11) | Customer identity data must be maintained for KYC compliance |
| ACCT-BR-013 | FFFS 2014:5 Ch. 3 | Customer master data integrity is required for account management |
