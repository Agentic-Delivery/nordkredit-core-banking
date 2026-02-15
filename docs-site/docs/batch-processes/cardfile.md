---
id: "PROC-CARDFILE-001"
title: "Card File Definition/Loading"
sidebar_position: 11
process_type: batch
domain: card-management
jcl_source: "CARDFILE.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database with secondary index
---

# Card File Definition/Loading

## Overview

Defines and loads the Card data VSAM KSDS cluster along with an Alternate Index (AIX) on account ID. This is a data file management job that handles the full lifecycle of the card data VSAM dataset: closing CICS file access, deleting and redefining the cluster and AIX, bulk-loading card data from a flat file, building the alternate index, and reopening CICS file access.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The job requires CICS coordination to close and reopen file definitions before and after the VSAM operations.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility with CICS coordination)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Card Management Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (Pre-step) | SDSF/CEMT | Close CICS file definitions: CEMT SET FIL(CARDDAT) CLO and CEMT SET FIL(CARDAIX) CLO | N/A | CICS files closed for exclusive VSAM access | Manual verification required; CICS must be active |
| 2 (STEP05) | IDCAMS DELETE | Delete existing card data VSAM cluster and AIX | N/A | Cluster and AIX removed (or no-op if absent) | MAXCC tolerated for not-found conditions |
| 3 (STEP10) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(16 0) and RECORDSIZE(150 150) | N/A | Empty VSAM KSDS cluster for card data | Abort on catalog or allocation failure |
| 4 (STEP15) | IDCAMS REPRO | Bulk-load card records from CARDDATA.PS flat file into the VSAM KSDS | CARDDATA.PS (flat file) | Populated card data VSAM KSDS | Abort on format mismatch or key sequence error |
| 5 (STEP40) | IDCAMS DEFINE AIX | Define Alternate Index on account ID field: KEYS(11 16), NONUNIQUEKEY | Card data VSAM KSDS (base cluster) | AIX definition on account ID at offset 16 | Abort on catalog error |
| 6 (STEP50) | IDCAMS DEFINE PATH | Define PATH to associate AIX with base cluster for access | AIX definition | PATH linking AIX to base cluster | Abort on definition error |
| 7 (STEP60) | IDCAMS BLDINDEX | Build the alternate index from existing base cluster records | Card data VSAM KSDS | Populated AIX | Abort on build failure |
| 8 (Post-step) | SDSF/CEMT | Reopen CICS file definitions: CEMT SET FIL(CARDDAT) OPE and CEMT SET FIL(CARDAIX) OPE | N/A | CICS files reopened for online access | Manual verification required |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CARDDATA.PS flat file | Data | Physical sequential file containing card records must be available |
| CICS region | System | CICS must be active for file close/open commands |
| PROC-ACCTFILE-001 | Process | Account file must be loaded first (AIX references account IDs) |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS card management transactions | System | Online card inquiry and update transactions depend on this VSAM cluster |
| PROC-XREFFILE-001 | Process | Cross-reference file links cards to accounts using card numbers from this cluster |
| Card billing batch processes | Process | Billing and statement jobs read card data from this cluster |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Card Operations and CICS support team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CARDDATA.PS | Physical Sequential (PS) | Fixed-length 150-byte records | Data migration / extract | Flat file containing card records for bulk load |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Card data VSAM KSDS | VSAM KSDS | KEYS(16 0), RECORDSIZE(150 150) | CICS online, batch programs | Card master data keyed on 16-byte card number at offset 0 |
| Card data AIX (Account ID) | VSAM AIX | KEYS(11 16), NONUNIQUEKEY | CICS online (via PATH) | Alternate index allowing lookup of cards by 11-byte account ID at offset 16 |
| Card data PATH | VSAM PATH | N/A | CICS CARDAIX file definition | Access path linking AIX to base cluster |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| CICS file close failure | CEMT SET FIL CLO fails (file in use or CICS down) | Cannot proceed with VSAM operations | Wait for active transactions to complete; retry CEMT command |
| Cluster not found on DELETE | IDCAMS DELETE returns entry-not-found | Tolerated; proceed to DEFINE | No recovery needed |
| Catalog error on DEFINE | Duplicate entry or space unavailable | Abort job step | Resolve catalog issue; re-run from STEP05 |
| Data load error on REPRO | Record format mismatch or input file unavailable | Abort job step | Verify input file; re-run from STEP05 |
| AIX build failure | BLDINDEX encounters key extraction errors | Abort job step | Verify base cluster data integrity; re-run BLDINDEX step |
| CICS file reopen failure | CEMT SET FIL OPE fails | Online transactions unavailable | Manually issue CEMT command; escalate to CICS support |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step
2. If any step failed: resolve the root cause
3. Re-run entire job from the beginning (CICS close, STEP05 through STEP60, CICS open) since VSAM cluster definition is atomic
4. After successful completion, verify CICS file status with CEMT INQ FIL(CARDDAT) and CEMT INQ FIL(CARDAIX)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database with secondary index
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL CARDFILE job | EF Core migration + seed script | One-time setup or reload scenario |
| CICS file close/open | N/A | Not needed; Azure SQL handles concurrent access natively |
| IDCAMS DELETE cluster + AIX | EF Core migration: `DROP TABLE IF EXISTS` | Single table replaces cluster + AIX |
| IDCAMS DEFINE CLUSTER (KEYS(16 0)) | EF Core migration: `CREATE TABLE` with primary key on CardNumber (16-char) | VSAM KSDS maps to SQL table with clustered primary key |
| IDCAMS DEFINE AIX (KEYS(11 16)) | SQL non-clustered index on AccountId column | NONUNIQUEKEY AIX maps to non-unique non-clustered index |
| IDCAMS DEFINE PATH | N/A | SQL indexes are accessed directly; no PATH concept needed |
| IDCAMS BLDINDEX | Automatic index maintenance | SQL Server maintains indexes automatically on insert |
| IDCAMS REPRO from flat file | Azure Data Factory Copy Activity or EF Core seed | Flat file source from Azure Blob Storage |
| CARDDATA.PS flat file | Azure Blob Storage (CSV/fixed-width) | EBCDIC-to-Unicode conversion during migration |

### Parallel-Run Considerations

- **Comparison strategy:** Record count and key-by-key field comparison between VSAM and Azure SQL
- **Tolerance:** Exact match required for all card fields
- **Comparison frequency:** After each reload during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| CARD-BR-001 | PSD2 Art. 63 | Card data must be accurately maintained for payment instrument management |
| CARD-BR-002 | GDPR Art. 5(1)(d) | Card holder data must be accurate and kept up to date |
| CARD-BR-003 | FFFS 2014:5 Ch. 4 | Card account linkage must be traceable for regulatory reporting |
