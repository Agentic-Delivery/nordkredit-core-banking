---
id: "PROC-XREFFILE-001"
title: "Cross-Reference File Definition/Loading"
sidebar_position: 14
process_type: batch
domain: card-management
jcl_source: "XREFFILE.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database join table
---

# Cross-Reference File Definition/Loading

## Overview

Defines and loads the Card-to-Account Cross-Reference VSAM KSDS cluster along with an Alternate Index (AIX) on account ID. This is a data file management job that deletes and redefines the cross-reference cluster and AIX, bulk-loads cross-reference data from a flat file, and builds the alternate index.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The cross-reference file provides the mapping between card numbers (primary key) and account IDs (alternate key), enabling bidirectional lookups between cards and their associated accounts.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Card Management Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (DELETE) | IDCAMS DELETE | Delete existing cross-reference VSAM KSDS cluster and AIX | N/A | Cluster and AIX removed (or no-op if absent) | MAXCC tolerated for not-found conditions |
| 2 (DEFINE) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(16 0) and RECORDSIZE(50 50) | N/A | Empty VSAM KSDS cluster for cross-reference data | Abort on catalog or allocation failure |
| 3 (REPRO) | IDCAMS REPRO | Bulk-load cross-reference records from CARDXREF.PS flat file into the VSAM KSDS | CARDXREF.PS (flat file) | Populated cross-reference VSAM KSDS | Abort on format mismatch or key sequence error |
| 4 (DEFINE AIX) | IDCAMS DEFINE AIX | Define Alternate Index on account ID field: KEYS(11 25), NONUNIQUEKEY | Cross-reference VSAM KSDS (base cluster) | AIX definition on account ID at offset 25 | Abort on catalog error |
| 5 (DEFINE PATH) | IDCAMS DEFINE PATH | Define PATH to associate AIX with base cluster for access | AIX definition | PATH linking AIX to base cluster | Abort on definition error |
| 6 (BLDINDEX) | IDCAMS BLDINDEX | Build the alternate index from existing base cluster records | Cross-reference VSAM KSDS | Populated AIX | Abort on build failure |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CARDXREF.PS flat file | Data | Physical sequential file containing cross-reference records must be available |
| PROC-ACCTFILE-001 | Process | Account file must be loaded (cross-reference maps to account IDs) |
| PROC-CARDFILE-001 | Process | Card file must be loaded (cross-reference maps to card numbers) |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS card-to-account lookup | System | Online transactions that look up accounts by card number use this cross-reference |
| Card billing batch processes | Process | Billing jobs use cross-reference to link card transactions to accounts |
| Card management online transactions | System | Account-to-card lookups via the AIX |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on data volume (typically small dataset) | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Card Operations team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CARDXREF.PS | Physical Sequential (PS) | Fixed-length 50-byte records | Data migration / extract | Flat file containing card-to-account cross-reference records |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Cross-reference VSAM KSDS | VSAM KSDS | KEYS(16 0), RECORDSIZE(50 50) | CICS online, batch programs | Cross-reference data keyed on 16-byte card number at offset 0 |
| Cross-reference AIX (Account ID) | VSAM AIX | KEYS(11 25), NONUNIQUEKEY | CICS online (via PATH) | Alternate index on 11-byte account ID at offset 25 for account-to-card lookups |
| Cross-reference PATH | VSAM PATH | N/A | CICS file definition | Access path linking AIX to base cluster |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Cluster not found on DELETE | IDCAMS DELETE returns entry-not-found | Tolerated; proceed to DEFINE | No recovery needed |
| Catalog error on DEFINE | Duplicate entry or space unavailable | Abort job step | Resolve catalog issue; re-run from DELETE step |
| Data load error on REPRO | Record format mismatch, key sequence error, or input file unavailable | Abort job step | Verify input file format and availability; re-run from DELETE step |
| Referential integrity | Card number or account ID in cross-reference does not exist in card/account files | Not validated at IDCAMS level; detected at application level | Correct source data; re-run load |
| AIX build failure | BLDINDEX encounters key extraction errors | Abort job step | Verify base cluster data integrity; re-run BLDINDEX step |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step
2. If any step failed: resolve the root cause
3. Re-run entire job from the beginning (DELETE through BLDINDEX) since VSAM cluster definition is atomic
4. Verify referential integrity by cross-checking card numbers and account IDs against their respective master files

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database join table
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL XREFFILE job | EF Core migration + seed script | One-time setup or reload scenario |
| IDCAMS DELETE cluster + AIX | EF Core migration: `DROP TABLE IF EXISTS` | Single join table replaces cluster + AIX + PATH |
| IDCAMS DEFINE CLUSTER (KEYS(16 0)) | EF Core migration: `CREATE TABLE` with primary key on CardNumber (16-char) | VSAM KSDS maps to SQL join table with clustered primary key |
| IDCAMS DEFINE AIX (KEYS(11 25)) | SQL non-clustered index on AccountId column | NONUNIQUEKEY AIX maps to non-unique non-clustered index |
| IDCAMS DEFINE PATH | N/A | SQL indexes are accessed directly; no PATH concept needed |
| IDCAMS BLDINDEX | Automatic index maintenance | SQL Server maintains indexes automatically on insert |
| IDCAMS REPRO from flat file | Azure Data Factory Copy Activity or EF Core seed | Flat file source from Azure Blob Storage |
| CARDXREF.PS flat file | Azure Blob Storage (CSV/fixed-width) | EBCDIC-to-Unicode conversion during migration |
| Cross-reference VSAM KSDS | Azure SQL Database `CardAccountXref` join table | Foreign keys to Cards and Accounts tables enforce referential integrity (unlike VSAM) |

### Parallel-Run Considerations

- **Comparison strategy:** Record count and key-by-key field comparison between VSAM and Azure SQL
- **Tolerance:** Exact match required for all cross-reference fields
- **Comparison frequency:** After each reload during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| CARD-BR-010 | PSD2 Art. 63 | Card-to-account linkage must be accurately maintained for payment instrument management |
| CARD-BR-011 | FFFS 2014:5 Ch. 4 | Cross-reference data integrity is required for account-card traceability |
