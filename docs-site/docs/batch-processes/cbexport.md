---
id: "PROC-CBEXPORT-001"
title: "Data Export"
sidebar_position: 30
process_type: batch
domain: account-management
jcl_source: "CBEXPORT.jcl"
cobol_programs:
  - "CBEXPORT.cbl"
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure Data Factory export pipeline / Azure SQL Database Export
---

# Data Export

## Overview

The Data Export batch process extracts data from multiple core banking VSAM files into a single consolidated VSAM export file. It reads customer, account, card cross-reference, transaction, and card data from five separate VSAM KSDS clusters and writes a multi-record export file containing all related data in a unified format. This process is used for branch migration, data transfer to downstream systems, or environment provisioning.

The job first deletes and redefines the export VSAM cluster to ensure a clean target, then executes the CBEXPORT COBOL program to read all five source files and produce the consolidated export. The multi-record export file uses a 4-byte record type key at offset 28 to distinguish between customer, account, card, cross-reference, and transaction records within the single output dataset.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand (branch migration / data transfer)
**Business owner:** Account Management / Data Migration Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP01) | IDCAMS DELETE/DEFINE | Delete existing EXPORT.DATA VSAM cluster and redefine as INDEXED KSDS with KEYS(4 28) and RECORDSIZE(500 500) | N/A | Empty EXPORT.DATA VSAM KSDS cluster | MAXCC tolerance on DELETE (cluster may not exist); abort on DEFINE failure |
| 2 (STEP02) | CBEXPORT.cbl | Reads five source VSAM files and writes consolidated multi-record export file | CUSTDATA.VSAM.KSDS, ACCTDATA.VSAM.KSDS, CARDXREF.VSAM.KSDS, TRANSACT.VSAM.KSDS, CARDDATA.VSAM.KSDS | EXPORT.DATA VSAM (multi-record export file) | Abend on I/O errors; record counts logged to SYSOUT for verification |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CUSTDATA.VSAM.KSDS | Data | Customer master VSAM file must be available and current |
| ACCTDATA.VSAM.KSDS | Data | Account master VSAM file must be available and current |
| CARDXREF.VSAM.KSDS | Data | Card-to-account cross-reference VSAM file must be available |
| TRANSACT.VSAM.KSDS | Data | Transaction master VSAM file must be available |
| CARDDATA.VSAM.KSDS | Data | Card master VSAM file must be available |
| CICS file close | System | CICS online files should be closed if running during batch window to avoid contention |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| PROC-CBIMPORT-001 | Process | Import process consumes the EXPORT.DATA file to produce normalized flat files for the target system |
| Branch migration | Process | Exported data is transferred to target branch environment |
| Data migration pipeline | Process | Export file feeds the mainframe-to-Azure data migration during parallel-run |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations or migration team |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on data volume across all five source files | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Data Migration team / Infrastructure Operations | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CUSTDATA.VSAM.KSDS | VSAM KSDS | Customer record layout | Account management | Customer master data |
| ACCTDATA.VSAM.KSDS | VSAM KSDS | Account record layout | Account management | Account master data |
| CARDXREF.VSAM.KSDS | VSAM KSDS | Cross-reference layout | Card management | Card-to-account cross-reference records |
| TRANSACT.VSAM.KSDS | VSAM KSDS | Transaction record layout | Transaction processing | Transaction master data |
| CARDDATA.VSAM.KSDS | VSAM KSDS | Card record layout | Card management | Card master data |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| EXPORT.DATA | VSAM KSDS | KEYS(4 28), RECORDSIZE(500 500), INDEXED | CBIMPORT, data migration | Consolidated multi-record export file containing customer, account, card, cross-reference, and transaction records |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Cluster not found on DELETE | IDCAMS DELETE returns MAXCC 8 (entry not found) | Tolerated; proceed to DEFINE | No recovery needed |
| Catalog error on DEFINE | Duplicate entry, space unavailable, or catalog offline | Abort job | Resolve catalog issue; re-run from STEP01 |
| Source file unavailable | One or more input VSAM files not available or closed by CICS | Abort STEP02 | Verify all five source files are available and not in use; re-run from STEP01 |
| I/O error during export | VSAM read or write error during CBEXPORT processing | Abort STEP02 | Investigate VSAM error; verify disk space; re-run from STEP01 |
| Record count mismatch | Exported record count does not match source record counts | Manual verification required | Compare record counts in SYSOUT against source file statistics; re-run if discrepancy found |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for STEP01 and STEP02
2. If STEP01 failed: investigate catalog or VSAM allocation issue; resolve and re-run entire job
3. If STEP02 failed: verify all five input VSAM files are available and accessible; check disk space for EXPORT.DATA; re-run from STEP01 (DEFINE requires empty cluster)
4. After successful completion, verify record counts in SYSOUT match expected totals from source files

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Data Factory export pipeline / Azure SQL Database Export
**Trigger:** Manual (on-demand) or Azure Data Factory pipeline trigger
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL CBEXPORT job | Azure Data Factory pipeline `DataExportPipeline` | On-demand data extraction |
| IDCAMS DELETE/DEFINE | Azure Data Factory: drop/recreate staging table or Blob container | Idempotent export target preparation |
| CBEXPORT.cbl | Azure Data Factory Copy Activities (one per source table) or C# `DataExportService` | Reads from Azure SQL, writes to staging |
| CUSTDATA.VSAM.KSDS | Azure SQL `Customers` table | Customer master data source |
| ACCTDATA.VSAM.KSDS | Azure SQL `Accounts` table | Account master data source |
| CARDXREF.VSAM.KSDS | Azure SQL `CardCrossReference` table | Cross-reference data source |
| TRANSACT.VSAM.KSDS | Azure SQL `Transactions` table | Transaction master data source |
| CARDDATA.VSAM.KSDS | Azure SQL `Cards` table | Card master data source |
| EXPORT.DATA VSAM | Azure Blob Storage container `data-exports` (Parquet/CSV) or Azure SQL staging database | Consolidated export output |

### Parallel-Run Considerations

- **Comparison strategy:** Record count comparison and field-by-field validation between VSAM export output and Azure Data Factory export output
- **Tolerance:** Exact match required for all fields (no calculation involved, pure data extraction)
- **Comparison frequency:** After each export run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| ACCT-BR-003 | GDPR Art. 5(1)(f) | Customer data export must ensure integrity and confidentiality; exported data must be protected against unauthorized access during transfer |
| ACCT-BR-004 | GDPR Art. 44-49 | Data export must not transfer personal data outside the EU/EEA without appropriate safeguards |
| ACCT-BR-005 | FFFS 2014:5 Ch. 3 | Account and customer master data must remain accurate and complete during export and transfer operations |
| ACCT-BR-006 | DORA Art. 11 | Data export processes must be documented as part of ICT business continuity and disaster recovery procedures |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
