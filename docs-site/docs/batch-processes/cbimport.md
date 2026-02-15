---
id: "PROC-CBIMPORT-001"
title: "Data Import"
sidebar_position: 31
process_type: batch
domain: account-management
jcl_source: "CBIMPORT.jcl"
cobol_programs:
  - "CBIMPORT.cbl"
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure Data Factory import pipeline / Bulk insert to Azure SQL
---

# Data Import

## Overview

The Data Import batch process reads the consolidated multi-record VSAM export file produced by the CBEXPORT process and splits it into separate normalized flat files for the target system. Each record type in the export file (customer, account, cross-reference, transaction) is identified and written to its corresponding output flat file. Records that cannot be parsed or have invalid record type indicators are written to an error file for operations review.

This process is the counterpart to CBEXPORT and together they form the data migration pipeline used for branch migration, environment provisioning, or data transfer between systems. The import job normalizes the consolidated export into individual files that can be independently loaded into the target system's data stores.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand (data import / migration)
**Business owner:** Account Management / Data Migration Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | CBIMPORT.cbl | Reads multi-record EXPORT.DATA VSAM file, identifies record types, and writes each record to the appropriate normalized output flat file. Invalid or unrecognized records are written to the error file | EXPORT.DATA VSAM | CUSTDATA.IMPORT, ACCTDATA.IMPORT, CARDXREF.IMPORT, TRANSACT.IMPORT, IMPORT.ERRORS | Invalid records written to IMPORT.ERRORS with error description; abend on system I/O errors |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-CBEXPORT-001 | Process | The EXPORT.DATA VSAM file must be produced by the CBEXPORT process before import can run |
| EXPORT.DATA VSAM | Data | Consolidated multi-record export file must be available and intact |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Target system data load | Process | Normalized flat files are consumed by the target system's bulk load process |
| Data validation | Process | Operations team reviews IMPORT.ERRORS file and validates record counts against export totals |
| Azure Data Factory import | Process | During migration, output files feed the Azure SQL bulk insert pipeline |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand (after CBEXPORT completes) | Manual trigger by operations or migration team |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on export file volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Data Migration team / Infrastructure Operations | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| EXPORT.DATA | VSAM KSDS | KEYS(4 28), RECORDSIZE(500 500), INDEXED | CBEXPORT process | Consolidated multi-record export file containing customer, account, cross-reference, and transaction records |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| CUSTDATA.IMPORT | Physical Sequential (PS) | LRECL=500, Fixed-length | Target system bulk load | Normalized customer records extracted from export file |
| ACCTDATA.IMPORT | Physical Sequential (PS) | LRECL=300, Fixed-length | Target system bulk load | Normalized account records extracted from export file |
| CARDXREF.IMPORT | Physical Sequential (PS) | LRECL=50, Fixed-length | Target system bulk load | Normalized card-to-account cross-reference records extracted from export file |
| TRANSACT.IMPORT | Physical Sequential (PS) | LRECL=350, Fixed-length | Target system bulk load | Normalized transaction records extracted from export file |
| IMPORT.ERRORS | Physical Sequential (PS) | LRECL=132, Fixed-length | Operations review | Records that could not be parsed or had invalid record type indicators |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Invalid record type | Record type indicator not recognized in export file | Write record to IMPORT.ERRORS with description | Operations reviews error file; may indicate corrupt export or format change |
| Record parse error | Record cannot be parsed according to expected layout | Write record to IMPORT.ERRORS with description | Verify export file integrity; re-run CBEXPORT if source data is suspect |
| Export file unavailable | EXPORT.DATA VSAM file not found or inaccessible | Abort job | Verify CBEXPORT completed successfully; ensure file is not in use |
| Output file allocation error | Unable to allocate one or more output flat files | Abort job | Check disk space and file catalog; resolve allocation issue and re-run |
| I/O error | System-level read or write error | Abort job | Investigate system error; verify disk and VSAM health; re-run |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool
2. Review IMPORT.ERRORS file for records that failed parsing
3. Compare record counts in each output file against expected totals from CBEXPORT SYSOUT
4. If job abended: verify EXPORT.DATA file is available and intact; check disk space for output files; re-run entire job
5. If excessive errors in IMPORT.ERRORS: investigate export file integrity; consider re-running CBEXPORT before re-running CBIMPORT

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Data Factory import pipeline / Bulk insert to Azure SQL
**Trigger:** Manual (on-demand) or Azure Data Factory pipeline trigger (chained after export pipeline)
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL CBIMPORT job | Azure Data Factory pipeline `DataImportPipeline` | On-demand data import and normalization |
| CBIMPORT.cbl | Azure Data Factory Data Flow (split by record type) or C# `DataImportService` | Splits consolidated export into normalized tables |
| EXPORT.DATA VSAM | Azure Blob Storage container `data-exports` (source) | Consolidated export file as Parquet/CSV |
| CUSTDATA.IMPORT | Azure SQL `Customers` table (bulk insert) | Direct insert into target table |
| ACCTDATA.IMPORT | Azure SQL `Accounts` table (bulk insert) | Direct insert into target table |
| CARDXREF.IMPORT | Azure SQL `CardCrossReference` table (bulk insert) | Direct insert into target table |
| TRANSACT.IMPORT | Azure SQL `Transactions` table (bulk insert) | Direct insert into target table |
| IMPORT.ERRORS | Azure Blob Storage container `import-errors` + Application Insights alert | Error records stored for review; alerts on error threshold |

### Parallel-Run Considerations

- **Comparison strategy:** Record count comparison per output file and field-by-field validation between mainframe flat file output and Azure SQL table contents
- **Tolerance:** Exact match required for all fields (no calculation involved, pure data splitting and normalization)
- **Comparison frequency:** After each import run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| ACCT-BR-007 | GDPR Art. 5(1)(f) | Customer data import must ensure integrity and confidentiality; imported data must be validated for completeness |
| ACCT-BR-008 | GDPR Art. 5(1)(d) | Imported customer and account data must be accurate; error records must be reviewed and reconciled |
| ACCT-BR-009 | FFFS 2014:5 Ch. 3 | Account data imports must maintain data integrity and completeness for banking operations |
| ACCT-BR-010 | DORA Art. 11 | Data import processes must be documented as part of ICT business continuity procedures |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
