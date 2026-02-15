---
id: "PROC-READACCT-001"
title: "Read Account File"
sidebar_position: 32
process_type: batch
domain: account-management
jcl_source: "READACCT.jcl"
cobol_programs:
  - "CBACT01C.cbl"
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure Function for data extraction / Azure SQL query export
---

# Read Account File

## Overview

The Read Account File batch process is a diagnostic and data extraction job that reads all records from the Account master VSAM KSDS cluster and writes them to three different output file formats. This enables verification of account data integrity and provides data extracts in multiple record formats for downstream consumption or analysis.

The job first deletes any previous output files from prior runs, then executes the CBACT01C COBOL program to read the entire account VSAM file sequentially. Each account record is reformatted and written to three output datasets: a compressed fixed-block format, an array format with five balance occurrences per record, and a variable-length format. This multi-format output supports different downstream consumers that require account data in specific layouts.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand (verification / data extraction)
**Business owner:** Account Management Team / Operations

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (PREDEL) | IEFBR14 | Deletes three previous output files (ACCTDATA.PSCOMP, ACCTDATA.ARRYPS, ACCTDATA.VBPS) to ensure clean output from the current run | N/A | Previous output files deleted | Tolerates file-not-found (files may not exist from a prior run) |
| 2 (STEP05) | CBACT01C.cbl | Reads all records from ACCTDATA.VSAM.KSDS sequentially and writes each record in three output formats: compressed fixed-block, array with balance occurrences, and variable-length | ACCTDATA.VSAM.KSDS | ACCTDATA.PSCOMP (LRECL=107, FB), ACCTDATA.ARRYPS (LRECL=110, FB), ACCTDATA.VBPS (LRECL=84, VB) | Abend on VSAM read error or output file allocation failure |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| ACCTDATA.VSAM.KSDS | Data | Account master VSAM file must be available and populated |
| PROC-ACCTFILE-001 | Process | Account file must have been defined and loaded before this job can read it |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Data verification | Process | Operations team uses output files to verify account data integrity |
| Data analysis | Process | Output files in multiple formats can be consumed by reporting or analysis tools |
| Migration validation | Process | During parallel-run, output files are compared against Azure SQL extracts |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on account data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Account Management Operations | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| ACCTDATA.VSAM.KSDS | VSAM KSDS | KEYS(11 0), RECORDSIZE(300 300), INDEXED | Account management | Account master data VSAM cluster |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| ACCTDATA.PSCOMP | Physical Sequential (PS) | LRECL=107, FB (fixed-block) | Verification / analysis | Compressed format account records with key fields extracted |
| ACCTDATA.ARRYPS | Physical Sequential (PS) | LRECL=110, FB (fixed-block) | Verification / analysis | Array format account records with 5 balance occurrences per record |
| ACCTDATA.VBPS | Physical Sequential (PS) | LRECL=84, VB (variable-block) | Verification / analysis | Variable-length format account records with variable-size balance section |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Previous file deletion failure | IEFBR14 cannot delete prior output files | Tolerated if files do not exist; abort if allocation error | Check file catalog; manually delete stale files and re-run |
| VSAM file unavailable | ACCTDATA.VSAM.KSDS not available or in use by CICS | Abort STEP05 | Verify VSAM file availability; ensure CICS files are closed if needed; re-run |
| VSAM read error | I/O error reading account records | Abort STEP05 | Investigate VSAM health; run IDCAMS VERIFY on cluster; re-run |
| Output allocation error | Unable to allocate one or more output datasets | Abort STEP05 | Check disk space; resolve allocation issue and re-run from PREDEL |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for PREDEL and STEP05
2. If PREDEL failed: manually delete previous output files if they exist; re-run entire job
3. If STEP05 failed: verify ACCTDATA.VSAM.KSDS is available and healthy; check output disk space; re-run from PREDEL
4. After successful completion, verify record counts in output files match account count in VSAM cluster

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Function for data extraction / Azure SQL query export
**Trigger:** Manual (on-demand) or HTTP trigger
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL READACCT job | Azure Function `AccountDataExtract` or Azure SQL query/stored procedure | On-demand data extraction |
| IEFBR14 (PREDEL) | Azure Function: delete previous Blob output or overwrite | Idempotent output preparation |
| CBACT01C.cbl | C# `AccountDataExtractService` or Azure SQL `SELECT` with formatting | Reads accounts, produces multiple output formats |
| ACCTDATA.VSAM.KSDS | Azure SQL `Accounts` table | Account master data source |
| ACCTDATA.PSCOMP | Azure Blob Storage (CSV/fixed-width, compressed format) | Compressed format extract |
| ACCTDATA.ARRYPS | Azure Blob Storage (CSV/JSON, array format) | Array format with balance occurrences |
| ACCTDATA.VBPS | Azure Blob Storage (CSV, variable-length format) | Variable-length format extract |

### Parallel-Run Considerations

- **Comparison strategy:** Record count comparison and field-by-field validation of all three output formats between mainframe files and Azure exports
- **Tolerance:** Exact match required for all fields (pure data extraction, no calculation)
- **Comparison frequency:** After each extraction run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| ACCT-BR-011 | FFFS 2014:5 Ch. 3 | Account data extracts must accurately reflect the current state of account master data |
| ACCT-BR-012 | FFFS 2014:5 Ch. 8 | Data verification procedures must be in place to ensure accuracy of financial records |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
