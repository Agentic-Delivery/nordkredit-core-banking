---
id: "PROC-TRANCATG-001"
title: "Transaction Category File Definition/Loading"
sidebar_position: 17
process_type: batch
domain: transactions
jcl_source: "TRANCATG.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database reference table
---

# Transaction Category File Definition/Loading

## Overview

Defines and loads the Transaction Category reference VSAM KSDS cluster. This is a data file management job that deletes any existing cluster, redefines it with the required key and record structure, and bulk-loads transaction category reference data from a flat file into the newly defined VSAM dataset.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The transaction category file is a reference data table used to categorize transactions (e.g., purchases, payments, cash advances, fees) for reporting, billing, and regulatory classification purposes.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Transaction Processing / Business Analysis Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (DELETE) | IDCAMS DELETE | Delete existing transaction category VSAM KSDS cluster | N/A | Cluster removed (or no-op if absent) | MAXCC tolerated for not-found conditions |
| 2 (DEFINE) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(6 0) and RECORDSIZE(60 60) | N/A | Empty VSAM KSDS cluster for transaction categories | Abort on catalog or allocation failure |
| 3 (REPRO) | IDCAMS REPRO | Bulk-load transaction category records from TRANCATG.PS flat file into the VSAM KSDS | TRANCATG.PS (flat file) | Populated transaction category VSAM KSDS | Abort on format mismatch or key sequence error |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| TRANCATG.PS flat file | Data | Physical sequential file containing transaction category reference records must be available |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS transaction processing | System | Online transaction categorization uses this reference file |
| PROC-TCATBALF-001 | Process | Transaction category balance file references categories defined here |
| Transaction reporting batch jobs | Process | Report generation uses category descriptions for readable output |
| Billing batch processes | Process | Billing jobs reference transaction categories for rate determination |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Seconds (small reference dataset) | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Transaction Processing team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TRANCATG.PS | Physical Sequential (PS) | Fixed-length 60-byte records | Data migration / reference data management | Flat file containing transaction category reference records |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Transaction category VSAM KSDS | VSAM KSDS | KEYS(6 0), RECORDSIZE(60 60) | CICS online, batch programs | Transaction category reference data keyed on 6-byte category code at offset 0 |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Cluster not found on DELETE | IDCAMS DELETE returns entry-not-found | Tolerated; proceed to DEFINE | No recovery needed |
| Catalog error on DEFINE | Duplicate entry or space unavailable | Abort job step | Resolve catalog issue; re-run from DELETE step |
| Data load error on REPRO | Record format mismatch, key sequence error, or input file unavailable | Abort job step | Verify input file format and availability; re-run from DELETE step |
| Duplicate key on REPRO | Input file contains duplicate category codes | Abort REPRO step | Deduplicate input file; re-run from DELETE step |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step
2. If any step failed: resolve the root cause
3. Re-run entire job from the beginning (DELETE through REPRO) since VSAM cluster definition is atomic
4. Verify record count after successful load matches expected number of categories

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database reference table
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL TRANCATG job | EF Core migration + seed script | One-time setup; reference data loaded via HasData() in EF Core configuration |
| IDCAMS DELETE cluster | EF Core migration: `DROP TABLE IF EXISTS` or `TRUNCATE TABLE` | Idempotent table recreation |
| IDCAMS DEFINE CLUSTER (KEYS(6 0)) | EF Core migration: `CREATE TABLE` with primary key on CategoryCode (6-char) | VSAM KSDS maps to SQL reference table with clustered primary key |
| IDCAMS REPRO from flat file | EF Core seed data (`HasData()`) or Azure Data Factory | Reference data can be embedded in migration for small datasets |
| TRANCATG.PS flat file | Azure Blob Storage (CSV/fixed-width) or embedded in EF Core seed | EBCDIC-to-Unicode conversion during migration |
| Transaction category VSAM KSDS | Azure SQL Database `TransactionCategories` reference table | RECORDSIZE(60 60) maps to table columns per copybook layout |

### Parallel-Run Considerations

- **Comparison strategy:** Full record comparison between VSAM and Azure SQL (small reference dataset)
- **Tolerance:** Exact match required for all fields
- **Comparison frequency:** After each reload during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| PAY-BR-020 | FFFS 2014:5 Ch. 5 | Transaction categorization must be consistent for regulatory reporting |
| PAY-BR-021 | PSD2 Art. 58 | Transaction types must be accurately classified for payment service reporting |
