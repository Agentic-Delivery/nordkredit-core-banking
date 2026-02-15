---
id: "PROC-DISCGRP-001"
title: "Disclosure Group File Definition/Loading"
sidebar_position: 19
process_type: batch
domain: billing
jcl_source: "DISCGRP.jcl"
cobol_programs: []
schedule: "on-demand (initial setup/reload)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database reference table
---

# Disclosure Group File Definition/Loading

## Overview

Defines and loads the Disclosure Group reference VSAM KSDS cluster. This is a data file management job that deletes any existing cluster (with MAXCC override), redefines it with the required key and record structure including the ERASE attribute for security, and bulk-loads disclosure group data from a flat file into the newly defined VSAM dataset.

This is an initialization job, not a recurring batch process. It is executed on-demand for initial environment setup, disaster recovery reload, or test environment provisioning. The disclosure group file contains interest rate disclosure groups used by the interest calculation process (INTCALC) to determine which interest rate disclosure rules apply to different account/product groupings.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup/reload)
**Business owner:** Billing / Interest Rate Management Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (DELETE) | IDCAMS DELETE | Delete existing disclosure group VSAM KSDS cluster with SET MAXCC = 0 override | N/A | Cluster removed (or no-op if absent); MAXCC forced to 0 | SET MAXCC = 0 ensures job continues regardless of delete outcome |
| 2 (DEFINE) | IDCAMS DEFINE CLUSTER | Define new VSAM KSDS cluster with KEYS(16 0), RECORDSIZE(50 50), and ERASE attribute | N/A | Empty VSAM KSDS cluster for disclosure groups with secure erase | Abort on catalog or allocation failure |
| 3 (REPRO) | IDCAMS REPRO | Bulk-load disclosure group records from DISCGRP.PS flat file into the VSAM KSDS | DISCGRP.PS (flat file) | Populated disclosure group VSAM KSDS | Abort on format mismatch or key sequence error |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| DISCGRP.PS flat file | Data | Physical sequential file containing disclosure group reference records must be available |
| Interest rate management | Process | Disclosure groups must be defined according to current interest rate policy |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| INTCALC (interest calculation) | Process | Nightly interest calculation uses disclosure groups to determine applicable rates |
| Billing batch processes | Process | Billing jobs reference disclosure groups for interest rate determination |
| Customer statement generation | Process | Statements display interest rates based on disclosure group assignments |
| Regulatory reporting | Process | Interest rate disclosures are subject to consumer protection regulations |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Seconds (small reference dataset) | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Billing Operations / Interest Rate Management team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| DISCGRP.PS | Physical Sequential (PS) | Fixed-length 50-byte records | Interest rate management / data migration | Flat file containing disclosure group reference records |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| Disclosure group VSAM KSDS | VSAM KSDS | KEYS(16 0), RECORDSIZE(50 50), ERASE | INTCALC, billing batch, CICS online | Disclosure group reference data keyed on 16-byte group identifier at offset 0; ERASE attribute ensures data is overwritten on deletion |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Cluster not found on DELETE | IDCAMS DELETE returns entry-not-found | Tolerated; SET MAXCC = 0 forces continuation | No recovery needed |
| Any DELETE error | Any IDCAMS DELETE error condition | Tolerated; SET MAXCC = 0 forces continuation | No recovery needed; DEFINE step will fail if cluster still exists |
| Catalog error on DEFINE | Duplicate entry or space unavailable | Abort job step | Resolve catalog issue (manual DELETE if needed); re-run from DELETE step |
| Data load error on REPRO | Record format mismatch, key sequence error, or input file unavailable | Abort job step | Verify input file format and availability; re-run from DELETE step |
| Duplicate key on REPRO | Input file contains duplicate group identifiers | Abort REPRO step | Deduplicate input file; re-run from DELETE step |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool for each step
2. Note: DELETE step always returns MAXCC = 0 due to SET MAXCC override; check DEFINE and REPRO steps for actual errors
3. If DEFINE or REPRO failed: resolve the root cause
4. Re-run entire job from the beginning (DELETE through REPRO) since VSAM cluster definition is atomic
5. Verify record count after successful load matches expected number of disclosure groups

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database reference table
**Trigger:** Manual (on-demand) or EF Core migration
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL DISCGRP job | EF Core migration + seed script | One-time setup; reference data loaded via HasData() in EF Core configuration |
| IDCAMS DELETE (SET MAXCC=0) | EF Core migration: `DROP TABLE IF EXISTS` or `TRUNCATE TABLE` | Idempotent table recreation; no need for MAXCC logic in SQL |
| IDCAMS DEFINE CLUSTER (KEYS(16 0), ERASE) | EF Core migration: `CREATE TABLE` with primary key on GroupId (16-char) | VSAM KSDS maps to SQL reference table; ERASE attribute handled by Azure SQL TDE (Transparent Data Encryption) |
| IDCAMS REPRO from flat file | EF Core seed data (`HasData()`) or Azure Data Factory | Reference data can be embedded in migration for small datasets |
| DISCGRP.PS flat file | Azure Blob Storage (CSV/fixed-width) or embedded in EF Core seed | EBCDIC-to-Unicode conversion during migration |
| Disclosure group VSAM KSDS | Azure SQL Database `DisclosureGroups` reference table | RECORDSIZE(50 50) maps to table columns per copybook layout |

### Parallel-Run Considerations

- **Comparison strategy:** Full record comparison between VSAM and Azure SQL (small reference dataset)
- **Tolerance:** Exact match required for all fields (interest rate data must be precise)
- **Comparison frequency:** After each reload during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| BILL-BR-010 | FFFS 2014:5 Ch. 16 | Interest rate disclosures must be accurate and current |
| BILL-BR-011 | Konsumentkreditlagen (2010:1846) | Consumer interest rate information must comply with Swedish consumer credit law |
| BILL-BR-012 | PSD2 Art. 52 | Payment service charges and interest must be transparently disclosed |
