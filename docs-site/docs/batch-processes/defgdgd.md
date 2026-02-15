---
id: "PROC-DEFGDGD-001"
title: "Define GDG Data (Reference Tables)"
sidebar_position: 25
process_type: batch
domain: transactions
jcl_source: "DEFGDGD.jcl"
cobol_programs: []
schedule: "on-demand (initial setup)"
sla: "N/A"
status: extracted
target_implementation: Azure SQL Database
---

# Define GDG Data (Reference Tables)

## Overview

The Define GDG Data job creates GDG base entries for reference data and populates their first generation (+1) from flat file (PS) sources. This is an initial setup job that establishes the versioned reference data infrastructure for transaction type codes, transaction category backup data, and discount group backup data.

The job follows a paired pattern for each reference data set: first define the GDG base (IDCAMS DEFINE GDG), then copy the initial data from a sequential (PS) file into the first generation (IEBGENER REPRO to GDG(+1)). The COND=(0,NE) parameter on data copy steps ensures that if the GDG definition step fails, the corresponding data copy step is skipped.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS/IEBGENER utilities)
**Frequency:** On-demand (initial setup, one-time)
**Business owner:** Transaction Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (Step 10) | IDCAMS | DEFINE GDG base for TRANTYPE (transaction type reference, LIMIT(5)) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 2 (Step 20) | IEBGENER | Copy initial transaction type data from TRANTYPE.PS to TRANTYPE GDG(+1) | TRANTYPE.PS (sequential file) | TRANTYPE GDG generation 1 | Standard IEBGENER error handling |
| 3 (Step 30) | IDCAMS | DEFINE GDG base for TRANCATG.PS.BKUP (transaction category backup, LIMIT(5)) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 4 (Step 40) | IEBGENER | Copy initial transaction category data to TRANCATG.PS.BKUP GDG(+1) | TRANCATG.PS (sequential file) | TRANCATG.PS.BKUP GDG generation 1 | COND=(0,NE) - skips if prior step failed |
| 5 (Step 50) | IDCAMS | DEFINE GDG base for DISCGRP.BKUP (discount group backup, LIMIT(5)) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 6 (Step 60) | IEBGENER | Copy initial discount group data to DISCGRP.BKUP GDG(+1) | DISCGRP.PS (sequential file) | DISCGRP.BKUP GDG generation 1 | COND=(0,NE) - skips if prior step failed |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| DEFGDGB | Process | GDG bases for transaction data must be defined (if overlapping GDG names) |
| Reference data flat files | Data | TRANTYPE.PS, TRANCATG.PS, and DISCGRP.PS must exist with initial reference data |
| System catalog | Infrastructure | ICF catalog must be available and accessible |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| POSTTRAN | Process | Transaction posting uses TRANTYPE reference data for transaction validation |
| Transaction reporting | Process | Reports reference transaction type and category codes |
| Billing processes | Process | Discount group data used in billing calculations |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand (initial setup) | Environment provisioning |
| Completion deadline | N/A (one-time setup utility) | N/A |
| Maximum duration | Less than 5 minutes | Operational expectation |
| Retry policy | Re-run (GDG definitions are idempotent; IEBGENER creates new generation) | Operational runbook |
| Escalation | Systems programmer if catalog or data errors occur | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TRANTYPE.PS | Sequential (PS) | Fixed-length records | Reference data setup | Transaction type code reference data (type codes, descriptions, processing rules) |
| TRANCATG.PS | Sequential (PS) | Fixed-length records | Reference data setup | Transaction category reference data |
| DISCGRP.PS | Sequential (PS) | Fixed-length records | Reference data setup | Discount group reference data |
| IDCAMS control statements | In-stream (SYSIN) | IDCAMS DEFINE GDG commands | JCL | Three DEFINE GDG commands with LIMIT(5) |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANTYPE GDG (gen 1) | GDG generation | Sequential, same as input PS | Transaction processing, reporting | First generation of transaction type reference data |
| TRANCATG.PS.BKUP GDG (gen 1) | GDG generation | Sequential, same as input PS | Backup/recovery | First generation of transaction category backup data |
| DISCGRP.BKUP GDG (gen 1) | GDG generation | Sequential, same as input PS | Backup/recovery, billing | First generation of discount group backup data |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Expected condition | GDG base already exists (LASTCC=12) | SET MAXCC=0 and continue | No action needed; subsequent IEBGENER creates new generation |
| Data error | Source PS file missing or empty | IEBGENER fails | Verify source file exists and contains data, re-run |
| System error | Catalog error on GDG DEFINE | Step fails | Verify catalog health, re-run |
| Conditional skip | COND=(0,NE) triggered | IEBGENER step skipped | Fix preceding IDCAMS step, re-run |

### Restart/Recovery Procedure

1. Check return codes for each step pair (IDCAMS + IEBGENER)
2. GDG DEFINE steps with RC=12 set to MAXCC=0 are normal (already exists)
3. IEBGENER steps skipped due to COND=(0,NE) indicate the preceding DEFINE failed
4. Fix any errors and re-run (DEFINE is idempotent; IEBGENER creates a new generation each run)
5. Note: re-running after partial success will create additional GDG generations for already-completed pairs

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL Database
**Trigger:** N/A (one-time setup via EF Core migrations and seed data)
**CRON expression:** N/A

In the Azure target, GDG-based reference data storage is replaced by Azure SQL Database reference tables. Initial data is seeded through EF Core migrations, which provide version-controlled, repeatable data setup. The GDG versioning concept is replaced by database change tracking and temporal tables for audit history.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| TRANTYPE GDG | Azure SQL `TransactionTypes` table | Reference data with EF Core seed data |
| TRANCATG.PS.BKUP GDG | Azure SQL `TransactionCategories` table + backup via Azure SQL automated backups | Backup handled by Azure SQL PITR |
| DISCGRP.BKUP GDG | Azure SQL `DiscountGroups` table + backup via Azure SQL automated backups | Backup handled by Azure SQL PITR |
| IDCAMS DEFINE GDG | EF Core migration (CreateTable + seed) | Schema and initial data managed through migrations |
| IEBGENER copy to GDG(+1) | EF Core `HasData()` seed method | Initial data populated during migration |
| GDG LIMIT(5) | Azure SQL automated backups (7-35 day retention) | Point-in-time restore replaces generation-based recovery |

### Parallel-Run Considerations

- **Comparison strategy:** Reference data comparison between mainframe GDG current generation and Azure SQL table contents
- **Tolerance:** Exact match required for reference data
- **Comparison frequency:** After initial setup and after any reference data updates
- EBCDIC-to-Unicode conversion must be validated for all text fields in reference data

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | FFFS 2014:5 Ch. 6 | Transaction type and category reference data supports orderly processing of payment transactions |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
