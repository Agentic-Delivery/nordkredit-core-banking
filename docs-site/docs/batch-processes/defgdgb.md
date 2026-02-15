---
id: "PROC-DEFGDGB-001"
title: "Define GDG Bases"
sidebar_position: 24
process_type: batch
domain: transactions
jcl_source: "DEFGDGB.jcl"
cobol_programs: []
schedule: "on-demand (initial setup)"
sla: "N/A"
status: extracted
target_implementation: Azure Blob Storage
---

# Define GDG Bases

## Overview

The Define GDG Bases job creates Generation Data Group (GDG) base entries in the system catalog for six transaction-related data sets. GDGs are a mainframe concept that maintains multiple versions (generations) of a data set, allowing batch jobs to create new generations (+1) while retaining previous generations for recovery, auditing, and reporting purposes.

Each GDG base is defined with LIMIT(5) and SCRATCH, meaning a maximum of 5 generations are retained and the oldest generation is deleted when a new one exceeds the limit. Each definition step includes `IF LASTCC=12 THEN SET MAXCC=0` to handle the case where the GDG base already exists (allowing the job to be re-run safely).

This is a one-time setup job that must be run before any batch processes that create GDG generation data sets.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (initial setup, one-time)
**Business owner:** Systems Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | IDCAMS | DEFINE GDG base for TRANSACT.BKUP (transaction backup generations, LIMIT(5), SCRATCH) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 2 | IDCAMS | DEFINE GDG base for TRANSACT.DALY (daily transaction extract generations, LIMIT(5), SCRATCH) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 3 | IDCAMS | DEFINE GDG base for TRANREPT (transaction report generations, LIMIT(5), SCRATCH) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 4 | IDCAMS | DEFINE GDG base for TCATBALF.BKUP (category balance backup generations, LIMIT(5), SCRATCH) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 5 | IDCAMS | DEFINE GDG base for SYSTRAN (system-generated transaction generations, LIMIT(5), SCRATCH) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |
| 6 | IDCAMS | DEFINE GDG base for TRANSACT.COMBINED (combined transaction generations, LIMIT(5), SCRATCH) | IDCAMS control statements | GDG base catalog entry | IF LASTCC=12 THEN SET MAXCC=0 (ignore if exists) |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| System catalog | Infrastructure | ICF catalog must be available and accessible |
| Initial environment setup | Process | Part of the initial environment provisioning sequence |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| POSTTRAN | Process | Creates TRANSACT.DALY generation for daily transaction extract and rejection output |
| DEFGDGD | Process | Creates first generation data sets for reference data GDGs |
| Transaction backup jobs | Process | Create TRANSACT.BKUP generations for recovery |
| TCATBALF backup jobs | Process | Create TCATBALF.BKUP generations for category balance recovery |
| Transaction reporting | Process | Creates TRANREPT generations for transaction reports |
| Transaction combination | Process | Creates TRANSACT.COMBINED and SYSTRAN generations |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand (initial setup) | Environment provisioning |
| Completion deadline | N/A (one-time setup utility) | N/A |
| Maximum duration | Less than 1 minute | Operational expectation |
| Retry policy | Re-run (idempotent due to LASTCC=12 handling) | Operational runbook |
| Escalation | Storage administrator if catalog errors occur | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| IDCAMS control statements | In-stream (SYSIN) | IDCAMS DEFINE GDG commands | JCL | Six DEFINE GDG commands with LIMIT(5) SCRATCH |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANSACT.BKUP (GDG base) | GDG base | Catalog entry, LIMIT(5) | Transaction backup jobs | Base for transaction backup generation data sets |
| TRANSACT.DALY (GDG base) | GDG base | Catalog entry, LIMIT(5) | POSTTRAN, daily extract | Base for daily transaction extract generation data sets |
| TRANREPT (GDG base) | GDG base | Catalog entry, LIMIT(5) | Transaction reporting | Base for transaction report generation data sets |
| TCATBALF.BKUP (GDG base) | GDG base | Catalog entry, LIMIT(5) | Category balance backup | Base for category balance backup generation data sets |
| SYSTRAN (GDG base) | GDG base | Catalog entry, LIMIT(5) | System transaction jobs | Base for system-generated transaction generation data sets |
| TRANSACT.COMBINED (GDG base) | GDG base | Catalog entry, LIMIT(5) | Transaction combination | Base for combined transaction generation data sets |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Expected condition | GDG base already exists (LASTCC=12) | SET MAXCC=0 and continue | No action needed; job is idempotent |
| System error | Catalog error or catalog unavailable | Job fails | Verify catalog health, re-run |
| Configuration error | Invalid GDG name or parameters | DEFINE fails with syntax error | Correct JCL, re-run |

### Restart/Recovery Procedure

1. Check IDCAMS return codes for each step in job output
2. Steps with RC=12 that were set to MAXCC=0 indicate GDG base already existed (normal)
3. Steps with other non-zero return codes indicate actual errors
4. Fix the error condition and re-run (job is fully idempotent)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Blob Storage with lifecycle management
**Trigger:** N/A (one-time infrastructure setup via Bicep/ARM templates)
**CRON expression:** N/A

The GDG concept (versioned generations of data sets with automatic cleanup of old generations) maps to Azure Blob Storage containers with lifecycle management policies. Each GDG base becomes a blob container, and lifecycle management rules enforce the 5-generation equivalent retention by automatically deleting blobs older than the retention threshold.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| IDCAMS DEFINE GDG | Bicep template / Azure CLI | Blob containers created via infrastructure-as-code |
| TRANSACT.BKUP GDG | Blob container `transaction-backups` | Lifecycle policy retains last 5 versions |
| TRANSACT.DALY GDG | Blob container `daily-transactions` | Lifecycle policy retains last 5 versions |
| TRANREPT GDG | Blob container `transaction-reports` | Lifecycle policy retains last 5 versions |
| TCATBALF.BKUP GDG | Blob container `category-balance-backups` | Lifecycle policy retains last 5 versions |
| SYSTRAN GDG | Blob container `system-transactions` | Lifecycle policy retains last 5 versions |
| TRANSACT.COMBINED GDG | Blob container `combined-transactions` | Lifecycle policy retains last 5 versions |
| GDG LIMIT(5) SCRATCH | Azure Blob lifecycle management | Auto-delete blobs beyond retention count |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable (infrastructure setup, no business data output)
- **Tolerance:** N/A
- **Comparison frequency:** N/A
- Blob container structure validated during environment setup testing

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | FFFS 2014:5 Ch. 6 | Transaction data retention supports orderly processing and audit trail requirements; 5-generation limit must be validated against regulatory retention periods |
| N/A | DORA Art. 12 | Backup and recovery procedures for transaction data must be documented and tested |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
