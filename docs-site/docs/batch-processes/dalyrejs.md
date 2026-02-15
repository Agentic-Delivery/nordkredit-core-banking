---
id: "PROC-DALYREJS-001"
title: "Daily Rejects GDG Definition"
sidebar_position: 7
process_type: batch
domain: transactions
jcl_source: "DALYREJS.jcl"
cobol_programs: []
schedule: "on-demand (initial setup)"
sla: "N/A"
status: extracted
target_implementation: Azure Blob Storage
---

# Daily Rejects GDG Definition

## Overview

The Daily Rejects GDG Definition job is a one-time setup process that defines the Generation Data Group (GDG) base entry for the DALYREJS dataset. This GDG is used by the POSTTRAN process to store rejected transactions from each nightly batch run. The GDG is configured with a limit of 5 generations and the SCRATCH option, which means that when the limit is exceeded, the oldest generation is automatically deleted.

This is not a recurring batch job. It is executed once during initial system setup or when the GDG base needs to be recreated (for example, after a catalog recovery).

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (one-time setup)
**Frequency:** On-demand (initial setup only)
**Business owner:** Transaction Operations / System Administration

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP05) | IDCAMS | Defines a Generation Data Group base entry for DALYREJS with LIMIT(5) and SCRATCH option, enabling automatic rotation of rejection files | None | DALYREJS GDG base entry in catalog | Abort on IDCAMS failure; verify catalog |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| System catalog | Infrastructure | The VSAM catalog must be available and the GDG base name must not already exist |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| PROC-POSTTRAN-001 | Process | POSTTRAN writes rejected transactions to DALYREJS GDG (+1); the GDG base must exist |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | N/A (one-time setup) | System setup procedure |
| Completion deadline | N/A | N/A |
| Maximum duration | N/A (completes in seconds) | N/A |
| Retry policy | Re-run after resolving catalog issues | System administration |
| Escalation | N/A | N/A |

## Input/Output Files

### Input

None. This is a catalog definition job with no data input.

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| DALYREJS GDG base | GDG base entry | Catalog entry only | POSTTRAN process | GDG base with LIMIT(5) and SCRATCH, enabling up to 5 generations of rejection files |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Catalog error | GDG base already exists | IDCAMS returns non-zero RC | Delete existing GDG base first if redefinition is intended |
| Catalog error | Catalog unavailable | Abort job | Verify catalog availability, re-run |

### Restart/Recovery Procedure

1. Check IDCAMS return code in JES2 spool
2. If GDG base already exists (RC=8 or 12), determine if it needs to be redefined
3. To redefine: delete existing GDG base (and all generations) first, then re-run
4. Verify GDG base exists by listing catalog entry with IDCAMS LISTCAT

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Blob Storage (container setup)
**Trigger:** Manual / Infrastructure as Code (Bicep template)
**CRON expression:** N/A (one-time setup)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| IDCAMS DEFINE GDG | Azure Blob Storage container | Container `daily-rejections` with lifecycle management policy |
| GDG LIMIT(5) | Azure Blob Storage lifecycle rule | Retention policy to delete blobs older than 5 days (equivalent to 5 generations) |
| GDG SCRATCH | Lifecycle management auto-delete | Automatic cleanup of old rejection files |
| GDG generation naming | Blob naming convention | Date-based naming: `daily-rejections/YYYY-MM-DD-rejections.csv` |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable for this setup job. The GDG structure is validated indirectly through the POSTTRAN comparison tests.
- **Tolerance:** N/A
- **Comparison frequency:** N/A

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| TXN-BR-011 | PSD2 Art. 71 | Rejected payment transactions must be recorded and retained for notification and audit purposes |
| TXN-BR-012 | FFFS 2014:5 Ch. 6 | Payment service records, including rejections, must be maintained per regulatory retention requirements |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
