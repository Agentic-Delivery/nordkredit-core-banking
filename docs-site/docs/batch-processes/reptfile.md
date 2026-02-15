---
id: "PROC-REPTFILE-001"
title: "Report File GDG Definition"
sidebar_position: 15
process_type: batch
domain: reporting
jcl_source: "REPTFILE.jcl"
cobol_programs: []
schedule: "on-demand (initial setup)"
sla: "N/A"
status: extracted
target_implementation: Azure Blob Storage container with retention policy
---

# Report File GDG Definition

## Overview

Defines the Generation Data Group (GDG) base entry for transaction report output files. This is a one-time setup job that creates the GDG definition used by transaction reporting batch processes to manage versioned report output. The GDG allows up to 10 generations of the TRANREPT report file to be retained, with older generations automatically managed by the system.

This is an initialization job, not a recurring batch process. It is executed once during initial environment setup to establish the GDG base entry. Once defined, report-generating batch jobs create new generations automatically. The GDG provides a simple versioning mechanism for sequential report files, ensuring that the last 10 report runs are retained for operational review and audit purposes.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IDCAMS utility)
**Frequency:** On-demand (one-time initial setup)
**Business owner:** Reporting Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | IDCAMS DEFINE GDG | Define Generation Data Group base for TRANREPT with LIMIT(10) | N/A | GDG base entry in ICF catalog allowing up to 10 generations | Abort if GDG base already exists or catalog error |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| ICF catalog | System | ICF catalog must be available for GDG base definition |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Transaction report batch jobs | Process | Report generation jobs create new GDG generations under this base entry |
| Operations review | System | Operational staff access report generations for review and troubleshooting |
| Audit retention | Process | Report generations provide audit trail of report outputs |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand (one-time) | Manual trigger by operations during environment setup |
| Completion deadline | N/A (one-time setup) | N/A |
| Maximum duration | Seconds (single catalog operation) | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Infrastructure Operations team | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| N/A | N/A | N/A | N/A | No input files; this is a catalog-only operation |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANREPT GDG base | GDG base entry | LIMIT(10) | Report generation batch jobs | Generation Data Group base allowing up to 10 versioned report output files |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| GDG already exists | IDCAMS DEFINE returns duplicate entry error | Abort job step | Verify existing GDG base is correct; no action needed if already defined |
| Catalog error | ICF catalog unavailable or full | Abort job step | Resolve catalog issue; retry |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool
2. If failed due to existing GDG: verify GDG base exists with correct LIMIT(10) using IDCAMS LISTCAT
3. If failed due to catalog error: resolve catalog issue and re-run
4. This is an idempotent operation if the GDG already exists with correct attributes

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Blob Storage container with retention policy
**Trigger:** Manual (one-time setup) or Bicep/ARM template deployment
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL REPTFILE job | Bicep template or Azure CLI script | One-time infrastructure setup |
| IDCAMS DEFINE GDG (LIMIT 10) | Azure Blob Storage container with lifecycle management policy | Retention policy replaces GDG generation limit |
| GDG base entry (TRANREPT) | Azure Blob Storage container `tranrept` | Container name maps to GDG base |
| GDG generations (G0001V00, etc.) | Azure Blob Storage blobs with timestamp-based naming | Blob naming convention: `tranrept/YYYY-MM-DD-HHmmss.csv` |
| LIMIT(10) | Azure Blob lifecycle rule: delete after 10 versions or N days | Lifecycle management policy enforces retention |

### Parallel-Run Considerations

- **Comparison strategy:** Verify that report outputs generated on Azure match mainframe GDG generation content
- **Tolerance:** Exact match for report content; formatting differences acceptable if content is equivalent
- **Comparison frequency:** After each report generation during parallel-run period

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| RPT-BR-001 | FFFS 2014:5 Ch. 16 | Report output retention must meet regulatory requirements |
| RPT-BR-002 | Bokforingslagen (1999:1078) | Financial reports must be retained per Swedish bookkeeping law (7 years) |
