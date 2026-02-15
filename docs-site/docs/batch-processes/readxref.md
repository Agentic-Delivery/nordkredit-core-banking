---
id: "PROC-READXREF-001"
title: "Read Cross-Reference File"
sidebar_position: 35
process_type: batch
domain: card-management
jcl_source: "READXREF.jcl"
cobol_programs:
  - "CBACT03C.cbl"
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure SQL query / Application Insights diagnostic query
---

# Read Cross-Reference File

## Overview

The Read Cross-Reference File batch process is a diagnostic and verification job that reads all records from the Card-to-Account cross-reference VSAM KSDS cluster and writes them to the console (SYSOUT). It provides a complete dump of all cross-reference records for operations staff to verify the integrity of card-to-account linkages, troubleshoot reference data issues, or confirm that the cross-reference file has been correctly loaded.

This is a read-only utility job with no data modifications. The CBACT03C COBOL program reads the CARDXREF VSAM file sequentially from beginning to end, formatting and displaying each cross-reference record to SYSOUT. The cross-reference file is a critical data linkage that maps card numbers to their associated account numbers, and its integrity is essential for transaction processing, authorization, and billing. This job is typically run after cross-reference file loading, during incident investigation, or as part of environment verification.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand (diagnostic / verification)
**Business owner:** Card Management Team / Operations

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | CBACT03C.cbl | Reads all records from CARDXREF.VSAM.KSDS sequentially and displays each cross-reference record to SYSOUT for verification | CARDXREF.VSAM.KSDS | SYSOUT (console dump of cross-reference records) | Abend on VSAM read error or file unavailable |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CARDXREF.VSAM.KSDS | Data | Card-to-account cross-reference VSAM file must be available and populated |
| Cross-reference file loading job | Process | Cross-reference file must have been defined and loaded before this job can read it |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Operations verification | Manual | Operations staff review SYSOUT for cross-reference data integrity |
| Incident investigation | Manual | Used to diagnose card-to-account linkage issues during incident response |
| Transaction posting verification | Manual | Verifies cross-reference integrity before nightly transaction posting (PROC-POSTTRAN-001) |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on cross-reference data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Card Management Operations | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CARDXREF.VSAM.KSDS | VSAM KSDS | Cross-reference record layout | Card management | Card-to-account cross-reference VSAM cluster mapping card numbers to account numbers |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| SYSOUT | Console output | Formatted cross-reference record display | Operations staff | Complete dump of all cross-reference records for visual verification |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| VSAM file unavailable | CARDXREF.VSAM.KSDS not available or in use by CICS | Abort job | Verify VSAM file availability; ensure CICS files are closed if needed; re-run |
| VSAM read error | I/O error reading cross-reference records | Abort job | Investigate VSAM health; run IDCAMS VERIFY on cluster; re-run |
| Empty file | CARDXREF.VSAM.KSDS contains no records | Job completes with zero records displayed | Verify cross-reference file has been loaded; no recovery needed if intentionally empty |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool
2. If job abended: verify CARDXREF.VSAM.KSDS is available and healthy
3. Re-run entire job (read-only, no restart considerations)
4. Review SYSOUT for expected cross-reference records
5. Validate cross-reference record count matches expected count from card and account master files

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL query / Application Insights diagnostic query
**Trigger:** Manual (on-demand) or HTTP-triggered Azure Function
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL READXREF job | Azure SQL query `SELECT * FROM CardCrossReference` or Azure Function `CrossReferenceDiagnostic` | On-demand diagnostic |
| CBACT03C.cbl | SQL query or C# `CrossReferenceDiagnosticService` | Simple sequential read and display |
| CARDXREF.VSAM.KSDS | Azure SQL `CardCrossReference` table | Cross-reference data source |
| SYSOUT (console dump) | Application Insights custom query / Azure Portal query results | Diagnostic output viewable in Azure Portal |

### Parallel-Run Considerations

- **Comparison strategy:** Record count comparison between SYSOUT record count and Azure SQL `SELECT COUNT(*)` from CardCrossReference table
- **Tolerance:** Exact match required (pure data read, no calculation)
- **Comparison frequency:** As needed during parallel-run period for verification
- **Comparison tool:** Manual comparison or `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| XREF-BR-001 | FFFS 2014:5 Ch. 3 | Cross-reference data integrity verification must be available to ensure correct card-to-account linkages across banking operations |
| XREF-BR-002 | PSD2 Art. 95 | Payment instrument reference data must be subject to operational and security measures including data verification |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
