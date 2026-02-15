---
id: "PROC-READCARD-001"
title: "Read Card File"
sidebar_position: 33
process_type: batch
domain: card-management
jcl_source: "READCARD.jcl"
cobol_programs:
  - "CBACT02C.cbl"
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure SQL query / Application Insights diagnostic query
---

# Read Card File

## Overview

The Read Card File batch process is a simple diagnostic and verification job that reads all records from the Card master VSAM KSDS cluster and writes them to the console (SYSOUT). It provides a complete dump of all card records for operations staff to verify card data integrity, troubleshoot data issues, or confirm that the card master file has been correctly loaded.

This is a read-only utility job with no data modifications. The CBACT02C COBOL program reads the CARDDATA VSAM file sequentially from beginning to end, formatting and displaying each card record to SYSOUT. It is typically run after card file loading, during incident investigation, or as part of environment verification.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand (diagnostic / verification)
**Business owner:** Card Management Team / Operations

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | CBACT02C.cbl | Reads all records from CARDDATA.VSAM.KSDS sequentially and displays each card record to SYSOUT for verification | CARDDATA.VSAM.KSDS | SYSOUT (console dump of all card records) | Abend on VSAM read error or file unavailable |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CARDDATA.VSAM.KSDS | Data | Card master VSAM file must be available and populated |
| Card file loading job | Process | Card file must have been defined and loaded before this job can read it |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Operations verification | Manual | Operations staff review SYSOUT for card data integrity |
| Incident investigation | Manual | Used to diagnose card data issues during incident response |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on card data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Card Management Operations | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CARDDATA.VSAM.KSDS | VSAM KSDS | Card record layout | Card management | Card master data VSAM cluster containing all card records |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| SYSOUT | Console output | Formatted card record display | Operations staff | Complete dump of all card records for visual verification |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| VSAM file unavailable | CARDDATA.VSAM.KSDS not available or in use by CICS | Abort job | Verify VSAM file availability; ensure CICS files are closed if needed; re-run |
| VSAM read error | I/O error reading card records | Abort job | Investigate VSAM health; run IDCAMS VERIFY on cluster; re-run |
| Empty file | CARDDATA.VSAM.KSDS contains no records | Job completes with zero records displayed | Verify card file has been loaded; no recovery needed if intentionally empty |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool
2. If job abended: verify CARDDATA.VSAM.KSDS is available and healthy
3. Re-run entire job (read-only, no restart considerations)
4. Review SYSOUT for expected card records

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL query / Application Insights diagnostic query
**Trigger:** Manual (on-demand) or HTTP-triggered Azure Function
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL READCARD job | Azure SQL query `SELECT * FROM Cards` or Azure Function `CardDataDiagnostic` | On-demand diagnostic |
| CBACT02C.cbl | SQL query or C# `CardDiagnosticService` | Simple sequential read and display |
| CARDDATA.VSAM.KSDS | Azure SQL `Cards` table | Card master data source |
| SYSOUT (console dump) | Application Insights custom query / Azure Portal query results | Diagnostic output viewable in Azure Portal |

### Parallel-Run Considerations

- **Comparison strategy:** Record count comparison between SYSOUT record count and Azure SQL `SELECT COUNT(*)` from Cards table
- **Tolerance:** Exact match required (pure data read, no calculation)
- **Comparison frequency:** As needed during parallel-run period for verification
- **Comparison tool:** Manual comparison or `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| CARD-BR-001 | FFFS 2014:5 Ch. 3 | Card master data verification must be available to ensure data integrity across banking operations |
| CARD-BR-002 | PSD2 Art. 95 | Payment instrument data (cards) must be subject to operational and security measures including data verification |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
