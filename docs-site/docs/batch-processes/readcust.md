---
id: "PROC-READCUST-001"
title: "Read Customer File"
sidebar_position: 34
process_type: batch
domain: account-management
jcl_source: "READCUST.jcl"
cobol_programs:
  - "CBCUS01C.cbl"
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure SQL query / Application Insights diagnostic query
---

# Read Customer File

## Overview

The Read Customer File batch process is a diagnostic and verification job that reads all records from the Customer master VSAM KSDS cluster and writes them to the console (SYSOUT). It provides a complete dump of all customer records for operations staff to verify customer master data integrity, troubleshoot data issues, or confirm that the customer file has been correctly loaded.

This is a read-only utility job with no data modifications. The CBCUS01C COBOL program reads the CUSTDATA VSAM file sequentially from beginning to end, formatting and displaying each customer record to SYSOUT. It is typically run after customer file loading, during incident investigation, or as part of environment verification. Because this job outputs customer personal data to SYSOUT, access to the job output must be restricted to authorized personnel in accordance with GDPR requirements.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand (diagnostic / verification)
**Business owner:** Account Management Team / Operations

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | CBCUS01C.cbl | Reads all records from CUSTDATA.VSAM.KSDS sequentially and displays each customer record to SYSOUT for verification | CUSTDATA.VSAM.KSDS | SYSOUT (console dump of customer records) | Abend on VSAM read error or file unavailable |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CUSTDATA.VSAM.KSDS | Data | Customer master VSAM file must be available and populated |
| Customer file loading job | Process | Customer file must have been defined and loaded before this job can read it |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Operations verification | Manual | Operations staff review SYSOUT for customer data integrity |
| Incident investigation | Manual | Used to diagnose customer data issues during incident response |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Manual trigger by operations |
| Completion deadline | N/A (not a scheduled job) | N/A |
| Maximum duration | Dependent on customer data volume | Operational estimate |
| Retry policy | Manual retry after root cause analysis | Operational runbook |
| Escalation | Account Management Operations | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CUSTDATA.VSAM.KSDS | VSAM KSDS | Customer record layout | Account management | Customer master data VSAM cluster containing all customer records including PII |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| SYSOUT | Console output | Formatted customer record display | Authorized operations staff | Complete dump of all customer records for visual verification (contains PII) |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| VSAM file unavailable | CUSTDATA.VSAM.KSDS not available or in use by CICS | Abort job | Verify VSAM file availability; ensure CICS files are closed if needed; re-run |
| VSAM read error | I/O error reading customer records | Abort job | Investigate VSAM health; run IDCAMS VERIFY on cluster; re-run |
| Empty file | CUSTDATA.VSAM.KSDS contains no records | Job completes with zero records displayed | Verify customer file has been loaded; no recovery needed if intentionally empty |

### Restart/Recovery Procedure

1. Check job completion status in JES2 spool
2. If job abended: verify CUSTDATA.VSAM.KSDS is available and healthy
3. Re-run entire job (read-only, no restart considerations)
4. Review SYSOUT for expected customer records
5. Ensure SYSOUT access is restricted to authorized personnel (PII content)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure SQL query / Application Insights diagnostic query
**Trigger:** Manual (on-demand) or HTTP-triggered Azure Function with authorization
**CRON expression:** N/A (not scheduled)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL READCUST job | Azure SQL query `SELECT * FROM Customers` or Azure Function `CustomerDataDiagnostic` | On-demand diagnostic; requires RBAC authorization |
| CBCUS01C.cbl | SQL query or C# `CustomerDiagnosticService` | Simple sequential read and display |
| CUSTDATA.VSAM.KSDS | Azure SQL `Customers` table | Customer master data source (contains PII) |
| SYSOUT (console dump) | Application Insights custom query / Azure Portal query results | Diagnostic output; must enforce access controls for PII |

### Parallel-Run Considerations

- **Comparison strategy:** Record count comparison between SYSOUT record count and Azure SQL `SELECT COUNT(*)` from Customers table
- **Tolerance:** Exact match required (pure data read, no calculation)
- **Comparison frequency:** As needed during parallel-run period for verification
- **Comparison tool:** Manual comparison or `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| CUST-BR-001 | GDPR Art. 5(1)(f) | Customer data displayed in SYSOUT contains PII; access to job output must be restricted to authorized personnel with a legitimate purpose |
| CUST-BR-002 | GDPR Art. 25 | Diagnostic tools that display customer PII must implement data protection by design; consider masking or pseudonymization in target implementation |
| CUST-BR-003 | GDPR Art. 30 | Execution of this job constitutes processing of personal data and must be recorded in the register of processing activities |
| CUST-BR-004 | FFFS 2014:5 Ch. 3 | Customer master data verification must be available to ensure data integrity across banking operations |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
