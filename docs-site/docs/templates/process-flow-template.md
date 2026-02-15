---
id: "PROC-[DOMAIN]-[NNN]"
title: "[Process Title]"
process_type: "batch | online | event"
domain: "payments | deposits | lending | account-management"
jcl_source: "[JCL member name, e.g., INTCALC.jcl]"
cobol_programs:
  - "[PROGRAM1.cbl]"
  - "[PROGRAM2.cbl]"
schedule: "[e.g., daily 02:00 CET | on-demand | event-driven]"
sla: "[e.g., Complete by 06:00 CET]"
status: "extracted | validated | implemented | tested"
target_implementation: "Azure Functions | App Service"
---

# [Process Title]

## Overview

_Brief description of what this process does, its business purpose, and why it exists. Include the business context and any regulatory drivers._

**Source system:** IBM z/OS mainframe
**Transaction type:** [IMS | CICS | JCL batch]
**Frequency:** [Daily | Monthly | On-demand | Event-driven]
**Business owner:** [Role or team]

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | [PROGRAM.cbl] | _What this step does_ | _Input files/tables_ | _Output files/tables_ | _What happens on failure_ |
| 2 | [PROGRAM.cbl] | _What this step does_ | _Input files/tables_ | _Output files/tables_ | _What happens on failure_ |
| 3 | [PROGRAM.cbl] | _What this step does_ | _Input files/tables_ | _Output files/tables_ | _What happens on failure_ |

## Dependencies

### Upstream

_Processes or data feeds that must complete before this process can run._

| Dependency | Type | Description |
|-----------|------|-------------|
| [PROC-XXX-NNN] | Process | _Must complete before this process starts_ |
| [Data feed name] | Data | _Input data that must be available_ |

### Downstream

_Processes or systems that depend on the output of this process._

| Dependent | Type | Description |
|-----------|------|-------------|
| [PROC-XXX-NNN] | Process | _Runs after this process completes_ |
| [System name] | System | _Consumes output from this process_ |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | [e.g., 02:00 CET] | _Operational schedule_ |
| Completion deadline | [e.g., 06:00 CET] | _Business SLA_ |
| Maximum duration | [e.g., 4 hours] | _Derived from start/deadline_ |
| Retry policy | [e.g., 2 retries, 15 min interval] | _Operational runbook_ |
| Escalation | [e.g., On-call after 2 failed retries] | _Incident management_ |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| [File name] | VSAM KSDS | [Record layout/copybook] | [Source system] | _Description_ |
| [Table name] | Db2 | [Table schema] | [Source] | _Description_ |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| [File name] | VSAM ESDS | [Record layout/copybook] | [Consumer system] | _Description_ |
| [Table name] | Db2 | [Table schema] | [Consumer] | _Description_ |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Data validation | _e.g., Invalid account number_ | _Log and skip record_ | _Manual correction, re-run_ |
| System error | _e.g., Db2 connection failure_ | _Abort job step_ | _Automatic retry per policy_ |
| Business rule violation | _e.g., Insufficient funds_ | _Write to exception file_ | _Business review queue_ |

### Restart/Recovery Procedure

_Describe how to restart the process after a failure. Include checkpoint/restart logic if applicable._

1. _Step 1: Check job completion status_
2. _Step 2: Identify last successful checkpoint_
3. _Step 3: Restart from checkpoint (or from beginning if no checkpoint)_

## Target Azure Implementation Notes

### Architecture

**Target service:** [Azure Functions (timer trigger) | Azure App Service]
**Trigger:** [Timer (CRON) | Service Bus message | HTTP | Event Grid]
**CRON expression:** [e.g., `0 0 2 * * *` for daily 02:00 UTC]

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL job | Azure Function | _Timer-triggered function_ |
| COBOL program | C# class | _Business logic implementation_ |
| VSAM file | Azure Blob Storage | _Intermediate file storage_ |
| Db2 table | Azure SQL Database | _Relational data_ |
| IBM MQ message | Azure Service Bus | _Async messaging_ |

### Parallel-Run Considerations

_Describe how this process will be validated during the parallel-run period._

- **Comparison strategy:** [Output file comparison | Record-by-record | Aggregate totals]
- **Tolerance:** [Exact match | Within rounding tolerance]
- **Comparison frequency:** [Every run | Daily | Weekly]

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| [PAY-BR-NNN] | [e.g., PSD2 Art. 97] | _Regulatory requirement addressed by this process_ |
| [PAY-BR-NNN] | [e.g., FFFS 2014:5] | _Regulatory requirement addressed by this process_ |
