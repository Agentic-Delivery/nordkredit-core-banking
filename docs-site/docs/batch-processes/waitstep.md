---
id: "PROC-WAITSTEP-001"
title: "Wait Step for Job Scheduling"
sidebar_position: 28
process_type: batch
domain: account-management
jcl_source: "WAITSTEP.jcl"
cobol_programs:
  - "COBSWAIT.cbl"
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure Functions Durable Functions
---

# Wait Step for Job Scheduling

## Overview

The Wait Step job introduces a timed delay in job scheduling chains. It executes the COBSWAIT COBOL program with an inline parameter specifying the wait duration in centiseconds. The default parameter value of 00003600 centiseconds equals 36 seconds.

This utility is used to introduce controlled pauses between batch jobs in scheduling chains, typically to allow system resources to settle, to wait for external dependencies to become available, or to stagger job execution to avoid resource contention. It acts as a simple timer/sleep mechanism within JCL job streams.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** On-demand (used within job scheduling chains)
**Business owner:** Systems Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | COBSWAIT.cbl | Waits for the duration specified in the PARM parameter (00003600 centiseconds = 36 seconds) | Inline PARM=00003600 | None (time delay only) | Program completes with RC=0 after wait period; non-zero RC indicates abnormal termination |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| Preceding job in chain | Process | The job that triggers this wait step must have completed |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Following job in chain | Process | The next job in the scheduling chain waits for this step to complete |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand (within job chains) | Job scheduling dependency |
| Completion deadline | N/A | N/A |
| Maximum duration | Wait duration + minimal overhead (approximately 36 seconds with default PARM) | PARM value |
| Retry policy | Re-run if abnormally terminated | Operational runbook |
| Escalation | N/A (utility job) | N/A |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| PARM parameter | JCL PARM | 8-digit centisecond value | JCL inline | Wait duration: 00003600 = 36 seconds (3600 centiseconds) |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| N/A | N/A | N/A | N/A | No data output; this job only introduces a time delay |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Normal completion | Wait period elapsed | RC=0 | Continue to next job in chain |
| System error | Program abend during wait | Non-zero return code | Investigate abend, re-run or skip wait step |
| Invalid parameter | PARM value not numeric or out of range | Program may abend | Correct PARM value, re-run |

### Restart/Recovery Procedure

1. Check job return code
2. If RC=0, wait completed successfully; proceed with downstream job
3. If non-zero RC, investigate system conditions and either re-run or proceed without wait

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions Durable Functions (timer delay) / Azure Logic Apps (delay action)
**Trigger:** Orchestrator activity within a durable function workflow
**CRON expression:** N/A (not scheduled independently)

In the Azure target, explicit wait/delay steps are handled natively by orchestration services. Azure Durable Functions provides `CreateTimer` for introducing delays within orchestrated workflows. Azure Logic Apps provides a built-in "Delay" action. Both approaches are more efficient than the mainframe pattern of running a COBOL program that simply sleeps, as they do not consume compute resources during the wait period.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| WAITSTEP JCL job | Durable Functions `CreateTimer` | Timer delay within orchestrator; does not consume compute during wait |
| COBSWAIT.cbl | `await context.CreateTimer(fireAt, CancellationToken.None)` | C# Durable Functions timer API |
| PARM=00003600 (centiseconds) | `TimeSpan.FromSeconds(36)` | Duration parameter converted to TimeSpan |
| Job scheduling chain | Durable Functions orchestrator / Azure Logic Apps workflow | Workflow orchestration replaces JCL job chains |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable (utility job, no business data output)
- **Tolerance:** N/A
- **Comparison frequency:** N/A
- Timing behavior validated as part of overall batch chain execution time comparison

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | N/A | Infrastructure utility with no direct regulatory requirements |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
