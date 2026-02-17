---
title: Batch Processes
sidebar_position: 1
---

# Batch Processes

Documentation of all JCL batch jobs running on the IBM z/OS mainframe and their Azure Functions migration mappings. The mainframe runs approximately 5 batch job chains covering transaction processing, interest calculation, statement generation, regulatory reporting, and AML screening.

---

## Batch Job Inventory

| Job | JCL Name | COBOL Programs | Schedule | SLA | Domain | Azure Status |
|-----|----------|---------------|----------|-----|--------|-------------|
| [Nightly Transaction Pipeline](./nightly-transaction-pipeline.md) | CBTRN01C&rarr;02C&rarr;03C | CBTRN01C, CBTRN02C, CBTRN03C | Daily 01:00 UTC | By 06:00 UTC | Transactions | Implemented (`DailyBatchOrchestrator`) |
| [Nightly Interest Calculation](./nightly-interest-calculation.md) | INTCALC (est.) | Pending | Daily (after txn pipeline) | By 06:00 CET | Deposits | Not started |
| [Monthly Statement Generation](./monthly-statement-generation.md) | STMTGEN (est.) | Pending | Monthly (1st biz day) | By day 3 | Lending | Not started |
| [Regulatory Reporting (FSA)](./regulatory-reporting.md) | FSAREPT (est.) | Pending | Per FSA calendar | Per calendar | Account Mgmt | Not started |
| [AML/KYC Screening](./aml-screening.md) | AMLSCRN (est.) | Pending | Nightly | By 08:00 CET | Account Mgmt | Not started |

---

## Nightly Execution Order

```
01:00 UTC ──► Transaction Pipeline (CBTRN01C → CBTRN02C → CBTRN03C)
                    │
                    ├──► Interest Calculation (after txn pipeline)
                    │
                    └──► AML/KYC Screening (after txn pipeline)

06:00 UTC ──► SLA DEADLINE (Transaction Pipeline + Interest Calc)

08:00 CET ──► AML Screening deadline
```

See [Batch Dependency Graph](./batch-dependency-graph.md) for the complete execution DAG and scheduling constraints.

---

## SLA Summary

| Job | SLA Deadline | Consequence of Breach |
|-----|-------------|----------------------|
| Nightly Transaction Pipeline | 06:00 UTC | Delayed account balances; batch SLA alert |
| Nightly Interest Calculation | 06:00 CET | Incorrect interest accrual; financial impact |
| Monthly Statements | Day 3 of month | Customer communication delay; regulatory risk |
| Regulatory Reporting | Per FSA calendar | FSA enforcement action |
| AML/KYC Screening | 08:00 CET | Compliance violation (AML Act 2017:630) |

---

## Migration Status Overview

| Phase | Transaction Pipeline | Interest Calc | Statements | Regulatory | AML |
|-------|---------------------|--------------|------------|-----------|-----|
| COBOL source obtained | Yes | Pending | Pending | Pending | Pending |
| Business rules extracted | Yes (TRN-BR-005&ndash;009) | No | No | No | No |
| Azure implemented | Yes | No | No | No | No |
| Unit tests | Yes | No | No | No | No |
| Parallel-run validated | No | No | No | No | No |

---

## Key Documents

- [Batch Dependency Graph](./batch-dependency-graph.md) &mdash; Execution order DAG and scheduling constraints
- [Cutover Runbook](./cutover-runbook.md) &mdash; Operational cutover procedures including batch timing windows
- [ADR-001: Batch Pipeline Orchestrator Pattern](/docs/Architecture/ADR-001-batch-pipeline-orchestrator.md) &mdash; Architecture decision for the transaction pipeline

---

## Regulatory Context

All batch jobs operate under the following regulatory frameworks:

| Regulation | Relevance |
|-----------|-----------|
| FFFS 2014:5 Ch.4 &sect;3 | Operational risk management during batch processing |
| DORA Art.11 | ICT system testing and risk monitoring |
| PSD2 Art.94 | Transaction record retention |
| AML Act 2017:630 | Mandatory nightly AML screening |
| FFFS 2017:11 | FSA AML guidelines |
| EU Consumer Credit Directive 2008/48/EC | Statement accuracy for lending |

---

## Outstanding Actions

1. **Obtain JCL source** from mainframe team for interest calculation, statement generation, regulatory reporting, and AML screening
2. **Extract business rules** from remaining COBOL batch programs
3. **Implement Azure Functions** for remaining batch jobs
4. **Validate parallel-run** for transaction pipeline (first)
5. **Domain expert sign-off** on all extracted batch business rules
