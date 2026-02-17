---
title: Batch Dependency Graph
sidebar_position: 8
---

# Batch Dependency Graph

This document shows the execution order and dependencies between all batch jobs in the NordKredit mainframe system and their Azure equivalents.

---

## Nightly Batch Execution DAG

```
                         ┌─────────────────────────┐
                         │     JCL Scheduler        │
                         │   (01:00 UTC nightly)    │
                         └────────────┬─────────────┘
                                      │
                                      ▼
                    ┌─────────────────────────────────────┐
                    │  NIGHTLY TRANSACTION PIPELINE        │
                    │  (JCL chain: CBTRN01C→02C→03C)      │
                    │  Azure: DailyBatchOrchestrator       │
                    │  SLA: Complete by 06:00 UTC          │
                    │                                     │
                    │  Step 1: Card Verification (CBTRN01C)│
                    │       │                             │
                    │       ▼                             │
                    │  Step 2a: Credit Validation          │
                    │  Step 2b: Transaction Posting         │
                    │       │              (CBTRN02C)      │
                    │       ▼                             │
                    │  Step 3: Report Generation (CBTRN03C)│
                    └──────┬──────────────┬───────────────┘
                           │              │
              ┌────────────┘              └────────────┐
              ▼                                        ▼
┌──────────────────────────┐          ┌──────────────────────────┐
│ NIGHTLY INTEREST CALC    │          │ AML/KYC SCREENING        │
│ JCL: INTCALC (est.)     │          │ JCL: AMLSCRN (est.)     │
│ Azure: Timer trigger     │          │ Azure: Timer trigger     │
│ Domain: Deposits         │          │ Domain: Acct Mgmt        │
│ SLA: By 06:00 CET       │          │ SLA: By 08:00 CET       │
│ Depends on: Txn pipeline │          │ Depends on: Txn pipeline │
└──────────────────────────┘          └──────────────────────────┘

                    ┌─────────────────────────┐
                    │     JCL Scheduler        │
                    │ (Monthly — 1st biz day)  │
                    └────────────┬─────────────┘
                                 │
                                 ▼
               ┌──────────────────────────────────┐
               │ MONTHLY STATEMENT GENERATION      │
               │ JCL: STMTGEN (est.)              │
               │ Azure: Timer trigger              │
               │ Domain: Lending / Cross-domain    │
               │ SLA: Complete by day 3            │
               │ Depends on: Month-end interest    │
               │             capitalization        │
               └──────────────────────────────────┘

                    ┌─────────────────────────┐
                    │     JCL Scheduler        │
                    │   (Per FSA calendar)     │
                    └────────────┬─────────────┘
                                 │
                                 ▼
               ┌──────────────────────────────────┐
               │ REGULATORY REPORTING (FSA)        │
               │ JCL: FSAREPT (est.)              │
               │ Azure: Timer trigger              │
               │ Domain: Acct Mgmt / Cross-domain  │
               │ SLA: Per regulatory calendar      │
               │ Depends on: Month-end closing     │
               └──────────────────────────────────┘
```

---

## Dependency Summary Table

| Batch Job | Frequency | Predecessor(s) | Successor(s) |
|-----------|-----------|-----------------|--------------|
| **Nightly Transaction Pipeline** | Daily 01:00 UTC | None (first in chain) | Interest Calc, AML Screening |
| &nbsp;&nbsp;Step 1: Card Verification (CBTRN01C) | — | Pipeline start | Step 2a |
| &nbsp;&nbsp;Step 2a: Credit Validation (CBTRN02C) | — | Step 1 | Step 2b |
| &nbsp;&nbsp;Step 2b: Transaction Posting (CBTRN02C) | — | Step 2a | Step 3 |
| &nbsp;&nbsp;Step 3: Report Generation (CBTRN03C) | — | Step 2b | Pipeline end |
| **Nightly Interest Calculation** | Daily (after txn pipeline) | Transaction Pipeline | (Month-end: Statement Gen) |
| **AML/KYC Screening** | Daily (after txn pipeline) | Transaction Pipeline | None |
| **Monthly Statement Generation** | Monthly (1st biz day) | Month-end interest capitalization | None |
| **Regulatory Reporting (FSA)** | Per FSA calendar | Month-end/quarter-end closing | None |

---

## Scheduling Constraints

### Nightly Window (01:00&ndash;06:00 CET)

| Time | Job | Notes |
|------|-----|-------|
| 01:00 | Transaction Pipeline starts | Triggered by JCL scheduler / Worker |
| ~01:00&ndash;04:00 | Transaction Pipeline executes | Steps 1&rarr;2a&rarr;2b&rarr;3 sequentially |
| ~04:00 | Interest Calculation starts | After transaction pipeline completes |
| ~04:00 | AML Screening starts | Can run in parallel with Interest Calc |
| 06:00 | **SLA deadline** | Transaction Pipeline + Interest Calc must complete |
| 08:00 | AML screening deadline | Must complete before start of business |

### Monthly Window

| Time | Job | Notes |
|------|-----|-------|
| Month-end | Interest capitalization | Part of nightly interest calc on last business day |
| Day 1 (1st biz day) | Statement generation starts | Triggered after month-end interest complete |
| Day 3 | **Statement SLA deadline** | All customer statements must be generated |

### Blackout Periods

| Period | Jobs affected | Reason |
|--------|--------------|--------|
| Month-end (last 3 + first 3 biz days) | All except AML | Statement generation, interest capitalization |
| FSA reporting dates | All except AML | Regulatory submission deadlines |
| Swedish public holidays | Non-critical maintenance only | Reduced staff |
| Dec 15 &ndash; Jan 15 | All cutover activities | Year-end processing freeze |

**AML screening has no blackout periods** &mdash; it runs every night without exception.

---

## Azure Migration: Orchestration Strategy

On the mainframe, JCL job scheduling controls batch execution order and dependency chains via JCL `COND` parameters and job schedulers (e.g., TWS/OPC). On Azure, the migration strategy is:

| Mainframe Mechanism | Azure Equivalent |
|---------------------|------------------|
| JCL job schedule | `Worker` BackgroundService (timer trigger) |
| JCL step `COND` parameter | C# `if` / orchestrator step filtering |
| JCL job dependency chain | Azure Functions chaining / orchestrator pattern |
| JES2 spool monitoring | Application Insights + Azure Monitor alerts |
| Operator restart | Manual re-trigger via API or portal |

The `DailyBatchOrchestrator` currently implements the transaction pipeline. Additional orchestrators will be needed for interest calculation, statement generation, regulatory reporting, and AML screening as those domains are migrated.
