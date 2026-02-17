---
title: Monthly Statement Generation
sidebar_position: 5
---

# Monthly Statement Generation

**JCL Job Name:** STMTGEN (estimated &mdash; JCL source pending from mainframe team)
**Schedule:** Monthly (triggered on first business day after month-end)
**SLA:** Must complete by day 3 of the new month
**Domain:** Lending / Cross-domain
**Azure Migration:** Azure Functions timer trigger (not yet implemented)
**Regulation:** FFFS 2014:5 Ch.7 (customer reporting), PSD2 Art.94 (transaction records), EU Consumer Credit Directive 2008/48/EC

---

## Overview

The monthly statement generation batch job produces customer statements covering all account activity for the preceding month. Statements include transaction history, balance summaries, interest charges/credits, and fee breakdowns. Statements are generated for both deposit and lending accounts and must be delivered to customers within the first 3 business days of the new month.

---

## Processing Logic (Inferred from Domain)

:::caution JCL Source Pending
The actual JCL source and COBOL program(s) for this batch job have not yet been obtained from the mainframe team. The processing logic below is inferred from domain requirements and the cutover runbook. This document will be updated when the COBOL source is available.
:::

1. **Determine statement period** &mdash; first day to last day of previous month
2. **Iterate over all active accounts** (deposits + lending):
   - Read account master record
   - Read all transactions within the statement period
   - Read interest accrual records for the period
   - Read fee/charge records for the period
3. **Generate statement data** for each account:
   - Opening balance
   - Transaction listing (date, description, amount, running balance)
   - Interest credited/charged
   - Fees charged
   - Closing balance
4. **Format statement output** (print-ready and/or digital format)
5. **Write statement records** to output dataset for printing/delivery
6. **Generate summary report** &mdash; accounts processed, statement count, exceptions

---

## Input/Output Datasets (Expected)

| Dataset | Type | Access | Description |
|---------|------|--------|-------------|
| Account master | VSAM KSDS | Input | All active accounts |
| Transaction history | VSAM KSDS | Input | Month's transactions |
| Interest accrual ledger | VSAM KSDS | Input | Interest records |
| Fee/charge ledger | VSAM KSDS | Input | Fee records |
| Statement output | Sequential | Output | Formatted statement records |
| Statement summary report | Sequential | Output | Processing summary |

---

## SLA and Scheduling

| Attribute | Mainframe | Azure (Target) |
|-----------|-----------|----------------|
| **Trigger** | JCL schedule (1st business day) | Azure Functions timer trigger |
| **Schedule** | Monthly | Monthly |
| **SLA deadline** | Complete by day 3 | Same |
| **Dependencies** | Month-end interest capitalization must be complete | Same |

---

## Error Handling

- Individual account statement failures should be logged and skipped
- Missing transaction data for an account should generate a partial statement with a warning
- Statement delivery failures (print queue, digital channel) require retry and operator notification

---

## Azure Migration Mapping

| Component | Mainframe | Azure Target |
|-----------|-----------|-------------|
| **Scheduler** | JCL | Azure Functions timer trigger |
| **Processing** | COBOL batch program | C# Azure Function |
| **Data store** | Db2 / VSAM | Azure SQL Database |
| **Statement delivery** | Print queue / mainframe spool | Azure Blob Storage + delivery pipeline |
| **Monitoring** | JES2 spool | Application Insights |

---

## Blackout Periods

Per the cutover runbook, no domain cutover may be scheduled during the month-end period (last 3 business days + first 3 business days) to protect statement generation.

---

## Migration Status

| Phase | Status |
|-------|--------|
| COBOL source obtained | Pending |
| Business rules extracted | Not started |
| Azure Function implemented | Not started |
| Parallel-run validation | Not started |

---

## Related Documents

- [Cutover Runbook &mdash; Lending (Domain 4)](./cutover-runbook.md#54-lending-domain-4)
