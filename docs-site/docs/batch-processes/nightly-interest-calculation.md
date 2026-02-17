---
title: Nightly Interest Calculation
sidebar_position: 4
---

# Nightly Interest Calculation

**JCL Job Name:** INTCALC (estimated &mdash; JCL source pending from mainframe team)
**Schedule:** Nightly (runs after transaction posting completes)
**SLA:** Must complete by 06:00 CET
**Domain:** Deposits
**Azure Migration:** Azure Functions timer trigger (not yet implemented)
**Regulation:** FFFS 2014:5 Ch.3 (financial reporting), GDPR Art.5(1)(f) (data integrity)

---

## Overview

The nightly interest calculation batch job computes accrued interest for all deposit accounts. It runs after the nightly transaction pipeline completes to ensure all daily transactions are reflected in the balance before interest accrual. This is one of the most financially sensitive batch jobs &mdash; calculation accuracy directly impacts customer balances and regulatory reporting.

---

## Processing Logic (Inferred from Domain)

:::caution JCL Source Pending
The actual JCL source and COBOL program(s) for this batch job have not yet been obtained from the mainframe team. The processing logic below is inferred from domain requirements, the cutover runbook, and interviews. This document will be updated when the COBOL source is available.
:::

1. **Read deposit account master** &mdash; iterate over all active deposit accounts
2. **Calculate daily interest accrual** for each account:
   - Determine applicable interest rate (fixed, variable, or tiered)
   - Calculate daily accrual: `(balance * annual_rate) / days_in_year`
   - Handle day-count conventions (ACT/360 or ACT/365)
3. **Update interest accrual ledger** &mdash; record daily accrual amount
4. **Month-end capitalization** (on last business day of month):
   - Add accumulated accrued interest to account balance
   - Reset accrual counter
   - Generate interest statement entries
5. **Write summary report** &mdash; total interest accrued, account count, exceptions

---

## Input/Output Datasets (Expected)

| Dataset | Type | Access | Description |
|---------|------|--------|-------------|
| Deposit account master | VSAM KSDS | I-O | Account balances and interest rates |
| Interest rate table | VSAM KSDS | Input | Current rate schedules |
| Interest accrual ledger | VSAM KSDS | I-O | Daily accrual records |
| Interest calculation report | Sequential | Output | Summary report |

---

## SLA and Scheduling

| Attribute | Mainframe | Azure (Target) |
|-----------|-----------|----------------|
| **Trigger** | JCL schedule (after transaction pipeline) | Azure Functions timer trigger |
| **Schedule** | Nightly | Nightly (after `DailyBatchOrchestrator` completes) |
| **SLA deadline** | Complete by 06:00 CET | Same |
| **Dependencies** | Must run after transaction posting | Same |

---

## Error Handling

- Interest calculation errors on individual accounts should be logged and skipped (not ABEND the entire batch)
- Month-end capitalization failures require immediate operator notification
- Rounding discrepancies must be logged for audit (regulation requires sub-cent accuracy)

---

## Azure Migration Mapping

| Component | Mainframe | Azure Target |
|-----------|-----------|-------------|
| **Scheduler** | JCL | Azure Functions timer trigger |
| **Processing** | COBOL batch program | C# Azure Function |
| **Data store** | Db2 / VSAM | Azure SQL Database |
| **Monitoring** | JES2 spool | Application Insights |
| **Interest rates** | VSAM table | Azure SQL configuration table |

---

## Validation Requirements

Per the cutover runbook:
- Interest calculation accuracy must be validated against mainframe output for **30 consecutive days** before cutover
- Month-end cutover blackout applies (last 3 + first 3 business days)
- Rounding differences between COBOL packed-decimal and C# decimal types must be within 0.01 SEK

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

- [Cutover Runbook &mdash; Deposits (Domain 3)](./cutover-runbook.md#53-deposits-domain-3)
