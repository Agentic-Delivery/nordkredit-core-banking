---
title: AML/KYC Screening
sidebar_position: 7
---

# AML/KYC Screening

**JCL Job Name:** AMLSCRN (estimated &mdash; JCL source pending from mainframe team)
**Schedule:** Nightly
**SLA:** Must complete before start of business (by 08:00 CET)
**Domain:** Account Management
**Azure Migration:** Azure Functions timer trigger (not yet implemented)
**Regulation:** Swedish AML Act 2017:630, EU AML Directive 2015/849, FFFS 2017:11 (FSA AML guidelines)

---

## Overview

The AML (Anti-Money Laundering) and KYC (Know Your Customer) screening batch job runs nightly to identify suspicious transaction patterns and customer activity. This is a **regulatory obligation** &mdash; the AML screening batch must not be interrupted during migration, and failure to run constitutes a compliance violation.

---

## Processing Logic (Inferred from Domain)

:::caution JCL Source Pending
The actual JCL source and COBOL program(s) for AML screening have not yet been obtained from the mainframe team. The processing logic below is inferred from regulatory requirements and the cutover runbook. This document will be updated when the COBOL source is available.
:::

1. **Read day's transactions** from transaction history
2. **Screen against sanctions lists** (EU consolidated list, UN sanctions, national lists)
3. **Apply transaction monitoring rules**:
   - Large transaction threshold (SEK 150,000+ single transaction)
   - Structuring detection (multiple transactions just below threshold)
   - Unusual pattern detection (deviation from customer profile)
   - Cross-border transaction flagging
   - High-risk country screening
4. **Score and flag** suspicious transactions
5. **Generate alerts** for compliance officer review:
   - Suspicious Activity Reports (SARs) for confirmed cases
   - Enhanced Due Diligence (EDD) triggers
6. **Update customer risk profiles** based on screening results
7. **Write screening report** with statistics and flagged transactions

---

## Input/Output Datasets (Expected)

| Dataset | Type | Access | Description |
|---------|------|--------|-------------|
| Transaction history | VSAM KSDS | Input | Day's posted transactions |
| Customer master | VSAM KSDS | I-O | Customer records and risk profiles |
| Sanctions lists | Sequential / DB2 | Input | Current sanctions lists |
| AML rules table | VSAM KSDS | Input | Screening rules and thresholds |
| AML alerts output | Sequential | Output | Flagged transactions for review |
| Screening report | Sequential | Output | Processing summary and statistics |

---

## SLA and Scheduling

| Attribute | Mainframe | Azure (Target) |
|-----------|-----------|----------------|
| **Trigger** | JCL schedule (after transaction pipeline) | Azure Functions timer trigger |
| **Schedule** | Nightly | Same |
| **SLA deadline** | Before start of business (08:00 CET) | Same |
| **Dependencies** | Must run after transaction posting is complete | Same |

---

## Error Handling

- AML screening **must complete every night** &mdash; failure is a compliance violation
- Individual transaction screening failures should be logged and queued for manual review (fail-open for safety, but flag for human review)
- Sanctions list update failures should trigger operator alert (screening continues with previous list version)
- All screening decisions must be logged for audit (minimum 5-year retention per AML Act)

---

## Azure Migration Mapping

| Component | Mainframe | Azure Target |
|-----------|-----------|-------------|
| **Scheduler** | JCL | Azure Functions timer trigger |
| **Processing** | COBOL batch program | C# Azure Function |
| **Data store** | Db2 / VSAM | Azure SQL Database |
| **Sanctions lists** | Mainframe file / Db2 | Azure SQL + scheduled list updates |
| **Alert output** | Mainframe spool | Azure Service Bus &rarr; Compliance dashboard |
| **Monitoring** | JES2 spool | Application Insights |
| **Audit trail** | Mainframe log | Azure SQL audit table (5-year retention) |

---

## Regulatory Requirements

| Requirement | Regulation | Impact |
|-------------|-----------|--------|
| Nightly screening mandatory | AML Act 2017:630 | Failure to screen &rarr; enforcement action |
| SAR filing within 24 hours | FFFS 2017:11 | Alert-to-filing SLA |
| 5-year data retention | AML Act 2017:630 | All screening records retained |
| Sanctions list currency | EU Regulation 2580/2001 | Lists updated within 24 hours of publication |
| Customer risk profiling | FFFS 2017:11 | Risk scores updated nightly |

---

## Blackout Considerations

AML screening has **no blackout periods** &mdash; it must run every night regardless of other maintenance windows, cutover activities, or holidays. During domain cutover weekends, the AML batch must run on whichever system (mainframe or Azure) is currently processing transactions.

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

- [Cutover Runbook &mdash; Account Management (Domain 5)](./cutover-runbook.md#55-account-management-domain-5--final)
- [Regulatory Traceability Matrix](../regulatory-traceability/traceability-matrix.md)
