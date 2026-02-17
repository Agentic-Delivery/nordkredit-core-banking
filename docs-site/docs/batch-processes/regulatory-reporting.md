---
title: Regulatory Reporting (FSA)
sidebar_position: 6
---

# Regulatory Reporting (FSA)

**JCL Job Name:** FSAREPT (estimated &mdash; JCL source pending from mainframe team)
**Schedule:** Per FSA regulatory calendar (varies by report type)
**SLA:** Per regulatory calendar deadlines
**Domain:** Account Management / Cross-domain
**Azure Migration:** Azure Functions timer trigger (not yet implemented)
**Regulation:** FFFS 2014:5 Ch.7-9 (supervisory reporting), DORA Art.11 (ICT reporting)

---

## Overview

The regulatory reporting batch job generates reports required by Finansinspektionen (FSA), the Swedish financial supervisory authority. These reports cover capital adequacy, liquidity, large exposures, and operational risk metrics. Missing or late regulatory submissions can trigger enforcement action, making this one of the highest-priority batch jobs.

---

## Report Types (Known)

| Report | Frequency | Deadline | Regulation |
|--------|-----------|----------|------------|
| Capital adequacy (COREP) | Quarterly | Per FSA calendar | FFFS 2014:5 Ch.7 |
| Liquidity coverage ratio (LCR) | Monthly | Per FSA calendar | FFFS 2014:5 Ch.7 |
| Large exposure reporting | Quarterly | Per FSA calendar | FFFS 2014:5 Ch.7 |
| Operational risk events | Quarterly | Per FSA calendar | FFFS 2014:5 Ch.4 |
| AML suspicious activity reports | As needed | Immediate | AML Act 2017:630 |
| DORA ICT incident reports | As needed | 4h initial, 72h intermediate | DORA Art.17-19 |

---

## Processing Logic (Inferred from Domain)

:::caution JCL Source Pending
The actual JCL source and COBOL program(s) for regulatory reporting have not yet been obtained from the mainframe team. The processing logic below is inferred from regulatory requirements and the cutover runbook. This document will be updated when the COBOL source is available.
:::

1. **Determine reporting period** based on FSA calendar
2. **Extract data** from relevant domain tables:
   - Account balances and classifications
   - Transaction volumes and values
   - Risk exposure calculations
   - Operational incident records
3. **Apply regulatory calculation rules** (capital ratios, liquidity metrics)
4. **Format output** per FSA submission templates (XBRL or CSV)
5. **Validate output** against FSA validation rules
6. **Write report files** for submission
7. **Generate audit trail** documenting report generation parameters and data sources

---

## Input/Output Datasets (Expected)

| Dataset | Type | Access | Description |
|---------|------|--------|-------------|
| Account master | VSAM KSDS | Input | Account classifications and balances |
| Transaction history | VSAM KSDS | Input | Transaction volumes |
| Risk classification table | VSAM KSDS | Input | Exposure categories |
| Regulatory report output | Sequential | Output | Formatted report files |
| Audit trail | Sequential | Output | Generation audit log |

---

## SLA and Scheduling

| Attribute | Mainframe | Azure (Target) |
|-----------|-----------|----------------|
| **Trigger** | JCL schedule per FSA calendar | Azure Functions timer trigger |
| **Schedule** | Monthly/Quarterly per report type | Same |
| **SLA deadline** | Per FSA regulatory calendar | Same |
| **Dependencies** | Month-end closing must be complete | Same |

---

## Error Handling

- Regulatory reports must **never** be submitted with known errors
- Validation failures halt submission and require operator/compliance review
- Failed reports must be regenerated and submitted before the deadline
- All generation attempts are logged for audit purposes (DORA Art.11)

---

## Azure Migration Mapping

| Component | Mainframe | Azure Target |
|-----------|-----------|-------------|
| **Scheduler** | JCL | Azure Functions timer trigger |
| **Processing** | COBOL batch program | C# Azure Function |
| **Data store** | Db2 / VSAM | Azure SQL Database |
| **Report output** | Mainframe spool / file transfer | Azure Blob Storage + secure transfer |
| **Monitoring** | JES2 spool | Application Insights |
| **Audit trail** | Mainframe log | Azure SQL audit table |

---

## Blackout Periods

Per the cutover runbook, no domain cutover may be scheduled during FSA regulatory reporting dates.

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
