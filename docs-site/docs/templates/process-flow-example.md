---
id: "PROC-INTCALC-001"
title: "Nightly Interest Calculation"
process_type: batch
domain: deposits
jcl_source: "INTCALC.jcl"
cobol_programs:
  - "INTCALC0.cbl"
  - "INTCALC1.cbl"
  - "INTUPDT0.cbl"
schedule: "daily 02:00 CET"
sla: "Complete by 06:00 CET"
status: extracted
target_implementation: Azure Functions
---

# Nightly Interest Calculation

## Overview

Calculates accrued daily interest for all active deposit accounts (savings and term deposits) and updates account balances. This is one of the most critical batch processes in the core banking system, running every night to ensure accurate interest accrual for approximately 450,000 active deposit accounts.

Interest rates are sourced from the rate table maintained by Treasury, and calculation rules vary by product type (savings, fixed-term, variable). The process must complete before the morning online cycle begins at 06:00 CET so that customer-facing balances reflect correct accrued interest.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** Daily
**Business owner:** Deposits Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | INTCALC0.cbl | Extract active deposit accounts and current rate table | Db2 ACCOUNT table, Db2 RATE_TABLE | VSAM work file WKACCT | Abort job if Db2 unavailable; alert on-call DBA |
| 2 | INTCALC1.cbl | Calculate daily interest per account using product-specific rules (actual/360, actual/365, 30/360) | VSAM work file WKACCT | VSAM result file WKINT, exception report EXCRPT | Skip account on calculation error; write to exception report |
| 3 | INTUPDT0.cbl | Apply calculated interest to account balances and write audit trail | VSAM result file WKINT, Db2 ACCOUNT table | Updated Db2 ACCOUNT table, Db2 INTEREST_AUDIT table | Rollback on Db2 error; restart from last committed checkpoint |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-EOD-001 | Process | End-of-day processing must complete (closes online transaction window) |
| PROC-RATEUPD-001 | Process | Rate table update job must complete (Treasury rate feed from Riksbank) |
| Db2 ACCOUNT table | Data | Account master data must be current |
| Db2 RATE_TABLE | Data | Interest rate table must reflect current rates from Treasury |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| PROC-STMNT-001 | Process | Monthly statement generation uses accrued interest totals |
| PROC-REGFSA-001 | Process | FSA regulatory reporting includes interest accrual figures |
| Online Banking | System | Customer-facing balances must reflect accrued interest by 06:00 CET |
| PROC-TAXRPT-001 | Process | Annual tax reporting (kontrolluppgift) aggregates daily interest |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | 02:00 CET (after EOD and rate update) | Operational schedule |
| Completion deadline | 06:00 CET | Business SLA (before online banking opens) |
| Maximum duration | 4 hours | Derived from start time and deadline |
| Retry policy | 2 retries, 15 min interval | Operational runbook OPSRUN-DEP-003 |
| Escalation | On-call DBA and Deposits Ops manager after 2 failed retries | Incident management SOP |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| ACCOUNT | Db2 table | CPYBK-ACCT (copybook) | Account Management domain | Active deposit account master records |
| RATE_TABLE | Db2 table | CPYBK-RATE (copybook) | Treasury | Current interest rates per product type |
| WKACCT | VSAM KSDS | CPYBK-WKACCT (copybook) | Step 1 output | Work file with account/rate pairs |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| WKINT | VSAM ESDS | CPYBK-WKINT (copybook) | Step 3 input | Calculated interest amounts per account |
| EXCRPT | Sequential | Fixed-length 133 bytes | Operations team | Exception report for accounts that failed calculation |
| ACCOUNT | Db2 table | CPYBK-ACCT (copybook) | All downstream | Updated balances with accrued interest |
| INTEREST_AUDIT | Db2 table | CPYBK-INTAUD (copybook) | Audit/compliance | Audit trail of every interest posting |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Data validation | Invalid account status or missing rate | Log to exception report (EXCRPT), skip account | Manual review by Deposits Ops; re-run after correction |
| System error | Db2 connection failure or timeout | Abort current job step with return code 8 | Automatic retry per policy (2 retries, 15 min apart) |
| Business rule violation | Negative interest result on non-negative product | Write to exception report, skip account | Business review by Deposits Ops team |
| Capacity | VSAM file space exhaustion | Abort with return code 12 | Extend VSAM allocation, restart from step 1 |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool (job INTCALC, steps 1-3)
2. If step 1 failed: fix Db2 issue, restart from step 1 (idempotent extract)
3. If step 2 failed: restart from step 2 (work file WKACCT is still valid)
4. If step 3 failed: identify last committed checkpoint in INTEREST_AUDIT table, restart INTUPDT0 with checkpoint parameter to resume from last committed batch (commits every 1,000 accounts)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (timer trigger)
**Trigger:** Timer (CRON)
**CRON expression:** `0 0 1 * * *` (daily 01:00 UTC = 02:00 CET)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL INTCALC job | Azure Function `NightlyInterestCalculation` | Timer-triggered durable function with orchestrator |
| INTCALC0.cbl (extract) | C# `InterestAccountExtractor` class | Query Azure SQL for active accounts with rates |
| INTCALC1.cbl (calculate) | C# `InterestCalculator` class | Implement day-count conventions (actual/360, actual/365, 30/360) |
| INTUPDT0.cbl (update) | C# `InterestPostingService` class | Batch update with transaction scope; checkpoint every 1,000 records |
| VSAM work files | Azure Blob Storage (intermediate) | JSON format; retained 7 days for debugging |
| Db2 tables | Azure SQL Database | Schema mapped via data migration tooling |
| Exception report | Application Insights custom events + Azure Blob CSV | Queryable via Log Analytics; CSV for Ops team compatibility |
| IBM MQ notification | Azure Service Bus topic `interest-calculation-complete` | Notify downstream processes |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of calculated interest amounts per account
- **Tolerance:** Within 0.01 SEK (rounding differences between COBOL COMP-3 and C# decimal)
- **Comparison frequency:** Every nightly run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project, test class `InterestCalculationComparisonTest`
- **Known differences:** COBOL uses COMP-3 packed decimal (15 digits); C# uses `decimal` (28-29 digits). Rounding at intermediate steps may differ by up to 0.01 SEK per account.

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| DEP-BR-001 | FFFS 2014:5 Ch. 3 | Interest must be calculated daily and posted to accounts accurately |
| DEP-BR-002 | FFFS 2014:5 Ch. 16 | Interest rate changes must be applied from effective date |
| DEP-BR-003 | Konsumentkreditlagen (2010:1846) | Consumer deposit interest must use actual/365 day-count convention |
| DEP-BR-015 | Skatteförfarandelagen (2011:1244) | Daily interest must be trackable for annual tax reporting (kontrolluppgift) |
| DEP-BR-016 | GDPR Art. 30 | Interest audit trail constitutes processing record; retain per data retention policy |
