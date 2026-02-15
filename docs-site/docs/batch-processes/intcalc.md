---
id: "PROC-INTCALC-001"
title: "Interest Calculation"
sidebar_position: 3
process_type: batch
domain: billing
jcl_source: "INTCALC.jcl"
cobol_programs:
  - "CBACT04C.cbl"
schedule: "daily 02:00 CET"
sla: "Complete by 06:00 CET"
status: extracted
target_implementation: Azure Functions
---

# Interest Calculation

## Overview

The Interest Calculation batch process computes daily interest charges and fees on credit card account balances. It reads the transaction category balance file (TCATBALF), cross-references card and account data, and applies interest rates from the disclosure group (DISCGRP) rate tables. The output is a set of system-generated interest and fee transactions written to the SYSTRAN GDG, which are subsequently merged into the transaction master by the COMBTRAN process.

The process accepts a date parameter (PARM='2022071800') that specifies the calculation date. Interest rates are determined by the disclosure group associated with each account, supporting multiple rate tiers and product types.

This is one of the most financially critical batch processes in the system. Incorrect interest calculations directly impact customer balances and regulatory compliance. The process must complete by 06:00 CET to ensure accurate balances are available for the morning online cycle.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** Daily
**Business owner:** Billing Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (STEP15) | CBACT04C.cbl | Processes transaction category balances, looks up card and account information via primary KSDS and AIX path, retrieves applicable interest rates from disclosure group file, calculates interest and fees, and writes system-generated transactions | TCATBALF.VSAM.KSDS, CARDXREF.VSAM.KSDS, CARDXREF.VSAM.AIX.PATH, ACCTDATA.VSAM.KSDS, DISCGRP.VSAM.KSDS | SYSTRAN GDG (+1) | Abort on file I/O errors; skip individual accounts on calculation errors |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-POSTTRAN-001 | Process | Transaction posting must complete so that TCATBALF reflects current day balances |
| TCATBALF.VSAM.KSDS | Data | Transaction category balance file must be current |
| CARDXREF.VSAM.KSDS | Data | Card cross-reference file must be available (primary and AIX path) |
| ACCTDATA.VSAM.KSDS | Data | Account master data must be current |
| DISCGRP.VSAM.KSDS | Data | Disclosure group / interest rate tables must reflect current rates |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| PROC-COMBTRAN-001 | Process | Uses SYSTRAN GDG output to merge interest/fee transactions into the transaction master |
| PROC-CREASTMT-001 | Process | Statement generation includes interest charges calculated by this process |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | 02:00 CET (after POSTTRAN completes) | Operational schedule |
| Completion deadline | 06:00 CET | Business SLA (before online banking opens) |
| Maximum duration | 4 hours | Derived from start time and deadline |
| Retry policy | 2 retries, 15 min interval | Operational runbook |
| Escalation | On-call Billing Ops and DBA after 2 failed retries | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TCATBALF.VSAM.KSDS | VSAM KSDS | Category balance record layout | POSTTRAN process | Transaction category balance file with current balances by category |
| CARDXREF.VSAM.KSDS | VSAM KSDS | Card cross-reference layout | Card management | Primary key access to card-account cross-reference |
| CARDXREF.VSAM.AIX.PATH | VSAM AIX | Alternate index path | Card management | Alternate index path for account-to-card lookups |
| ACCTDATA.VSAM.KSDS | VSAM KSDS | Account record layout | Account management | Account master data including account status and product type |
| DISCGRP.VSAM.KSDS | VSAM KSDS | Disclosure group layout | Rate management | Interest rate tables organized by disclosure group |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| SYSTRAN GDG (+1) | Sequential (GDG) | Fixed-length 350-byte records (RECFM=F, LRECL=350) | COMBTRAN process | System-generated interest and fee transactions |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Data validation | Account not found in ACCTDATA | Skip account, log to SYSPRINT | Review account data integrity; re-run after correction |
| Data validation | Card not found in XREF | Skip card, log to SYSPRINT | Review card cross-reference data |
| Data validation | Disclosure group not found in DISCGRP | Skip account, log to SYSPRINT | Verify rate table configuration |
| System error | VSAM I/O error | Abort job step | Verify file availability, restart from beginning |
| Business rule violation | Negative interest result | Log exception, skip account | Business review by Billing Ops |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool (job INTCALC, step STEP15)
2. If step failed due to I/O error: verify all input VSAM files are available, restart from beginning
3. If step failed during processing: SYSTRAN GDG (+1) may be partially written; delete the partial generation and re-run
4. The process is idempotent for a given date parameter -- re-running with the same PARM date produces the same output

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (timer trigger)
**Trigger:** Timer (CRON)
**CRON expression:** `0 0 1 * * *` (daily 01:00 UTC = 02:00 CET)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL INTCALC job | Azure Function `NightlyInterestCalculation` | Timer-triggered durable function |
| CBACT04C.cbl | C# `InterestCalculationService` class | Core interest/fee calculation logic |
| TCATBALF.VSAM.KSDS | Azure SQL `TransactionCategoryBalances` table | Category balance data |
| CARDXREF.VSAM.KSDS (primary + AIX) | Azure SQL `CardCrossReference` table | Indexed by both card number and account number |
| ACCTDATA.VSAM.KSDS | Azure SQL `Accounts` table | Account master data |
| DISCGRP.VSAM.KSDS | Azure SQL `DisclosureGroups` / `InterestRates` tables | Rate configuration |
| SYSTRAN GDG | Azure SQL staging table `SystemTransactions` | Interest/fee transactions for merge |
| PARM date parameter | Azure Function configuration / input binding | Calculation date passed as parameter |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of calculated interest and fee amounts per account between mainframe SYSTRAN output and Azure output
- **Tolerance:** Within 0.01 SEK per account (rounding differences between COBOL COMP-3 packed decimal and C# decimal)
- **Comparison frequency:** Every nightly run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project, test class `InterestCalculationComparisonTest`
- **Known differences:** COBOL uses COMP-3 packed decimal (15 digits); C# uses `decimal` (28-29 digits). Intermediate rounding steps may produce differences up to 0.01 SEK per account.

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| BILL-BR-001 | FFFS 2014:5 Ch. 3 | Interest must be calculated daily and posted accurately to customer accounts |
| BILL-BR-002 | FFFS 2014:5 Ch. 16 | Interest rate changes must be applied from their effective date |
| BILL-BR-003 | Konsumentkreditlagen (2010:1846) | Consumer credit interest calculation must follow prescribed methods |
| BILL-BR-004 | FFFS 2014:5 Ch. 4 | Disclosure of interest rates and fees must be transparent and accurate |
| BILL-BR-005 | PSD2 Art. 78 | Interest and fee charges must not include unauthorized deductions |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
