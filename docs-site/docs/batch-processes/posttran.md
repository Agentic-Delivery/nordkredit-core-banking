---
id: "PROC-POSTTRAN-001"
title: "Post Transactions"
sidebar_position: 1
process_type: batch
domain: transactions
jcl_source: "POSTTRAN.jcl"
cobol_programs:
  - "CBTRN02C.cbl"
schedule: "daily (nightly batch)"
sla: "Complete before interest calculation"
status: extracted
target_implementation: Azure Functions
---

# Post Transactions

## Overview

The Post Transactions batch process validates and posts daily credit card transactions to the transaction master file. It is the first major step in the nightly batch cycle, processing all transactions accumulated during the business day. Each transaction is validated against the card cross-reference file and account master to ensure the card exists, the account is active, the credit limit is not exceeded, and the card has not expired. Valid transactions are written to the transaction master VSAM file and the transaction category balance file is updated. Rejected transactions are written to a Generation Data Group (GDG) file for operations review.

This process is critical to the integrity of the card transaction lifecycle. Downstream processes such as interest calculation, transaction reporting, and transaction combination all depend on the successful completion of this job.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** Daily (nightly)
**Business owner:** Transaction Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | CBTRN02C.cbl | Reads daily transaction file, validates each transaction against card cross-reference and account master (credit limit, expiration date), posts valid transactions to the transaction master VSAM, updates category balance file, and writes rejected transactions to the rejections GDG | DALYTRAN.PS, TRANSACT.VSAM.KSDS, CARDXREF.VSAM.KSDS, ACCTDATA.VSAM.KSDS, TCATBALF.VSAM.KSDS | Updated TRANSACT.VSAM.KSDS, updated TCATBALF.VSAM.KSDS, DALYREJS GDG (+1) | Rejected transactions written to DALYREJS with rejection code; job abends on system errors |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CLOSEFIL | Process | CICS files must be closed before batch processing can begin |
| DALYTRAN.PS | Data | Daily transaction file must be available from the online capture system |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| PROC-INTCALC-001 | Process | Interest calculation uses updated TCATBALF and TRANSACT data |
| PROC-TRANREPT-001 | Process | Transaction reporting reads posted transactions |
| PROC-COMBTRAN-001 | Process | Combine transactions merges posted data with system-generated transactions |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | After CICS file close (nightly batch window) | Operational schedule |
| Completion deadline | Before INTCALC starts (02:00 CET) | Batch dependency chain |
| Maximum duration | Determined by transaction volume | Operational runbook |
| Retry policy | Re-run after correcting input data | Operational runbook |
| Escalation | On-call Transaction Ops after failed run | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| DALYTRAN.PS | Sequential (PS) | Fixed-length 350-byte records | Online transaction capture | Daily transaction file containing all transactions from the business day |
| TRANSACT.VSAM.KSDS | VSAM KSDS | 16-byte key, 350-byte records | Transaction master | Existing posted transactions |
| CARDXREF.VSAM.KSDS | VSAM KSDS | Card cross-reference layout | Card management | Maps card numbers to account numbers for validation |
| ACCTDATA.VSAM.KSDS | VSAM KSDS | Account record layout | Account management | Account master data including credit limit and expiration date |
| TCATBALF.VSAM.KSDS | VSAM KSDS | Category balance layout | Billing | Transaction category balance file for balance tracking |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRANSACT.VSAM.KSDS | VSAM KSDS | 16-byte key, 350-byte records | Downstream batch processes | Updated transaction master with newly posted transactions |
| TCATBALF.VSAM.KSDS | VSAM KSDS | Category balance layout | INTCALC, PRTCATBL | Updated transaction category balances |
| DALYREJS GDG (+1) | Sequential (GDG) | Rejection record layout | Operations review | Rejected transactions with rejection codes |

## Error Handling and Recovery

### Rejection Codes

| Code | Condition | Description |
|------|-----------|-------------|
| 100 | Invalid card | Card number not found in CARDXREF cross-reference file |
| 101 | Account not found | Account number from cross-reference not found in ACCTDATA |
| 102 | Over limit | Transaction would cause account balance to exceed credit limit |
| 103 | Expired | Card expiration date has passed |

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Data validation | Invalid card (100) | Write transaction to DALYREJS with code 100 | Operations reviews and corrects card data; re-submit transaction |
| Data validation | Account not found (101) | Write transaction to DALYREJS with code 101 | Operations reviews account linkage; re-submit transaction |
| Business rule violation | Over credit limit (102) | Write transaction to DALYREJS with code 102 | Business review; customer notification |
| Business rule violation | Expired card (103) | Write transaction to DALYREJS with code 103 | Card renewal process; re-submit if renewed |
| System error | VSAM I/O error or file unavailable | Abort job step | Verify file availability, restart from beginning |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool
2. Review DALYREJS file for rejected transactions requiring manual intervention
3. If job abended, verify all input files are available and undamaged
4. Re-run from beginning (input file is read-only; posted transactions are idempotent by transaction ID)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (timer trigger)
**Trigger:** Timer (CRON)
**CRON expression:** `0 0 23 * * *` (daily 23:00 UTC, nightly batch window)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL POSTTRAN job | Azure Function `NightlyTransactionPosting` | Timer-triggered function |
| CBTRN02C.cbl | C# `TransactionPostingService` class | Validates and posts transactions |
| DALYTRAN.PS | Azure Blob Storage / Azure SQL staging table | Daily transaction input |
| TRANSACT.VSAM.KSDS | Azure SQL `Transactions` table | Transaction master |
| CARDXREF.VSAM.KSDS | Azure SQL `CardCrossReference` table | Card-to-account mapping |
| ACCTDATA.VSAM.KSDS | Azure SQL `Accounts` table | Account master data |
| TCATBALF.VSAM.KSDS | Azure SQL `TransactionCategoryBalances` table | Category balance tracking |
| DALYREJS GDG | Azure Blob Storage container `daily-rejections` | Rejection output files |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of posted transactions and rejection files between mainframe and Azure outputs
- **Tolerance:** Exact match required for transaction amounts, rejection codes, and category balances
- **Comparison frequency:** Every nightly run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| TXN-BR-001 | PSD2 Art. 64 | Transaction posting must process all received payment orders within the business day |
| TXN-BR-002 | PSD2 Art. 78 | Full transaction amount must be posted without unauthorized deductions |
| TXN-BR-003 | PSD2 Art. 71 | Rejected transactions must be reported with clear reason codes for payer notification |
| TXN-BR-004 | FFFS 2014:5 Ch. 6 | Payment services must follow orderly processing procedures with complete audit trail |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
