---
id: "PROC-CREASTMT-001"
title: "Create Statements"
sidebar_position: 4
process_type: batch
domain: reporting
jcl_source: "CREASTMT.JCL"
cobol_programs:
  - "CBSTM03A.cbl"
schedule: "monthly (day 1)"
sla: "Complete by day 3"
status: extracted
target_implementation: Azure Functions
---

# Create Statements

## Overview

The Create Statements batch process generates monthly credit card statements in both plain text and HTML formats. It is a multi-step job that first prepares a work file of transactions sorted by card number, then produces formatted statements for each card by looking up customer, account, and transaction data.

The job reorganizes the transaction file so that transactions are keyed by card number (rather than transaction ID), enabling efficient sequential processing of all transactions per card. It then invokes the CBSTM03A program, which iterates through each card in the cross-reference file and produces a complete statement including transaction details, balances, and customer information.

Statements must be completed by the 3rd of each month per the operational SLA, and the output is used for both customer delivery and regulatory reporting.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch
**Frequency:** Monthly
**Business owner:** Reporting and Customer Communications Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (DELDEF01) | IDCAMS | Deletes and redefines the TRXFL work VSAM cluster (32-byte key, 350-byte records, indexed) to ensure a clean workspace | None | Empty TRXFL.VSAM.KSDS cluster | SET MAXCC=0 ensures non-existence is not an error |
| 2 (STEP010) | SORT | Sorts TRANSACT.VSAM.KSDS by card number (position 263, 16 bytes) and transaction ID (position 1, 16 bytes), restructures records with OUTREC to place card number as the leading key field | TRANSACT.VSAM.KSDS | TRXFL.SEQ (sequential, LRECL=350) | Abort on sort failure |
| 3 (STEP020) | IDCAMS REPRO | Loads the sorted sequential file into the TRXFL.VSAM.KSDS work cluster | TRXFL.SEQ | TRXFL.VSAM.KSDS | COND=(0,NE) -- skips if prior step failed |
| 4 (STEP030) | IEFBR14 | Deletes previous statement output files (STATEMNT.HTML and STATEMNT.PS) to prevent appending to old data | STATEMNT.HTML, STATEMNT.PS | Files deleted | COND=(0,NE) -- skips if prior step failed |
| 5 (STEP040) | CBSTM03A.cbl | Produces formatted statements in both text (LRECL=80) and HTML (LRECL=100) formats by reading the work transaction file and looking up card, account, and customer data | TRXFL.VSAM.KSDS, CARDXREF.VSAM.KSDS, ACCTDATA.VSAM.KSDS, CUSTDATA.VSAM.KSDS | STATEMNT.PS (text), STATEMNT.HTML (HTML) | COND=(0,NE) -- skips if prior step failed |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| PROC-POSTTRAN-001 | Process | All transactions for the statement period must be posted |
| PROC-INTCALC-001 | Process | Interest charges for the period must be calculated and merged into transactions |
| TRANSACT.VSAM.KSDS | Data | Complete transaction master file for the statement period |
| CARDXREF.VSAM.KSDS | Data | Card cross-reference data must be current |
| ACCTDATA.VSAM.KSDS | Data | Account master data must be current |
| CUSTDATA.VSAM.KSDS | Data | Customer master data (names, addresses) must be current |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| Statement delivery | System | Text and HTML statements are delivered to customers (print/email) |
| Regulatory reporting | Process | Statement data is used for FSA regulatory reporting |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | Day 1 of each month (after period close) | Operational schedule |
| Completion deadline | Day 3 of each month | Business SLA |
| Maximum duration | 3 days (typically completes in hours) | Derived from start and deadline |
| Retry policy | Re-run from step 1 (DELDEF cleans workspace) | Operational runbook |
| Escalation | Reporting Ops manager if not complete by day 2 | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| TRANSACT.VSAM.KSDS | VSAM KSDS | 16-byte key, 350-byte records | Transaction master | Complete transaction file for the statement period |
| CARDXREF.VSAM.KSDS | VSAM KSDS | Card cross-reference layout | Card management | Maps card numbers to account numbers |
| ACCTDATA.VSAM.KSDS | VSAM KSDS | Account record layout | Account management | Account balances, limits, and status |
| CUSTDATA.VSAM.KSDS | VSAM KSDS | Customer record layout | Customer management | Customer names and addresses for statement headers |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| TRXFL.VSAM.KSDS | VSAM KSDS | 32-byte key, 350-byte records | Internal (step 5 input) | Work file with transactions keyed by card number + transaction ID |
| STATEMNT.PS | Sequential | Fixed-length, LRECL=80 | Customer delivery (print) | Plain text formatted statements |
| STATEMNT.HTML | Sequential | Fixed-length, LRECL=100 | Customer delivery (email/web) | HTML formatted statements |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| System error | IDCAMS DEFINE failure (step 1) | Abort job | Check VSAM catalog, resolve space issues, re-run |
| System error | SORT failure (step 2) | Abort job | Verify TRANSACT file integrity, re-run from step 1 |
| System error | REPRO failure (step 3) | Skip remaining steps (COND) | Verify TRXFL.SEQ was created, re-run from step 3 |
| Data validation | Card not found in XREF during statement generation | Skip card, log to SYSPRINT | Review card data, may require manual statement |
| Data validation | Customer not found in CUSTDATA | Skip statement for card, log | Review customer data linkage |

### Restart/Recovery Procedure

1. Check job completion status via JES2 spool for all 5 steps
2. If any step failed, the COND=(0,NE) logic prevents subsequent steps from running on bad data
3. Re-run from step 1 (DELDEF01) to ensure clean workspace -- all steps are idempotent
4. Verify output statement files contain expected number of statements (one per active card)

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure Functions (timer trigger)
**Trigger:** Timer (CRON)
**CRON expression:** `0 0 2 1 * *` (1st of each month at 02:00 UTC)

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| JCL CREASTMT job | Azure Function `MonthlyStatementGeneration` | Timer-triggered durable function with orchestrator |
| SORT (step 2) | C# LINQ query with OrderBy on card number + transaction ID | Replace SORT with SQL query or in-memory sort |
| IDCAMS REPRO (step 3) | Eliminated -- SQL query replaces VSAM work file | No need for intermediate VSAM cluster in Azure |
| IEFBR14 delete (step 4) | Azure Blob Storage overwrite | New files replace previous month output |
| CBSTM03A.cbl (step 5) | C# `StatementGenerationService` class | Produces PDF/HTML statements |
| TRXFL.VSAM.KSDS | Eliminated (SQL query) | Work file replaced by database query |
| STATEMNT.PS | Azure Blob Storage (PDF format) | Modernized from fixed-width text to PDF |
| STATEMNT.HTML | Azure Blob Storage (HTML) | Retained as HTML format |
| CUSTDATA.VSAM.KSDS | Azure SQL `Customers` table | Customer data for statement addressing |

### Parallel-Run Considerations

- **Comparison strategy:** Record-by-record comparison of statement content (transaction lines, balances, interest amounts) between mainframe text output and Azure output
- **Tolerance:** Exact match for financial figures; formatting differences acceptable between text and PDF
- **Comparison frequency:** Every monthly run during parallel-run period
- **Comparison tool:** `NordKredit.ComparisonTests` project

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| RPT-BR-001 | GDPR Art. 15 | Customers have the right to access their transaction data; statements fulfill this right |
| RPT-BR-002 | GDPR Art. 17 | Statement retention must comply with data minimization; old statements purged per retention policy |
| RPT-BR-003 | PSD2 Art. 57 | Payment service providers must provide regular statements of payment transactions |
| RPT-BR-004 | FFFS 2014:5 Ch. 6 | Statements must include all transaction details required by payment services regulations |
| RPT-BR-005 | Konsumentkreditlagen (2010:1846) | Credit card statements must disclose interest charges and minimum payment information |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
