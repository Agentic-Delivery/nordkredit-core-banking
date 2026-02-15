---
id: "uc-trn-04"
title: "UC-TRN-04: Daily Transaction Verification"
domain: "transactions"
type: "use-case"
source_business_rules:
  - "TRN-BR-004"
  - "TRN-BR-009"
cobol_source: "CBTRN01C.cbl:154-251"
regulations:
  - "PSD2 Art. 64"
  - "FSA FFFS 2014:5"
  - "AML 2017:11"
status: "draft"
priority: "high"
---

# UC-TRN-04: Verify Daily Transactions

## Overview

| Field | Value |
|-------|-------|
| **Use Case ID** | UC-TRN-04 |
| **Title** | Verify Daily Transactions |
| **Primary Actor** | Batch Processing System |
| **Secondary Actor** | External Transaction Source (provides DALYTRAN) |
| **Trigger** | Nightly batch schedule initiates JCL job step 1 |
| **Precondition** | Daily transaction file (DALYTRAN) is available; reference files (XREF, ACCT) are accessible |
| **Postcondition** | All transactions verified; verification results logged to console (SYSOUT) |
| **Source Business Rules** | [TRN-BR-004](../../business-rules/transactions/trn-br-004), [TRN-BR-009](../../business-rules/transactions/trn-br-009) |
| **COBOL Source** | `CBTRN01C.cbl:154-251` |
| **Pipeline Position** | Step 1 of 3 in the daily batch transaction processing pipeline |

## Description

The Daily Transaction Verification use case is the first step in the three-step nightly batch pipeline (CBTRN01C → CBTRN02C → CBTRN03C). The batch system reads each record from the daily transaction file (DALYTRAN) sequentially and performs two verification checks:

1. **Card verification** — confirms the transaction's card number exists in the cross-reference file (XREF)
2. **Account verification** — confirms the account linked to the card exists in the account master file (ACCT)

This step is **advisory only**: it produces console log output (SYSOUT) but does not write an output file, reject transactions, or modify any data. Downstream step 2 (CBTRN02C) re-performs card/account verification independently before posting. Step 1 serves as an early warning system that surfaces data integrity issues before the posting step begins.

### Processing Flow

```
DALYTRAN (sequential) ──► Read record
                              │
                              ▼
                    Card # in XREF? ──► No ──► Log "CARD NUMBER {num} COULD NOT BE VERIFIED" & Skip
                              │
                              ▼ Yes
                    Account in ACCT? ──► No ──► Log "ACCOUNT {id} NOT FOUND"
                              │
                              ▼ Yes
                    Log success (Card #, Account ID, Customer ID)
                    Continue to next record
```

### File Dependencies

| File | Organization | Access | Key | Purpose |
|------|-------------|--------|-----|---------|
| DALYTRAN | Sequential | Sequential read | N/A | Daily transaction input feed |
| XREFFILE | Indexed (VSAM) | Random read | Card number (X(16)) | Card-to-account cross-reference |
| ACCTFILE | Indexed (VSAM) | Random read | Account ID (9(11)) | Account master |
| CUSTFILE | Indexed (VSAM) | Random read | Customer ID (9(09)) | Customer master (opened but not actively used) |
| CARDFILE | Indexed (VSAM) | Random read | Card number (X(16)) | Card master (opened but not actively used) |
| TRANFILE | Indexed (VSAM) | Random read | Transaction ID (X(16)) | Transaction master (opened but not actively used) |

## User Stories

### US-TRN-04.1: Process daily transaction file sequentially

**As** the batch system,
**I want to** read each record from the daily transaction file sequentially,
**So that** all incoming transactions are processed in order.

**Acceptance Criteria:**

- The system opens DALYTRAN as a sequential input file
- Each record is read one at a time until end-of-file
- The entire DALYTRAN-RECORD is displayed to the console for each record (audit trail)
- Processing terminates gracefully when end-of-file is reached
- A "START OF EXECUTION" message is logged at program start
- An "END OF EXECUTION" message is logged at program end

**Source:** TRN-BR-004 Scenario 6, `CBTRN01C.cbl:164-186`

### US-TRN-04.2: Verify card number exists in cross-reference

**As** the batch system,
**I want to** verify each transaction's card number exists in the XREF file,
**So that** invalid card references are detected early before the posting step.

**Acceptance Criteria:**

- For each transaction, the card number (DALYTRAN-CARD-NUM) is looked up in the XREF file
- If the card is found (read status = 0): the card number, account ID, and customer ID are logged as "SUCCESSFUL READ OF XREF"
- If the card is NOT found (invalid key): the message `CARD NUMBER <num> COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-<id>` is logged
- Transactions with invalid card numbers are skipped (no account verification attempted)
- An invalid card does NOT cause the program to ABEND — processing continues with the next record

**Source:** TRN-BR-004 Scenarios 1–2, `CBTRN01C.cbl:170-184, 227-239`

### US-TRN-04.3: Verify linked account exists

**As** the batch system,
**I want to** verify the account linked to each card exists in the account master,
**So that** orphaned card-to-account references are detected before posting.

**Acceptance Criteria:**

- After successful card verification, the account ID from XREF (XREF-ACCT-ID) is looked up in the account master (ACCTFILE)
- If the account is found (read status = 0): verification is complete; processing continues
- If the account is NOT found (read status ≠ 0): the message `ACCOUNT <id> NOT FOUND` is logged
- A missing account does NOT cause the program to ABEND — processing continues with the next record
- A missing account does NOT explicitly skip the transaction (unlike an invalid card)

**Source:** TRN-BR-004 Scenarios 1, 3, `CBTRN01C.cbl:174-179, 241-250`

### US-TRN-04.4: Log verification results for audit

**As** a compliance officer,
**I want** all verification results logged to console output (SYSOUT),
**So that** there is an audit trail for the daily batch run satisfying FSA FFFS 2014:5 Ch. 7.

**Acceptance Criteria:**

- Every transaction record is displayed in full when read (raw DALYTRAN-RECORD content)
- Successful XREF lookups log: card number, account ID, and customer ID
- Failed XREF lookups log: card number and transaction ID with "COULD NOT BE VERIFIED" message
- Failed account lookups log: account ID with "NOT FOUND" message
- Program start and end are logged with execution markers
- All log entries go to SYSOUT (console) for capture in the JCL job log

**Source:** TRN-BR-004 Scenario 6, `CBTRN01C.cbl:155, 168, 178, 181-183, 187`

### US-TRN-04.5: Handle file errors with ABEND

**As** the batch system,
**I want to** ABEND (code 999) on file I/O errors,
**So that** downstream steps (posting and reporting) do not execute with potentially corrupted input.

**Acceptance Criteria:**

- If any required file (DALYTRAN, XREFFILE, ACCTFILE, CUSTFILE, CARDFILE, TRANFILE) cannot be opened: an error message is displayed with the file status code, and the program ABENDs with code 999
- If a read error occurs on DALYTRAN (file status not '00' or '10'): the error is displayed with the I/O status, and the program ABENDs with code 999
- An ABEND in step 1 prevents step 2 (CBTRN02C) and step 3 (CBTRN03C) from executing, enforced by JCL COND parameter `COND=(4,LT,VERIFY)`
- Normal end-of-file (status '10') is NOT treated as an error

**Source:** TRN-BR-004 Scenarios 4–5, TRN-BR-009 Scenario 3, `CBTRN01C.cbl:252-467, 469-489`

## Acceptance Criteria (Use Case Level)

### Scenario 1: Successful card and account verification

```gherkin
GIVEN a daily transaction record with a valid card number
  AND the card number exists in the XREF file
  AND the linked account ID exists in the account master
WHEN the batch program processes the record
THEN the card number, account ID, and customer ID are logged
  AND processing continues to the next record
```

### Scenario 2: Invalid card number

```gherkin
GIVEN a daily transaction record with a card number not in the XREF file
WHEN the batch program looks up the cross-reference
THEN the message "CARD NUMBER {num} COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-{id}" is logged
  AND the transaction is skipped
```

### Scenario 3: Account not found for valid card

```gherkin
GIVEN a daily transaction with a valid card number in XREF
  AND the linked account ID does not exist in the account master
WHEN the batch program reads the account file
THEN the message "ACCOUNT {id} NOT FOUND" is logged
  AND processing continues to the next record
```

### Scenario 4: File open error

```gherkin
GIVEN any required file cannot be opened
WHEN the batch program starts
THEN an error message is displayed with the file status
  AND the program ABENDs with code 999
  AND downstream pipeline steps do not execute
```

### Scenario 5: Read error on daily transaction file

```gherkin
GIVEN a read error occurs on the DALYTRAN file (status not '00' or '10')
WHEN the batch program reads the next record
THEN the error is displayed with the IO status
  AND the program ABENDs with code 999
  AND downstream pipeline steps do not execute
```

### Scenario 6: Full file processing with audit trail

```gherkin
GIVEN a daily transaction file with N records
WHEN the batch program processes all records
THEN each DALYTRAN-RECORD is displayed to the console
  AND "START OF EXECUTION" is logged before processing
  AND "END OF EXECUTION" is logged after all records are processed
  AND the complete SYSOUT log provides an audit trail for compliance review
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Use Case Satisfies It |
|------------|----------------|-------------|-------------------------------|
| PSD2 | Art. 64 | Integrity of payment data | Card number verification ensures each transaction references a valid payment instrument before posting |
| FSA FFFS 2014:5 | Ch. 7 | Batch processing controls | Console logging of every record and verification result provides a complete audit trail for the daily batch run |
| AML 2017:11 | Para. 3 | Transaction monitoring | Card and account verification is a prerequisite step for downstream AML screening in the nightly batch |

## Migration Notes

### Azure Target Architecture

The daily transaction verification (CBTRN01C) maps to an **Azure Function** (timer-triggered or orchestrated via Durable Functions) as the first step in the batch pipeline. Key migration considerations:

1. **Sequential file → Azure Blob Storage or Azure SQL**: The DALYTRAN sequential file should be ingested from Azure Blob Storage (or Azure SQL staging table) rather than sequential file reads
2. **VSAM indexed files → Azure SQL lookups**: XREF and ACCT file lookups become SQL queries against the card cross-reference and account master tables
3. **DISPLAY → Structured logging**: SYSOUT-based DISPLAY statements should be replaced with Application Insights structured logging, preserving the audit trail requirement
4. **ABEND → Exception handling**: ABEND code 999 maps to throwing an exception that stops the Azure Functions orchestration, preventing downstream steps from executing
5. **Advisory-only nature**: Since this step produces no output file, the migrated version could be folded into step 2 (CBTRN02C). This is a domain expert decision — see open questions below

### Open Questions for Domain Expert

1. **Is step 1 redundant?** CBTRN02C re-performs card/account verification independently. Should step 1 be preserved as a separate pre-check, or can its logic be merged into step 2?
2. **Unused file references**: CUSTFILE, CARDFILE, and TRANFILE are opened but not actively used in the main processing loop. Are these vestigial, or are they used by called subroutines?
3. **Account-not-found behavior**: When an account is not found, the transaction is logged but NOT explicitly skipped or rejected. Should the migrated system treat this as a soft warning or a hard rejection?

## Traceability Matrix

| Artifact | Identifier | Link |
|----------|-----------|------|
| Business Rule (verification) | TRN-BR-004 | [trn-br-004](../../business-rules/transactions/trn-br-004) |
| Business Rule (pipeline) | TRN-BR-009 | [trn-br-009](../../business-rules/transactions/trn-br-009) |
| COBOL Source | CBTRN01C.cbl:154-251 | Main processing loop |
| Regulation | PSD2 Art. 64 | Payment data integrity |
| Regulation | FSA FFFS 2014:5 Ch. 7 | Batch processing controls |
| Regulation | AML 2017:11 Para. 3 | Transaction monitoring |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
