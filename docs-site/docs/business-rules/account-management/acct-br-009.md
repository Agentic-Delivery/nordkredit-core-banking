---
id: "acct-br-009"
title: "Transaction category balance tracking"
domain: "account-management"
cobol_source: "CBTRN02C.cbl:467-543"
requirement_id: "ACCT-BR-009"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "PSD2 Art. 64"
  - "EU Consumer Credit Directive 2008/48/EC Art. 6"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-009: Transaction category balance tracking

## Summary

The daily batch posting program (CBTRN02C) maintains a separate transaction category balance file (TCATBAL-FILE) that tracks account-level balances broken down by transaction type and category code. Each posted transaction updates or creates a record in this indexed VSAM file keyed by the composite of account ID, transaction type code, and transaction category code. This categorized balance tracking provides the foundation for interest calculation, disclosure grouping, and periodic statement generation — each category can carry different interest rates, fee structures, or regulatory treatment.

The TCATBAL record is maintained alongside the account balance update (ACCT-BR-004). While the account master tracks aggregate balances, the TCATBAL file provides the granular breakdown needed for regulatory disclosure and financial reporting.

## Business Logic

### Pseudocode

```
PERFORM 2700-UPDATE-TCATBAL:
    -- Step 1: Build composite key
    MOVE XREF-ACCT-ID    TO FD-TRANCAT-ACCT-ID
    MOVE DALYTRAN-TYPE-CD TO FD-TRANCAT-TYPE-CD
    MOVE DALYTRAN-CAT-CD  TO FD-TRANCAT-CD

    -- Step 2: Attempt to read existing record
    SET WS-CREATE-TRANCAT-REC = 'N'
    READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD
        INVALID KEY
            -- Record does not exist for this key
            SET WS-CREATE-TRANCAT-REC = 'Y'
    END-READ

    -- Step 3: Validate file status (accept '00' or '23' = not found)
    IF TCATBALF-STATUS = '00' OR '23'
        CONTINUE
    ELSE
        DISPLAY 'ERROR READING TRANSACTION BALANCE FILE'
        ABEND
    END-IF

    -- Step 4: Create or update
    IF WS-CREATE-TRANCAT-REC = 'Y'
        PERFORM 2700-A-CREATE-TCATBAL-REC
    ELSE
        PERFORM 2700-B-UPDATE-TCATBAL-REC
    END-IF

PERFORM 2700-A-CREATE-TCATBAL-REC:
    INITIALIZE TRAN-CAT-BAL-RECORD
    MOVE XREF-ACCT-ID    TO TRANCAT-ACCT-ID
    MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD
    MOVE DALYTRAN-CAT-CD  TO TRANCAT-CD
    ADD DALYTRAN-AMT TO TRAN-CAT-BAL   (starts at zero after INITIALIZE)
    WRITE record to TCATBAL-FILE

PERFORM 2700-B-UPDATE-TCATBAL-REC:
    ADD DALYTRAN-AMT TO TRAN-CAT-BAL
    REWRITE record to TCATBAL-FILE
```

### Decision Table

| Scenario | TCATBAL Record Exists? | Action | TRAN-CAT-BAL After |
|---|---|---|---|
| First transaction in category | No (file status '23') | CREATE: Initialize record, set balance to transaction amount | DALYTRAN-AMT |
| Subsequent transaction in same category | Yes (file status '00') | UPDATE: Add transaction amount to existing balance | Previous balance + DALYTRAN-AMT |
| I/O error reading TCATBAL | N/A (file status other than '00'/'23') | ABEND program | N/A |
| I/O error writing TCATBAL | N/A (file status not '00') | ABEND program | N/A |

### TCATBAL Record Structure

| Field | COBOL PIC | Migrated Type | Purpose |
|---|---|---|---|
| FD-TRANCAT-ACCT-ID | 9(11) | bigint / char(11) | Account identifier (part of composite key) |
| FD-TRANCAT-TYPE-CD | X(02) | char(2) | Transaction type code (part of composite key) |
| FD-TRANCAT-CD | 9(04) | smallint / char(4) | Transaction category code (part of composite key) |
| FD-TRAN-CAT-DATA | X(33) | — | Data area containing TRAN-CAT-BAL and other fields |
| TRAN-CAT-BAL | S9(09)V99 (inferred) | decimal(11,2) | Running balance for this account/type/category |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 57-61 (file definition), 91-97 (record layout), 467-543 (update logic)

**File definition (lines 57-61):**

```cobol
000057            SELECT TCATBAL-FILE ASSIGN TO TCATBALF
000058                   ORGANIZATION IS INDEXED
000059                   ACCESS MODE  IS RANDOM
000060                   RECORD KEY   IS FD-TRAN-CAT-KEY
000061                   FILE STATUS  IS TCATBALF-STATUS.
```
*(Lines 57-61 — TCATBAL-FILE is an indexed VSAM KSDS with random access by composite key.)*

**Record layout (lines 91-97):**

```cobol
000091        FD  TCATBAL-FILE.
000092        01  FD-TRAN-CAT-BAL-RECORD.
000093            05 FD-TRAN-CAT-KEY.
000094               10 FD-TRANCAT-ACCT-ID             PIC 9(11).
000095               10 FD-TRANCAT-TYPE-CD             PIC X(02).
000096               10 FD-TRANCAT-CD                  PIC 9(04).
000097            05 FD-FD-TRAN-CAT-DATA               PIC X(33).
```
*(Lines 91-97 — composite key is ACCT-ID + TYPE-CD + CATEGORY-CD, totaling 17 bytes.)*

**Main update paragraph (lines 467-501):**

```cobol
000467        2700-UPDATE-TCATBAL.
000468       * Update the balances in transaction balance file.
000469            MOVE XREF-ACCT-ID TO FD-TRANCAT-ACCT-ID
000470            MOVE DALYTRAN-TYPE-CD TO FD-TRANCAT-TYPE-CD
000471            MOVE DALYTRAN-CAT-CD TO FD-TRANCAT-CD
000473            MOVE 'N' TO WS-CREATE-TRANCAT-REC
000474            READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD
000475               INVALID KEY
000476                 DISPLAY 'TCATBAL record not found for key : '
000477                    FD-TRAN-CAT-KEY '.. Creating.'
000478                 MOVE 'Y' TO WS-CREATE-TRANCAT-REC
000479            END-READ.
000481            IF  TCATBALF-STATUS = '00'  OR '23'
000482                MOVE 0 TO APPL-RESULT
000495            IF WS-CREATE-TRANCAT-REC = 'Y'
000496               PERFORM 2700-A-CREATE-TCATBAL-REC
000497            ELSE
000498               PERFORM 2700-B-UPDATE-TCATBAL-REC
000499            END-IF
```
*(Lines 467-499 — read-or-create pattern using INVALID KEY handler.)*

**Create new record (lines 503-524):**

```cobol
000503        2700-A-CREATE-TCATBAL-REC.
000504            INITIALIZE TRAN-CAT-BAL-RECORD
000505            MOVE XREF-ACCT-ID TO TRANCAT-ACCT-ID
000506            MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD
000507            MOVE DALYTRAN-CAT-CD TO TRANCAT-CD
000508            ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000510            WRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
```
*(Lines 503-510 — initializes record, sets key fields, adds transaction amount as first balance.)*

**Update existing record (lines 526-542):**

```cobol
000526        2700-B-UPDATE-TCATBAL-REC.
000527            ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000528            REWRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
```
*(Lines 526-528 — adds transaction amount to existing category balance and rewrites.)*

## Acceptance Criteria

### Scenario 1: First transaction creates a new TCATBAL record

```gherkin
GIVEN an account 41000000001 with no existing TCATBAL record for type 'SA' category 5001
WHEN a transaction of +150.00 with type 'SA' and category 5001 is posted
THEN a new TCATBAL record is created with:
  | Field | Value |
  | TRANCAT-ACCT-ID | 41000000001 |
  | TRANCAT-TYPE-CD | SA |
  | TRANCAT-CD | 5001 |
  | TRAN-CAT-BAL | 150.00 |
```

### Scenario 2: Subsequent transaction updates existing TCATBAL record

```gherkin
GIVEN an existing TCATBAL record for account 41000000001, type 'SA', category 5001 with balance 150.00
WHEN a transaction of +75.00 with type 'SA' and category 5001 is posted
THEN the TCATBAL record is updated:
  | Field | Value |
  | TRAN-CAT-BAL | 225.00 |
  AND the record is persisted via REWRITE
```

### Scenario 3: Different category creates separate record

```gherkin
GIVEN an existing TCATBAL record for account 41000000001, type 'SA', category 5001
WHEN a transaction with type 'SA' and category 5002 is posted
THEN a NEW TCATBAL record is created for category 5002
  AND the existing category 5001 record is unchanged
```

### Scenario 4: Negative transaction reduces category balance

```gherkin
GIVEN an existing TCATBAL record with TRAN-CAT-BAL = 500.00
WHEN a transaction of -200.00 is posted for the same account/type/category
THEN TRAN-CAT-BAL = 300.00
```

### Scenario 5: I/O error causes program ABEND

```gherkin
GIVEN an I/O error occurs reading or writing the TCATBAL file
  AND the file status is not '00' or '23'
WHEN the error is detected
THEN the program ABENDs with error display
  AND no partial update is persisted
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Financial institutions must maintain detailed accounting records with transaction categorization | Category-level balance tracking provides the granular breakdown needed for regulatory financial reporting and audit |
| PSD2 | Art. 64 | Transaction data integrity — amounts must be accurately categorized and traceable | Each transaction is categorized by type and category code, with balances maintained per category for traceability |
| EU Consumer Credit Directive | Art. 6 (2008/48/EC) | Annual percentage rate and periodic statement disclosure must break down charges by category | Transaction category balances enable disclosure statements that itemize charges, payments, and fees by category |

## Edge Cases

1. **INITIALIZE zeroes all fields**: The `INITIALIZE` statement in 2700-A-CREATE-TCATBAL-REC sets all fields to their default values (zeros for numeric, spaces for alphanumeric). This means `TRAN-CAT-BAL` starts at zero before `ADD DALYTRAN-AMT`, so the first transaction amount becomes the initial balance. The migrated system must replicate this initialization behavior.

2. **Composite key uniqueness**: The 17-byte composite key (11-digit account + 2-char type + 4-digit category) allows up to 99 type codes and 9,999 category codes per account. The migrated system should use a composite primary key or unique index on the equivalent SQL table.

3. **No cycle reset visible**: The available source does not show when or how TCATBAL records are reset for a new billing cycle. Category balances may accumulate indefinitely or be reset by a separate batch job. The migrated system must identify the cycle reset mechanism.

4. **File status '23' handling**: COBOL file status '23' indicates "record not found" for indexed file READ. The program treats this as a valid condition (triggering record creation) rather than an error. The migrated system should use "upsert" or "insert-or-update" semantics.

5. **Concurrent batch access**: The TCATBAL-FILE is opened with random access in the batch program. If multiple batch runs could process concurrently, there is a risk of lost updates. The migrated system should use database-level concurrency control.

6. **Balance precision**: The TRAN-CAT-BAL field uses `S9(09)V99` (inferred from the data area), supporting balances up to ±999,999,999.99. The migrated system should use `decimal(11,2)` and add overflow detection.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) What transaction type codes and category codes are defined in the system — is there a reference table? (2) Are TCATBAL records reset at billing cycle close, or do they accumulate across cycles? (3) How are TCATBAL records used for interest calculation — does each category carry a different interest rate? (4) Is the TCATBAL file used for regulatory reporting or only internal disclosure? (5) What is the relationship between TCATBAL categories and the TRANCATG reference file used in CBTRN03C?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
