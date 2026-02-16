---
id: "TRN-BR-008"
title: "Transaction category balance maintenance (upsert pattern)"
domain: "transactions"
cobol_source: "CBTRN02C.cbl:467-542"
requirement_id: "TRN-BR-008"
regulations:
  - "FFFS 2014:5 Ch. 3 — Accurate accounting records"
  - "FFFS 2014:5 Ch. 16 — Financial reporting by category"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-008: Transaction category balance maintenance (upsert pattern)

## Summary

During transaction posting (CBTRN02C), the system maintains a running balance per account, transaction type, and transaction category in the TCATBAL VSAM file. For each posted transaction, the system reads the TCATBAL record using a composite key (Account ID + Type Code + Category Code). If the record exists, the transaction amount is added to the existing balance. If the record does not exist, a new record is created with the transaction amount as the initial balance. This provides an aggregate view of spending/credits by category for each account.

## Business Logic

### Pseudocode

```
PERFORM 2700-UPDATE-TCATBAL:
    Compose key:
        FD-TRANCAT-ACCT-ID = Account ID (from XREF lookup)
        FD-TRANCAT-TYPE-CD = Transaction Type Code
        FD-TRANCAT-CD      = Transaction Category Code

    SET create-flag = 'N'
    READ TCATBAL file using composite key
    IF INVALID KEY (record not found):
        DISPLAY "TCATBAL record not found for key: ... Creating."
        SET create-flag = 'Y'
    END-IF

    IF file status = '00' or '23' (success or not-found):
        CONTINUE
    ELSE:
        ABEND — file read error
    END-IF

    IF create-flag = 'Y':
        PERFORM 2700-A-CREATE:
            INITIALIZE TRAN-CAT-BAL-RECORD
            SET key fields (Acct ID, Type CD, Category CD)
            ADD DALYTRAN-AMT TO TRAN-CAT-BAL (starts at 0 after INITIALIZE)
            WRITE new record
    ELSE:
        PERFORM 2700-B-UPDATE:
            ADD DALYTRAN-AMT TO TRAN-CAT-BAL (existing balance)
            REWRITE updated record
    END-IF
```

### Composite Key Structure

| Field | PIC | Source | Example |
|-------|-----|--------|---------|
| TRANCAT-ACCT-ID | 9(11) | XREF-ACCT-ID | 00000000042 |
| TRANCAT-TYPE-CD | X(02) | DALYTRAN-TYPE-CD | 01 |
| TRANCAT-CD | 9(04) | DALYTRAN-CAT-CD | 0001 |

**Full key example:** `0000000004201​0001` (17 characters)

### Balance Field

| Field | PIC | Description |
|-------|-----|-------------|
| TRAN-CAT-BAL | S9(09)V99 | Running signed balance — accumulates all transaction amounts for this key |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 467-542

```cobol
000467 2700-UPDATE-TCATBAL.
000469     MOVE XREF-ACCT-ID TO FD-TRANCAT-ACCT-ID
000470     MOVE DALYTRAN-TYPE-CD TO FD-TRANCAT-TYPE-CD
000471     MOVE DALYTRAN-CAT-CD TO FD-TRANCAT-CD
000473     MOVE 'N' TO WS-CREATE-TRANCAT-REC
000474     READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD
000475        INVALID KEY
000476          DISPLAY 'TCATBAL record not found for key : '
000477             FD-TRAN-CAT-KEY '.. Creating.'
000478          MOVE 'Y' TO WS-CREATE-TRANCAT-REC
000479     END-READ.
000481     IF  TCATBALF-STATUS = '00'  OR '23'
000482         MOVE 0 TO APPL-RESULT
000483     ELSE
000484         MOVE 12 TO APPL-RESULT
000485     END-IF
...
000503 2700-A-CREATE-TCATBAL-REC.
000504     INITIALIZE TRAN-CAT-BAL-RECORD
000505     MOVE XREF-ACCT-ID TO TRANCAT-ACCT-ID
000506     MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD
000507     MOVE DALYTRAN-CAT-CD TO TRANCAT-CD
000508     ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000510     WRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
...
000526 2700-B-UPDATE-TCATBAL-REC.
000527     ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000528     REWRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
```

## Acceptance Criteria

### Scenario 1: Create new category balance record

```gherkin
GIVEN account "00000000042" has no existing TCATBAL record for type "01" category "0001"
  AND a transaction of +250.00 is being posted
WHEN the category balance is updated
THEN a new TCATBAL record is created with key "00000000042|01|0001"
  AND TRAN-CAT-BAL is set to 250.00
```

### Scenario 2: Update existing category balance

```gherkin
GIVEN account "00000000042" has an existing TCATBAL record for type "01" category "0001"
  AND the existing TRAN-CAT-BAL is 1000.00
  AND a transaction of +150.00 is being posted
WHEN the category balance is updated
THEN TRAN-CAT-BAL becomes 1150.00 (1000.00 + 150.00)
  AND the record is rewritten
```

### Scenario 3: Credit reduces category balance

```gherkin
GIVEN an existing TCATBAL record with balance 500.00
  AND a transaction of -200.00 (credit/return) is being posted
WHEN the category balance is updated
THEN TRAN-CAT-BAL becomes 300.00 (500.00 + (-200.00))
```

### Scenario 4: File write error causes abend

```gherkin
GIVEN a new TCATBAL record needs to be created
  AND the WRITE operation fails
WHEN the write is attempted
THEN the program displays an error message
  AND abends with code 999
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 3 | Accurate accounting records at category level | Category balance tracks spending by type/category per account |
| FFFS 2014:5 | Ch. 16 | Financial reports must break down by transaction category | TCATBAL provides the data source for category-level reporting |

## Edge Cases

1. **INITIALIZE before ADD**: When creating a new record, TRAN-CAT-BAL-RECORD is initialized to zeros before adding the amount (line 504, 508). This ensures no residual data from previous operations.

2. **File status '23' handling**: Status '23' means record not found (VSAM KSDS). The code explicitly accepts both '00' and '23' as valid statuses (line 481), treating not-found as a create trigger rather than an error.

3. **Concurrent updates**: The batch program processes transactions sequentially within a single run, so TCATBAL updates are serialized. However, if multiple batch runs overlap, REWRITE without locking could cause lost updates. The migrated system should use proper concurrency control.

4. **Record size**: The TRAN-CAT-BAL-RECORD is 50 bytes (see CVTRA01Y.cpy): 11-byte account ID + 2-byte type code + 4-byte category code + 11-byte balance (S9(09)V99) + 22-byte filler.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Is the TCATBAL file used for reporting only, or do other programs depend on it?
- Should category balances be reset at cycle boundaries (monthly)?
- Are there any category codes that have special business meaning?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
