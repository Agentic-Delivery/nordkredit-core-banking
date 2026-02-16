---
id: "TRN-BR-007"
title: "Transaction posting with account balance update"
domain: "transactions"
cobol_source: "CBTRN02C.cbl:424-579"
requirement_id: "TRN-BR-007"
regulations:
  - "FFFS 2014:5 Ch. 3 — Accurate accounting records"
  - "FFFS 2014:5 Ch. 16 — Financial reporting accuracy"
  - "PSD2 Art. 94 — Transaction record retention"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# TRN-BR-007: Transaction posting with account balance update

## Summary

After a daily transaction passes all validation checks in CBTRN02C, the posting process performs three operations atomically: (1) updates the transaction category balance file to maintain running totals by account/type/category, (2) updates the account master record to reflect the new current balance and cycle-level debit/credit totals, and (3) writes the transaction record to the TRANSACT master file with a DB2-format processing timestamp. This is the core financial posting logic of the system.

## Business Logic

### Pseudocode

```
PERFORM 2000-POST-TRANSACTION:
    ** Prepare transaction record **
    MOVE daily transaction fields to TRAN-RECORD:
        ID, Type Code, Category Code, Source, Description,
        Amount, Merchant details, Card Number, Origination TS
    Generate DB2-format processing timestamp (YYYY-MM-DD-HH.MM.SS.HH0000)
    MOVE timestamp to TRAN-PROC-TS

    ** Step 1: Update transaction category balance **
    PERFORM 2700-UPDATE-TCATBAL:
        Compose key: Account-ID + Type-Code + Category-Code
        READ TCATBAL file by composite key
        IF record not found:
            CREATE new record with initial balance = transaction amount
        ELSE:
            ADD transaction amount to existing TRAN-CAT-BAL
            REWRITE updated record
        END-IF

    ** Step 2: Update account master balance **
    PERFORM 2800-UPDATE-ACCOUNT-REC:
        ADD transaction amount to ACCT-CURR-BAL
        IF transaction amount >= 0:
            ADD amount to ACCT-CURR-CYC-CREDIT
        ELSE:
            ADD amount to ACCT-CURR-CYC-DEBIT
        END-IF
        REWRITE account record

    ** Step 3: Write transaction to master file **
    PERFORM 2900-WRITE-TRANSACTION-FILE:
        WRITE TRAN-RECORD to TRANSACT file
        IF error → ABEND program
```

### Account Balance Update Logic

```
ACCT-CURR-BAL = ACCT-CURR-BAL + DALYTRAN-AMT

IF DALYTRAN-AMT >= 0:
    ACCT-CURR-CYC-CREDIT = ACCT-CURR-CYC-CREDIT + DALYTRAN-AMT
ELSE:
    ACCT-CURR-CYC-DEBIT = ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT
```

**Important precision note:** All balance fields use PIC S9(09)V99 — signed with 2 decimal places. The ADD operation preserves the sign, so a negative DALYTRAN-AMT correctly decreases the balance.

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 424-579

```cobol
000424 2000-POST-TRANSACTION.
000425     MOVE  DALYTRAN-ID            TO    TRAN-ID
000426     MOVE  DALYTRAN-TYPE-CD       TO    TRAN-TYPE-CD
000427     MOVE  DALYTRAN-CAT-CD        TO    TRAN-CAT-CD
000428     MOVE  DALYTRAN-SOURCE        TO    TRAN-SOURCE
000429     MOVE  DALYTRAN-DESC          TO    TRAN-DESC
000430     MOVE  DALYTRAN-AMT           TO    TRAN-AMT
...
000437     PERFORM Z-GET-DB2-FORMAT-TIMESTAMP
000438     MOVE  DB2-FORMAT-TS          TO    TRAN-PROC-TS
000440     PERFORM 2700-UPDATE-TCATBAL
000441     PERFORM 2800-UPDATE-ACCOUNT-REC
000442     PERFORM 2900-WRITE-TRANSACTION-FILE
...
000545 2800-UPDATE-ACCOUNT-REC.
000547     ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548     IF DALYTRAN-AMT >= 0
000549        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550     ELSE
000551        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552     END-IF
000554     REWRITE FD-ACCTFILE-REC FROM  ACCOUNT-RECORD
000555        INVALID KEY
000556          MOVE 109 TO WS-VALIDATION-FAIL-REASON
000557          MOVE 'ACCOUNT RECORD NOT FOUND'
000558            TO WS-VALIDATION-FAIL-REASON-DESC
000559     END-REWRITE.
...
000562 2900-WRITE-TRANSACTION-FILE.
000563     MOVE 8 TO  APPL-RESULT.
000564     WRITE FD-TRANFILE-REC FROM TRAN-RECORD
000566     IF  TRANFILE-STATUS = '00'
000567         MOVE 0 TO  APPL-RESULT
000568     ELSE
000569         MOVE 12 TO APPL-RESULT
000570     END-IF
...
000692 Z-GET-DB2-FORMAT-TIMESTAMP.
000693     MOVE FUNCTION CURRENT-DATE TO COBOL-TS
000694     MOVE COB-YYYY TO DB2-YYYY
000695     MOVE COB-MM   TO DB2-MM
000696     MOVE COB-DD   TO DB2-DD
000697     MOVE COB-HH   TO DB2-HH
000698     MOVE COB-MIN  TO DB2-MIN
000699     MOVE COB-SS   TO DB2-SS
000700     MOVE COB-MIL  TO DB2-MIL
000701     MOVE '0000'   TO DB2-REST
000702     MOVE '-' TO DB2-STREEP-1 DB2-STREEP-2 DB2-STREEP-3
000703     MOVE '.' TO DB2-DOT-1 DB2-DOT-2 DB2-DOT-3
```

## Acceptance Criteria

### Scenario 1: Post a debit transaction (positive amount)

```gherkin
GIVEN an account with current balance 1000.00
  AND current cycle credit 500.00
  AND current cycle debit 200.00
  AND a transaction amount of +150.00
WHEN the transaction is posted
THEN ACCT-CURR-BAL becomes 1150.00 (1000.00 + 150.00)
  AND ACCT-CURR-CYC-CREDIT becomes 650.00 (500.00 + 150.00)
  AND ACCT-CURR-CYC-DEBIT remains 200.00
  AND the transaction is written to the TRANSACT file
```

### Scenario 2: Post a credit/payment transaction (negative amount)

```gherkin
GIVEN an account with current balance 5000.00
  AND current cycle credit 4000.00
  AND current cycle debit 1000.00
  AND a transaction amount of -500.00
WHEN the transaction is posted
THEN ACCT-CURR-BAL becomes 4500.00 (5000.00 + (-500.00))
  AND ACCT-CURR-CYC-CREDIT remains 4000.00
  AND ACCT-CURR-CYC-DEBIT becomes 500.00 (1000.00 + (-500.00))
  AND the transaction is written to the TRANSACT file
```

### Scenario 3: Processing timestamp generation

```gherkin
GIVEN a transaction is being posted at 2026-02-15 14:30:45.12
WHEN the processing timestamp is generated
THEN TRAN-PROC-TS contains "2026-02-15-14.30.45.120000"
```

### Scenario 4: Transaction file write error causes abend

```gherkin
GIVEN a validated transaction is ready to post
  AND the TRANSACT file write fails
WHEN the write is attempted
THEN the program abends with code 999
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 3 | Credit institutions must maintain accurate accounting records | Balance update ensures account reflects all posted transactions |
| FFFS 2014:5 | Ch. 16 | Financial reporting must be accurate and timely | Cycle-level credit/debit tracking supports periodic financial reporting |
| PSD2 | Art. 94 | Transaction records must be retained for reference | Transaction written to master file with processing timestamp |

## Edge Cases

1. **Non-atomic operation**: The three steps (update TCATBAL, update ACCOUNT, write TRANSACT) are performed sequentially without a transaction scope. If the program abends after updating the account but before writing the transaction, the account balance will be incorrect with no transaction record. The migrated system MUST use database transactions for atomicity.

2. **REWRITE with INVALID KEY on account**: If the REWRITE fails (line 554-559), the validation fail reason is set to 109, but the program does not immediately stop — it continues to write the transaction record. This is a potential data integrity issue.

3. **Zero-amount transactions**: A transaction amount of exactly 0.00 is >= 0, so it would be added to ACCT-CURR-CYC-CREDIT. This is technically correct but unusual.

4. **Processing timestamp precision**: The timestamp format is DB2-compatible (YYYY-MM-DD-HH.MM.SS.HH0000) with millisecond precision from COBOL's CURRENT-DATE function. The last 4 digits are always "0000" (hardcoded). The migrated system should use full microsecond precision.

5. **Transaction category balance auto-creation**: If no TCATBAL record exists for the account/type/category key, a new record is created (2700-A-CREATE-TCATBAL-REC). This is an upsert pattern — read, create if missing, update if exists.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Must the three posting steps be atomic (all-or-nothing)?
- What is the recovery procedure if the program abends mid-posting?
- Is the debit/credit sign convention documented elsewhere? (Positive = charge to card, Negative = payment received)
- Should ACCT-CURR-BAL ever go negative, and if so, what are the business implications?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
