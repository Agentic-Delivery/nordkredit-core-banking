---
id: "bill-br-004"
title: "Transaction category balance accumulation for billing"
domain: "billing"
cobol_source: "CBTRN02C.cbl:467-501"
requirement_id: "BILL-BR-004"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 64"
  - "EU Consumer Credit Directive 2008/48/EC"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-004: Transaction category balance accumulation for billing

## Summary

The billing system maintains running balances at the transaction category level through the `TRAN-CAT-BAL` records, updated during daily batch posting in `CBTRN02C.cbl`. Each combination of account ID, transaction type code, and category code has a separate balance record. When a transaction is posted, the system looks up the corresponding category balance record and adds the transaction amount. If no record exists for the combination, a new one is created. These category-level balances are the foundation for tiered interest calculation — each category balance is charged interest at the rate defined in the corresponding disclosure group record (BILL-BR-001). Extracted from `CBTRN02C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM 2700-UPDATE-TCATBAL:
    Build composite key:
        FD-TRANCAT-KEY = DALYTRAN-ACCT-ID(11) + DALYTRAN-TYPE-CD(2) + DALYTRAN-CAT-CD(4)
            (17-byte key from card cross-reference account ID + transaction type + category)

    READ TCATBALF file by FD-TRANCAT-KEY

    EVALUATE file status:
        WHEN '00' (record found):
            ADD DALYTRAN-AMT TO TRAN-CAT-BAL
            REWRITE TRAN-CAT-BAL-RECORD
            IF REWRITE status NOT '00'
                PERFORM 9999-ABEND (halt processing)
            END-IF

        WHEN '23' (record not found):
            Initialize new TRAN-CAT-BAL-RECORD
            SET TRANCAT-ACCT-ID = account ID
            SET TRANCAT-TYPE-CD = transaction type code
            SET TRANCAT-CD = transaction category code
            SET TRAN-CAT-BAL = DALYTRAN-AMT
            WRITE new TRAN-CAT-BAL-RECORD
            IF WRITE status NOT '00'
                PERFORM 9999-ABEND (halt processing)
            END-IF

        WHEN OTHER (unexpected error):
            PERFORM 9999-ABEND (halt processing)
    END-EVALUATE

RELATIONSHIP TO INTEREST CALCULATION:
    For each TRAN-CAT-BAL-RECORD:
        Look up DIS-GROUP-RECORD by (account's group ID + TRANCAT-TYPE-CD + TRANCAT-CD)
        Interest for category = TRAN-CAT-BAL * DIS-INT-RATE / 100 / 12
            (formula to be confirmed by domain expert — see BILL-BR-001)
```

### Decision Table

| File Status | Condition | Action | Result |
|---|---|---|---|
| '00' | Record exists for this account/type/category | ADD amount to existing balance, REWRITE | Balance updated |
| '23' | No record for this combination | CREATE new record with transaction amount | New category tracking started |
| Other | I/O error | ABEND with code 999 | Batch halted — entire run stops |

### Data Structure (from CVTRA01Y.cpy)

| Field | COBOL Name | PIC | Length | Description |
|---|---|---|---|---|
| Account ID | TRANCAT-ACCT-ID | 9(11) | 11 | Account identifier |
| Type Code | TRANCAT-TYPE-CD | X(02) | 2 | Transaction type code |
| Category Code | TRANCAT-CD | 9(04) | 4 | Transaction category code |
| Balance | TRAN-CAT-BAL | S9(09)V99 | 11 | Running balance for this category |
| Filler | FILLER | X(22) | 22 | Reserved |
| **Total** | | | **50** | |

### Financial Precision

| Field | COBOL PIC | Migrated Type | Notes |
|---|---|---|---|
| TRAN-CAT-BAL | S9(09)V99 | decimal(11,2) | Running category balance |
| DALYTRAN-AMT | S9(09)V99 | decimal(11,2) | Transaction amount being added |

**Critical**: The category balance and transaction amount share the same precision (`S9(09)V99`). Over multiple billing cycles, the running balance could theoretically exceed the 9-integer-digit maximum (999,999,999.99) for high-volume categories. The migrated system should use `decimal(11,2)` to match the COBOL precision, with overflow monitoring.

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 467-501 (category balance update)

```cobol
000467 2700-UPDATE-TCATBAL.
000468
000470           MOVE ACCT-ID              TO TRANCAT-ACCT-ID
000471           MOVE DALYTRAN-TYPE-CD     TO TRANCAT-TYPE-CD
000472           MOVE DALYTRAN-CAT-CD      TO TRANCAT-CD
000473
000475           READ TCATBALF
000476               KEY IS FD-TRANCAT-KEY
000478
000480           EVALUATE FD-TCATBALF-STATUS
000481             WHEN '00'
000482               ADD DALYTRAN-AMT      TO TRAN-CAT-BAL
000483               REWRITE TRAN-CAT-BAL-RECORD
000484               IF FD-TCATBALF-STATUS NOT = '00'
000485                   PERFORM 9999-ABEND-PROGRAM
000486               END-IF
000487             WHEN '23'
000488               INITIALIZE TRAN-CAT-BAL-RECORD
000489               MOVE ACCT-ID          TO TRANCAT-ACCT-ID
000490               MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD
000491               MOVE DALYTRAN-CAT-CD  TO TRANCAT-CD
000492               MOVE DALYTRAN-AMT     TO TRAN-CAT-BAL
000493               WRITE TRAN-CAT-BAL-RECORD
000494               IF FD-TCATBALF-STATUS NOT = '00'
000495                   PERFORM 9999-ABEND-PROGRAM
000496               END-IF
000497             WHEN OTHER
000498               PERFORM 9999-ABEND-PROGRAM
000499           END-EVALUATE
000500
000501           .
```

### VSAM File Organization

| Attribute | Value |
|---|---|
| File | TCATBALF |
| Key | TRANCAT-ACCT-ID + TRANCAT-TYPE-CD + TRANCAT-CD |
| Key Length | 17 bytes |
| Record Length | 50 bytes |
| Access Method | KSDS (key sequential) |
| Open Mode | I-O (read and update) |

## Acceptance Criteria

### Scenario 1: Update existing category balance

```gherkin
GIVEN an account "12345678901" has a category balance record
  AND type code "01", category "0001", balance = 500.00
WHEN a transaction of +100.00 with type "01" and category "0001" is posted
THEN the category balance is updated to 600.00
  AND the record is rewritten to the TCATBALF file
```

### Scenario 2: Create new category balance record

```gherkin
GIVEN an account "12345678901" has no category balance record
  AND type code "02", category "0003"
WHEN a transaction of +250.00 with type "02" and category "0003" is posted
THEN a new TRAN-CAT-BAL-RECORD is created
  AND TRANCAT-ACCT-ID = "12345678901"
  AND TRANCAT-TYPE-CD = "02"
  AND TRANCAT-CD = "0003"
  AND TRAN-CAT-BAL = 250.00
```

### Scenario 3: Negative transaction reduces category balance

```gherkin
GIVEN an account has a category balance of 1000.00 for type "01", category "0001"
WHEN a credit/refund transaction of -200.00 is posted
THEN the category balance is updated to 800.00
```

### Scenario 4: Multiple transactions accumulate within same category

```gherkin
GIVEN an account starts with category balance = 0.00 for type "01", category "0001"
WHEN three transactions are posted: +100.00, +200.00, +50.00
THEN the final category balance is 350.00
  AND each transaction reads the latest balance (updated by prior transactions in same batch)
```

### Scenario 5: File I/O error causes ABEND

```gherkin
GIVEN a transaction is being posted
WHEN the REWRITE or WRITE to TCATBALF fails with an unexpected status
THEN the program ABENDs with code 999
  AND the entire batch run is halted
  AND no further transactions are processed
```

### Scenario 6: Category balance links to disclosure group for interest

```gherkin
GIVEN a category balance record exists for account group "STANDARD1"
  AND type "01", category "0001" with balance 1500.00
  AND the disclosure group (BILL-BR-001) defines rate 18.99 for this combination
WHEN monthly interest is calculated
THEN interest is computed on the 1500.00 balance at the 18.99 rate
  AND the interest amount is precise to 2 decimal places
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 7 | Financial records must be complete and traceable | Category-level balance tracking provides granular audit trail of how charges accumulate by transaction type and category |
| PSD2 | Art. 64 | Transaction data integrity | Each category balance update is validated (file status check) and failures trigger an immediate halt, preventing corrupted balance data |
| EU Consumer Credit Directive | 2008/48/EC Art. 10 | Different rates may apply to different types of credit usage | Category-level balances enable differentiated interest rate application (e.g., different rates for purchases vs. cash advances) |

## Edge Cases

1. **Composite key collision**: The 17-byte key (11 + 2 + 4) uniquely identifies each category balance. If transaction type or category codes are reused with different meanings, the balances would be incorrectly merged. The migrated system must ensure type and category code uniqueness.

2. **INITIALIZE clears all fields**: When creating a new record, `INITIALIZE TRAN-CAT-BAL-RECORD` sets all fields to their default values (spaces for alphanumeric, zeros for numeric). The subsequent MOVE statements then set the key fields and balance. The 22-byte FILLER is zeroed out. The migrated system should ensure nullable columns are handled appropriately.

3. **Batch ABEND leaves partial state**: If the batch ABENDs after updating some category balances but before completing, the VSAM file is left in a partially updated state. There is no rollback mechanism. The migrated system should use database transactions to ensure atomicity.

4. **Category balance can go negative**: If refunds or credits exceed charges in a category, the balance becomes negative. This is valid — it represents a credit position in that category. The migrated system must support negative balances.

5. **Cycle reset for category balances**: The available COBOL source does not show when category balances are reset for a new billing cycle. This is likely coordinated with the account cycle reset (BILL-BR-003) and statement generation. The domain expert should confirm the cycle reset process for category balances.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Are category balances reset at the start of each billing cycle, or do they accumulate indefinitely? (2) How are category balances used in interest calculation — are they multiplied by the disclosure group rate directly, or is there an intermediate calculation? (3) What are the valid transaction type codes and category codes? (4) Is there a batch job that closes billing cycles and resets category balances? (5) What is the recovery procedure when a batch ABENDs mid-run with partially updated category balances?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
