---
id: "trn-br-005"
title: "Daily transaction posting with validation and balance updates"
domain: "transactions"
cobol_source: "CBTRN02C.cbl:193-579"
requirement_id: "TRN-BR-005"
regulations:
  - "PSD2 Art. 64"
  - "PSD2 Art. 73"
  - "FSA FFFS 2014:5"
  - "AML 2017:11"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# TRN-BR-005: Daily transaction posting with validation and balance updates

## Summary

CBTRN02C is the core batch posting program that processes the daily transaction file, validates each transaction against business rules, posts valid transactions to the TRANSACT file, updates account balances, maintains transaction category balance records, and writes rejected transactions to a rejects file. This is the most financially critical program in the transaction processing pipeline — it performs the actual balance calculations that affect account standing. Extracted from `CBTRN02C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM MAIN:
    Open all files:
        DALYTRAN (input) - daily transaction feed
        TRANSACT (output) - posted transaction file
        XREFFILE (input) - card cross-reference
        DALYREJS (output) - rejected transactions
        ACCTFILE (I-O) - account master (read and update)
        TCATBALF (I-O) - transaction category balance (read and update)

    PERFORM UNTIL end-of-file = 'Y'
        READ next DALYTRAN record
        ADD 1 TO WS-TRANSACTION-COUNT

        Initialize validation fail reason to 0
        PERFORM 1500-VALIDATE-TRAN

        IF validation passed (fail reason = 0)
            PERFORM 2000-POST-TRANSACTION
        ELSE
            ADD 1 TO WS-REJECT-COUNT
            PERFORM 2500-WRITE-REJECT-REC
        END-IF
    END-PERFORM

    Close all files
    DISPLAY transaction and reject counts
    IF WS-REJECT-COUNT > 0
        SET RETURN-CODE = 4 (warning)
    END-IF
    GOBACK

1500-VALIDATE-TRAN:
    Step 1: PERFORM 1500-A-LOOKUP-XREF
        Read XREF file by card number
        IF card not found: fail reason = 100
            Description: "INVALID CARD NUMBER FOUND"

    IF step 1 passed:
    Step 2: PERFORM 1500-B-LOOKUP-ACCT
        Read account file by account ID (from XREF)
        IF account not found: fail reason = 101
            Description: "ACCOUNT RECORD NOT FOUND"

        IF account found:
            Calculate projected balance:
            WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
                        - ACCT-CURR-CYC-DEBIT
                        + DALYTRAN-AMT

            IF ACCT-CREDIT-LIMIT < WS-TEMP-BAL:
                fail reason = 102
                Description: "OVERLIMIT TRANSACTION"

            IF ACCT-EXPIRAION-DATE < DALYTRAN-ORIG-TS(1:10):
                fail reason = 103
                Description: "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"

2000-POST-TRANSACTION:
    Map all fields from DALYTRAN-RECORD to TRAN-RECORD
    Generate processing timestamp (DB2 format: YYYY-MM-DD-HH.MM.SS.HH0000)

    PERFORM 2700-UPDATE-TCATBAL
        (update transaction category balance)
    PERFORM 2800-UPDATE-ACCOUNT-REC
        (update account balances)
    PERFORM 2900-WRITE-TRANSACTION-FILE
        (write to posted transaction file)

2700-UPDATE-TCATBAL:
    Build key: ACCT-ID + TYPE-CD + CAT-CD
    READ TCATBAL file by key

    IF record not found (status '23'):
        Create new TRAN-CAT-BAL record
        Initialize with transaction amount
        WRITE new record
    ELSE IF record found:
        ADD DALYTRAN-AMT TO TRAN-CAT-BAL
        REWRITE updated record

2800-UPDATE-ACCOUNT-REC:
    ADD DALYTRAN-AMT TO ACCT-CURR-BAL

    IF DALYTRAN-AMT >= 0 (credit/positive)
        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
    ELSE (debit/negative)
        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
    END-IF

    REWRITE account record
```

### Validation Rules

| Code | Reason | Description | Impact |
|---|---|---|---|
| 100 | Card not in XREF | Card number from transaction not found in cross-reference | Transaction rejected |
| 101 | Account not found | Account ID from cross-reference not found in account master | Transaction rejected |
| 102 | Over credit limit | Projected balance exceeds account credit limit | Transaction rejected |
| 103 | Expired account | Transaction date is after account expiration date | Transaction rejected |

### Balance Calculation Rules

**Credit limit check:**
```
WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT
IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL → Transaction passes
IF ACCT-CREDIT-LIMIT <  WS-TEMP-BAL → OVERLIMIT (rejected, code 102)
```

**Account balance update (after posting):**
```
ACCT-CURR-BAL += DALYTRAN-AMT  (always, regardless of sign)

IF DALYTRAN-AMT >= 0:
    ACCT-CURR-CYC-CREDIT += DALYTRAN-AMT
ELSE:
    ACCT-CURR-CYC-DEBIT += DALYTRAN-AMT
```

**Category balance update:**
```
Key = ACCT-ID(11) + TYPE-CD(2) + CAT-CD(4)
TRAN-CAT-BAL += DALYTRAN-AMT
(Create new record if key does not exist)
```

### Financial Precision

| Field | COBOL PIC | Precision | Notes |
|---|---|---|---|
| DALYTRAN-AMT | S9(09)V99 | Signed, 9 integer, 2 decimal | Input amount (from daily transaction record) |
| ACCT-CURR-BAL | S9(10)V99 | Signed, 10 integer, 2 decimal | Running account balance (from CVACT01Y.cpy) |
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | Signed, 10 integer, 2 decimal | Current cycle credits (from CVACT01Y.cpy) |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | Signed, 10 integer, 2 decimal | Current cycle debits (from CVACT01Y.cpy) |
| ACCT-CREDIT-LIMIT | S9(10)V99 | Signed, 10 integer, 2 decimal | Account credit limit (from CVACT01Y.cpy) |
| WS-TEMP-BAL | S9(09)V99 | Signed, 9 integer, 2 decimal | Projected balance (working storage — note: smaller than account fields) |
| TRAN-CAT-BAL | S9(09)V99 | Signed, 9 integer, 2 decimal | Category running balance (from CVTRA01Y.cpy) |

**Critical**: Financial calculations use COBOL fixed-point arithmetic. Account fields use `S9(10)V99` (10 integer digits) while transaction and working storage fields use `S9(09)V99` (9 integer digits). The WS-TEMP-BAL field is smaller than the account balance fields it computes from, creating a potential truncation risk for accounts with balances exceeding 999,999,999.99. There is no rounding — COBOL truncates at the implied decimal point. The migrated system MUST use `decimal(12,2)` for account fields and `decimal(11,2)` for transaction fields (or equivalent) to preserve exact arithmetic. Floating-point types (float/double) are NOT acceptable.

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 193-234 (main loop), 370-422 (validation), 424-444 (post transaction), 467-501 (update category balance), 545-560 (update account), 562-579 (write transaction)

```cobol
000403               COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                   - ACCT-CURR-CYC-DEBIT
000405                                   + DALYTRAN-AMT
000407               IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408                 CONTINUE
000409               ELSE
000410                 MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411                 MOVE 'OVERLIMIT TRANSACTION'
000412                   TO WS-VALIDATION-FAIL-REASON-DESC
000413               END-IF
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                 CONTINUE
000416               ELSE
000417                 MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                 MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                   TO WS-VALIDATION-FAIL-REASON-DESC
000420               END-IF
```

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```

### Reject Record Format

| Field | Positions | Length | Description |
|---|---|---|---|
| Transaction data | 1-350 | 350 | Complete DALYTRAN-RECORD |
| Fail reason code | 351-354 | 4 | Numeric code (100-103) |
| Fail reason description | 355-430 | 76 | Text description |

## Acceptance Criteria

### Scenario 1: Successful transaction posting

GIVEN a daily transaction with a valid card number
  AND the linked account exists and has sufficient credit limit
  AND the account is not expired
WHEN the batch program processes the transaction
THEN the transaction is written to the TRANSACT file
  AND the account current balance is updated
  AND the cycle credit or debit is updated based on amount sign
  AND the transaction category balance is updated

### Scenario 2: Invalid card number rejection

GIVEN a daily transaction with a card number not in the XREF file
WHEN the batch program validates the transaction
THEN the transaction is written to the rejects file with code 100
  AND the reject count is incremented

### Scenario 3: Over credit limit rejection

GIVEN a daily transaction where the projected balance exceeds the credit limit
  AND projected balance = current cycle credit - current cycle debit + transaction amount
WHEN the batch program validates the transaction
THEN the transaction is written to the rejects file with code 102
  AND the reject count is incremented

### Scenario 4: Expired account rejection

GIVEN a daily transaction where the origination date is after the account expiration date
  AND comparison uses first 10 characters of DALYTRAN-ORIG-TS (YYYY-MM-DD)
WHEN the batch program validates the transaction
THEN the transaction is written to the rejects file with code 103

### Scenario 5: Credit vs debit classification

GIVEN a posted transaction with amount >= 0
WHEN the account balances are updated
THEN the amount is added to ACCT-CURR-CYC-CREDIT

GIVEN a posted transaction with amount < 0
WHEN the account balances are updated
THEN the amount is added to ACCT-CURR-CYC-DEBIT

### Scenario 6: New transaction category balance record

GIVEN a transaction with a type/category combination not yet tracked
WHEN the category balance update is performed
THEN a new TRAN-CAT-BAL record is created
  AND initialized with the transaction amount

### Scenario 7: Existing transaction category balance update

GIVEN a transaction with an existing type/category balance record
WHEN the category balance update is performed
THEN the transaction amount is added to the existing TRAN-CAT-BAL

### Scenario 8: Batch return code with rejections

GIVEN the batch run completes with WS-REJECT-COUNT > 0
WHEN the program terminates
THEN RETURN-CODE is set to 4 (warning condition)
  AND total processed and rejected counts are displayed

### Scenario 9: Processing timestamp generation

GIVEN a transaction is being posted
WHEN the processing timestamp is generated
THEN it uses FUNCTION CURRENT-DATE converted to DB2 format
  AND format is YYYY-MM-DD-HH.MM.SS.HH0000

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 64 | Payment transaction data integrity | Validates card, account, credit limit, and expiration before posting |
| PSD2 | Art. 73 | Refund and credit rules | Credit/debit classification determines cycle balance updates |
| FSA FFFS 2014:5 | Ch. 7 | Financial calculation accuracy | Fixed-point arithmetic with S9(09)V99 ensures no floating-point errors |
| AML 2017:11 | Para. 3 | Transaction monitoring | Reject file with coded reasons provides audit trail for compliance review |
| DORA | Art. 11 | ICT risk management | ABEND handling and return codes support operational resilience monitoring |

## Edge Cases

1. **Validation order matters**: If both overlimit (102) and expired (103) conditions are true, code 103 overwrites 102 because the checks are sequential. The migrated system should capture all validation failures, not just the last one.

2. **Negative amounts in credit limit check**: The formula `CYC-CREDIT - CYC-DEBIT + AMT` means negative amounts (debits) reduce the projected balance, making it less likely to exceed the limit. This is correct business logic — debits free up credit.

3. **Account expiration date comparison**: The comparison `ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)` compares date strings. Note the typo "EXPIRAION" in the COBOL field name. The comparison works because YYYY-MM-DD format preserves chronological ordering in string comparison.

4. **DB2 timestamp format**: The processing timestamp is generated from FUNCTION CURRENT-DATE and reformatted to DB2 format with dashes and dots. The millisecond portion is truncated to 2 digits with '0000' padded. The migrated system should use native datetime types.

5. **ABEND on file errors**: Any file I/O error triggers an ABEND with code 999. This means a single bad write stops the entire batch. The migrated system should consider whether to implement transaction-level error handling with continue-on-error.

6. **Account file opened I-O**: Unlike CBTRN01C which opens files INPUT only, CBTRN02C opens ACCTFILE and TCATBALF in I-O mode because it updates balances. The migrated system must use database transactions with appropriate isolation levels.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Confirm the credit limit check formula: is `CYC-CREDIT - CYC-DEBIT + AMT` the correct projected balance calculation? (2) Should all validation failures be captured (not just the last one)? (3) Is the "EXPIRAION" typo in the field name carried through to the data dictionary? (4) What is the expected behavior when the batch ABENDs mid-run — are partial updates rolled back? (5) Should the migrated system use database transactions to ensure atomicity of the post+balance-update+category-update sequence?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
