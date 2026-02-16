---
id: "TRN-BR-006"
title: "Transaction validation — credit limit check and account expiration"
domain: "transactions"
cobol_source: "CBTRN02C.cbl:370-422"
requirement_id: "TRN-BR-006"
regulations:
  - "PSD2 Art. 97 — Strong customer authentication"
  - "FFFS 2014:5 Ch. 4 §3 — Credit risk management"
  - "EBA Guidelines on creditworthiness assessment"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# TRN-BR-006: Transaction validation — credit limit check and account expiration

## Summary

During daily batch transaction processing (CBTRN02C), each transaction is validated before posting. The validation pipeline checks: (1) card number exists in the cross-reference file, (2) the linked account exists, (3) the transaction does not exceed the account's credit limit, and (4) the transaction was not received after the account expiration date. Transactions failing any check are rejected with a coded reason and written to the daily rejects file.

## Business Logic

### Pseudocode

```
PERFORM 1500-VALIDATE-TRAN:
    PERFORM 1500-A-LOOKUP-XREF:
        READ XREF file by card number from daily transaction
        IF INVALID KEY:
            SET validation-fail-reason = 100
            SET fail-description = "INVALID CARD NUMBER FOUND"
            EXIT validation (transaction will be rejected)
        END-IF

    IF xref lookup passed (fail-reason = 0):
        PERFORM 1500-B-LOOKUP-ACCT:
            READ ACCOUNT file by account ID from XREF
            IF INVALID KEY:
                SET validation-fail-reason = 101
                SET fail-description = "ACCOUNT RECORD NOT FOUND"
                EXIT validation
            END-IF

            ** CREDIT LIMIT CHECK **
            COMPUTE temp-balance = current-cycle-credit
                                 - current-cycle-debit
                                 + transaction-amount

            IF credit-limit >= temp-balance
                CONTINUE (within limit)
            ELSE
                SET validation-fail-reason = 102
                SET fail-description = "OVERLIMIT TRANSACTION"
            END-IF

            ** ACCOUNT EXPIRATION CHECK **
            IF account-expiration-date >= transaction-orig-date(1:10)
                CONTINUE (account not expired)
            ELSE
                SET validation-fail-reason = 103
                SET fail-description = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
            END-IF

    IF validation-fail-reason = 0:
        PERFORM 2000-POST-TRANSACTION
    ELSE:
        INCREMENT reject-count
        PERFORM 2500-WRITE-REJECT-REC
```

### Validation Codes

| Code | Description | Meaning |
|------|-------------|---------|
| 0 | No failure | Transaction is valid |
| 100 | INVALID CARD NUMBER FOUND | Card not in XREF cross-reference file |
| 101 | ACCOUNT RECORD NOT FOUND | Account not in ACCOUNT master file |
| 102 | OVERLIMIT TRANSACTION | Transaction would exceed credit limit |
| 103 | TRANSACTION RECEIVED AFTER ACCT EXPIRATION | Transaction date is after account expiration |

### Credit Limit Calculation

```
temp-balance = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT

IF ACCT-CREDIT-LIMIT >= temp-balance → APPROVED
ELSE → REJECTED (code 102: OVERLIMIT)
```

**Field types:**
- `ACCT-CURR-CYC-CREDIT`: PIC S9(09)V99 (current cycle credits — payments received)
- `ACCT-CURR-CYC-DEBIT`: PIC S9(09)V99 (current cycle debits — charges)
- `DALYTRAN-AMT`: PIC S9(09)V99 (transaction amount, signed)
- `ACCT-CREDIT-LIMIT`: PIC S9(09)V99 (account credit limit)
- `WS-TEMP-BAL`: PIC S9(09)V99 (calculated temporary balance)

**Precision:** All amounts use 2 decimal places (V99). Maximum value: ±999,999,999.99.

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 370-422

```cobol
000370 1500-VALIDATE-TRAN.
000371     PERFORM 1500-A-LOOKUP-XREF.
000372     IF WS-VALIDATION-FAIL-REASON = 0
000373        PERFORM 1500-B-LOOKUP-ACCT
000374     ELSE
000375        CONTINUE
000376     END-IF
000378     EXIT.
000380 1500-A-LOOKUP-XREF.
000382     MOVE DALYTRAN-CARD-NUM TO FD-XREF-CARD-NUM
000383     READ XREF-FILE INTO CARD-XREF-RECORD
000384        INVALID KEY
000385          MOVE 100 TO WS-VALIDATION-FAIL-REASON
000386          MOVE 'INVALID CARD NUMBER FOUND'
000387            TO WS-VALIDATION-FAIL-REASON-DESC
000393 1500-B-LOOKUP-ACCT.
000394     MOVE XREF-ACCT-ID TO FD-ACCT-ID
000395     READ ACCOUNT-FILE INTO ACCOUNT-RECORD
000396        INVALID KEY
000397          MOVE 101 TO WS-VALIDATION-FAIL-REASON
000398          MOVE 'ACCOUNT RECORD NOT FOUND'
000399            TO WS-VALIDATION-FAIL-REASON-DESC
000403          COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                              - ACCT-CURR-CYC-DEBIT
000405                              + DALYTRAN-AMT
000407          IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408            CONTINUE
000409          ELSE
000410            MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411            MOVE 'OVERLIMIT TRANSACTION'
000412              TO WS-VALIDATION-FAIL-REASON-DESC
000413          END-IF
000414          IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415            CONTINUE
000416          ELSE
000417            MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418            MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419              TO WS-VALIDATION-FAIL-REASON-DESC
000420          END-IF
```

## Acceptance Criteria

### Scenario 1: Transaction within credit limit

```gherkin
GIVEN an account with credit limit 10000.00
  AND current cycle credits of 5000.00
  AND current cycle debits of 3000.00
  AND a transaction amount of 1500.00
WHEN the transaction is validated
THEN temp-balance = 5000.00 - 3000.00 + 1500.00 = 3500.00
  AND 10000.00 >= 3500.00 is TRUE
  AND the transaction passes credit limit validation
```

### Scenario 2: Transaction exceeds credit limit

```gherkin
GIVEN an account with credit limit 5000.00
  AND current cycle credits of 4000.00
  AND current cycle debits of 200.00
  AND a transaction amount of 1500.00
WHEN the transaction is validated
THEN temp-balance = 4000.00 - 200.00 + 1500.00 = 5300.00
  AND 5000.00 >= 5300.00 is FALSE
  AND the transaction is rejected with code 102 "OVERLIMIT TRANSACTION"
```

### Scenario 3: Expired account

```gherkin
GIVEN an account with expiration date "2025-12-31"
  AND a transaction with origination timestamp starting with "2026-01-15"
WHEN the transaction is validated
THEN "2025-12-31" >= "2026-01-15" is FALSE
  AND the transaction is rejected with code 103
```

### Scenario 4: Invalid card number

```gherkin
GIVEN a daily transaction with card number not in the XREF file
WHEN the transaction is validated
THEN the transaction is rejected with code 100 "INVALID CARD NUMBER FOUND"
  AND the account lookup is skipped
```

### Scenario 5: Card valid but account missing

```gherkin
GIVEN a daily transaction with a valid card in XREF
  AND the linked account does not exist in the ACCOUNT file
WHEN the transaction is validated
THEN the transaction is rejected with code 101 "ACCOUNT RECORD NOT FOUND"
```

### Scenario 6: Negative transaction amount (credit/payment)

```gherkin
GIVEN an account with credit limit 5000.00
  AND current cycle credits of 4800.00
  AND current cycle debits of 100.00
  AND a transaction amount of -500.00 (credit/payment)
WHEN the transaction is validated
THEN temp-balance = 4800.00 - 100.00 + (-500.00) = 4200.00
  AND 5000.00 >= 4200.00 is TRUE
  AND the transaction passes credit limit validation
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for payment transactions | Card verification through XREF validates payment instrument authorization |
| FFFS 2014:5 | Ch. 4 §3 | Credit institutions must manage credit risk | Credit limit check prevents unauthorized credit extension |
| EBA Guidelines | Creditworthiness | Ongoing monitoring of credit exposures | Real-time balance calculation against credit limit |

## Edge Cases

1. **Non-sequential validation**: The COBOL code checks both credit limit AND expiration in sequence within the NOT INVALID KEY block. If credit limit fails (code 102), the expiration check still runs and can overwrite the fail reason to 103. This means the last failing validation "wins." The migrated system should either fail on the first check or collect all failures.

2. **Date comparison as string**: The expiration check compares `ACCT-EXPIRAION-DATE` (note the typo in the original field name) with `DALYTRAN-ORIG-TS(1:10)` as strings. Both must be in YYYY-MM-DD format for the comparison to work correctly. The migrated system should use proper date comparison.

3. **Credit limit calculation sign convention**: The formula `credit - debit + amount` means a positive amount increases the balance (more usage of credit), while a negative amount (payment/credit) reduces it. This aligns with standard credit card accounting where charges are positive.

4. **Reject file format**: Rejected transactions are written with the original 350-byte record plus an 80-byte validation trailer containing the fail reason code (PIC 9(04)) and description (PIC X(76)).

5. **Return code on rejects**: At program end, if any rejections occurred (WS-REJECT-COUNT > 0), RETURN-CODE is set to 4, signaling a warning condition to the JCL job scheduler. This enables downstream processing to check for rejects.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Should all validation failures be collected or should processing stop on the first failure?
- Is the `ACCT-EXPIRAION-DATE` field name a known typo in the COBOL source?
- What is the business process for handling rejected transactions in the DALYREJS file?
- Are there additional validation rules that should be added (e.g., velocity checks, transaction limits)?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
