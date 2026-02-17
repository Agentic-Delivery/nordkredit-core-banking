---
id: "lnd-br-009"
title: "Account expiration enforcement for lending"
domain: "lending"
cobol_source: "CBTRN02C.cbl:414-420"
requirement_id: "LND-BR-009"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
  - "PSD2 Art. 64"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# LND-BR-009: Account expiration enforcement for lending

## Summary

The account expiration enforcement rule prevents transactions from being posted to accounts that have passed their expiration (maturity) date. In the COBOL system, this check is performed during daily batch transaction posting (CBTRN02C) by comparing the account's expiration date field (`ACCT-EXPIRAION-DATE`) against the transaction's origination timestamp. If the transaction date is after the account expiration date, the transaction is rejected with validation failure code 103 ("TRANSACTION RECEIVED AFTER ACCT EXPIRATION").

In the lending context, the expiration date represents the loan maturity date — the final date by which the loan must be fully repaid. After maturity, no new drawdowns should be permitted on the credit facility. The check uses a string comparison of dates in YYYY-MM-DD format, which provides correct ordering for date values in this format. Note: the COBOL field contains the historical typo "EXPIRAION" (missing 'T') which is preserved across all programs.

## Business Logic

### Pseudocode

```
PERFORM EXPIRATION-CHECK (within 1500-B-LOOKUP-ACCT):
    -- Account record has been read successfully
    -- Credit limit check has already been performed

    -- Compare account expiration date against transaction date
    IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
        -- Account has not expired relative to the transaction date
        CONTINUE (transaction passes expiration check)
    ELSE
        -- Transaction date is after account expiration
        SET validation-fail-reason = 103
        SET fail-description = 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
    END-IF

    -- NOTE: String comparison works because both dates are in YYYY-MM-DD format
    -- DALYTRAN-ORIG-TS (1:10) extracts the first 10 characters (date portion)
    -- from the full timestamp field
```

### Decision Table

| Account Expiration Date | Transaction Date | Comparison Result | Outcome |
|------------------------|-----------------|-------------------|---------|
| 2027-12-31 | 2026-06-15 | 2027-12-31 >= 2026-06-15 = TRUE | Transaction accepted |
| 2026-06-30 | 2026-06-30 | 2026-06-30 >= 2026-06-30 = TRUE | Transaction accepted (boundary: same day) |
| 2026-05-31 | 2026-06-01 | 2026-05-31 >= 2026-06-01 = FALSE | Transaction rejected (code 103) |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 414-420 (expiration check)

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                  CONTINUE
000416                ELSE
000417                  MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                  MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                    TO WS-VALIDATION-FAIL-REASON-DESC
000420                END-IF
```
*(Lines 414-420, CBTRN02C.cbl — the account expiration enforcement: compares the 10-character date portion of the account expiration against the 10-character date portion of the transaction origination timestamp)*

**Context — this check follows the credit limit check:**

```cobol
000407                IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408                  CONTINUE
000409                ELSE
000410                  MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411                  MOVE 'OVERLIMIT TRANSACTION'
000412                    TO WS-VALIDATION-FAIL-REASON-DESC
000413                END-IF
000414                IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
```
*(Lines 407-414 — the expiration check runs sequentially after the credit limit check, both within the 1500-B-LOOKUP-ACCT paragraph)*

## Acceptance Criteria

### Scenario 1: Transaction before maturity date is accepted

```gherkin
GIVEN a loan account with expiration date "2028-12-31"
WHEN a transaction with origination date "2026-06-15" is submitted
THEN the expiration check passes ("2028-12-31" >= "2026-06-15")
  AND the transaction proceeds to posting
```

### Scenario 2: Transaction on maturity date is accepted

```gherkin
GIVEN a loan account with expiration date "2026-06-30"
WHEN a transaction with origination date "2026-06-30" is submitted
THEN the expiration check passes ("2026-06-30" >= "2026-06-30" — boundary case)
  AND the transaction proceeds to posting
```

### Scenario 3: Transaction after maturity date is rejected

```gherkin
GIVEN a loan account with expiration date "2026-05-31"
WHEN a transaction with origination date "2026-06-01" is submitted
THEN the expiration check fails ("2026-05-31" >= "2026-06-01" is FALSE)
  AND the transaction is rejected with code 103
  AND the rejection reason is "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
  AND the transaction is written to the daily rejects file
```

### Scenario 4: Repayment after maturity is still rejected

```gherkin
GIVEN a loan account with expiration date "2026-05-31"
  AND the account has an outstanding balance of 10000.00
WHEN a repayment of -5000.00 with origination date "2026-06-15" is submitted
THEN the expiration check fails (the check does not distinguish charges from repayments)
  AND the repayment is rejected with code 103
```

### Scenario 5: Overlimit AND expired both trigger rejection

```gherkin
GIVEN a loan account that is both overlimit and expired
WHEN a transaction is submitted
THEN the overlimit check sets code 102
  AND the expiration check overwrites with code 103
  AND the final rejection code is 103 (last check wins)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — credit facilities must not be extended beyond their authorized term | The expiration check prevents new credit utilization after the loan maturity date |
| Consumer Credit Directive | Art. 10(2)(h) | Credit agreement must specify duration of the credit agreement | The expiration date field enforces the agreed duration by blocking transactions after the end date |
| PSD2 | Art. 64 | Transaction data integrity — only authorized transactions should be executed | Transactions on expired accounts are unauthorized and are correctly rejected with an audit trail |

## Edge Cases

1. **Repayment rejection after maturity**: The COBOL code rejects ALL transactions after expiration, including repayments. This is a potential issue — borrowers should be able to make repayments after maturity to settle outstanding balances. The migrated system should allow repayments (negative amounts) on expired accounts while blocking new drawdowns.

2. **Validation order dependency**: The expiration check (code 103) runs after the credit limit check (code 102). If both fail, only code 103 is recorded because it overwrites code 102. The migrated system should collect all validation failures rather than overwriting.

3. **Date format dependency**: The string comparison works correctly only because both dates use YYYY-MM-DD format. If either date used a different format (e.g., DD-MM-YYYY), the comparison would produce incorrect results. The migrated system should use proper date types instead of string comparison.

4. **Timestamp truncation**: `DALYTRAN-ORIG-TS (1:10)` extracts only the date portion from the transaction's full timestamp. This means the check is date-level, not timestamp-level. A transaction at 23:59:59 on the expiration date passes, but a transaction at 00:00:01 the next day fails.

5. **Maturity date extension**: If a loan is renewed or extended, the ACCT-EXPIRAION-DATE must be updated. The mechanism for updating this field (and whether it requires re-origination or a simple field update) is not visible in the available COBOL source.

6. **Timezone considerations**: The COBOL system runs on z/OS in the bank's local timezone (CET/CEST for Sweden). The migrated Azure system must ensure timezone-consistent date comparisons to avoid rejecting transactions that should pass (or vice versa) near midnight.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Is it intentional that repayments are rejected after account expiration, or is this a defect that should be corrected in the migrated system? (2) What is the process for extending/renewing a loan maturity date? (3) Are there different expiration behaviors for revolving credit (which may auto-renew) vs. term loans (which mature)? (4) Is there a grace period after expiration before the account is fully blocked?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
