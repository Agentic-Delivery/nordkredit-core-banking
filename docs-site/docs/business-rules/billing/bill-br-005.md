---
id: "bill-br-005"
title: "Account expiration enforcement in billing"
domain: "billing"
cobol_source: "CBTRN02C.cbl:414-420,COCRDUPC.cbl:113-123"
requirement_id: "BILL-BR-005"
regulations:
  - "FSA FFFS 2014:5 Ch. 4"
  - "PSD2 Art. 64"
  - "GDPR Art. 17"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-005: Account expiration enforcement in billing

## Summary

The billing system enforces account expiration dates during the daily batch transaction posting process in `CBTRN02C.cbl`. After the credit limit check passes, the system compares the account's expiration date against the transaction's origination timestamp. If the transaction date falls after the account expiration date, the transaction is rejected with reason code 103 ("TRANSACTION RECEIVED AFTER ACCT EXPIRATION"). The expiration date field is defined in the card update program `COCRDUPC.cbl` with format YYYY-MM-DD and is validated during card updates. This is a critical billing control that prevents charges from being applied to expired accounts. Extracted from `CBTRN02C.cbl` and `COCRDUPC.cbl`.

## Business Logic

### Pseudocode

```
ACCOUNT EXPIRATION CHECK (in 1500-B-LOOKUP-ACCT, after credit limit check):
    INPUT:
        ACCT-EXPIRAION-DATE  (account expiration date, PIC X(10), format YYYY-MM-DD)
        DALYTRAN-ORIG-TS     (transaction origination timestamp, PIC X(26))

    Extract date portion from transaction timestamp:
        TRAN-DATE = DALYTRAN-ORIG-TS(1:10)    (first 10 characters = YYYY-MM-DD)

    Compare dates as strings:
        IF ACCT-EXPIRAION-DATE >= TRAN-DATE
            Account is NOT expired as of transaction date
            Transaction passes expiration check
        ELSE
            Account IS expired as of transaction date
            SET fail reason = 103
            SET fail description = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
            Transaction is REJECTED
        END-IF

EXPIRATION DATE STRUCTURE (from COCRDUPC.cbl):
    CARD-EXPIRAION-DATE-X:
        CARD-EXPIRY-YEAR   PIC X(4)     (positions 1-4)
        FILLER             PIC X(1)     (position 5, separator '-')
        CARD-EXPIRY-MONTH  PIC X(2)     (positions 6-7)
        FILLER             PIC X(1)     (position 8, separator '-')
        CARD-EXPIRY-DAY    PIC X(2)     (positions 9-10)
    Full format: YYYY-MM-DD
```

### Decision Table

| Account Expiration | Transaction Date | Comparison | Result |
|---|---|---|---|
| 2026-12-31 | 2026-06-15 | 2026-12-31 >= 2026-06-15 | PASS (account valid) |
| 2026-06-30 | 2026-07-01 | 2026-06-30 < 2026-07-01 | REJECT code 103 |
| 2026-06-15 | 2026-06-15 | 2026-06-15 >= 2026-06-15 | PASS (same day is valid) |
| 2025-12-31 | 2026-01-01 | 2025-12-31 < 2026-01-01 | REJECT code 103 |
| 2030-01-01 | 2026-06-15 | 2030-01-01 >= 2026-06-15 | PASS (far future expiry) |

### Expiration Date Validation (from COCRDUPC.cbl)

When the expiration date is updated through the card update screen, the following validation rules apply:

| Component | Valid Range | Error Message |
|---|---|---|
| Expiry Month | 1-12 | "Card expiry month must be between 1 and 12" |
| Expiry Year | 1950-2099 | "Invalid card expiry year" |
| Expiry Day | Not user-editable | Carried from original record |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 414-420 (expiration date check in batch posting)

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                 CONTINUE
000416               ELSE
000417                 MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                 MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                   TO WS-VALIDATION-FAIL-REASON-DESC
000420               END-IF
```

**Program:** `COCRDUPC.cbl`
**Lines:** 113-123 (expiration date field structure)

```cobol
000114           10  CARD-STATUS-X                       PIC X.
000115           10  CARD-EXPIRAION-DATE-X               PIC X(10).
000116           10  FILLER REDEFINES CARD-EXPIRAION-DATE-X.
000117               20 CARD-EXPIRY-YEAR                 PIC X(4).
000118               20 FILLER                           PIC X(1).
000119               20 CARD-EXPIRY-MONTH                PIC X(2).
000120               20 FILLER                           PIC X(1).
000121               20 CARD-EXPIRY-DAY                  PIC X(2).
000122           10  CARD-EXPIRAION-DATE-N REDEFINES
000123               CARD-EXPIRAION-DATE-X               PIC 9(10).
```

### Note on Field Name Typo

The COBOL field name `ACCT-EXPIRAION-DATE` (and `CARD-EXPIRAION-DATE`) contains a typo — "EXPIRAION" instead of "EXPIRATION". This typo is consistent across all programs and copybooks. The migrated system should use the correct spelling but must maintain a mapping for data migration purposes.

## Acceptance Criteria

### Scenario 1: Transaction on a valid (non-expired) account

```gherkin
GIVEN an account with expiration date "2026-12-31"
WHEN a transaction with origination date "2026-06-15" is processed
THEN the expiration check passes
  AND the transaction proceeds to posting
```

### Scenario 2: Transaction on an expired account

```gherkin
GIVEN an account with expiration date "2026-06-30"
WHEN a transaction with origination date "2026-07-01" is processed
THEN the transaction is rejected with code 103
  AND the rejection description is "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
```

### Scenario 3: Transaction on the expiration date itself

```gherkin
GIVEN an account with expiration date "2026-06-15"
WHEN a transaction with origination date "2026-06-15" is processed
THEN the expiration check passes (same-day transactions are allowed)
  AND the comparison uses >= (greater-than-or-equal)
```

### Scenario 4: Both overlimit and expired conditions

```gherkin
GIVEN an account that is both over the credit limit AND expired
WHEN the transaction is validated
THEN the fail reason is 103 (expired) because:
  - The credit limit check (code 102) runs first at lines 403-413
  - The expiration check (code 103) runs second at lines 414-420
  - Code 103 overwrites code 102
  AND only the expiration rejection is recorded
```

### Scenario 5: String comparison preserves date ordering

```gherkin
GIVEN dates stored in YYYY-MM-DD format as PIC X(10) strings
WHEN "2025-12-31" is compared with "2026-01-01"
THEN "2025-12-31" < "2026-01-01" in string comparison
  AND the chronological ordering is preserved by the ISO date format
```

### Scenario 6: Expiration date validation on card update

```gherkin
GIVEN a user is updating a card's expiration date
WHEN the month is set to "13"
THEN the error "Card expiry month must be between 1 and 12" is displayed
  AND the update is rejected
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 4 | Credit institutions must manage operational risk including expired accounts | Rejecting transactions on expired accounts prevents unauthorized billing and reduces operational risk |
| PSD2 | Art. 64 | Payment transactions must be authorized by valid payment instruments | An expired account/card is no longer a valid payment instrument; rejecting post-expiration transactions enforces this |
| GDPR | Art. 17 | Right to erasure — data should not be processed beyond the purpose period | Account expiration provides a mechanism to cease transaction processing, supporting data lifecycle management |

## Edge Cases

1. **String comparison for dates**: The COBOL code compares expiration dates as PIC X(10) strings. This works correctly because the YYYY-MM-DD format preserves lexicographic ordering. However, if dates are stored in a different format (e.g., MM/DD/YYYY), the comparison would fail. The migrated system should use native date types for comparison.

2. **Expiration date typo in field name**: The field is consistently named `EXPIRAION` (missing 'T') across all COBOL programs. The data dictionary and migration mapping must account for this.

3. **Year 2099 boundary**: The card update validation allows years up to 2099. Accounts with far-future expiration dates (e.g., 2099-12-31) are effectively non-expiring. The migrated system should consider whether a maximum expiry window should be enforced.

4. **Expiry day is not user-modifiable**: The card update program carries the day forward from the original record, but users can change month and year. This means the actual expiration day depends on when the card was originally issued. The migrated system should clarify whether the day should always be the last day of the month.

5. **Timestamp extraction**: The transaction date is extracted as `DALYTRAN-ORIG-TS(1:10)`, taking the first 10 characters of a 26-character timestamp. If the timestamp is malformed (e.g., embedded spaces), the date comparison may produce unexpected results. The migrated system should validate timestamp format before comparison.

6. **Sequential validation**: The expiration check is the last validation step (after card lookup, account lookup, and credit limit check). If the expiration check fails, it overwrites any previous failure reason. In the migrated system, all failures should be captured simultaneously.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Should transactions be allowed on the expiration date itself (current behavior allows it via >= comparison)? (2) Is the expiry day always the last day of the expiry month, or does it depend on original issuance? (3) Is there a grace period after expiration before the account is fully blocked? (4) What happens to pending/recurring billing when an account expires — are scheduled charges also rejected? (5) Should the migrated system use native date types instead of string comparison for expiration checks?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
