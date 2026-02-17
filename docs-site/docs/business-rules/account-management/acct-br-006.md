---
id: "acct-br-006"
title: "Account expiration date management and enforcement"
domain: "account-management"
cobol_source: "CBTRN02C.cbl:414-420, COCRDUPC.cbl:913-947"
requirement_id: "ACCT-BR-006"
regulations:
  - "FSA FFFS 2014:5 Ch. 4 §3"
  - "PSD2 Art. 97"
  - "GDPR Art. 5(1)(e)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-006: Account expiration date management and enforcement

## Summary

Account expiration is enforced at two levels in the COBOL system: at the card level through expiration date validation in the card update program (COCRDUPC.cbl), and at the account level through batch transaction rejection in the posting program (CBTRN02C.cbl). The expiration date is stored as a string in YYYY-MM-DD format (10 characters) and compared as a string against transaction dates — this works because the ISO date format preserves chronological ordering. Card expiration date components (year, month, day) are validated individually, with year constrained to 1950-2099 and month to 01-12. Account expiration is checked during daily batch posting, and expired accounts have all transactions rejected with code 103.

## Business Logic

### Pseudocode

```
-- Account expiration check during batch posting (CBTRN02C)
PERFORM CHECK-ACCOUNT-EXPIRATION:
    -- Compare account expiry date with transaction origination date
    -- Both in YYYY-MM-DD string format
    IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)
        -- Account not expired, transaction proceeds
        CONTINUE
    ELSE
        -- Account expired, reject transaction
        MOVE 103 TO fail-reason
        MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
            TO fail-reason-description
    END-IF

-- Card expiration date validation (COCRDUPC)
PERFORM VALIDATE-EXPIRY-YEAR:
    IF expiry-year = LOW-VALUES OR SPACES OR ZEROS
        SET input-error = TRUE
        EXIT PARAGRAPH
    END-IF
    IF expiry-year IS NUMERIC
        AND expiry-year >= 1950
        AND expiry-year <= 2099
            SET year-is-valid = TRUE
    ELSE
        SET input-error = TRUE
        MOVE 'VALID YEAR MUST BE BETWEEN 1950 AND 2099'
            TO error-message
    END-IF

PERFORM VALIDATE-EXPIRY-MONTH:
    IF expiry-month = LOW-VALUES OR SPACES OR ZEROS
        SET input-error = TRUE
        EXIT PARAGRAPH
    END-IF
    IF expiry-month IS NUMERIC
        AND expiry-month >= 01
        AND expiry-month <= 12
            SET month-is-valid = TRUE
    ELSE
        SET input-error = TRUE
        MOVE 'VALID MONTH MUST BE BETWEEN 01 AND 12'
            TO error-message
    END-IF
```

### Decision Table — Account Expiration Enforcement

| Account Expiry Date | Transaction Date | Comparison | Result |
|---|---|---|---|
| 2028-12-31 | 2026-02-17 | 2028-12-31 >= 2026-02-17 | Transaction proceeds |
| 2026-02-17 | 2026-02-17 | 2026-02-17 >= 2026-02-17 | Transaction proceeds (same-day is valid) |
| 2025-06-30 | 2026-02-17 | 2025-06-30 >= 2026-02-17 | REJECTED: code 103 |
| NULL/spaces | 2026-02-17 | Undefined | See edge case #3 |

### Decision Table — Card Expiry Year Validation

| Year Value | Numeric? | Range | Outcome |
|-----------|----------|-------|---------|
| "2027" | Yes | 1950-2099 | Valid |
| "1949" | Yes | Below range | Error: "VALID YEAR MUST BE BETWEEN 1950 AND 2099" |
| "2100" | Yes | Above range | Error: "VALID YEAR MUST BE BETWEEN 1950 AND 2099" |
| "ABCD" | No | N/A | Error: not numeric |
| "0000" | Yes | Treated as zeros | Error: blank/zero handling |

### Decision Table — Card Expiry Month Validation

| Month Value | Numeric? | Range | Outcome |
|------------|----------|-------|---------|
| "06" | Yes | 01-12 | Valid |
| "00" | Yes | Below range | Error: blank/zero handling |
| "13" | Yes | Above range | Error: "VALID MONTH MUST BE BETWEEN 01 AND 12" |
| "AB" | No | N/A | Error: not numeric |

## Source COBOL Reference

**Programs:** `CBTRN02C.cbl`, `COCRDUPC.cbl`

### CBTRN02C.cbl — Account expiration enforcement in batch

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                 CONTINUE
000416               ELSE
000417                 MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                 MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                   TO WS-VALIDATION-FAIL-REASON-DESC
000420               END-IF
```
*(Lines 414-420 — string comparison of YYYY-MM-DD formatted dates)*

### COCRDUPC.cbl — Card expiry year validation

```cobol
000913 1250-EDIT-EXPIRY-YEAR.
000914*    Must be between 1950 and 2099
000915     SET FLG-EXPYEAR-NOT-OK      TO TRUE
000916
000917*    Not supplied
000918     IF CCUP-NEW-EXPYEAR   EQUAL LOW-VALUES
000919     OR CCUP-NEW-EXPYEAR   EQUAL SPACES
000920     OR CCUP-NEW-EXPYEAR   EQUAL ZEROS
000921        SET INPUT-ERROR           TO TRUE
000922        SET FLG-EXPYEAR-BLANK     TO TRUE
000923        IF WS-RETURN-MSG-OFF
000924           SET EXP-YEAR-MUST-BE-VALID TO TRUE
000925        END-IF
000926        GO TO  1250-EDIT-EXPIRY-YEAR-EXIT
000927     END-IF
000928
000929     IF CCUP-NEW-EXPYEAR IS NUMERIC
000930       IF CCUP-NEW-EXPYEAR >= 1950
000931         AND CCUP-NEW-EXPYEAR <= 2099
000932            SET FLG-EXPYEAR-ISVALID  TO TRUE
000933       ELSE
000934            SET INPUT-ERROR           TO TRUE
000935            SET FLG-EXPYEAR-NOT-OK    TO TRUE
000936            IF WS-RETURN-MSG-OFF
000937               SET EXP-YEAR-MUST-BE-VALID TO TRUE
000938            END-IF
000939       END-IF
000940     ELSE
000941        SET INPUT-ERROR             TO TRUE
000942        SET FLG-EXPYEAR-NOT-OK      TO TRUE
000943        IF WS-RETURN-MSG-OFF
000944           SET EXP-YEAR-MUST-BE-VALID TO TRUE
000945        END-IF
000946     END-IF
000947     .
```

### COCRDUPC.cbl — Card expiry month validation

```cobol
000877 1260-EDIT-EXPIRY-MONTH.
000878*    Must be between 01 and 12
000879     SET FLG-EXPMON-NOT-OK      TO TRUE
000880
000881*    Not supplied
000882     IF CCUP-NEW-EXPMON   EQUAL LOW-VALUES
000883     OR CCUP-NEW-EXPMON   EQUAL SPACES
000884     OR CCUP-NEW-EXPMON   EQUAL ZEROS
000885        SET INPUT-ERROR           TO TRUE
000886        SET FLG-EXPMON-BLANK      TO TRUE
000887        IF WS-RETURN-MSG-OFF
000888           SET EXP-MON-MUST-BE-VALID TO TRUE
000889        END-IF
000890        GO TO  1260-EDIT-EXPIRY-MONTH-EXIT
000891     END-IF
000892
000893     IF CCUP-NEW-EXPMON IS NUMERIC
000894       IF CCUP-NEW-EXPMON >= 01
000895         AND CCUP-NEW-EXPMON <= 12
000896            SET FLG-EXPMON-ISVALID  TO TRUE
000897       ELSE
000898            SET INPUT-ERROR           TO TRUE
000899            SET FLG-EXPMON-NOT-OK     TO TRUE
000900            IF WS-RETURN-MSG-OFF
000901               SET EXP-MON-MUST-BE-VALID TO TRUE
000902            END-IF
000903       END-IF
000904     ELSE
000905        SET INPUT-ERROR             TO TRUE
000906        SET FLG-EXPMON-NOT-OK       TO TRUE
000907        IF WS-RETURN-MSG-OFF
000908           SET EXP-MON-MUST-BE-VALID TO TRUE
000909        END-IF
000910     END-IF
000911     .
```

### Expiration date structure in COBOL

```cobol
          05 CCUP-OLD-EXPIRAION-DATE.
             25 CCUP-OLD-EXPYEAR             PIC X(4).
             25 CCUP-OLD-EXPMON              PIC X(2).
             25 CCUP-OLD-EXPDAY              PIC X(2).
```
*(COCRDUPC.cbl lines ~296-299 — date broken into year/month/day components, 8 chars without separators)*

## Acceptance Criteria

### Scenario 1: Non-expired account allows transactions

```gherkin
GIVEN an account with expiration date "2028-12-31"
  AND a daily transaction with origination date "2026-02-17"
WHEN the batch validates the transaction
THEN the expiration check passes (2028-12-31 >= 2026-02-17)
  AND the transaction proceeds to the next validation step
```

### Scenario 2: Same-day expiration allows transactions

```gherkin
GIVEN an account with expiration date "2026-02-17"
  AND a daily transaction with origination date "2026-02-17"
WHEN the batch validates the transaction
THEN the expiration check passes (2026-02-17 >= 2026-02-17)
  AND the transaction proceeds (same-day is valid)
```

### Scenario 3: Expired account rejects transactions

```gherkin
GIVEN an account with expiration date "2025-06-30"
  AND a daily transaction with origination date "2026-02-17"
WHEN the batch validates the transaction
THEN the transaction is rejected with code 103
  AND reason "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
```

### Scenario 4: Valid expiry year accepted

```gherkin
GIVEN the user enters expiry year "2027"
WHEN expiry year validation is performed
THEN the year is accepted as valid (within 1950-2099 range)
```

### Scenario 5: Invalid expiry year rejected

```gherkin
GIVEN the user enters expiry year "2100"
WHEN expiry year validation is performed
THEN the year is rejected
  AND the error message indicates year must be between 1950 and 2099
```

### Scenario 6: Valid expiry month accepted

```gherkin
GIVEN the user enters expiry month "06"
WHEN expiry month validation is performed
THEN the month is accepted as valid (within 01-12 range)
```

### Scenario 7: Invalid expiry month rejected

```gherkin
GIVEN the user enters expiry month "13"
WHEN expiry month validation is performed
THEN the month is rejected
  AND the error message indicates month must be between 01 and 12
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 4 §3 | Operational risk — expired instruments must not process new transactions | Account expiration enforcement prevents transactions on expired accounts, reducing operational risk from stale payment instruments |
| PSD2 | Art. 97 | Payment instrument lifecycle management — expired instruments must be blocked | The batch rejection (code 103) automatically blocks transactions on expired accounts without manual intervention |
| GDPR | Art. 5(1)(e) | Storage limitation — data should not be kept longer than necessary | Account expiration dates support data lifecycle management; expired accounts can be flagged for data retention review |

## Edge Cases

1. **EXPIRAION-DATE typo**: The COBOL field name contains a persistent typo ("EXPIRAION" instead of "EXPIRATION"). This typo is present in both the ACCT and CARD record structures. The migrated system should use the correct spelling but must document the mapping.

2. **String comparison for dates**: The COBOL code compares dates as strings (`ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)`). This works because YYYY-MM-DD format preserves chronological ordering. The migrated system should use proper date types (`DateOnly` in .NET) for type safety.

3. **Null/missing expiration date**: The COBOL source does not explicitly handle a null or spaces expiration date in the batch check. If `ACCT-EXPIRAION-DATE` is spaces and the transaction date is a valid date string, the comparison `spaces >= "2026-02-17"` would fail (spaces < digits in EBCDIC), rejecting the transaction. The .NET domain model uses `DateTime?` (nullable) to handle accounts without expiration. The migrated system must define explicit behavior for null expiration.

4. **Day component not validated**: While year (1950-2099) and month (01-12) are validated in the card update program, the day component is NOT validated and is NOT editable by users. Invalid days (e.g., "2027-02-30") could exist in the data. The migrated system should validate the complete date.

5. **Year 2099 limit**: The valid year range (1950-2099) has an upper boundary that will eventually need adjustment. This is a Y2.1K concern — the migrated system should use a wider range or remove the artificial ceiling.

6. **8-char vs 10-char date format**: The card record stores the expiry date in 10-char format with separators (YYYY-MM-DD), but the internal working storage breaks it into 4+2+2 = 8 characters without separators. The batch comparison uses the 10-character format. The migrated system should standardize on a single date representation.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) What should happen when an account has no expiration date (null/spaces) — should it be treated as "never expires" or as an error? (2) Is the day component of the expiration date meaningful, or does expiration only apply at the month/year level (like credit card expiry)? (3) Are there business processes to extend account expiration dates? (4) Should expired accounts be automatically transitioned to a "Closed" status, or can they be reactivated by updating the expiration date? (5) Is the 1950-2099 year range correct for account expiration, or should it be narrower?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
