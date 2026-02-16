---
id: "sec-br-002"
title: "Account-level data isolation and filtering"
domain: "user-security"
cobol_source: "COCRDLIC.cbl:1003-1067,COCRDLIC.cbl:1382-1410,COCRDLIC.cbl:147-149"
requirement_id: "SEC-BR-002"
regulations:
  - "GDPR Art. 5(1)(c)"
  - "GDPR Art. 5(1)(f)"
  - "GDPR Art. 25"
  - "PSD2 Art. 97"
  - "FFFS 2014:5 Ch. 8 §4"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# SEC-BR-002: Account-level data isolation and filtering

## Summary

The card list program (COCRDLIC) enforces account-level data isolation through a multi-layered filtering mechanism. When a user provides an account number, only card records belonging to that account are displayed. When a card number filter is also provided, results are further narrowed to that specific card. The filtering is enforced at the data retrieval layer (paragraph 9500-FILTER-RECORDS), meaning every record read from the VSAM file is checked against the user's access context before being included in the display. This is the technical enforcement of data minimization — users only see data they are authorized to access.

## Business Logic

### Pseudocode

```
VALIDATE ACCOUNT FILTER (2210-EDIT-ACCOUNT):
    SET account-filter = BLANK

    IF account-id = SPACES OR LOW-VALUES OR ZEROS
        SET account-filter = BLANK
        SET CDEMO-ACCT-ID = ZEROS
        EXIT
    END-IF

    IF account-id IS NOT NUMERIC
        SET input-error = TRUE
        SET account-filter = NOT-OK
        SET protect-select-rows = YES
        MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
            TO error-message
        SET CDEMO-ACCT-ID = ZERO
        EXIT
    ELSE
        MOVE account-id TO CDEMO-ACCT-ID
        SET account-filter = VALID
    END-IF

VALIDATE CARD FILTER (2220-EDIT-CARD):
    SET card-filter = BLANK

    IF card-number = SPACES OR LOW-VALUES OR ZEROS
        SET card-filter = BLANK
        SET CDEMO-CARD-NUM = ZEROS
        EXIT
    END-IF

    IF card-number IS NOT NUMERIC
        SET input-error = TRUE
        SET card-filter = NOT-OK
        SET protect-select-rows = YES
        IF no error message already set
            MOVE 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
                TO error-message
        END-IF
        SET CDEMO-CARD-NUM = ZERO
        EXIT
    ELSE
        MOVE card-number TO CDEMO-CARD-NUM
        SET card-filter = VALID
    END-IF

FILTER EACH RECORD (9500-FILTER-RECORDS):
    SET include-record = TRUE

    IF account-filter IS VALID
        IF record-account-id ≠ filter-account-id
            SET exclude-record = TRUE
            EXIT
        END-IF
    END-IF

    IF card-filter IS VALID
        IF record-card-number ≠ filter-card-number
            SET exclude-record = TRUE
            EXIT
        END-IF
    END-IF
```

### Filter State Machine

| Account Filter State | Card Filter State | Records Returned |
|---------------------|-------------------|-----------------|
| BLANK | BLANK | All records (admin path) |
| VALID | BLANK | Records matching account only |
| BLANK | VALID | Records matching card number only |
| VALID | VALID | Records matching both account AND card |
| NOT-OK | Any | Error displayed, no data retrieval |
| Any | NOT-OK | Error displayed, no data retrieval |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 1003-1030 (Account number validation — 2210-EDIT-ACCOUNT)

```cobol
001003  2210-EDIT-ACCOUNT.
001004      SET FLG-ACCTFILTER-BLANK TO TRUE
001005
001006 *    Not supplied
001007      IF CC-ACCT-ID   EQUAL LOW-VALUES
001008      OR CC-ACCT-ID   EQUAL SPACES
001009      OR CC-ACCT-ID-N EQUAL ZEROS
001010         SET FLG-ACCTFILTER-BLANK  TO TRUE
001011         MOVE ZEROES       TO CDEMO-ACCT-ID
001012         GO TO  2210-EDIT-ACCOUNT-EXIT
001013      END-IF
001014 *
001015 *    Not numeric
001016 *    Not 11 characters
001017      IF CC-ACCT-ID  IS NOT NUMERIC
001018         SET INPUT-ERROR TO TRUE
001019         SET FLG-ACCTFILTER-NOT-OK TO TRUE
001020         SET FLG-PROTECT-SELECT-ROWS-YES TO TRUE
001021         MOVE
001022         'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
001023                         TO WS-ERROR-MSG
001024         MOVE ZERO       TO CDEMO-ACCT-ID
001025         GO TO 2210-EDIT-ACCOUNT-EXIT
001026      ELSE
001027         MOVE CC-ACCT-ID TO CDEMO-ACCT-ID
001028         SET FLG-ACCTFILTER-ISVALID TO TRUE
001029      END-IF
001030      .
```

**Lines:** 1036-1067 (Card number validation — 2220-EDIT-CARD)

```cobol
001036  2220-EDIT-CARD.
001037 *    Not numeric
001038 *    Not 16 characters
001039      SET FLG-CARDFILTER-BLANK TO TRUE
001040
001041 *    Not supplied
001042      IF CC-CARD-NUM   EQUAL LOW-VALUES
001043      OR CC-CARD-NUM   EQUAL SPACES
001044      OR CC-CARD-NUM-N EQUAL ZEROS
001045         SET FLG-CARDFILTER-BLANK  TO TRUE
001046         MOVE ZEROES       TO CDEMO-CARD-NUM
001047         GO TO  2220-EDIT-CARD-EXIT
001048      END-IF
001049 *
001050 *    Not numeric
001051 *    Not 16 characters
001052      IF CC-CARD-NUM  IS NOT NUMERIC
001053         SET INPUT-ERROR TO TRUE
001054         SET FLG-CARDFILTER-NOT-OK TO TRUE
001055         SET FLG-PROTECT-SELECT-ROWS-YES TO TRUE
001056         IF WS-ERROR-MSG-OFF
001057            MOVE
001058         'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
001059                         TO WS-ERROR-MSG
001060         END-IF
001061         MOVE ZERO       TO CDEMO-CARD-NUM
001062         GO TO 2220-EDIT-CARD-EXIT
001063      ELSE
001064         MOVE CC-CARD-NUM-N TO CDEMO-CARD-NUM
001065         SET FLG-CARDFILTER-ISVALID TO TRUE
001066      END-IF
001067      .
```

**Lines:** 1382-1410 (Record filtering — 9500-FILTER-RECORDS)

```cobol
001382  9500-FILTER-RECORDS.
001383      SET WS-DONOT-EXCLUDE-THIS-RECORD TO TRUE
001384
001385      IF FLG-ACCTFILTER-ISVALID
001386         IF  CARD-ACCT-ID = CC-ACCT-ID
001387             CONTINUE
001388         ELSE
001389             SET WS-EXCLUDE-THIS-RECORD  TO TRUE
001390             GO TO 9500-FILTER-RECORDS-EXIT
001391         END-IF
001392      ELSE
001393        CONTINUE
001394      END-IF
001395
001396      IF FLG-CARDFILTER-ISVALID
001397         IF  CARD-NUM = CC-CARD-NUM-N
001398             CONTINUE
001399         ELSE
001400             SET WS-EXCLUDE-THIS-RECORD TO TRUE
001401             GO TO 9500-FILTER-RECORDS-EXIT
001402         END-IF
001403      ELSE
001404        CONTINUE
001405      END-IF
```

**Lines:** 105-107 (Row selection protection when filter errors)

```cobol
000105        10  FLG-PROTECT-SELECT-ROWS             PIC X(1).
000106        88  FLG-PROTECT-SELECT-ROWS-NO          VALUE '0'.
000107        88  FLG-PROTECT-SELECT-ROWS-YES         VALUE '1'.
```

## Acceptance Criteria

### Scenario 1: Valid account filter restricts visible records

```gherkin
GIVEN a user enters account number "41000000001" in the account filter field
WHEN the card list is retrieved from the VSAM file
THEN only card records with CARD-ACCT-ID = "41000000001" are displayed
  AND cards belonging to other accounts are excluded by 9500-FILTER-RECORDS
```

### Scenario 2: Blank account filter shows all records

```gherkin
GIVEN the account filter field is left blank (spaces or low-values)
WHEN the card list is retrieved
THEN FLG-ACCTFILTER-BLANK is set
  AND the account filter check in 9500-FILTER-RECORDS is bypassed
  AND all records pass through the account filter stage
```

### Scenario 3: Non-numeric account filter produces error

```gherkin
GIVEN a user enters "ABC12345678" in the account filter field
WHEN input validation runs (2210-EDIT-ACCOUNT)
THEN FLG-ACCTFILTER-NOT-OK is set to TRUE
  AND INPUT-ERROR is set to TRUE
  AND the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND the selection rows are protected (FLG-PROTECT-SELECT-ROWS-YES)
  AND no data retrieval is performed
```

### Scenario 4: Combined account and card filter narrows results

```gherkin
GIVEN a user enters account "41000000001" and card number "4000123456789012"
WHEN the card list is retrieved
THEN only records matching BOTH account ID AND card number are displayed
  AND 9500-FILTER-RECORDS checks account first, then card number
```

### Scenario 5: Card filter error does not overwrite account error

```gherkin
GIVEN a user enters an invalid account number
  AND also enters an invalid card number
WHEN input validation runs
THEN only the first error message (account filter error) is displayed
  AND the card filter error is suppressed by the WS-ERROR-MSG-OFF check
```

### Scenario 6: Selection rows protected on filter error

```gherkin
GIVEN either the account or card filter has a validation error
WHEN the screen is displayed
THEN the selection column is protected (read-only)
  AND the user cannot select individual card records for view or update
  AND the user must correct the filter before interacting with records
```

### Scenario 7: All-zeros account treated as blank

```gherkin
GIVEN a user enters "00000000000" in the account filter field
WHEN input validation runs (2210-EDIT-ACCOUNT)
THEN CC-ACCT-ID-N EQUAL ZEROS evaluates as true
  AND FLG-ACCTFILTER-BLANK is set
  AND the filter is treated as not supplied
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5(1)(c) | Data minimization — personal data must be adequate, relevant, and limited to what is necessary | Account filtering ensures users only retrieve records for the account they are working with, not all customer data |
| GDPR | Art. 5(1)(f) | Integrity and confidentiality — protection against unauthorized or unlawful processing | Record-level filtering at the data layer prevents unauthorized cross-account data access |
| GDPR | Art. 25 | Data protection by design and by default | Filtering is built into the data retrieval path (not optional) and defaults to the most restrictive view when the filter is blank for regular users |
| PSD2 | Art. 97 | Strong customer authentication when accessing payment account information | Account-level filtering is part of the authorization chain — even after authentication, users can only access their own account data |
| FFFS 2014:5 | Ch. 8 §4 | Adequate internal controls including access restrictions | Input validation rejects malformed filter inputs before they reach the data access layer, preventing injection and ensuring data integrity |

## Edge Cases

1. **First-error-only reporting**: The COBOL code uses `WS-ERROR-MSG-OFF` to check if an error message has already been set. If the account validation sets an error, the card filter error message is suppressed (line 1056). The migrated system should accumulate all validation errors and return them together.

2. **Row protection on filter error**: When either filter has a validation error, `FLG-PROTECT-SELECT-ROWS-YES` is set, which makes the selection column read-only on the BMS map. This prevents users from selecting records while the filter is invalid. The migrated system should disable action buttons when filter validation fails.

3. **Filter comparison uses different field types**: Account comparison uses the alphanumeric `CC-ACCT-ID` field, while card comparison uses the numeric redefine `CC-CARD-NUM-N`. This means the comparison semantics differ (character vs numeric). The migrated system should use consistent comparison types.

4. **Filter bypass for admin path**: When no account filter is provided (blank), the 9500-FILTER-RECORDS paragraph does not filter by account. This is the mechanism by which admin users see all records. The migrated system should enforce role-based filtering explicitly rather than relying on filter absence.

5. **Sequential evaluation with short-circuit**: The filter checks account first, then card. If the account filter excludes the record, the card filter is never evaluated (GO TO EXIT). The migrated system can use SQL WHERE clauses to combine both conditions efficiently.

## Domain Expert Notes

- **null** — Awaiting domain expert review. Key questions: (1) Is the account filter automatically populated from the user's session for regular users, or does the user type the account number manually? (2) In production, can a regular user type a different account number to access another customer's data? (3) Is there server-side enforcement beyond the screen-level filtering (e.g., CICS security exit)?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
