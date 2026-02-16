---
id: "acct-br-003"
title: "Account update field validation rules"
domain: "account-management"
cobol_source: "COACTUPC.cbl:200-680"
requirement_id: "ACCT-BR-003"
regulations:
  - "FSA FFFS 2014:5 Ch. 4 §3"
  - "PSD2 Art. 97"
  - "GDPR Art. 5"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-003: Account update field validation rules

## Summary

The COACTUPC program handles account record updates via CICS online transaction processing. Before writing changes to the ACCTDAT file, the program validates every modifiable field against a strict set of rules. The validation follows a sequential field-by-field approach: if any field fails validation, the cursor is positioned at the offending field, an error message is displayed, and the update is rejected. All monetary fields must be valid numeric values. Date fields must conform to YYYY-MM-DD format with valid calendar dates. The account status field accepts only 'Y' or 'N'. This is the only program that can modify account records — all field validation rules documented here are the authoritative source for account data integrity.

## Business Logic

### Pseudocode

```
PERFORM PROCESS-ENTER-KEY:
    SET WS-EDIT-FLAG-OK = TRUE

    PERFORM VALIDATE-ACCOUNT-STATUS:
        IF ACCT-ACTIVE-STATUS NOT = 'Y' AND NOT = 'N'
            SET WS-EDIT-FLAG-NOT-OK = TRUE
            MOVE -1 TO status-field-cursor-position
            MOVE 'ACCOUNT STATUS MUST BE Y OR N' TO WS-MESSAGE
            EXIT PARAGRAPH
        END-IF

    PERFORM VALIDATE-CREDIT-LIMIT:
        IF ACCT-CREDIT-LIMIT IS NOT NUMERIC
            SET WS-EDIT-FLAG-NOT-OK = TRUE
            MOVE -1 TO credit-limit-cursor-position
            MOVE 'CREDIT LIMIT MUST BE NUMERIC' TO WS-MESSAGE
            EXIT PARAGRAPH
        END-IF
        IF ACCT-CREDIT-LIMIT < 0
            SET WS-EDIT-FLAG-NOT-OK = TRUE
            MOVE -1 TO credit-limit-cursor-position
            MOVE 'CREDIT LIMIT CANNOT BE NEGATIVE' TO WS-MESSAGE
            EXIT PARAGRAPH
        END-IF

    PERFORM VALIDATE-CASH-CREDIT-LIMIT:
        IF ACCT-CASH-CREDIT-LIMIT IS NOT NUMERIC
            SET WS-EDIT-FLAG-NOT-OK = TRUE
            MOVE -1 TO cash-limit-cursor-position
            MOVE 'CASH CREDIT LIMIT MUST BE NUMERIC' TO WS-MESSAGE
            EXIT PARAGRAPH
        END-IF
        IF ACCT-CASH-CREDIT-LIMIT < 0
            SET WS-EDIT-FLAG-NOT-OK = TRUE
            MOVE 'CASH CREDIT LIMIT CANNOT BE NEGATIVE'
                TO WS-MESSAGE
            EXIT PARAGRAPH
        END-IF
        IF ACCT-CASH-CREDIT-LIMIT > ACCT-CREDIT-LIMIT
            SET WS-EDIT-FLAG-NOT-OK = TRUE
            MOVE 'CASH LIMIT CANNOT EXCEED CREDIT LIMIT'
                TO WS-MESSAGE
            EXIT PARAGRAPH
        END-IF

    PERFORM VALIDATE-EXPIRATION-DATE:
        IF ACCT-EXPIRAION-DATE NOT = SPACES
            PERFORM VALIDATE-DATE-FORMAT
            IF date-not-valid
                SET WS-EDIT-FLAG-NOT-OK = TRUE
                MOVE -1 TO expiry-date-cursor-position
                MOVE 'EXPIRATION DATE INVALID' TO WS-MESSAGE
                EXIT PARAGRAPH
            END-IF
        END-IF

    PERFORM VALIDATE-GROUP-ID:
        IF ACCT-GROUP-ID = SPACES OR LOW-VALUES
            SET WS-EDIT-FLAG-NOT-OK = TRUE
            MOVE -1 TO group-id-cursor-position
            MOVE 'GROUP ID CANNOT BE EMPTY' TO WS-MESSAGE
            EXIT PARAGRAPH
        END-IF

    IF WS-EDIT-FLAG-OK
        PERFORM WRITE-ACCOUNT-UPDATE
    ELSE
        PERFORM SEND-ACCTUP-SCREEN (redisplay with error)
    END-IF

VALIDATE-DATE-FORMAT:
    Extract year (positions 1-4), month (6-7), day (9-10)
    IF year IS NOT NUMERIC OR month IS NOT NUMERIC OR day IS NOT NUMERIC
        SET date-not-valid = TRUE
        EXIT PARAGRAPH
    END-IF
    IF month < 01 OR month > 12
        SET date-not-valid = TRUE
        EXIT PARAGRAPH
    END-IF
    IF day < 01 OR day > 31
        SET date-not-valid = TRUE
        EXIT PARAGRAPH
    END-IF
    IF separator at position 5 NOT = '-' OR position 8 NOT = '-'
        SET date-not-valid = TRUE
        EXIT PARAGRAPH
    END-IF
    SET date-valid = TRUE
```

### Decision Table — Field Validation Rules

| Field | Rule | Valid Values | Error Message |
|---|---|---|---|
| ACCT-ACTIVE-STATUS | Must be 'Y' or 'N' | 'Y', 'N' | "ACCOUNT STATUS MUST BE Y OR N" |
| ACCT-CREDIT-LIMIT | Must be numeric, >= 0 | 0 to 9,999,999,999.99 | "CREDIT LIMIT MUST BE NUMERIC" / "CREDIT LIMIT CANNOT BE NEGATIVE" |
| ACCT-CASH-CREDIT-LIMIT | Must be numeric, >= 0, <= CREDIT-LIMIT | 0 to ACCT-CREDIT-LIMIT | "CASH CREDIT LIMIT MUST BE NUMERIC" / "CASH LIMIT CANNOT EXCEED CREDIT LIMIT" |
| ACCT-EXPIRAION-DATE | YYYY-MM-DD format, valid calendar date (or spaces) | Valid date or blank | "EXPIRATION DATE INVALID" |
| ACCT-GROUP-ID | Must not be empty | Non-blank, non-LOW-VALUES | "GROUP ID CANNOT BE EMPTY" |
| ACCT-CURR-BAL | Read-only (not editable via this screen) | N/A | N/A |
| ACCT-OPEN-BAL | Read-only (not editable via this screen) | N/A | N/A |
| ACCT-CURR-CYC-CREDIT | Read-only (not editable via this screen) | N/A | N/A |
| ACCT-CURR-CYC-DEBIT | Read-only (not editable via this screen) | N/A | N/A |

### Non-Editable Fields

The following fields are displayed but cannot be modified through the update screen. They are maintained by batch programs (CBTRN02C, CBACT03C):

- ACCT-CURR-BAL — Updated by transaction posting (CBTRN02C)
- ACCT-OPEN-BAL — Set by end-of-cycle processing (CBACT03C)
- ACCT-CURR-CYC-CREDIT — Accumulated by transaction posting (CBTRN02C)
- ACCT-CURR-CYC-DEBIT — Accumulated by transaction posting (CBTRN02C)
- ACCT-PEND-CREDIT — Updated during transaction verification
- ACCT-PEND-DEBIT — Updated during transaction verification

## Source COBOL Reference

**Program:** `COACTUPC.cbl`
**Lines:** 200-680

```cobol
000200 PROCESS-ENTER-KEY.
000201     SET WS-EDIT-FLAG-OK TO TRUE.
000202
000203     IF WS-RETURN-FLAG-EDIT
000204         PERFORM 2200-EDIT-ACCT-DATA
000205         IF WS-EDIT-FLAG-NOT-OK
000206             GO TO PROCESS-ENTER-KEY-EXIT
000207         END-IF
000208     END-IF.
```

```cobol
000310 2200-EDIT-ACCT-DATA.
000311     SET WS-EDIT-FLAG-OK TO TRUE.
000312
000313 *   VALIDATE ACCOUNT STATUS
000314     IF APTS1I OF COACTUPI NOT = 'Y'
000315        AND APTS1I OF COACTUPI NOT = 'N'
000316         SET WS-EDIT-FLAG-NOT-OK TO TRUE
000317         MOVE -1                  TO APTS1L
000318         MOVE 'ACCOUNT STATUS MUST BE Y OR N'
000319                                  TO MESSAGEO
000320         GO TO 2200-EDIT-ACCT-DATA-EXIT
000321     END-IF.
000322
000323 *   VALIDATE CREDIT LIMIT
000324     IF CRLIM1I OF COACTUPI IS NOT NUMERIC
000325         SET WS-EDIT-FLAG-NOT-OK TO TRUE
000326         MOVE -1                  TO CRLIM1L
000327         MOVE 'CREDIT LIMIT MUST BE NUMERIC'
000328                                  TO MESSAGEO
000329         GO TO 2200-EDIT-ACCT-DATA-EXIT
000330     END-IF.
000331
000332     IF CRLIM1I OF COACTUPI < 0
000333         SET WS-EDIT-FLAG-NOT-OK TO TRUE
000334         MOVE -1                  TO CRLIM1L
000335         MOVE 'CREDIT LIMIT CANNOT BE NEGATIVE'
000336                                  TO MESSAGEO
000337         GO TO 2200-EDIT-ACCT-DATA-EXIT
000338     END-IF.
```

```cobol
000360 *   VALIDATE CASH CREDIT LIMIT
000361     IF CASHCRLIM1I OF COACTUPI IS NOT NUMERIC
000362         SET WS-EDIT-FLAG-NOT-OK TO TRUE
000363         MOVE -1                  TO CASHCRLIM1L
000364         MOVE 'CASH CREDIT LIMIT MUST BE NUMERIC'
000365                                  TO MESSAGEO
000366         GO TO 2200-EDIT-ACCT-DATA-EXIT
000367     END-IF.
000368
000369     IF CASHCRLIM1I OF COACTUPI < 0
000370         SET WS-EDIT-FLAG-NOT-OK TO TRUE
000371         MOVE -1                  TO CASHCRLIM1L
000372         MOVE 'CASH CREDIT LIMIT CANNOT BE NEGATIVE'
000373                                  TO MESSAGEO
000374         GO TO 2200-EDIT-ACCT-DATA-EXIT
000375     END-IF.
000376
000377     IF CASHCRLIM1I > CRLIM1I
000378         SET WS-EDIT-FLAG-NOT-OK TO TRUE
000379         MOVE -1                  TO CASHCRLIM1L
000380         MOVE 'CASH LIMIT CANNOT EXCEED CREDIT LIMIT'
000381                                  TO MESSAGEO
000382         GO TO 2200-EDIT-ACCT-DATA-EXIT
000383     END-IF.
```

```cobol
000420 *   VALIDATE EXPIRATION DATE
000421     IF EXPDT1I OF COACTUPI NOT = SPACES
000422         PERFORM 2210-VALIDATE-DATE-FORMAT
000423         IF WS-DATE-NOT-VALID
000424             SET WS-EDIT-FLAG-NOT-OK TO TRUE
000425             MOVE -1              TO EXPDT1L
000426             MOVE 'EXPIRATION DATE INVALID'
000427                                  TO MESSAGEO
000428             GO TO 2200-EDIT-ACCT-DATA-EXIT
000429         END-IF
000430     END-IF.
000431
000432 *   VALIDATE GROUP ID
000433     IF GRPID1I OF COACTUPI = SPACES OR LOW-VALUES
000434         SET WS-EDIT-FLAG-NOT-OK TO TRUE
000435         MOVE -1                  TO GRPID1L
000436         MOVE 'GROUP ID CANNOT BE EMPTY'
000437                                  TO MESSAGEO
000438         GO TO 2200-EDIT-ACCT-DATA-EXIT
000439     END-IF.
```

```cobol
000480 2210-VALIDATE-DATE-FORMAT.
000481     SET WS-DATE-VALID TO TRUE.
000482     MOVE EXPDT1I(1:4)   TO WS-DATE-YEAR.
000483     MOVE EXPDT1I(6:2)   TO WS-DATE-MONTH.
000484     MOVE EXPDT1I(9:2)   TO WS-DATE-DAY.
000485
000486     IF WS-DATE-YEAR IS NOT NUMERIC
000487        OR WS-DATE-MONTH IS NOT NUMERIC
000488        OR WS-DATE-DAY IS NOT NUMERIC
000489         SET WS-DATE-NOT-VALID TO TRUE
000490         GO TO 2210-VALIDATE-DATE-EXIT
000491     END-IF.
000492
000493     IF WS-DATE-MONTH < 01 OR WS-DATE-MONTH > 12
000494         SET WS-DATE-NOT-VALID TO TRUE
000495         GO TO 2210-VALIDATE-DATE-EXIT
000496     END-IF.
000497
000498     IF WS-DATE-DAY < 01 OR WS-DATE-DAY > 31
000499         SET WS-DATE-NOT-VALID TO TRUE
000500         GO TO 2210-VALIDATE-DATE-EXIT
000501     END-IF.
000502
000503     IF EXPDT1I(5:1) NOT = '-'
000504        OR EXPDT1I(8:1) NOT = '-'
000505         SET WS-DATE-NOT-VALID TO TRUE
000506     END-IF.
000507
000508 2210-VALIDATE-DATE-EXIT.
000509     EXIT.
```

## Acceptance Criteria

### Scenario 1: Valid account status update

```gherkin
GIVEN the account update screen is displayed for account "12345678901"
WHEN the user changes the account status to "Y"
  AND presses ENTER
THEN the status change is accepted
  AND no validation error is displayed
```

### Scenario 2: Invalid account status

```gherkin
GIVEN the account update screen is displayed
WHEN the user enters "X" as the account status
  AND presses ENTER
THEN the message "ACCOUNT STATUS MUST BE Y OR N" is displayed
  AND the cursor is positioned at the status field
  AND the update is not written
```

### Scenario 3: Non-numeric credit limit

```gherkin
GIVEN the account update screen is displayed
WHEN the user enters "ABC" as the credit limit
  AND presses ENTER
THEN the message "CREDIT LIMIT MUST BE NUMERIC" is displayed
  AND the cursor is positioned at the credit limit field
```

### Scenario 4: Negative credit limit

```gherkin
GIVEN the account update screen is displayed
WHEN the user enters "-5000.00" as the credit limit
  AND presses ENTER
THEN the message "CREDIT LIMIT CANNOT BE NEGATIVE" is displayed
```

### Scenario 5: Cash credit limit exceeds credit limit

```gherkin
GIVEN the account update screen is displayed
  AND the credit limit is set to 50,000.00
WHEN the user enters 75,000.00 as the cash credit limit
  AND presses ENTER
THEN the message "CASH LIMIT CANNOT EXCEED CREDIT LIMIT" is displayed
  AND the cursor is positioned at the cash credit limit field
```

### Scenario 6: Invalid expiration date format

```gherkin
GIVEN the account update screen is displayed
WHEN the user enters "13/31/2026" as the expiration date (wrong format)
  AND presses ENTER
THEN the message "EXPIRATION DATE INVALID" is displayed
  AND the cursor is positioned at the expiration date field
```

### Scenario 7: Valid expiration date

```gherkin
GIVEN the account update screen is displayed
WHEN the user enters "2028-12-31" as the expiration date
  AND presses ENTER
THEN the date is accepted (valid YYYY-MM-DD format)
```

### Scenario 8: Blank expiration date (no expiry)

```gherkin
GIVEN the account update screen is displayed
WHEN the user clears the expiration date field (sets to spaces)
  AND presses ENTER
THEN the blank date is accepted (account has no expiration)
```

### Scenario 9: Empty group ID

```gherkin
GIVEN the account update screen is displayed
WHEN the user clears the group ID field
  AND presses ENTER
THEN the message "GROUP ID CANNOT BE EMPTY" is displayed
  AND the cursor is positioned at the group ID field
```

### Scenario 10: Sequential validation — first error wins

```gherkin
GIVEN the account update screen is displayed
WHEN the user enters invalid values for both status and credit limit
  AND presses ENTER
THEN only the status validation error is displayed (first in sequence)
  AND the cursor is positioned at the status field
  AND the credit limit error is not shown until the status is corrected
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 4 §3 (Operational Risk) | Input validation to prevent data corruption | Sequential field validation ensures only valid data is written to the account record |
| PSD2 | Art. 97 (Strong Authentication) | Account modifications require authenticated access | Account update is gated by CICS terminal authentication; changes are made in authenticated session only |
| GDPR | Art. 5 (Data Accuracy) | Personal data must be accurate and kept up to date | Validation rules ensure data integrity for account status, limits, and dates |

## Edge Cases

1. **Date validation does not check calendar validity**: The COBOL date validation checks month (01-12) and day (01-31) ranges but does not validate day-of-month per month (e.g., February 30 would pass validation). The migrated system should use proper date parsing (DateTime.TryParseExact) to reject invalid calendar dates.

2. **Sequential validation short-circuit**: The COBOL validation exits on the first error found. This means users must fix errors one at a time. The migrated system could optionally validate all fields at once and return all errors, improving user experience while maintaining the same data integrity.

3. **No audit trail for changes**: The COACTUPC program writes the updated record directly via CICS REWRITE without logging what changed. The migrated system should implement an audit log recording the before/after values, the user who made the change, and the timestamp.

4. **Balance fields not validated**: Since balance fields (ACCT-CURR-BAL, ACCT-OPEN-BAL, cycle credits/debits) are read-only on this screen, they are not validated. However, they are still sent in the screen buffer — the program ignores their values when building the REWRITE record.

5. **Concurrent edit detection**: See ACCT-BR-004 for the optimistic concurrency control mechanism used by COACTUPC. Field validation occurs before the concurrency check.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Should the date validation be enhanced to check calendar validity (e.g., reject Feb 30)?
- Is there a maximum value for credit limits (regulatory or policy cap)?
- Should changes to credit limits require supervisory approval (four-eyes principle)?
- Are there additional validation rules for the group ID (e.g., must match an existing group in the DIS-GROUP file)?
- Should the migrated system validate that the expiration date is in the future?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
