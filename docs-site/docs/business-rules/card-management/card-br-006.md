---
id: "card-br-006"
title: "Card update field validation rules"
domain: "card-management"
cobol_source: "COCRDUPC.cbl:806-947"
requirement_id: "CARD-BR-006"
regulations:
  - "FFFS 2014:5 Ch. 4 §3"
  - "PSD2 Art. 97"
  - "GDPR Art. 5(1)(d)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# CARD-BR-006: Card update field validation rules

## Summary

When updating a credit card record, the COCRDUPC program validates four editable fields: embossed name, active status, expiry month, and expiry year. Each field has specific validation rules enforced in separate COBOL paragraphs (1230, 1240, 1250, 1260). The embossed name must contain only alphabetic characters and spaces. The active status must be 'Y' or 'N'. The expiry month must be between 1 and 12. The expiry year must be between 1950 and 2099. The expiry day field is not user-editable and is carried forward from the original record. All validations are performed only when actual changes are detected compared to the stored values.

## Business Logic

### Pseudocode

```
PERFORM 1200-EDIT-MAP-INPUTS:
    IF details-not-fetched
        VALIDATE search keys only (account + card number)
        EXIT
    END-IF

    COMPARE new-card-data WITH old-card-data (case-insensitive)
    IF no-changes-detected
        SET all-fields-valid
        EXIT
    END-IF

    SET changes-not-ok = TRUE

    PERFORM 1230-EDIT-NAME:
        IF embossed-name = BLANK OR LOW-VALUES
            ERROR: 'Card name not provided'
            EXIT
        END-IF
        COPY embossed-name TO check-field
        CONVERT all-alphabetics TO spaces IN check-field
        IF TRIM(check-field) LENGTH > 0
            ERROR: 'Card name can only contain alphabets and spaces'
            EXIT
        END-IF
        SET name-valid = TRUE

    PERFORM 1240-EDIT-CARDSTATUS:
        IF active-status = BLANK OR LOW-VALUES
            ERROR: 'Card Active Status must be Y or N'
            EXIT
        END-IF
        IF active-status NOT IN ('Y', 'N')
            ERROR: 'Card Active Status must be Y or N'
            EXIT
        END-IF
        SET status-valid = TRUE

    PERFORM 1250-EDIT-EXPIRY-MON:
        IF expiry-month = BLANK OR LOW-VALUES OR ZEROS
            ERROR: 'Card expiry month must be between 1 and 12'
            EXIT
        END-IF
        IF expiry-month NOT IN RANGE 1-12
            ERROR: 'Card expiry month must be between 1 and 12'
            EXIT
        END-IF
        SET month-valid = TRUE

    PERFORM 1260-EDIT-EXPIRY-YEAR:
        IF expiry-year = BLANK OR LOW-VALUES OR ZEROS
            ERROR: 'Invalid card expiry year'
            EXIT
        END-IF
        IF expiry-year NOT IN RANGE 1950-2099
            ERROR: 'Invalid card expiry year'
            EXIT
        END-IF
        SET year-valid = TRUE

    IF all-fields-valid
        SET changes-ok-not-confirmed = TRUE
    END-IF
```

### Decision Table

| Field | Valid Values | Blank/Empty | Invalid | Error Message |
|-------|-------------|-------------|---------|---------------|
| Embossed Name | Alphabetics (A-Z, a-z) and spaces only | "Card name not provided" | Non-alpha characters | "Card name can only contain alphabets and spaces" |
| Active Status | 'Y' or 'N' | "Card Active Status must be Y or N" | Any other character | "Card Active Status must be Y or N" |
| Expiry Month | 1-12 (numeric) | "Card expiry month must be between 1 and 12" | 0, 13+, non-numeric | "Card expiry month must be between 1 and 12" |
| Expiry Year | 1950-2099 (numeric) | "Invalid card expiry year" | Outside range, non-numeric | "Invalid card expiry year" |
| Expiry Day | Not user-editable | N/A | N/A | Carried from original record |

## Source COBOL Reference

**Program:** `COCRDUPC.cbl`

**Lines:** 806-843 (1230-EDIT-NAME — embossed name validation)

```cobol
000806 1230-EDIT-NAME.
000807 *    Not BLANK
000808      SET FLG-CARDNAME-NOT-OK      TO TRUE
000809
000810 *    Not supplied
000811      IF CCUP-NEW-CRDNAME   EQUAL LOW-VALUES
000812      OR CCUP-NEW-CRDNAME   EQUAL SPACES
000813      OR CCUP-NEW-CRDNAME   EQUAL ZEROS
000814         SET INPUT-ERROR           TO TRUE
000815         SET FLG-CARDNAME-BLANK  TO TRUE
000816         IF WS-RETURN-MSG-OFF
000817            SET WS-PROMPT-FOR-NAME TO TRUE
000818         END-IF
000819         GO TO  1230-EDIT-NAME-EXIT
000820      END-IF
000821
000822 *    Only Alphabets and space allowed
000823      MOVE CCUP-NEW-CRDNAME        TO CARD-NAME-CHECK
000824      INSPECT CARD-NAME-CHECK
000825        CONVERTING LIT-ALL-ALPHA-FROM
000826                TO LIT-ALL-SPACES-TO
000827
000828      IF FUNCTION LENGTH(FUNCTION TRIM(CARD-NAME-CHECK)) = 0
000829         CONTINUE
000830      ELSE
000831         SET INPUT-ERROR           TO TRUE
000832         SET FLG-CARDNAME-NOT-OK   TO TRUE
000833         IF WS-RETURN-MSG-OFF
000834            SET WS-NAME-MUST-BE-ALPHA  TO TRUE
000835         END-IF
000836         GO TO  1230-EDIT-NAME-EXIT
000837      END-IF
000838
000839      SET FLG-CARDNAME-ISVALID     TO TRUE
000840      .
000841 1230-EDIT-NAME-EXIT.
000842      EXIT
000843      .
```

**Lines:** 845-876 (1240-EDIT-CARDSTATUS — active status validation)

```cobol
000845 1240-EDIT-CARDSTATUS.
000846 *    Must be Y or N
000847      SET FLG-CARDSTATUS-NOT-OK      TO TRUE
000848
000849 *    Not supplied
000850      IF CCUP-NEW-CRDSTCD   EQUAL LOW-VALUES
000851      OR CCUP-NEW-CRDSTCD   EQUAL SPACES
000852      OR CCUP-NEW-CRDSTCD   EQUAL ZEROS
000853         SET INPUT-ERROR           TO TRUE
000854         SET FLG-CARDSTATUS-BLANK  TO TRUE
000855         IF WS-RETURN-MSG-OFF
000856            SET CARD-STATUS-MUST-BE-YES-NO TO TRUE
000857         END-IF
000858         GO TO  1240-EDIT-CARDSTATUS-EXIT
000859      END-IF
000860
000861      MOVE CCUP-NEW-CRDSTCD          TO FLG-YES-NO-CHECK
000862
000863      IF FLG-YES-NO-VALID
000864         SET FLG-CARDSTATUS-ISVALID  TO TRUE
000865      ELSE
000866         SET INPUT-ERROR             TO TRUE
000867         SET FLG-CARDSTATUS-NOT-OK   TO TRUE
000868         IF WS-RETURN-MSG-OFF
000869            SET CARD-STATUS-MUST-BE-YES-NO  TO TRUE
000870         END-IF
000871         GO TO  1240-EDIT-CARDSTATUS-EXIT
000872      END-IF
000873      .
000874 1240-EDIT-CARDSTATUS-EXIT.
000875      EXIT
000876      .
```

**Lines:** 877-912 (1250-EDIT-EXPIRY-MON — expiry month validation)

```cobol
000877 1250-EDIT-EXPIRY-MON.
000878
000879      SET FLG-CARDEXPMON-NOT-OK      TO TRUE
000880
000881 *    Not supplied
000882      IF CCUP-NEW-EXPMON   EQUAL LOW-VALUES
000883      OR CCUP-NEW-EXPMON   EQUAL SPACES
000884      OR CCUP-NEW-EXPMON   EQUAL ZEROS
000885         SET INPUT-ERROR           TO TRUE
000886         SET FLG-CARDEXPMON-BLANK  TO TRUE
000887         IF WS-RETURN-MSG-OFF
000888            SET CARD-EXPIRY-MONTH-NOT-VALID TO TRUE
000889         END-IF
000890         GO TO  1250-EDIT-EXPIRY-MON-EXIT
000891      END-IF
000892
000893 *    Must be numeric
000894 *    Must be 1 to 12
000895      MOVE CCUP-NEW-EXPMON           TO CARD-MONTH-CHECK
000896
000897      IF VALID-MONTH
000898         SET FLG-CARDEXPMON-ISVALID  TO TRUE
000899      ELSE
000900         SET INPUT-ERROR             TO TRUE
000901         SET FLG-CARDEXPMON-NOT-OK   TO TRUE
000902         IF WS-RETURN-MSG-OFF
000903            SET CARD-EXPIRY-MONTH-NOT-VALID  TO TRUE
000904         END-IF
000905         GO TO  1250-EDIT-EXPIRY-MON-EXIT
000906      END-IF
000907      .
000908
000909 1250-EDIT-EXPIRY-MON-EXIT.
000910      EXIT
000911      .
```

**Lines:** 913-947 (1260-EDIT-EXPIRY-YEAR — expiry year validation)

```cobol
000913 1260-EDIT-EXPIRY-YEAR.
000914
000915 *    Not supplied
000916      IF CCUP-NEW-EXPYEAR   EQUAL LOW-VALUES
000917      OR CCUP-NEW-EXPYEAR   EQUAL SPACES
000918      OR CCUP-NEW-EXPYEAR   EQUAL ZEROS
000919         SET INPUT-ERROR           TO TRUE
000920         SET FLG-CARDEXPYEAR-BLANK  TO TRUE
000921         IF WS-RETURN-MSG-OFF
000922            SET CARD-EXPIRY-YEAR-NOT-VALID TO TRUE
000923         END-IF
000924         GO TO  1260-EDIT-EXPIRY-YEAR-EXIT
000925      END-IF
000926
000927 *    Must be numeric
000928 *    Must be 1 to 12
000929
000930      SET FLG-CARDEXPYEAR-NOT-OK      TO TRUE
000931
000932      MOVE CCUP-NEW-EXPYEAR           TO CARD-YEAR-CHECK
000933
000934      IF VALID-YEAR
000935         SET FLG-CARDEXPYEAR-ISVALID  TO TRUE
000936      ELSE
000937         SET INPUT-ERROR              TO TRUE
000938         SET FLG-CARDEXPYEAR-NOT-OK   TO TRUE
000939         IF WS-RETURN-MSG-OFF
000940            SET CARD-EXPIRY-YEAR-NOT-VALID  TO TRUE
000941         END-IF
000942         GO TO  1260-EDIT-EXPIRY-YEAR-EXIT
000943      END-IF
000944      .
000945 1260-EDIT-EXPIRY-YEAR-EXIT.
000946      EXIT
000947      .
```

**Lines:** 87-99 (validation constants)

```cobol
000087    05  CARD-NAME-CHECK              PIC X(50)
000088                                     VALUE LOW-VALUES.
000089    05  FLG-YES-NO-CHECK             PIC X(1)
000090                                     VALUE 'N'.
000091      88 FLG-YES-NO-VALID            VALUES 'Y', 'N'.
000092    05  CARD-MONTH-CHECK             PIC X(2).
000093    05  CARD-MONTH-CHECK-N REDEFINES
000094        CARD-MONTH-CHECK             PIC 9(2).
000095        88 VALID-MONTH               VALUES 1 THRU 12.
000096    05  CARD-YEAR-CHECK             PIC X(4).
000097    05  CARD-YEAR-CHECK-N REDEFINES
000098        CARD-YEAR-CHECK             PIC 9(4).
000099        88 VALID-YEAR               VALUES 1950 THRU 2099.
```

**Lines:** 255-263 (alphabet conversion constants for name validation)

```cobol
000255    05 LIT-ALL-ALPHA-FROM           PIC X(52)
000256       VALUE
000257       'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.
000258    05 LIT-ALL-SPACES-TO            PIC X(52)
000259                                    VALUE SPACES.
```

## Acceptance Criteria

### Scenario 1: Valid embossed name accepted

```gherkin
GIVEN the card update screen shows card details
  AND the user changes the embossed name to "JANE DOE"
WHEN the input is validated
THEN the embossed name validation passes
  AND the system proceeds to the confirmation step
```

### Scenario 2: Embossed name with numbers rejected

```gherkin
GIVEN the card update screen shows card details
  AND the user changes the embossed name to "JOHN DOE 3RD"
WHEN the input is validated
THEN the error message "Card name can only contain alphabets and spaces" is displayed
  AND the card name field is highlighted in red
```

### Scenario 3: Blank embossed name rejected

```gherkin
GIVEN the card update screen shows card details
  AND the user clears the embossed name field
WHEN the input is validated
THEN the error message "Card name not provided" is displayed
```

### Scenario 4: Valid active status 'Y' accepted

```gherkin
GIVEN the card update screen shows card details with status 'N'
  AND the user changes the active status to "Y"
WHEN the input is validated
THEN the status validation passes
```

### Scenario 5: Invalid active status rejected

```gherkin
GIVEN the card update screen shows card details
  AND the user enters "A" as the active status
WHEN the input is validated
THEN the error message "Card Active Status must be Y or N" is displayed
```

### Scenario 6: Valid expiry month accepted

```gherkin
GIVEN the card update screen shows card details
  AND the user changes the expiry month to "06"
WHEN the input is validated
THEN the expiry month validation passes
```

### Scenario 7: Expiry month 13 rejected

```gherkin
GIVEN the card update screen shows card details
  AND the user changes the expiry month to "13"
WHEN the input is validated
THEN the error message "Card expiry month must be between 1 and 12" is displayed
```

### Scenario 8: Expiry month zero rejected

```gherkin
GIVEN the card update screen shows card details
  AND the user changes the expiry month to "00"
WHEN the input is validated
THEN the error message "Card expiry month must be between 1 and 12" is displayed
```

### Scenario 9: Valid expiry year accepted

```gherkin
GIVEN the card update screen shows card details
  AND the user changes the expiry year to "2028"
WHEN the input is validated
THEN the expiry year validation passes
```

### Scenario 10: Expiry year outside range rejected

```gherkin
GIVEN the card update screen shows card details
  AND the user changes the expiry year to "2100"
WHEN the input is validated
THEN the error message "Invalid card expiry year" is displayed
```

### Scenario 11: No changes detected

```gherkin
GIVEN the card update screen shows card details
  AND the user submits without making any changes
WHEN the input is validated
THEN the message "No change detected with respect to values fetched." is displayed
  AND no update operation is performed
```

### Scenario 12: Case-insensitive change detection

```gherkin
GIVEN the card has embossed name "JOHN DOE" stored in the database
  AND the user changes the embossed name to "John Doe"
WHEN the input is validated
THEN no change is detected (comparison is case-insensitive using UPPER-CASE)
  AND the message "No change detected with respect to values fetched." is displayed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §3 | Credit institutions must have adequate systems for managing operational risk | Field-level validation prevents corrupted or malformed data from being persisted to the card master file, maintaining data integrity |
| PSD2 | Art. 97 | Strong customer authentication for payment instrument modifications | Card update validation ensures only properly formatted changes are applied to payment instruments; invalid modifications are rejected before reaching the data layer |
| GDPR | Art. 5(1)(d) | Personal data must be accurate and kept up to date | The embossed name validation (alphabetics only) and expiry date validation ensure that cardholder personal data updates meet format requirements; the no-change detection prevents unnecessary data modifications |

## Edge Cases

1. **Embossed name with accented characters**: The COBOL validation converts only A-Z and a-z to spaces. Accented characters (e.g., 'Å', 'Ö', 'É') common in Swedish names would fail the alphabetics check. The migrated system must support Unicode characters appropriate for Swedish names, including Swedish alphabet characters (Å, Ä, Ö).

2. **Active status case sensitivity**: The COBOL 88-level condition FLG-YES-NO-VALID accepts only uppercase 'Y' and 'N'. Lowercase 'y' and 'n' would be rejected. The migrated system should decide whether to accept lowercase input with automatic conversion to uppercase.

3. **Expiry date in the past**: The COBOL code does not validate whether the new expiry date is in the future. A user could set an expiry date of "01/1950" and it would pass validation. The migrated system should consider adding a future-date check, subject to domain expert approval.

4. **Expiry day not editable**: The COBOL program explicitly does NOT allow users to change the expiry day field (line 1122-1123 shows the day is always carried from the old record). The migrated system must preserve this behavior or document the reason for allowing day changes.

5. **Multiple validation errors**: The COBOL code validates fields sequentially (name, status, month, year) but uses the WS-RETURN-MSG-OFF flag to report only the first error encountered. Subsequent fields still set their NOT-OK flags (used for cursor positioning and field highlighting) but their error messages are suppressed. The migrated system should present all validation errors simultaneously.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The embossed name validation needs to be verified against Swedish character set requirements. Also need to confirm whether the 1950-2099 year range is still appropriate or should be updated for the migrated system.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
