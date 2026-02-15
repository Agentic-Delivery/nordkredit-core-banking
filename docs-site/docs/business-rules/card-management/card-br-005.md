---
id: "card-br-005"
title: "Card number input validation"
domain: "card-management"
cobol_source: "COCRDSLC.cbl:685-724,COCRDUPC.cbl:762-800"
requirement_id: "CARD-BR-005"
regulations:
  - "FFFS 2014:5 Ch. 4 §3"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# CARD-BR-005: Card number input validation

## Summary

Card number input validation is a shared business rule applied consistently across the card detail (COCRDSLC) and card update (COCRDUPC) programs. The card number must be exactly 16 digits, purely numeric, not blank, and not all zeros. This validation prevents invalid card numbers from reaching the VSAM file lookup layer. The rule is implemented in paragraph 2220-EDIT-CARD (detail program) and 1220-EDIT-CARD (update program) with identical logic. In the card list program (COCRDLIC), card number validation follows a similar pattern but treats blank input as an optional filter rather than a required field.

## Business Logic

### Pseudocode

```
PERFORM EDIT-CARD:
    IF card-number = SPACES OR LOW-VALUES OR ZEROS
        MOVE 'Card number not provided' TO WS-MESSAGE
        SET input-error = TRUE
        SET card-filter-blank = TRUE
        EXIT
    END-IF

    IF card-number IS NOT NUMERIC
        MOVE 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
            TO WS-MESSAGE
        SET input-error = TRUE
        EXIT
    END-IF

    SET card-number-valid = TRUE
```

### Decision Table

| Card Number Value | Condition | Outcome |
|-------------------|-----------|---------|
| Spaces / LOW-VALUES | Blank input | Error: "Card number not provided" |
| "0000000000000000" | All zeros | Error: "Card number not provided" (treated as blank) |
| "ABCD123456789012" | Contains non-numeric characters | Error: "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER" |
| "1234567890" | Fewer than 16 digits (padded with spaces) | Error: non-numeric due to trailing spaces |
| "4000123456789012" | Valid 16-digit numeric | Validation passes |

## Source COBOL Reference

**Program:** `COCRDSLC.cbl`
**Lines:** 685-724 (2220-EDIT-CARD)

```cobol
000685 2220-EDIT-CARD.
000686 *    Not numeric
000687 *    Not 16 characters
000688      SET FLG-CARDFILTER-NOT-OK TO TRUE
000689
000690 *    Not supplied
000691      IF CC-CARD-NUM   EQUAL LOW-VALUES
000692      OR CC-CARD-NUM   EQUAL SPACES
000693      OR CC-CARD-NUM-N EQUAL ZEROS
000694         SET INPUT-ERROR           TO TRUE
000695         SET FLG-CARDFILTER-BLANK  TO TRUE
000696         IF WS-RETURN-MSG-OFF
000697            SET WS-PROMPT-FOR-CARD TO TRUE
000698         END-IF
000699
000700         MOVE ZEROES       TO CDEMO-CARD-NUM
000701         GO TO  2220-EDIT-CARD-EXIT
000702      END-IF
000703 *
000704 *    Not numeric
000705 *    Not 16 characters
000706      IF CC-CARD-NUM  IS NOT NUMERIC
000707         SET INPUT-ERROR TO TRUE
000708         SET FLG-CARDFILTER-NOT-OK TO TRUE
000709         IF WS-RETURN-MSG-OFF
000710            MOVE
000711         'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
000712                         TO WS-RETURN-MSG
000713         END-IF
000714         MOVE ZERO       TO CDEMO-CARD-NUM
000715         GO TO 2220-EDIT-CARD-EXIT
000716      ELSE
000717         MOVE CC-CARD-NUM-N TO CDEMO-CARD-NUM
000718         SET FLG-CARDFILTER-ISVALID TO TRUE
000719      END-IF
000720      .
000721
000722 2220-EDIT-CARD-EXIT.
000723      EXIT
000724      .
```

**Program:** `COCRDUPC.cbl`
**Lines:** 762-800 (1220-EDIT-CARD)

```cobol
000762 1220-EDIT-CARD.
000763 *    Not numeric
000764 *    Not 16 characters
000765      SET FLG-CARDFILTER-NOT-OK TO TRUE
000766
000767 *    Not supplied
000768      IF CC-CARD-NUM   EQUAL LOW-VALUES
000769      OR CC-CARD-NUM   EQUAL SPACES
000770      OR CC-CARD-NUM-N EQUAL ZEROS
000771         SET INPUT-ERROR           TO TRUE
000772         SET FLG-CARDFILTER-BLANK  TO TRUE
000773         IF WS-RETURN-MSG-OFF
000774            SET WS-PROMPT-FOR-CARD TO TRUE
000775         END-IF
000776
000777         MOVE ZEROES        TO CDEMO-CARD-NUM
000778                               CCUP-NEW-CARDID
000779         GO TO  1220-EDIT-CARD-EXIT
000780      END-IF
000781 *
000782 *    Not numeric
000783 *    Not 16 characters
000784      IF CC-CARD-NUM  IS NOT NUMERIC
000785         SET INPUT-ERROR TO TRUE
000786         SET FLG-CARDFILTER-NOT-OK TO TRUE
000787         IF WS-RETURN-MSG-OFF
000788            MOVE
000789         'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
000790                            TO WS-RETURN-MSG
000791         END-IF
000792         MOVE ZERO          TO CDEMO-CARD-NUM
000793         MOVE LOW-VALUES    TO CCUP-NEW-CARDID
000794         GO TO 1220-EDIT-CARD-EXIT
000795      ELSE
000796         MOVE CC-CARD-NUM-N TO CDEMO-CARD-NUM
000797         MOVE CC-CARD-NUM   TO CCUP-NEW-CARDID
000798         SET FLG-CARDFILTER-ISVALID TO TRUE
000799      END-IF
000800      .
```

## Acceptance Criteria

### Scenario 1: Valid 16-digit card number accepted

```gherkin
GIVEN the user enters card number "4000123456789012"
WHEN the card number is validated
THEN the validation passes without error
  AND the system proceeds to the next step (lookup or update)
```

### Scenario 2: Blank card number rejected

```gherkin
GIVEN the user leaves the card number field blank
WHEN the card number is validated in the card detail or card update program
THEN the error message "Card number not provided" is displayed
  AND no data operation is performed
```

### Scenario 3: Non-numeric card number rejected

```gherkin
GIVEN the user enters "ABCD123456789012" as the card number
WHEN the card number is validated
THEN the error message "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER" is displayed
  AND no data operation is performed
```

### Scenario 4: All-zeros card number rejected

```gherkin
GIVEN the user enters "0000000000000000" as the card number
WHEN the card number is validated
THEN the error message "Card number not provided" is displayed
  AND no data operation is performed
```

### Scenario 5: Short card number rejected

```gherkin
GIVEN the user enters "400012345678" (12 digits) as the card number
WHEN the card number is validated
THEN the error message "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER" is displayed
  AND the trailing spaces in the PIC X(16) field cause IS NOT NUMERIC to evaluate as true
```

### Scenario 6: Card number with special characters rejected

```gherkin
GIVEN the user enters "4000-1234-5678-9012" as the card number
WHEN the card number is validated
THEN the error message "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER" is displayed
  AND hyphens cause IS NOT NUMERIC to evaluate as true
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §3 | Credit institutions must have adequate systems for managing operational risk, including input validation controls | Card number validation prevents malformed input from reaching data access layers, reducing risk of data corruption and ensuring referential integrity with the card master file |
| PSD2 | Art. 97 | Strong customer authentication for accessing payment instrument details | Card number validation ensures only properly formatted card identifiers are used to access card data; prevents parameter injection or bypass |

## Edge Cases

1. **Leading zeros in valid card number**: A card number like "0000123456789012" is a valid 16-digit numeric value and is NOT all zeros. The COBOL code correctly passes this through validation. The migrated system must treat leading zeros as significant and not trim them.

2. **Consistency across programs**: The validation logic is duplicated across COCRDSLC (2220-EDIT-CARD) and COCRDUPC (1220-EDIT-CARD) with identical checks. The update program additionally stores the validated card number in CCUP-NEW-CARDID. The migrated system should extract this into a shared validation service.

3. **First-error-only reporting**: The COBOL code uses WS-RETURN-MSG-OFF to check if an error message has already been set. If account validation already set an error, the card validation error message is suppressed (the flag check prevents overwriting). The migrated system should accumulate all validation errors rather than reporting only the first one.

4. **Zeros check uses numeric redefine**: The COBOL code checks CC-CARD-NUM-N (PIC 9(16)) EQUAL ZEROS rather than the alphanumeric CC-CARD-NUM. This correctly handles the all-zeros case for the numeric interpretation. The migrated system should check for all-zeros as a distinct validation rule separate from the blank check.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The 16-digit card number format should be confirmed against the NordKredit card numbering scheme. Need to verify whether Luhn check digit validation is performed elsewhere in the system or if this is the only card number validation point.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
