---
id: "card-br-004"
title: "Account number input validation"
domain: "card-management"
cobol_source: "COCRDLIC.cbl:1003-1034,COCRDSLC.cbl:647-683,COCRDUPC.cbl:721-760"
requirement_id: "CARD-BR-004"
regulations:
  - "FFFS 2014:5 Ch. 4 §3"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# CARD-BR-004: Account number input validation

## Summary

Account number input validation is a shared business rule applied consistently across all three card management programs (list, detail view, and update). The account number must be exactly 11 digits, purely numeric, not blank, and not all zeros. This validation prevents invalid data from reaching the VSAM file lookup layer and ensures data integrity across card operations. The rule is implemented in paragraph 2210-EDIT-ACCOUNT (or 1210-EDIT-ACCOUNT in the update program) with identical logic in each program.

## Business Logic

### Pseudocode

```
PERFORM EDIT-ACCOUNT:
    IF account-id = SPACES OR LOW-VALUES
        IF account-id is required (detail/update programs)
            MOVE 'Account number not provided' TO WS-MESSAGE
            SET input-error = TRUE
        END-IF
        EXIT
    END-IF

    IF account-id IS NOT NUMERIC
        MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
            TO WS-MESSAGE
        SET input-error = TRUE
        EXIT
    END-IF

    IF account-id = ZEROS (all 11 digits are zero)
        MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
            TO WS-MESSAGE
        SET input-error = TRUE
        EXIT
    END-IF

    SET account-id-valid = TRUE
```

### Decision Table

| Account ID Value | Condition | Outcome |
|-----------------|-----------|---------|
| Spaces / LOW-VALUES | Blank input | Error: "Account number not provided" (detail/update) or silently skipped (list filter) |
| "ABC12345678" | Contains non-numeric characters | Error: "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" |
| "12345" | Fewer than 11 digits (padded with spaces) | Error: non-numeric due to trailing spaces |
| "00000000000" | All zeros | Error: "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" |
| "12345678901" | Valid 11-digit numeric | Validation passes |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 1003-1034 (2210-EDIT-ACCOUNT)

```cobol
001003 2210-EDIT-ACCOUNT.
001004     IF WS-FILTER-ACCT-ID = SPACES OR LOW-VALUES
001005         GO TO 2210-EXIT
001006     END-IF.
001007
001008     IF WS-FILTER-ACCT-ID IS NOT NUMERIC
001009         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
001010           ' 11 DIGIT NUMBER'
001011             TO WS-MESSAGE
001012         SET WS-INPUT-ERROR TO TRUE
001013         GO TO 2210-EXIT
001014     END-IF.
001015
001016     IF WS-FILTER-ACCT-ID = ZEROS
001017         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
001018           ' 11 DIGIT NUMBER'
001019             TO WS-MESSAGE
001020         SET WS-INPUT-ERROR TO TRUE
001021         GO TO 2210-EXIT
001022     END-IF.
001023
001024     SET WS-ACCT-FILTER-ACTIVE TO TRUE.
001025
001034 2210-EXIT.
001035     EXIT.
```

**Program:** `COCRDSLC.cbl`
**Lines:** 647-683 (2210-EDIT-ACCOUNT)

```cobol
000647 2210-EDIT-ACCOUNT.
000648     IF ACCT-ID-INPUT = SPACES OR LOW-VALUES
000649         MOVE 'Account number not provided'
000650             TO WS-MESSAGE
000651         SET WS-INPUT-ERROR TO TRUE
000652         GO TO 2210-EXIT
000653     END-IF.
000654
000655     IF ACCT-ID-INPUT IS NOT NUMERIC
000656         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
000657           ' 11 DIGIT NUMBER'
000658             TO WS-MESSAGE
000659         SET WS-INPUT-ERROR TO TRUE
000660         GO TO 2210-EXIT
000661     END-IF.
000662
000663     IF ACCT-ID-INPUT = ZEROS
000664         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
000665           ' 11 DIGIT NUMBER'
000666             TO WS-MESSAGE
000667         SET WS-INPUT-ERROR TO TRUE
000668         GO TO 2210-EXIT
000669     END-IF.
000670
000671 2210-EXIT.
000672     EXIT.
```

**Program:** `COCRDUPC.cbl`
**Lines:** 721-760 (1210-EDIT-ACCOUNT)

```cobol
000721 1210-EDIT-ACCOUNT.
000722     IF ACCT-ID-INPUT = SPACES OR LOW-VALUES
000723         MOVE 'Account number not provided'
000724             TO WS-MESSAGE
000725         SET WS-INPUT-ERROR TO TRUE
000726         GO TO 1210-EXIT
000727     END-IF.
000728
000729     IF ACCT-ID-INPUT IS NOT NUMERIC
000730         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
000731           ' 11 DIGIT NUMBER'
000732             TO WS-MESSAGE
000733         SET WS-INPUT-ERROR TO TRUE
000734         GO TO 1210-EXIT
000735     END-IF.
000736
000737     IF ACCT-ID-INPUT = ZEROS
000738         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
000739           ' 11 DIGIT NUMBER'
000740             TO WS-MESSAGE
000741         SET WS-INPUT-ERROR TO TRUE
000742         GO TO 1210-EXIT
000743     END-IF.
000744
000745     SET WS-ACCT-ID-VALID TO TRUE.
000746
000760 1210-EXIT.
000761     EXIT.
```

## Acceptance Criteria

### Scenario 1: Valid 11-digit account number accepted

```gherkin
GIVEN the user enters account number "12345678901"
WHEN the account number is validated
THEN the validation passes without error
  AND the system proceeds to the next step (lookup or filter)
```

### Scenario 2: Blank account number in detail/update program

```gherkin
GIVEN the user leaves the account number field blank
WHEN the account number is validated in the card detail or card update program
THEN the error message "Account number not provided" is displayed
  AND no data operation is performed
```

### Scenario 3: Blank account number in list filter

```gherkin
GIVEN the user leaves the account filter field blank on the card list screen
WHEN the account filter is validated
THEN no error is raised
  AND the account filter is not applied (all accounts shown)
```

### Scenario 4: Non-numeric account number rejected

```gherkin
GIVEN the user enters "ABCDEFGHIJK" as the account number
WHEN the account number is validated
THEN the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND no data operation is performed
```

### Scenario 5: All-zeros account number rejected

```gherkin
GIVEN the user enters "00000000000" as the account number
WHEN the account number is validated
THEN the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND no data operation is performed
```

### Scenario 6: Mixed alphanumeric account number rejected

```gherkin
GIVEN the user enters "1234567890A" as the account number
WHEN the account number is validated
THEN the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
```

### Scenario 7: Account number with embedded spaces rejected

```gherkin
GIVEN the user enters "123 4567890" as the account number
WHEN the account number is validated
THEN the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND the space character causes IS NOT NUMERIC to evaluate as true
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §3 | Credit institutions must have adequate systems for managing operational risk, including input validation controls | Account number validation prevents malformed input from reaching data access layers, reducing risk of data corruption and ensuring referential integrity with the account master file |

## Edge Cases

1. **Trailing spaces in shorter input**: If a user enters fewer than 11 digits (e.g., "12345"), the COBOL PIC X(11) field is padded with trailing spaces. The IS NOT NUMERIC check catches this because spaces are not numeric characters. The migrated system should apply explicit length validation in addition to numeric checks.

2. **Leading zeros in valid account number**: An account number like "00012345678" is a valid 11-digit numeric value and is NOT all zeros. The COBOL code correctly passes this through validation. The migrated system must treat leading zeros as significant and not trim them.

3. **LOW-VALUES vs spaces**: COBOL LOW-VALUES (hex 00) and SPACES (hex 40) are distinct. The COBOL code explicitly checks for both. In .NET, both would typically map to empty/null strings, but the migrated system should handle both null and empty-string inputs as the "not provided" case.

4. **Consistency across programs**: The validation logic is duplicated across three programs with minor variations (list screen treats blank as optional; detail/update screens treat blank as an error). The migrated system should extract this into a shared validation service with a "required" parameter to handle both behaviors.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The 11-digit account number format should be confirmed against the NordKredit account numbering scheme. Swedish bank account numbers vary in format by bank; this validation needs to be verified against the actual account master data structure.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
