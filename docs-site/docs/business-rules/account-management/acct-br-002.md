---
id: "acct-br-002"
title: "Account ID input validation rules"
domain: "account-management"
cobol_source: "COCRDLIC.cbl:1003-1034, COCRDSLC.cbl:647-683, COCRDUPC.cbl:721-760"
requirement_id: "ACCT-BR-002"
regulations:
  - "FSA FFFS 2014:5 Ch. 4 §3"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-002: Account ID input validation rules

## Summary

Account ID validation is a shared business rule implemented identically across all three card management online programs (COCRDLIC, COCRDSLC, COCRDUPC). The validation enforces that account IDs are exactly 11 numeric digits and rejects non-numeric input with a specific error message. Blank or zero account IDs are handled differently depending on context: in the card list program (COCRDLIC), a blank account ID means "show all cards" (no filter); in the card detail and update programs (COCRDSLC, COCRDUPC), a blank account ID is treated as an input error requiring the user to provide one. This validation is the first line of defense for all account-related operations and must be preserved exactly in the migrated system.

## Business Logic

### Pseudocode

```
PERFORM EDIT-ACCOUNT:
    IF account-id = LOW-VALUES
    OR account-id = SPACES
    OR account-id-numeric = ZEROS
        -- Context-dependent handling:
        -- COCRDLIC: SET filter-blank (no error, show all cards)
        -- COCRDSLC/COCRDUPC: SET input-error, prompt for account
        MOVE ZEROES TO demo-account-id
        EXIT PARAGRAPH
    END-IF

    IF account-id IS NOT NUMERIC
        SET input-error = TRUE
        SET filter-not-ok = TRUE
        MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
            TO error-message
        MOVE ZERO TO demo-account-id
        EXIT PARAGRAPH
    END-IF

    -- Valid: 11 numeric digits, non-zero
    MOVE account-id TO demo-account-id
    SET filter-is-valid = TRUE
```

### Decision Table

| Input Value | Numeric? | All Zeros? | Blank/Spaces? | COCRDLIC Outcome | COCRDSLC/COCRDUPC Outcome |
|------------|----------|-----------|--------------|------------------|---------------------------|
| "12345678901" | Yes | No | No | Filter active, show matching cards | Valid, proceed with lookup |
| "00000000000" | Yes | Yes | No | No filter, show all cards | Input error, prompt for account |
| "" (spaces) | N/A | N/A | Yes | No filter, show all cards | Input error, prompt for account |
| LOW-VALUES | N/A | N/A | Yes (equivalent) | No filter, show all cards | Input error, prompt for account |
| "1234567890A" | No | No | No | Error: must be 11 digit number | Error: must be 11 digit number |
| "ABCDEFGHIJK" | No | No | No | Error: must be 11 digit number | Error: must be 11 digit number |
| "1234567890" | No* | No | No | Error: must be 11 digit number | Error: must be 11 digit number |

*Note: The PIC X(11) field is always 11 characters. A 10-digit number would be space-padded, making it non-numeric due to the trailing space.

### Validation Flag States

| Flag | Value | Meaning |
|------|-------|---------|
| FLG-ACCTFILTER-BLANK | ' ' | Account ID not provided (blank/zero/low-values) |
| FLG-ACCTFILTER-ISVALID | '1' | Account ID is valid 11-digit numeric |
| FLG-ACCTFILTER-NOT-OK | '0' | Account ID validation failed (non-numeric) |

## Source COBOL Reference

**Programs:** `COCRDLIC.cbl`, `COCRDSLC.cbl`, `COCRDUPC.cbl`
**Lines:** COCRDLIC:1003-1034, COCRDSLC:647-683, COCRDUPC:721-760

### COCRDLIC.cbl (card list — blank is acceptable)

```cobol
001003 2210-EDIT-ACCOUNT.
001004     SET FLG-ACCTFILTER-BLANK TO TRUE
001005
001006*    Not supplied
001007     IF CC-ACCT-ID   EQUAL LOW-VALUES
001008     OR CC-ACCT-ID   EQUAL SPACES
001009     OR CC-ACCT-ID-N EQUAL ZEROS
001010        SET FLG-ACCTFILTER-BLANK  TO TRUE
001011        MOVE ZEROES       TO CDEMO-ACCT-ID
001012        GO TO  2210-EDIT-ACCOUNT-EXIT
001013     END-IF
001014*
001015*    Not numeric
001016*    Not 11 characters
001017     IF CC-ACCT-ID  IS NOT NUMERIC
001018        SET INPUT-ERROR TO TRUE
001019        SET FLG-ACCTFILTER-NOT-OK TO TRUE
001020        SET FLG-PROTECT-SELECT-ROWS-YES TO TRUE
001021        MOVE
001022        'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
001023                        TO WS-ERROR-MSG
001024        MOVE ZERO       TO CDEMO-ACCT-ID
001025        GO TO 2210-EDIT-ACCOUNT-EXIT
001026     ELSE
001027        MOVE CC-ACCT-ID TO CDEMO-ACCT-ID
001028        SET FLG-ACCTFILTER-ISVALID TO TRUE
001029     END-IF
001030     .
001031
001032 2210-EDIT-ACCOUNT-EXIT.
001033     EXIT
001034     .
```

### COCRDSLC.cbl (card detail — blank is an error)

```cobol
000647 2210-EDIT-ACCOUNT.
000648     SET FLG-ACCTFILTER-NOT-OK TO TRUE
000649
000650*    Not supplied
000651     IF CC-ACCT-ID   EQUAL LOW-VALUES
000652     OR CC-ACCT-ID   EQUAL SPACES
000653     OR CC-ACCT-ID-N EQUAL ZEROS
000654        SET INPUT-ERROR           TO TRUE
000655        SET FLG-ACCTFILTER-BLANK  TO TRUE
000656        IF WS-RETURN-MSG-OFF
000657           SET WS-PROMPT-FOR-ACCT TO TRUE
000658        END-IF
000659        MOVE ZEROES       TO CDEMO-ACCT-ID
000660        GO TO  2210-EDIT-ACCOUNT-EXIT
000661     END-IF
000662*
000663*    Not numeric
000664*    Not 11 characters
000665     IF CC-ACCT-ID  IS NOT NUMERIC
000666        SET INPUT-ERROR TO TRUE
000667        SET FLG-ACCTFILTER-NOT-OK TO TRUE
000668        IF WS-RETURN-MSG-OFF
000669          MOVE
000670        'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
000671                        TO WS-RETURN-MSG
000672        END-IF
000673        MOVE ZERO       TO CDEMO-ACCT-ID
000674        GO TO 2210-EDIT-ACCOUNT-EXIT
000675     ELSE
000676        MOVE CC-ACCT-ID TO CDEMO-ACCT-ID
000677        SET FLG-ACCTFILTER-ISVALID TO TRUE
000678     END-IF
000679     .
000680
000681 2210-EDIT-ACCOUNT-EXIT.
000682     EXIT
000683     .
```

### COCRDUPC.cbl (card update — blank is an error)

```cobol
000721 1210-EDIT-ACCOUNT.
000722     SET FLG-ACCTFILTER-NOT-OK TO TRUE
000723
000724*    Not supplied
000725     IF CC-ACCT-ID   EQUAL LOW-VALUES
000726     OR CC-ACCT-ID   EQUAL SPACES
000727     OR CC-ACCT-ID-N EQUAL ZEROS
000728        SET INPUT-ERROR           TO TRUE
000729        SET FLG-ACCTFILTER-BLANK  TO TRUE
000730        IF WS-RETURN-MSG-OFF
000731           SET WS-PROMPT-FOR-ACCT TO TRUE
000732        END-IF
000733        MOVE ZEROES       TO CDEMO-ACCT-ID
000734        MOVE LOW-VALUES   TO CCUP-NEW-ACCTID
000735        GO TO  1210-EDIT-ACCOUNT-EXIT
000736     END-IF
000737*
000738*    Not numeric
000739*    Not 11 characters
000740     IF CC-ACCT-ID  IS NOT NUMERIC
000741        SET INPUT-ERROR TO TRUE
000742        SET FLG-ACCTFILTER-NOT-OK TO TRUE
000743        IF WS-RETURN-MSG-OFF
000744          MOVE
000745        'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
000746                        TO WS-RETURN-MSG
000747        END-IF
000748        MOVE ZERO       TO CDEMO-ACCT-ID
000749        MOVE LOW-VALUES TO CCUP-NEW-ACCTID
000750        GO TO 1210-EDIT-ACCOUNT-EXIT
000751     ELSE
000752        MOVE CC-ACCT-ID TO CDEMO-ACCT-ID
000753                           CCUP-NEW-ACCTID
000754        SET FLG-ACCTFILTER-ISVALID TO TRUE
000755     END-IF
000756     .
000757
000758 1210-EDIT-ACCOUNT-EXIT.
000759     EXIT
000760     .
```

## Acceptance Criteria

### Scenario 1: Valid 11-digit account ID

```gherkin
GIVEN the user enters account ID "12345678901"
WHEN account ID validation is performed
THEN the account ID is accepted as valid
  AND the account filter flag is set to ISVALID
  AND no error message is displayed
```

### Scenario 2: Non-numeric account ID rejected

```gherkin
GIVEN the user enters account ID "1234567890A"
WHEN account ID validation is performed
THEN the input is rejected
  AND the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND the account filter flag is set to NOT-OK
```

### Scenario 3: Blank account ID in card list (acceptable)

```gherkin
GIVEN the card list screen is displayed
  AND the user leaves the account ID field blank
WHEN account ID validation is performed
THEN no error is raised
  AND the account filter flag is set to BLANK
  AND all cards are displayed without account filtering
```

### Scenario 4: Blank account ID in card detail (error)

```gherkin
GIVEN the card detail screen is displayed
  AND the user leaves the account ID field blank
WHEN account ID validation is performed
THEN an input error is raised
  AND the user is prompted to provide an account ID
  AND the account filter flag is set to BLANK with INPUT-ERROR
```

### Scenario 5: All-zeros account ID treated as blank

```gherkin
GIVEN the user enters account ID "00000000000"
WHEN account ID validation is performed
THEN the input is treated identically to a blank account ID
  AND the demo-account-id is set to zeros
```

### Scenario 6: Leading zeros preserved

```gherkin
GIVEN the user enters account ID "00000000042"
WHEN account ID validation is performed
THEN the account ID is accepted as valid
  AND leading zeros are preserved in the 11-digit field
  AND the value "00000000042" is stored exactly as entered
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 4 §3 | Operational risk management — input validation must prevent erroneous data from entering the system | Strict 11-digit numeric validation prevents malformed account IDs from reaching database lookups or financial operations |
| PSD2 | Art. 97 | Strong customer authentication for accessing payment account information | Account ID validation is the first step in identifying which account data to display; invalid IDs are rejected before any data access occurs |

## Edge Cases

1. **PIC X(11) vs PIC 9(11) dual check**: The COBOL code checks both `CC-ACCT-ID` (PIC X) and `CC-ACCT-ID-N` (PIC 9 REDEFINES). The alphanumeric check for SPACES/LOW-VALUES catches non-printable input, while the numeric check for ZEROS catches "00000000000". The IS NOT NUMERIC test catches mixed alphanumeric input. The migrated system should replicate all three checks.

2. **Error message routing**: In COCRDLIC, errors go to `WS-ERROR-MSG`. In COCRDSLC and COCRDUPC, errors go to `WS-RETURN-MSG` (only if `WS-RETURN-MSG-OFF`). This means the first error takes precedence — subsequent validation failures do not overwrite the message. The migrated system should collect all validation errors.

3. **Context-dependent blank handling**: The different treatment of blank account IDs between list (acceptable) and detail/update (error) programs reflects different use cases. The card list supports browsing all cards, while detail/update requires a specific account. The migrated API should enforce this at the endpoint level.

4. **COCRDUPC additional field clear**: When account validation fails in the update program, `CCUP-NEW-ACCTID` is also cleared to LOW-VALUES (lines 734, 749). This prevents stale account data from being used in subsequent update operations.

5. **Swedish characters in account ID**: Account IDs are strictly numeric, so character encoding is not a concern for this field. However, the error message text must be converted from EBCDIC to UTF-8 for the migrated system.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Are there any account ID validation rules beyond 11-digit numeric (e.g., check digit algorithms like Luhn)? (2) Should the migrated system validate that the account ID exists in the database as part of input validation, or only check format? (3) Is the different blank-handling between list and detail/update programs intentional, or an inconsistency that should be normalized?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
