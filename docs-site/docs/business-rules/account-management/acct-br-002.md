---
id: "acct-br-002"
title: "Account view read-only lookup"
domain: "account-management"
cobol_source: "COACTVWC.cbl:261-941"
requirement_id: "ACCT-BR-002"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# ACCT-BR-002: Account view read-only lookup

## Summary

The COACTVWC program provides read-only account viewing within a CICS online transaction (transaction code CAVW). The user enters an 11-digit account ID on the screen. The program performs a three-file read chain to assemble the complete account view: (1) the card cross-reference file (CXACAIX) is read to find the associated customer via the account-to-card alternate index, (2) the account master file (ACCTDAT) is read for account data, and (3) the customer master file (CUSTDAT) is read for customer personal data. All fields are displayed in a protected (read-only) screen with no update capability. SSN is formatted as xxx-xx-xxxx for display. Only ENTER (refresh/submit) and PF3 (exit) are valid function keys; all other function keys are silently remapped to ENTER behavior. Specific error messages are returned for each file read failure, including CICS RESP and RESP2 codes for diagnostic purposes.

## Business Logic

### Pseudocode

```
PERFORM PROCESS-ACCOUNT-VIEW:

    -- Step 1: Validate account ID input
    PERFORM 9000-VALIDATE-ACCOUNT-ID:
        IF account-id = SPACES OR LOW-VALUES
            MOVE 'ACCOUNT NUMBER MUST BE ENTERED' TO WS-MESSAGE
            SET input-error = TRUE
            EXIT
        END-IF
        IF account-id IS NOT NUMERIC
            MOVE 'ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC' TO WS-MESSAGE
            SET input-error = TRUE
            EXIT
        END-IF
        IF account-id = ZEROS
            MOVE 'ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC' TO WS-MESSAGE
            SET input-error = TRUE
            EXIT
        END-IF

    -- Step 2: Three-file read chain
    PERFORM 9100-READ-CARD-XREF:
        EXEC CICS READ FILE('CXACAIX')
            INTO(CARD-XREF-RECORD)
            RIDFLD(account-id)
            RESP(WS-RESP-CD) RESP2(WS-RESP2-CD)
        END-EXEC
        IF WS-RESP-CD NOT = DFHRESP(NORMAL)
            MOVE 'CARD XREF READ FAILED' TO WS-MESSAGE
            MOVE WS-RESP-CD TO WS-DISPLAY-RESP
            MOVE WS-RESP2-CD TO WS-DISPLAY-RESP2
            SET read-failed = TRUE
            EXIT
        END-IF
        EXTRACT customer-id FROM CARD-XREF-RECORD

    PERFORM 9200-READ-ACCOUNT:
        EXEC CICS READ FILE('ACCTDAT')
            INTO(ACCOUNT-RECORD)
            RIDFLD(account-id)
            RESP(WS-RESP-CD) RESP2(WS-RESP2-CD)
        END-EXEC
        IF WS-RESP-CD NOT = DFHRESP(NORMAL)
            MOVE 'ACCOUNT READ FAILED' TO WS-MESSAGE
            SET read-failed = TRUE
            EXIT
        END-IF

    PERFORM 9300-READ-CUSTOMER:
        EXEC CICS READ FILE('CUSTDAT')
            INTO(CUSTOMER-RECORD)
            RIDFLD(customer-id)
            RESP(WS-RESP-CD) RESP2(WS-RESP2-CD)
        END-EXEC
        IF WS-RESP-CD NOT = DFHRESP(NORMAL)
            MOVE 'CUSTOMER READ FAILED' TO WS-MESSAGE
            SET read-failed = TRUE
            EXIT
        END-IF

    -- Step 3: Format display fields
    PERFORM 9400-FORMAT-DISPLAY:
        FORMAT SSN as xxx-xx-xxxx using STRING:
            STRING ssn-part1 '-' ssn-part2 '-' ssn-part3
                DELIMITED BY SIZE
                INTO formatted-ssn
        SET all screen fields to PROTECTED (read-only)
        SET account-id field to UNPROTECTED (editable for new lookup)

    -- Step 4: Handle function keys
    EVALUATE function-key
        WHEN PF3
            RETURN to calling program or main menu
        WHEN ENTER
            Process current input (new account lookup)
        WHEN OTHER
            Remap to ENTER behavior (silently ignore invalid keys)
    END-EVALUATE
```

### Decision Table

| Account ID Input | CXACAIX Read | ACCTDAT Read | CUSTDAT Read | Outcome |
|-----------------|-------------|-------------|-------------|---------|
| Blank/spaces | N/A | N/A | N/A | Error: "ACCOUNT NUMBER MUST BE ENTERED" |
| Non-numeric | N/A | N/A | N/A | Error: "ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC" |
| All zeros | N/A | N/A | N/A | Error: "ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC" |
| Valid, no xref | NOTFND | N/A | N/A | Error: "CARD XREF READ FAILED" + RESP/RESP2 |
| Valid, xref found | OK | NOTFND | N/A | Error: "ACCOUNT READ FAILED" + RESP/RESP2 |
| Valid, acct found | OK | OK | NOTFND | Error: "CUSTOMER READ FAILED" + RESP/RESP2 |
| Valid, all found | OK | OK | OK | Full account/customer data displayed read-only |

## Source COBOL Reference

**Program:** `COACTVWC.cbl`
**Lines:** 261-941 (main processing), 649-681 (account validation), 687-721 (three-file read chain), 496-503 (SSN formatting), 543 (screen field protection)

**Account ID validation (lines 649-681):**

```cobol
000649 9000-VALIDATE-ACCOUNT-ID.
000650     IF ACCT-ID-INPUT = SPACES OR LOW-VALUES
000651         MOVE 'ACCOUNT NUMBER MUST BE ENTERED'
000652             TO WS-MESSAGE
000653         SET WS-INPUT-ERROR TO TRUE
000654         GO TO 9000-EXIT
000655     END-IF.
000656
000657     IF ACCT-ID-INPUT IS NOT NUMERIC
000658         MOVE 'ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC'
000659             TO WS-MESSAGE
000660         SET WS-INPUT-ERROR TO TRUE
000661         GO TO 9000-EXIT
000662     END-IF.
000663
000664     IF ACCT-ID-INPUT = ZEROS
000665         MOVE 'ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC'
000666             TO WS-MESSAGE
000667         SET WS-INPUT-ERROR TO TRUE
000668         GO TO 9000-EXIT
000669     END-IF.
000670
000681 9000-EXIT.
000682     EXIT.
```

**Three-file read chain (lines 687-721):**

```cobol
000687 9100-READ-CARD-XREF.
000688     EXEC CICS READ
000689         FILE('CXACAIX')
000690         INTO(CARD-XREF-RECORD)
000691         RIDFLD(WS-ACCT-ID)
000692         KEYLENGTH(LENGTH OF WS-ACCT-ID)
000693         RESP(WS-RESP-CD)
000694         RESP2(WS-RESP2-CD)
000695     END-EXEC.
000696
000697     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
000698         MOVE 'CARD XREF READ FAILED' TO WS-MESSAGE
000699         GO TO 9100-EXIT
000700     END-IF.
000701
000702 9200-READ-ACCOUNT.
000703     EXEC CICS READ
000704         FILE('ACCTDAT')
000705         INTO(ACCOUNT-RECORD)
000706         RIDFLD(WS-ACCT-ID)
000707         RESP(WS-RESP-CD)
000708         RESP2(WS-RESP2-CD)
000709     END-EXEC.
000710
000711     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
000712         MOVE 'ACCOUNT READ FAILED' TO WS-MESSAGE
000713         GO TO 9200-EXIT
000714     END-IF.
000715
000716 9300-READ-CUSTOMER.
000717     EXEC CICS READ
000718         FILE('CUSTDAT')
000719         INTO(CUSTOMER-RECORD)
000720         RIDFLD(WS-CUST-ID)
000721         RESP(WS-RESP-CD)
000722         RESP2(WS-RESP2-CD)
000723     END-EXEC.
```

**SSN formatting for display (lines 496-503):**

```cobol
000496     STRING WS-SSN-PART1 DELIMITED BY SIZE
000497            '-'          DELIMITED BY SIZE
000498            WS-SSN-PART2 DELIMITED BY SIZE
000499            '-'          DELIMITED BY SIZE
000500            WS-SSN-PART3 DELIMITED BY SIZE
000501            INTO WS-SSN-DISPLAY
000502     END-STRING.
000503     MOVE WS-SSN-DISPLAY TO SSNO.
```

**Screen field protection (line 543):**

```cobol
000543     MOVE DFHBMFSE TO ACCTSIDA.
```

This BMS attribute byte (DFHBMFSE) sets the account ID field as the only unprotected (editable) field on the screen. All other fields are set to DFHBMPRF (protected/read-only).

## Acceptance Criteria

### Scenario 1: Successful account view with all data

```gherkin
GIVEN an account "00012345678" exists in ACCTDAT
  AND a card cross-reference exists in CXACAIX for that account
  AND the associated customer exists in CUSTDAT
WHEN the user enters account ID "00012345678" and presses ENTER
THEN the screen displays all account fields (balance, credit limit, dates, status)
  AND the screen displays all customer fields (name, address, SSN, phone)
  AND the SSN is formatted as "xxx-xx-xxxx"
  AND all displayed fields are read-only (protected)
  AND only the account ID input field is editable for a new lookup
```

### Scenario 2: Account ID validation rejects blank input

```gherkin
GIVEN the account view screen is displayed
  AND the account ID field is empty
WHEN the user presses ENTER
THEN the error message "ACCOUNT NUMBER MUST BE ENTERED" is displayed
  AND no file reads are performed
```

### Scenario 3: Account ID validation rejects non-numeric input

```gherkin
GIVEN the user enters "ABCDEFGHIJK" as the account ID
WHEN the input is validated
THEN the error message "ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC" is displayed
  AND no file reads are performed
```

### Scenario 4: Account ID validation rejects all-zeros input

```gherkin
GIVEN the user enters "00000000000" as the account ID
WHEN the input is validated
THEN the error message "ACCOUNT NUMBER MUST BE 11 DIGIT NUMERIC" is displayed
  AND no file reads are performed
```

### Scenario 5: Card cross-reference read failure

```gherkin
GIVEN the user enters a valid account ID "99999999999"
  AND no card cross-reference record exists for that account in CXACAIX
WHEN the three-file read chain executes
THEN the error message "CARD XREF READ FAILED" is displayed
  AND the CICS RESP and RESP2 codes are included in the error display
  AND no further file reads (ACCTDAT, CUSTDAT) are attempted
```

### Scenario 6: Account master read failure

```gherkin
GIVEN the user enters a valid account ID "12345678901"
  AND a card cross-reference exists for the account
  AND no account record exists in ACCTDAT
WHEN the three-file read chain executes
THEN the error message "ACCOUNT READ FAILED" is displayed
  AND the CICS RESP and RESP2 codes are included in the error display
  AND no customer file read is attempted
```

### Scenario 7: Customer master read failure

```gherkin
GIVEN the user enters a valid account ID "12345678901"
  AND the card cross-reference and account records exist
  AND the referenced customer record does not exist in CUSTDAT
WHEN the three-file read chain executes
THEN the error message "CUSTOMER READ FAILED" is displayed
  AND the CICS RESP and RESP2 codes are included in the error display
```

### Scenario 8: PF3 exits to calling program

```gherkin
GIVEN the account view screen is displayed
WHEN the user presses PF3
THEN the program returns to the calling program (if invoked via XCTL)
  OR the program returns to the main menu (if invoked directly)
```

### Scenario 9: Invalid function keys are silently remapped

```gherkin
GIVEN the account view screen is displayed
WHEN the user presses PF5 (or any key other than ENTER or PF3)
THEN the keypress is treated as if ENTER was pressed
  AND no error message about invalid keys is displayed
```

### Scenario 10: SSN display formatting

```gherkin
GIVEN a customer record with SSN = "123456789"
WHEN the customer data is displayed on the account view screen
THEN the SSN is shown as "123-45-6789"
  AND the dashes are inserted between positions 3-4 and 5-6
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for accessing payment account information | The account view is a CICS transaction requiring terminal authentication; the migrated REST API must enforce SCA (e.g., OAuth 2.0 + MFA) before returning account data. The read-only nature limits exposure to information disclosure only (no modification risk) |
| GDPR | Art. 15 | Data subject's right of access to personal data | The account view enables authorized staff to retrieve customer data including SSN, address, and account details to fulfill data access requests. The migrated system must log all access events for audit purposes |
| GDPR | Art. 5(1)(f) | Integrity and confidentiality -- appropriate security measures for personal data processing | SSN formatting (xxx-xx-xxxx) is a display convenience, not a masking control. The migrated system should consider whether SSN should be partially masked (e.g., xxx-xx-6789) in the UI, with full SSN available only to authorized roles |

## Edge Cases

1. **Orphaned cross-reference records**: If a card cross-reference record exists in CXACAIX but the referenced account record has been deleted from ACCTDAT, the second read in the chain fails. The COBOL program handles this with a generic "ACCOUNT READ FAILED" message. The migrated system should provide a more specific diagnostic indicating a referential integrity issue between CXACAIX and ACCTDAT.

2. **Multiple cards per account in cross-reference**: The CXACAIX alternate index may return only the first card cross-reference for a given account ID. The COBOL program uses a single READ (not STARTBR/READNEXT), so it only retrieves the first matching cross-reference record. If the account has multiple cards, the customer ID extracted from the first cross-reference is used. The migrated system should verify that all cross-references for an account point to the same customer ID.

3. **SSN with leading zeros**: If the first part of the SSN starts with zeros (e.g., "001-23-4567"), the STRING operation preserves the leading zeros because WS-SSN-PART1 is defined as PIC X(3). The migrated system must ensure that SSN formatting does not inadvertently strip leading zeros when using numeric or string parsing.

4. **Concurrent data changes during display**: Because the account view performs three separate READ operations (not within a CICS unit of work with isolation), another transaction could modify the account or customer data between reads. The displayed data may show an inconsistent snapshot. The migrated system should consider whether these reads should be wrapped in a single database transaction with appropriate isolation level.

5. **CICS RESP2 diagnostic codes**: The COBOL program captures both RESP and RESP2 codes for error reporting. RESP2 provides additional diagnostic information (e.g., for NOTFND, RESP2 indicates whether the key was not found vs. the file is empty). The migrated system must provide equivalent diagnostic granularity, for example by logging specific SQL error codes or exception details.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Is the three-file read chain (CXACAIX -> ACCTDAT -> CUSTDAT) always required, or are there cases where an account view can be displayed without the customer data? (2) Should the migrated system mask the SSN on the view screen (GDPR data minimisation), or is full SSN display required for business operations? (3) Are the RESP/RESP2 codes displayed to the end user in production, or only in the development/test environment? (4) Does the silent remapping of invalid function keys to ENTER cause any confusion for users in practice?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
