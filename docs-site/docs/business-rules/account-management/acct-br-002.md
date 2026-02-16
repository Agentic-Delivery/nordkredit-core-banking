---
id: "acct-br-002"
title: "Account detail view and balance enquiry"
domain: "account-management"
cobol_source: "COACTVWC.cbl:94-450"
requirement_id: "ACCT-BR-002"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
  - "FSA FFFS 2014:5 Ch. 7"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-002: Account detail view and balance enquiry

## Summary

The COACTVWC program provides read-only account detail view functionality via CICS online transaction processing. When invoked, it receives an account ID through the COMMAREA (passed from the account list or card management programs), reads the ACCOUNT-RECORD from the ACCTDAT VSAM file, and displays all account fields on a formatted BMS screen. The program supports the standard navigation keys: PF3 (return to previous screen) and ENTER (refresh display). No modification of account data is permitted through this program — updates are handled by COACTUPC. This is the primary account enquiry function used by bank staff and is referenced from both the card management (COCRDSLC) and transaction management (COTRN01C) programs.

## Business Logic

### Pseudocode

```
PERFORM MAIN-PARA:
    IF COMMAREA is empty (EIBCALEN = 0)
        Return to sign-on screen (COSGN00C)
    END-IF

    IF first entry into program (not reenter)
        SET reenter flag = TRUE
        MOVE account-id from COMMAREA to working storage
        PERFORM READ-ACCOUNT-DATA
        PERFORM FORMAT-DISPLAY-FIELDS
        PERFORM SEND-ACTVW-SCREEN
    ELSE
        RECEIVE screen input
        EVALUATE user key pressed (EIBAID)
            WHEN ENTER
                PERFORM READ-ACCOUNT-DATA
                PERFORM FORMAT-DISPLAY-FIELDS
                PERFORM SEND-ACTVW-SCREEN
            WHEN PF3
                Return to calling program via XCTL
            WHEN OTHER
                Display "Invalid key pressed. Please try again."
                PERFORM SEND-ACTVW-SCREEN
        END-EVALUATE
    END-IF

    EXEC CICS RETURN with COMMAREA preserving state

READ-ACCOUNT-DATA:
    MOVE account-id TO ACCT-ID
    EXEC CICS READ FILE('ACCTDAT')
        INTO(ACCOUNT-RECORD)
        RIDFLD(ACCT-ID)
        RESP(WS-RESP-CD)
    END-EXEC
    IF RESP = NOTFND
        MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE
    ELSE IF RESP NOT = NORMAL
        MOVE 'ERROR READING ACCOUNT FILE' TO WS-MESSAGE
    END-IF

FORMAT-DISPLAY-FIELDS:
    MOVE ACCT-ID               TO display-account-id
    MOVE ACCT-ACTIVE-STATUS    TO display-status
    MOVE ACCT-CURR-BAL         TO display-current-balance
        FORMAT: +9,999,999,999.99 (signed with comma separators)
    MOVE ACCT-CREDIT-LIMIT     TO display-credit-limit
    MOVE ACCT-CASH-CREDIT-LIMIT TO display-cash-limit
    MOVE ACCT-OPEN-BAL         TO display-open-balance
    MOVE ACCT-CURR-CYC-CREDIT  TO display-cycle-credit
    MOVE ACCT-CURR-CYC-DEBIT   TO display-cycle-debit
    MOVE ACCT-EXPIRAION-DATE   TO display-expiration-date
    MOVE ACCT-REISSUE-DATE     TO display-reissue-date
    MOVE ACCT-GROUP-ID         TO display-group-id

    COMPUTE display-available-credit =
        ACCT-CREDIT-LIMIT - ACCT-CURR-BAL
    COMPUTE display-cash-available =
        ACCT-CASH-CREDIT-LIMIT - ACCT-CURR-BAL
```

### Decision Table

| User Action | Account Found | Outcome |
|---|---|---|
| First entry (from COMMAREA) | Yes | Display all account fields |
| First entry (from COMMAREA) | No | Display "ACCOUNT NOT FOUND" message |
| ENTER | Yes | Refresh account data display |
| ENTER | No | Display "ACCOUNT NOT FOUND" message |
| PF3 | N/A | Return to calling program |
| Other key | N/A | Display "Invalid key pressed" error |
| Empty COMMAREA | N/A | Return to sign-on screen (COSGN00C) |

## Source COBOL Reference

**Program:** `COACTVWC.cbl`
**Lines:** 94-450

```cobol
000094 PROCEDURE DIVISION.
000095 MAIN-PARA.
000096     IF EIBCALEN = 0
000097         MOVE LOW-VALUES        TO COACTVWO
000098         MOVE -1                TO ACTIDINL
000099         MOVE 'PLEASE ENTER ACCOUNT ID OR PRESS PF3'
000100                                TO MESSAGEO
000101         PERFORM SEND-ACTVW-SCREEN
000102         EXEC CICS RETURN
000103             TRANSID('AV00')
000104             COMMAREA(WS-COMMAREA)
000105         END-EXEC
000106     END-IF.
```

```cobol
000150 READ-ACCOUNT-DATA.
000151     MOVE WS-ACCT-ID           TO ACCT-ID.
000152     EXEC CICS READ
000153         FILE('ACCTDAT')
000154         INTO(ACCOUNT-RECORD)
000155         RIDFLD(ACCT-ID)
000156         KEYLENGTH(LENGTH OF ACCT-ID)
000157         RESP(WS-RESP-CD)
000158     END-EXEC.
000159
000160     IF WS-RESP-CD = DFHRESP(NOTFND)
000161         MOVE 'ACCOUNT NOT FOUND'   TO WS-MESSAGE
000162         GO TO READ-ACCOUNT-EXIT
000163     END-IF.
000164
000165     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
000166         MOVE 'ERROR READING ACCOUNT FILE'
000167                                    TO WS-MESSAGE
000168         GO TO READ-ACCOUNT-EXIT
000169     END-IF.
```

```cobol
000200 FORMAT-DISPLAY-FIELDS.
000201     MOVE ACCT-ID              TO ACTIDIN1O.
000202     MOVE ACCT-ACTIVE-STATUS   TO APTS1O.
000203     MOVE ACCT-CURR-BAL        TO CURBAL1O.
000204     MOVE ACCT-CREDIT-LIMIT    TO CRLIM1O.
000205     MOVE ACCT-CASH-CREDIT-LIMIT TO CASHCRLIM1O.
000206     MOVE ACCT-OPEN-BAL        TO OPENBAL1O.
000207     MOVE ACCT-EXPIRAION-DATE  TO EXPDT1O.
000208     MOVE ACCT-REISSUE-DATE    TO REISSDT1O.
000209     MOVE ACCT-CURR-CYC-CREDIT TO CYCCRE1O.
000210     MOVE ACCT-CURR-CYC-DEBIT  TO CYCDEB1O.
000211     MOVE ACCT-GROUP-ID        TO GRPID1O.
000212
000213     COMPUTE WS-AVAIL-CREDIT =
000214         ACCT-CREDIT-LIMIT - ACCT-CURR-BAL.
000215     MOVE WS-AVAIL-CREDIT      TO APTS2O.
000216
000217     COMPUTE WS-CASH-AVAIL =
000218         ACCT-CASH-CREDIT-LIMIT - ACCT-CURR-BAL.
000219     MOVE WS-CASH-AVAIL        TO APTS3O.
```

### Calculated Display Fields

The program computes two derived values for display that are not stored in the record:

| Display Field | Calculation | Purpose |
|---|---|---|
| Available Credit | ACCT-CREDIT-LIMIT − ACCT-CURR-BAL | Shows remaining credit capacity |
| Cash Available | ACCT-CASH-CREDIT-LIMIT − ACCT-CURR-BAL | Shows remaining cash advance capacity |

### COMMAREA Contract

| Field | PIC | Purpose |
|---|---|---|
| CDEMO-ACCT-ID | 9(11) | Account ID passed from calling program |
| CDEMO-ACCT-STATUS | X(01) | Account status passed from calling program |
| CDEMO-FROM-PROGRAM | X(08) | Originating program for PF3 return navigation |

## Acceptance Criteria

### Scenario 1: Display account details for valid account

```gherkin
GIVEN the account view screen is loaded
  AND account ID "12345678901" is passed via COMMAREA
  AND the account exists in the ACCTDAT file
WHEN the system reads the account record
THEN the account ID "12345678901" is displayed
  AND the current balance is formatted with sign and comma separators
  AND the credit limit, cash credit limit, and open balance are displayed
  AND the expiration date and reissue date are displayed
  AND the current cycle credits and debits are shown
  AND the account group ID is shown
  AND the available credit (credit limit minus current balance) is computed and displayed
  AND the cash available (cash credit limit minus current balance) is computed and displayed
```

### Scenario 2: Account not found

```gherkin
GIVEN the account view screen is loaded
  AND account ID "99999999999" is passed via COMMAREA
  AND the account does not exist in the ACCTDAT file
WHEN the system attempts to read the account record
THEN the message "ACCOUNT NOT FOUND" is displayed
  AND no account data fields are populated on the screen
```

### Scenario 3: Refresh account data on ENTER

```gherkin
GIVEN the account view screen is displaying account "12345678901"
WHEN the user presses ENTER
THEN the account record is re-read from the ACCTDAT file
  AND the display is refreshed with the latest balance and status values
```

### Scenario 4: Return to calling program on PF3

```gherkin
GIVEN the account view screen is displayed
WHEN the user presses PF3
THEN control returns to the calling program (as specified in CDEMO-FROM-PROGRAM)
  AND the COMMAREA state is preserved
```

### Scenario 5: Empty COMMAREA security check

```gherkin
GIVEN a user attempts to invoke the account view program directly
  AND EIBCALEN is 0 (no COMMAREA)
WHEN the program starts
THEN the user is prompted to enter an account ID
  AND no account data is displayed without authentication context
```

### Scenario 6: Invalid key pressed

```gherkin
GIVEN the account view screen is displayed
WHEN the user presses any key other than ENTER or PF3
THEN the message "Invalid key pressed. Please try again." is displayed
  AND the current account data remains displayed
```

### Scenario 7: Available credit calculation with negative balance

```gherkin
GIVEN an account with credit limit 50,000.00 and current balance -2,500.00
WHEN the available credit is computed
THEN the available credit is 50,000.00 - (-2,500.00) = 52,500.00
  AND the cash available follows the same formula with cash credit limit
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 97 | Strong customer authentication for accessing account information | Account view is gated by CICS terminal authentication and COMMAREA validation; migrated system must enforce SCA before displaying account data |
| GDPR | Art. 15 | Data subject's right of access to personal data | Account view provides authorized staff and account holders with read access to their account records |
| FSA FFFS 2014:5 | Ch. 7 | Requirements for information systems providing financial data | Account view presents complete and accurate financial position including balances, limits, and derived available credit |

## Edge Cases

1. **Account file unavailable**: If the ACCTDAT file is not available (CICS RESP = NOTOPEN or DISABLED), the COBOL program falls through to the error handler displaying "ERROR READING ACCOUNT FILE". The migrated system must handle database unavailability with an appropriate error message and audit log entry.

2. **Concurrent updates**: The READ operation does not acquire an update lock (no UPDATE keyword). If another user is simultaneously updating the account via COACTUPC, the view may show slightly stale data. This is acceptable for read-only operations — the migrated system does not need pessimistic locking for account view.

3. **Negative available credit**: If ACCT-CURR-BAL exceeds ACCT-CREDIT-LIMIT (account is over-limit), the computed available credit will be negative. The COBOL program displays the negative value as-is. The migrated system should display this with a clear indicator (e.g., red text, "OVER LIMIT" label).

4. **Large balance formatting**: The display format +9,999,999,999.99 handles up to 10 integer digits. With Swedish Krona (SEK) amounts, this supports balances up to ~10 billion SEK which is sufficient for individual account balances.

5. **Account with all zero balances**: A newly opened account will have all balance fields set to zero. The view screen must display these as "0.00" rather than blank fields to confirm the read was successful.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Is COACTVWC invoked only from card management programs, or is there a dedicated account list program (COACLISC) that is not yet captured?
- Does the available credit calculation need to account for pending debits (ACCT-PEND-DEBIT)?
- Are there any accounts that should be hidden from view (e.g., internal settlement accounts, system accounts)?
- Is the COMMAREA validation (EIBCALEN check) the only security gate, or are there additional CICS resource-level security checks?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
