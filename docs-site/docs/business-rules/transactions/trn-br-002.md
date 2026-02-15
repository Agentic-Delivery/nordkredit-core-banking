---
id: "trn-br-002"
title: "Transaction detail view and lookup"
domain: "transactions"
cobol_source: "COTRN01C.cbl:85-296"
requirement_id: "TRN-BR-002"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
  - "FSA FFFS 2014:5"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-002: Transaction detail view and lookup

## Summary

The transaction detail view screen allows users to look up and display the full details of a single transaction from the TRANSACT file. The user enters a Transaction ID, and the program reads the matching record from the VSAM file and populates all fields on the screen. This includes transaction metadata (ID, type code, category code, source), financial data (amount), merchant information (ID, name, city, zip), and timestamps (origination and processing dates). The program supports navigation back to the transaction list (PF5) or main menu (PF3), and screen clearing (PF4). Extracted from `COTRN01C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM MAIN-PARA:
    IF COMMAREA is empty (EIBCALEN = 0)
        Return to sign-on screen (COSGN00C)
    END-IF

    IF first entry (not reenter)
        SET reenter flag = TRUE
        Initialize screen to LOW-VALUES
        IF a transaction was pre-selected from list (CDEMO-CT01-TRN-SELECTED not empty)
            Populate input field with selected transaction ID
            PERFORM PROCESS-ENTER-KEY (auto-lookup)
        END-IF
        SEND screen
    ELSE
        RECEIVE screen input
        EVALUATE EIBAID
            WHEN ENTER
                PERFORM PROCESS-ENTER-KEY
            WHEN PF3
                Return to calling program (or COMEN01C)
            WHEN PF4
                Clear all screen fields
            WHEN PF5
                Return to transaction list (COTRN00C)
            WHEN OTHER
                Display "Invalid key pressed"
        END-EVALUATE
    END-IF

    EXEC CICS RETURN with COMMAREA

PROCESS-ENTER-KEY:
    IF Transaction ID input is empty
        Display "Tran ID can NOT be empty..."
        Exit
    END-IF

    Clear all detail fields on screen
    MOVE input Transaction ID to TRAN-ID key
    PERFORM READ-TRANSACT-FILE

    IF record found
        Populate screen with all transaction fields:
        - TRAN-ID (display)
        - TRAN-CARD-NUM (card number)
        - TRAN-TYPE-CD (transaction type code)
        - TRAN-CAT-CD (category code)
        - TRAN-SOURCE (source)
        - TRAN-AMT formatted as +99999999.99
        - TRAN-DESC (description)
        - TRAN-ORIG-TS (origination timestamp)
        - TRAN-PROC-TS (processing timestamp)
        - TRAN-MERCHANT-ID
        - TRAN-MERCHANT-NAME
        - TRAN-MERCHANT-CITY
        - TRAN-MERCHANT-ZIP
        SEND screen
    END-IF

READ-TRANSACT-FILE:
    EXEC CICS READ DATASET('TRANSACT')
        INTO(TRAN-RECORD) RIDFLD(TRAN-ID)
        UPDATE RESP(WS-RESP-CD)
    END-EXEC
    IF NOTFND: Display "Transaction ID NOT found..."
    IF OTHER error: Display "Unable to lookup Transaction..."
```

### Decision Table

| User Action | Transaction ID Input | Outcome |
|---|---|---|
| ENTER | Empty | Error: "Tran ID can NOT be empty..." |
| ENTER | Valid numeric ID (exists) | Transaction details displayed |
| ENTER | Valid numeric ID (not found) | Error: "Transaction ID NOT found..." |
| PF3 | N/A | Return to calling program |
| PF4 | N/A | Clear all screen fields |
| PF5 | N/A | Return to transaction list (COTRN00C) |
| Other key | N/A | Error: "Invalid key pressed" |

## Source COBOL Reference

**Program:** `COTRN01C.cbl`
**Lines:** 85-139 (main control flow), 144-192 (enter key processing), 267-296 (read file), 309-326 (initialize fields)

```cobol
000176           IF NOT ERR-FLG-ON
000177               MOVE TRAN-AMT TO WS-TRAN-AMT
000178               MOVE TRAN-ID      TO TRNIDI    OF COTRN1AI
000179               MOVE TRAN-CARD-NUM      TO CARDNUMI    OF COTRN1AI
000180               MOVE TRAN-TYPE-CD        TO TTYPCDI   OF COTRN1AI
000181               MOVE TRAN-CAT-CD        TO TCATCDI   OF COTRN1AI
000182               MOVE TRAN-SOURCE       TO TRNSRCI  OF COTRN1AI
000183               MOVE WS-TRAN-AMT      TO TRNAMTI    OF COTRN1AI
000184               MOVE TRAN-DESC      TO TDESCI    OF COTRN1AI
000185               MOVE TRAN-ORIG-TS        TO TORIGDTI   OF COTRN1AI
000186               MOVE TRAN-PROC-TS       TO TPROCDTI  OF COTRN1AI
000187               MOVE TRAN-MERCHANT-ID       TO MIDI  OF COTRN1AI
000188               MOVE TRAN-MERCHANT-NAME       TO MNAMEI  OF COTRN1AI
000189               MOVE TRAN-MERCHANT-CITY       TO MCITYI  OF COTRN1AI
000190               MOVE TRAN-MERCHANT-ZIP       TO MZIPI  OF COTRN1AI
000191               PERFORM SEND-TRNVIEW-SCREEN
000192           END-IF.
```

### Transaction Detail Fields

| Screen Field | COBOL Field | Type | Description |
|---|---|---|---|
| Transaction ID | TRAN-ID | X(16) | Unique transaction identifier |
| Card Number | TRAN-CARD-NUM | X(16) | Associated credit card number |
| Type Code | TRAN-TYPE-CD | X(02) | Transaction type classification |
| Category Code | TRAN-CAT-CD | 9(04) | Transaction category classification |
| Source | TRAN-SOURCE | X(10) | Transaction origination source |
| Amount | TRAN-AMT | S9(09)V99 | Transaction amount (signed, 2 decimal places) |
| Description | TRAN-DESC | X(100) | Free-text transaction description |
| Origination Date | TRAN-ORIG-TS | X(26) | Timestamp when transaction was originated |
| Processing Date | TRAN-PROC-TS | X(26) | Timestamp when transaction was processed |
| Merchant ID | TRAN-MERCHANT-ID | 9(09) | Merchant identifier |
| Merchant Name | TRAN-MERCHANT-NAME | X(50) | Merchant business name |
| Merchant City | TRAN-MERCHANT-CITY | X(50) | Merchant city |
| Merchant ZIP | TRAN-MERCHANT-ZIP | X(10) | Merchant postal code |

## Acceptance Criteria

### Scenario 1: View transaction from list selection

GIVEN the user selected a transaction from the transaction list (COTRN00C)
  AND the selected transaction ID is passed via COMMAREA
WHEN the transaction view screen loads for the first time
THEN the transaction record is automatically looked up
  AND all 13 detail fields are populated on the screen

### Scenario 2: Manual transaction ID lookup

GIVEN the transaction view screen is displayed
WHEN the user enters a valid transaction ID and presses ENTER
THEN the matching transaction record is read from the TRANSACT file
  AND all detail fields are populated

### Scenario 3: Transaction not found

GIVEN the transaction view screen is displayed
WHEN the user enters a transaction ID that does not exist
  AND presses ENTER
THEN the message "Transaction ID NOT found..." is displayed
  AND no detail fields are populated

### Scenario 4: Empty transaction ID

GIVEN the transaction view screen is displayed
WHEN the user presses ENTER without entering a transaction ID
THEN the message "Tran ID can NOT be empty..." is displayed

### Scenario 5: Clear screen

GIVEN the transaction view screen shows transaction details
WHEN the user presses PF4
THEN all detail fields are cleared to spaces
  AND the cursor returns to the transaction ID input field

### Scenario 6: Return to transaction list

GIVEN the transaction view screen is displayed
WHEN the user presses PF5
THEN control returns to the transaction list program (COTRN00C)

### Scenario 7: Amount formatting precision

GIVEN a transaction record with TRAN-AMT = S9(09)V99
WHEN the amount is displayed on screen
THEN it is formatted as +99999999.99 (signed with 2 decimal places)
  AND the original precision is preserved without rounding

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 97 | Strong customer authentication for payment account access | Transaction view requires CICS session authentication; migrated system must enforce SCA |
| GDPR | Art. 15 | Right of access to personal data | Provides detailed transaction view including merchant data linked to cardholder |
| FSA FFFS 2014:5 | Ch. 7 | Traceability of financial transactions | Full transaction detail including origination and processing timestamps supports audit requirements |

## Edge Cases

1. **UPDATE mode on READ**: The CICS READ uses the UPDATE keyword (line 275), which acquires an exclusive lock on the record. This was likely intended for future update functionality that was never implemented. The migrated system should use a read-only query unless update capability is explicitly required.

2. **Pre-selected transaction from list**: When the program is entered from COTRN00C with a selected transaction ID, the lookup is performed automatically. The migrated system must preserve this workflow — the detail view should accept a transaction ID parameter and auto-populate.

3. **Return navigation**: PF3 returns to the calling program (CDEMO-FROM-PROGRAM), not always to the menu. PF5 explicitly returns to COTRN00C. The migrated system must maintain both navigation paths.

4. **Amount display format**: The working storage field WS-TRAN-AMT uses PIC +99999999.99, which means the sign is displayed. The migrated system must preserve this signed display format.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The UPDATE keyword on the READ operation needs clarification — is this intentional for planned edit functionality or a development artifact? The migrated system should clarify whether transaction records can be modified after posting.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
