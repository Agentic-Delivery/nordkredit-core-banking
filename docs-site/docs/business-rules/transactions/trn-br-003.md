---
id: "trn-br-003"
title: "Transaction add with validation"
domain: "transactions"
cobol_source: "COTRN02C.cbl:106-784"
requirement_id: "TRN-BR-003"
regulations:
  - "PSD2 Art. 97"
  - "PSD2 Art. 64"
  - "FSA FFFS 2014:5"
  - "AML 2017:11"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-003: Transaction add with validation

## Summary

The transaction add screen allows authorized users to create a new transaction in the TRANSACT file. The program performs comprehensive input validation covering key fields (Account ID or Card Number), data fields (type code, category code, source, description, amount, dates, merchant information), and confirmation. Transactions are assigned sequential IDs by reading the last record in the file and incrementing. The program validates dates using the CSUTLDTC utility and cross-references card numbers with account records. Extracted from `COTRN02C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM MAIN-PARA:
    IF COMMAREA is empty (EIBCALEN = 0)
        Return to sign-on screen
    END-IF

    IF first entry (not reenter)
        SET reenter flag = TRUE
        IF a card was pre-selected
            Populate card number input
            PERFORM PROCESS-ENTER-KEY
        END-IF
        SEND add screen
    ELSE
        RECEIVE screen input
        EVALUATE EIBAID
            WHEN ENTER
                PERFORM PROCESS-ENTER-KEY
            WHEN PF3
                Return to calling program
            WHEN PF4
                Clear all fields
            WHEN PF5
                Copy last transaction data (template feature)
            WHEN OTHER
                Display "Invalid key pressed"
        END-EVALUATE
    END-IF

PROCESS-ENTER-KEY:
    PERFORM VALIDATE-INPUT-KEY-FIELDS
    PERFORM VALIDATE-INPUT-DATA-FIELDS

    EVALUATE confirmation flag (CONFIRMI)
        WHEN 'Y' or 'y'
            PERFORM ADD-TRANSACTION
        WHEN 'N', 'n', SPACES, LOW-VALUES
            Display "Confirm to add this transaction..."
        WHEN OTHER
            Display "Invalid value. Valid values are (Y/N)..."
    END-EVALUATE

VALIDATE-INPUT-KEY-FIELDS:
    IF Account ID is provided (not spaces)
        IF Account ID is NOT numeric
            Error: "Account ID must be Numeric..."
        END-IF
        Convert to numeric, look up in CXACAIX file (account AIX)
        Populate Card Number from cross-reference
    ELSE IF Card Number is provided (not spaces)
        IF Card Number is NOT numeric
            Error: "Card Number must be Numeric..."
        END-IF
        Convert to numeric, look up in CCXREF file (card xref)
        Populate Account ID from cross-reference
    ELSE
        Error: "Account or Card Number must be entered..."
    END-IF

VALIDATE-INPUT-DATA-FIELDS:
    Validate each field is not empty (all mandatory):
    1. Type CD (must be numeric)
    2. Category CD (must be numeric)
    3. Source
    4. Description
    5. Amount (format: -99999999.99, sign + 8 digits + decimal + 2 digits)
    6. Orig Date (format: YYYY-MM-DD, validated via CSUTLDTC)
    7. Proc Date (format: YYYY-MM-DD, validated via CSUTLDTC)
    8. Merchant ID (must be numeric)
    9. Merchant Name
    10. Merchant City
    11. Merchant Zip

    Amount format validation:
        Position 1: must be '+' or '-'
        Positions 2-9: must be numeric (8 digits)
        Position 10: must be '.'
        Positions 11-12: must be numeric (2 decimal places)

    Date validation:
        Positions 1-4: numeric (YYYY)
        Position 5: '-'
        Positions 6-7: numeric (MM)
        Position 8: '-'
        Positions 9-10: numeric (DD)
        Then validated via CALL 'CSUTLDTC' for calendar validity

ADD-TRANSACTION:
    Position to end of TRANSACT file (STARTBR with HIGH-VALUES)
    READPREV to get last record's TRAN-ID
    ENDBR
    New TRAN-ID = last TRAN-ID + 1
    INITIALIZE TRAN-RECORD
    Populate all fields from screen input
    Convert amount using NUMVAL-C function
    WRITE new record to TRANSACT file
    Display success: "Transaction added successfully. Your Tran ID is NNNN."
```

### Validation Rules

| Field | Type | Validation | Error Message |
|---|---|---|---|
| Account ID | Key | Must be numeric; must exist in CXACAIX | "Account ID must be Numeric..." / "Account ID NOT found..." |
| Card Number | Key | Must be numeric; must exist in CCXREF | "Card Number must be Numeric..." / "Card Number NOT found..." |
| Type CD | Data | Cannot be empty; must be numeric | "Type CD can NOT be empty..." / "Type CD must be Numeric..." |
| Category CD | Data | Cannot be empty; must be numeric | "Category CD can NOT be empty..." / "Category CD must be Numeric..." |
| Source | Data | Cannot be empty | "Source can NOT be empty..." |
| Description | Data | Cannot be empty | "Description can NOT be empty..." |
| Amount | Data | Cannot be empty; format ±99999999.99 | "Amount can NOT be empty..." / "Amount should be in format -99999999.99" |
| Orig Date | Data | Cannot be empty; format YYYY-MM-DD; calendar valid | "Orig Date can NOT be empty..." / "Orig Date should be in format YYYY-MM-DD" / "Orig Date - Not a valid date..." |
| Proc Date | Data | Cannot be empty; format YYYY-MM-DD; calendar valid | "Proc Date can NOT be empty..." / "Proc Date should be in format YYYY-MM-DD" / "Proc Date - Not a valid date..." |
| Merchant ID | Data | Cannot be empty; must be numeric | "Merchant ID can NOT be empty..." / "Merchant ID must be Numeric..." |
| Merchant Name | Data | Cannot be empty | "Merchant Name can NOT be empty..." |
| Merchant City | Data | Cannot be empty | "Merchant City can NOT be empty..." |
| Merchant Zip | Data | Cannot be empty | "Merchant Zip can NOT be empty..." |
| Confirm | Flow | Must be 'Y'/'y' to proceed | "Confirm to add this transaction..." / "Invalid value. Valid values are (Y/N)..." |

## Source COBOL Reference

**Program:** `COTRN02C.cbl`
**Lines:** 164-188 (enter key), 193-230 (key field validation), 235-437 (data field validation), 442-466 (add transaction), 471-495 (copy last), 576-637 (cross-reference lookups), 711-749 (write file)

```cobol
000383           COMPUTE WS-TRAN-AMT-N = FUNCTION NUMVAL-C(TRNAMTI OF
000384           COTRN2AI)
000385           MOVE WS-TRAN-AMT-N TO WS-TRAN-AMT-E
000386           MOVE WS-TRAN-AMT-E TO TRNAMTI OF COTRN2AI
```

```cobol
000444           MOVE HIGH-VALUES TO TRAN-ID
000445           PERFORM STARTBR-TRANSACT-FILE
000446           PERFORM READPREV-TRANSACT-FILE
000447           PERFORM ENDBR-TRANSACT-FILE
000448           MOVE TRAN-ID     TO WS-TRAN-ID-N
000449           ADD 1 TO WS-TRAN-ID-N
000450           INITIALIZE TRAN-RECORD
000451           MOVE WS-TRAN-ID-N         TO TRAN-ID
```

### Financial Precision

| Field | COBOL PIC | Precision | Notes |
|---|---|---|---|
| TRAN-AMT (record) | S9(09)V99 | Signed, 9 integer digits, 2 decimal places | Implied decimal point |
| WS-TRAN-AMT-N | S9(9)V99 | Same as record format | Working storage conversion |
| WS-TRAN-AMT-E | +99999999.99 | Display format with explicit sign and decimal | Used for screen display |
| Input format | ±99999999.99 | 12 characters total | Sign + 8 digits + dot + 2 digits |

**Critical**: The NUMVAL-C intrinsic function is used to convert the edited display format to a numeric value. The migrated system must use equivalent parsing that handles the sign character and decimal point identically.

## Acceptance Criteria

### Scenario 1: Successful transaction add with Account ID

GIVEN the user enters a valid Account ID
  AND all data fields are populated with valid values
  AND the user enters 'Y' for confirmation
WHEN the system processes the transaction
THEN the card number is auto-populated from the CXACAIX cross-reference
  AND a new sequential transaction ID is assigned
  AND the transaction record is written to the TRANSACT file
  AND the message "Transaction added successfully. Your Tran ID is NNNN." is displayed in green

### Scenario 2: Successful transaction add with Card Number

GIVEN the user enters a valid Card Number
  AND all data fields are populated with valid values
  AND the user enters 'Y' for confirmation
WHEN the system processes the transaction
THEN the account ID is auto-populated from the CCXREF cross-reference
  AND the transaction is written successfully

### Scenario 3: Neither Account ID nor Card Number provided

GIVEN the transaction add screen is displayed
WHEN the user presses ENTER without entering an Account ID or Card Number
THEN the message "Account or Card Number must be entered..." is displayed

### Scenario 4: Invalid amount format

GIVEN the user enters an amount not matching ±99999999.99
WHEN the system validates the input
THEN the message "Amount should be in format -99999999.99" is displayed

### Scenario 5: Invalid origination date

GIVEN the user enters an origination date that is not a valid calendar date
WHEN the system validates via CSUTLDTC
THEN the message "Orig Date - Not a valid date..." is displayed

### Scenario 6: Copy last transaction (PF5)

GIVEN the user has entered a valid Account ID or Card Number
WHEN the user presses PF5
THEN the last transaction's data fields are populated on screen
  AND the key fields remain as entered
  AND the user can modify and submit

### Scenario 7: Sequential ID assignment

GIVEN transactions exist in the TRANSACT file
WHEN a new transaction is added
THEN the new ID equals the highest existing ID + 1
  AND the ID is unique (DUPKEY/DUPREC check on write)

### Scenario 8: Duplicate transaction ID

GIVEN a race condition causes a duplicate transaction ID
WHEN the WRITE encounters DUPKEY or DUPREC
THEN the message "Tran ID already exist..." is displayed
  AND no record is written

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 97 | Strong customer authentication | Transaction creation requires authenticated CICS session; migrated system must enforce SCA |
| PSD2 | Art. 64 | Transaction data integrity | All mandatory fields validated before write; amount precision enforced |
| FSA FFFS 2014:5 | Ch. 7 | Financial record integrity | Sequential ID assignment ensures traceability; cross-reference validation ensures account linkage |
| AML 2017:11 | Para. 3 | Transaction monitoring prerequisites | Merchant information (ID, name, city, zip) captured for each transaction enables downstream AML screening |

## Edge Cases

1. **Account-to-Card vs Card-to-Account lookup**: The program supports two entry paths — entering Account ID looks up the card via CXACAIX (alternate index), while entering Card Number looks up the account via CCXREF. The migrated system must support both lookup directions.

2. **NUMVAL-C precision**: The FUNCTION NUMVAL-C converts edited numeric strings (with sign, decimal point, currency symbols) to a numeric value. The migrated system's parsing must handle the exact same format string to avoid precision loss.

3. **Date validation via CSUTLDTC**: Dates are validated by calling the CSUTLDTC utility subroutine. If the severity code is not '0000' and the message number is not '2513', the date is rejected. Message '2513' appears to be a warning that is tolerated. The migrated system must replicate this exact validation logic.

4. **Sequential ID generation under concurrency**: The current approach reads the last record and increments the ID. Under concurrent access, this could produce duplicates (caught by the DUPREC check). The migrated system should use database sequences or similar atomic ID generation.

5. **Copy-last-transaction feature**: PF5 copies data from the last transaction in the file to pre-populate the add form. This is a productivity feature for data entry operators. The migrated system should provide equivalent template/copy functionality.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Is the copy-last-transaction feature (PF5) still needed in the migrated system? (2) Should sequential ID generation be replaced with a database sequence? (3) Are there business rules around which transaction type/category combinations are valid? (4) The date validation tolerates CSUTLDTC message '2513' — what does this warning mean and should it be preserved?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
