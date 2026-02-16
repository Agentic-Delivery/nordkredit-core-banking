---
id: "TRN-BR-003"
title: "Transaction add input validation rules"
domain: "transactions"
cobol_source: "COTRN02C.cbl:164-437"
requirement_id: "TRN-BR-003"
regulations:
  - "PSD2 Art. 97 — Strong customer authentication"
  - "FFFS 2014:5 Ch. 4 §3 — Operational risk management"
  - "AML/KYC — Transaction data completeness for monitoring"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# TRN-BR-003: Transaction add input validation rules

## Summary

When adding a new transaction via the online CICS screen (CT02), the system enforces a comprehensive validation chain before the transaction can be posted. Validation covers key field identification (Account ID or Card Number must be provided and verified against the cross-reference file), mandatory data field presence, data type validation, amount format validation, and date format/validity checks. Any validation failure prevents the transaction from being created and displays a specific error message.

## Business Logic

### Pseudocode

```
PERFORM VALIDATE-INPUT-KEY-FIELDS:
    IF Account ID is entered AND not spaces
        IF Account ID is NOT numeric
            ERROR: "Account ID must be Numeric..."
        END-IF
        COMPUTE numeric account ID from input
        READ CXACAIX file (Account-to-Card cross-reference) by Account ID
        IF not found → ERROR: "Account ID NOT found..."
        ELSE → populate Card Number from cross-reference
    ELSE IF Card Number is entered AND not spaces
        IF Card Number is NOT numeric
            ERROR: "Card Number must be Numeric..."
        END-IF
        COMPUTE numeric card number from input
        READ CCXREF file (Card cross-reference) by Card Number
        IF not found → ERROR: "Card Number NOT found..."
        ELSE → populate Account ID from cross-reference
    ELSE
        ERROR: "Account or Card Number must be entered..."
    END-IF

PERFORM VALIDATE-INPUT-DATA-FIELDS:
    Validate each field is not empty (in order):
        Type Code, Category Code, Source, Description,
        Amount, Origination Date, Processing Date,
        Merchant ID, Merchant Name, Merchant City, Merchant Zip

    Validate data types:
        IF Type Code NOT numeric → ERROR
        IF Category Code NOT numeric → ERROR
        IF Amount NOT in format ±99999999.99 → ERROR
        IF Orig Date NOT in format YYYY-MM-DD → ERROR
        IF Proc Date NOT in format YYYY-MM-DD → ERROR
        IF Merchant ID NOT numeric → ERROR

    Validate date values using CSUTLDTC utility:
        IF Orig Date is not a valid calendar date → ERROR
        IF Proc Date is not a valid calendar date → ERROR

    Validate confirmation:
        IF Confirm = 'Y' or 'y' → proceed to add transaction
        IF Confirm = 'N', 'n', spaces → prompt "Confirm to add..."
        IF Confirm = other → ERROR: "Invalid value. Valid values are (Y/N)..."
```

### Decision Table — Key Field Validation

| Account ID | Card Number | XREF Lookup | Outcome |
|-----------|-------------|-------------|---------|
| Provided (numeric) | Empty | Found | Card Number auto-populated from XREF |
| Provided (numeric) | Empty | Not found | Error: "Account ID NOT found..." |
| Provided (non-numeric) | Empty | N/A | Error: "Account ID must be Numeric..." |
| Empty | Provided (numeric) | Found | Account ID auto-populated from XREF |
| Empty | Provided (numeric) | Not found | Error: "Card Number NOT found..." |
| Empty | Provided (non-numeric) | N/A | Error: "Card Number must be Numeric..." |
| Empty | Empty | N/A | Error: "Account or Card Number must be entered..." |

### Decision Table — Data Field Validation

| Field | Validation | Error Message |
|-------|-----------|---------------|
| Type Code | Not empty, numeric | "Type CD can NOT be empty..." / "Type CD must be Numeric..." |
| Category Code | Not empty, numeric | "Category CD can NOT be empty..." / "Category CD must be Numeric..." |
| Source | Not empty | "Source can NOT be empty..." |
| Description | Not empty | "Description can NOT be empty..." |
| Amount | Not empty, format ±99999999.99 | "Amount can NOT be empty..." / "Amount should be in format -99999999.99" |
| Orig Date | Not empty, format YYYY-MM-DD, valid date | "Orig Date can NOT be empty..." / format error / "Not a valid date..." |
| Proc Date | Not empty, format YYYY-MM-DD, valid date | "Proc Date can NOT be empty..." / format error / "Not a valid date..." |
| Merchant ID | Not empty, numeric | "Merchant ID can NOT be empty..." / "Merchant ID must be Numeric..." |
| Merchant Name | Not empty | "Merchant Name can NOT be empty..." |
| Merchant City | Not empty | "Merchant City can NOT be empty..." |
| Merchant Zip | Not empty | "Merchant Zip can NOT be empty..." |

## Source COBOL Reference

**Program:** `COTRN02C.cbl`
**Lines:** 193-437

```cobol
000193 VALIDATE-INPUT-KEY-FIELDS.
000195     EVALUATE TRUE
000196         WHEN ACTIDINI OF COTRN2AI NOT = SPACES AND LOW-VALUES
000197             IF ACTIDINI OF COTRN2AI IS NOT NUMERIC
000198                 MOVE 'Y'     TO WS-ERR-FLG
000199                 MOVE 'Account ID must be Numeric...' TO WS-MESSAGE
...
000210         WHEN CARDNINI OF COTRN2AI NOT = SPACES AND LOW-VALUES
000211             IF CARDNINI OF COTRN2AI IS NOT NUMERIC
000212                 MOVE 'Y'     TO WS-ERR-FLG
000213                 MOVE 'Card Number must be Numeric...' TO WS-MESSAGE
...
000224         WHEN OTHER
000225             MOVE 'Y'     TO WS-ERR-FLG
000226             MOVE 'Account or Card Number must be entered...' TO WS-MESSAGE
...
000235 VALIDATE-INPUT-DATA-FIELDS.
000251     EVALUATE TRUE
000252         WHEN TTYPCDI OF COTRN2AI = SPACES OR LOW-VALUES
000253             MOVE 'Y'     TO WS-ERR-FLG
000254             MOVE 'Type CD can NOT be empty...' TO WS-MESSAGE
...
000339     EVALUATE TRUE
000340         WHEN TRNAMTI OF COTRN2AI(1:1) NOT EQUAL '-' AND '+'
000341         WHEN TRNAMTI OF COTRN2AI(2:8) NOT NUMERIC
000342         WHEN TRNAMTI OF COTRN2AI(10:1) NOT = '.'
000343         WHEN TRNAMTI OF COTRN2AI(11:2) IS NOT NUMERIC
000344             MOVE 'Y'     TO WS-ERR-FLG
000345             MOVE 'Amount should be in format -99999999.99' TO WS-MESSAGE
```

## Acceptance Criteria

### Scenario 1: Successful validation with Account ID

```gherkin
GIVEN the user enters Account ID "00000000001"
  AND the Account ID exists in the cross-reference file
  AND all data fields are filled with valid values
  AND the user confirms with 'Y'
WHEN the user presses ENTER
THEN the Card Number is auto-populated from the cross-reference
  AND the transaction passes validation
```

### Scenario 2: Validation failure — missing key fields

```gherkin
GIVEN the user leaves both Account ID and Card Number empty
WHEN the user presses ENTER
THEN the error message "Account or Card Number must be entered..." is displayed
  AND the transaction is not created
```

### Scenario 3: Invalid amount format

```gherkin
GIVEN the user enters amount "12345" (missing sign and decimal)
WHEN the user presses ENTER
THEN the error message "Amount should be in format -99999999.99" is displayed
```

### Scenario 4: Invalid origination date

```gherkin
GIVEN the user enters origination date "2026-02-30"
  AND all other fields are valid
WHEN the user presses ENTER
THEN the error message "Orig Date - Not a valid date..." is displayed
```

### Scenario 5: Card number not in cross-reference

```gherkin
GIVEN the user enters Card Number "1234567890123456"
  AND this card number does not exist in the CCXREF file
WHEN the user presses ENTER
THEN the error message "Card Number NOT found..." is displayed
```

### Scenario 6: Confirmation required

```gherkin
GIVEN all fields are valid
  AND the Confirm field is empty
WHEN the user presses ENTER
THEN the message "Confirm to add this transaction..." is displayed
  AND the transaction is not yet created
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for payment transactions | Input validation is a prerequisite check before transaction creation; card/account verification ensures authorization |
| FFFS 2014:5 | Ch. 4 §3 | Operational risk management — adequate controls | Multi-field validation prevents erroneous or fraudulent transactions |
| AML/KYC | General | Transaction data completeness for monitoring | All 11 mandatory fields ensure sufficient data for AML screening |

## Edge Cases

1. **Amount precision**: The amount field uses PIC S9(9)V99 internally, supporting values from -999,999,999.99 to +999,999,999.99. The input format requires explicit sign character (+/-) as the first character. The NUMVAL-C function is used for conversion (line 383-384).

2. **Date validation utility**: The CSUTLDTC utility is called to validate dates (lines 393-427). Message number '2513' appears to be an acceptable condition (possibly a leap year or end-of-month warning) that is not treated as an error.

3. **Cross-reference bidirectional lookup**: Account ID can look up Card Number (via CXACAIX alternate index file), and Card Number can look up Account ID (via CCXREF primary file). This enables entry from either direction.

4. **Sequential validation exit**: The validation uses PERFORM SEND-TRNADD-SCREEN which executes EXEC CICS RETURN, effectively exiting the validation chain on the first error. Only one error message is displayed at a time.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Is the CSUTLDTC date validation utility reusable, or should standard .NET date parsing replace it?
- Should the amount format be updated from ±99999999.99 to a locale-aware format for SEK?
- Are there additional validation rules not in the COBOL that should be added (e.g., transaction amount limits)?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
