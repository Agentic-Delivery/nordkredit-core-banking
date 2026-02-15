---
id: "uc-trn-03"
title: "UC-TRN-03: Add Transaction"
sidebar_label: "UC-TRN-03: Add Transaction"
domain: "transactions"
cobol_source: "COTRN02C.cbl:106-784"
related_business_rules:
  - "TRN-BR-003"
regulations:
  - "PSD2 Art. 97 — Strong Customer Authentication"
  - "PSD2 Art. 64 — Transaction data integrity"
  - "FSA FFFS 2014:5 Ch. 7 — Financial record integrity"
  - "AML 2017:11 Para. 3 — Transaction monitoring prerequisites"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# UC-TRN-03: Add Transaction

## Overview

| Attribute | Value |
|---|---|
| **Use Case ID** | UC-TRN-03 |
| **Title** | Add Transaction |
| **Primary Actor** | Bank Operator |
| **Source Program** | `COTRN02C.cbl` (lines 106–784) |
| **Business Rule** | [TRN-BR-003](/docs/business-rules/transactions/trn-br-003) |
| **Trigger** | Operator navigates to transaction add screen |
| **Preconditions** | Operator is authenticated; valid account or card exists in the system |
| **Postcondition** | New transaction record is written to TRANSACT file with a unique sequential ID |
| **Priority** | High |

## Summary

The Add Transaction use case is the most validation-intensive online operation in the transaction domain. It allows an authenticated bank operator to create a new transaction record by entering an Account ID or Card Number (with automatic cross-reference lookup) and populating 13 mandatory fields. The system validates all input, requires explicit confirmation, and assigns a sequential transaction ID before writing the record.

This use case is extracted from `COTRN02C.cbl` and formalizes the business rule [TRN-BR-003](/docs/business-rules/transactions/trn-br-003).

## User Stories

### US-TRN-03.1: Create transaction via Account ID

> As a **bank operator**, I want to enter an Account ID to create a transaction, so that the system auto-populates the linked card number from the cross-reference.

**Acceptance criteria:**

```gherkin
Scenario: Successful transaction creation with Account ID
  Given the operator is authenticated and on the transaction add screen
  And a valid Account ID exists in the CXACAIX cross-reference file
  When the operator enters the Account ID and presses Enter
  Then the system looks up the Account ID in the CXACAIX alternate index
  And the linked Card Number is auto-populated on screen
  And the operator can proceed to fill in data fields
```

**Source:** `COTRN02C.cbl:193-209` — Account ID key field validation and CXACAIX lookup.

### US-TRN-03.2: Create transaction via Card Number

> As a **bank operator**, I want to enter a Card Number to create a transaction, so that the system auto-populates the linked account ID from the cross-reference.

**Acceptance criteria:**

```gherkin
Scenario: Successful transaction creation with Card Number
  Given the operator is authenticated and on the transaction add screen
  And a valid Card Number exists in the CCXREF cross-reference file
  When the operator enters the Card Number and presses Enter
  Then the system looks up the Card Number in the CCXREF file
  And the linked Account ID is auto-populated on screen
  And the operator can proceed to fill in data fields
```

**Source:** `COTRN02C.cbl:210-230` — Card Number key field validation and CCXREF lookup.

### US-TRN-03.3: Validate all mandatory fields

> As a **bank operator**, I want the system to validate all 13 mandatory fields before accepting the transaction, so that incomplete or invalid records are never persisted.

**Acceptance criteria:**

```gherkin
Scenario: All mandatory fields validated
  Given the operator has entered a valid Account ID or Card Number
  When the operator presses Enter with one or more empty mandatory fields
  Then the system displays an error message identifying the first empty field
  And the transaction is not saved

Scenario: Numeric fields validated
  Given the operator has entered non-numeric data in a numeric-only field
  When the system validates the input
  Then the error message "<Field> must be Numeric..." is displayed
  And the transaction is not saved
```

**Mandatory fields (13):**

| # | Field | Type | Validation |
|---|---|---|---|
| 1 | Type CD | Numeric | Cannot be empty; must be numeric |
| 2 | Category CD | Numeric | Cannot be empty; must be numeric |
| 3 | Source | Text | Cannot be empty |
| 4 | Description | Text | Cannot be empty |
| 5 | Amount | Financial | Cannot be empty; format ±99999999.99 |
| 6 | Origination Date | Date | Cannot be empty; YYYY-MM-DD; calendar valid |
| 7 | Processing Date | Date | Cannot be empty; YYYY-MM-DD; calendar valid |
| 8 | Merchant ID | Numeric | Cannot be empty; must be numeric |
| 9 | Merchant Name | Text | Cannot be empty |
| 10 | Merchant City | Text | Cannot be empty |
| 11 | Merchant Zip | Text | Cannot be empty |
| 12 | Account ID | Key | Must be numeric; must exist in CXACAIX |
| 13 | Card Number | Key | Must be numeric; must exist in CCXREF |

**Note:** Either Account ID or Card Number must be provided (the other is auto-populated via cross-reference). Both are considered mandatory through this lookup mechanism.

**Source:** `COTRN02C.cbl:235-437` — Data field validation block.

### US-TRN-03.4: Validate amount format

> As a **bank operator**, I want the amount validated as ±99999999.99 format (sign + 8 digits + decimal + 2 digits), so that financial precision is maintained.

**Acceptance criteria:**

```gherkin
Scenario: Valid amount accepted
  Given the operator enters an amount in the format "+12345678.90"
  When the system validates the amount field
  Then the amount is accepted
  And converted using NUMVAL-C equivalent to decimal(11,2)

Scenario: Invalid amount format rejected
  Given the operator enters an amount not matching ±99999999.99
  When the system validates the amount field
  Then the message "Amount should be in format -99999999.99" is displayed
  And the transaction is not saved

Scenario: Amount format position rules
  Given the amount field is 12 characters
  When the system validates character positions
  Then position 1 must be '+' or '-'
  And positions 2-9 must be numeric (8 digits)
  And position 10 must be '.'
  And positions 11-12 must be numeric (2 decimal places)
```

**Financial precision mapping:**

| COBOL | .NET Equivalent | Notes |
|---|---|---|
| `S9(09)V99` (record) | `decimal(11,2)` | Implied decimal point in COBOL |
| `+99999999.99` (display) | `string` → `decimal` | Parsed via NUMVAL-C equivalent |

**Source:** `COTRN02C.cbl:340-380` — Amount format validation. Lines 383–386 — NUMVAL-C conversion.

### US-TRN-03.5: Validate dates

> As a **bank operator**, I want origination and processing dates validated as YYYY-MM-DD with calendar validity, so that invalid dates are rejected.

**Acceptance criteria:**

```gherkin
Scenario: Valid date accepted
  Given the operator enters a date in YYYY-MM-DD format
  And the date is a valid calendar date
  When the system validates via CSUTLDTC equivalent
  Then the date is accepted

Scenario: Invalid date format rejected
  Given the operator enters a date not in YYYY-MM-DD format
  When the system validates the date field
  Then the message "<Field> should be in format YYYY-MM-DD" is displayed

Scenario: Invalid calendar date rejected
  Given the operator enters "2025-02-30" (not a real calendar date)
  When the system validates via CSUTLDTC equivalent
  Then the message "<Field> - Not a valid date..." is displayed
```

**Date format position rules:**

| Position | Content |
|---|---|
| 1–4 | Numeric (YYYY) |
| 5 | Hyphen '-' |
| 6–7 | Numeric (MM) |
| 8 | Hyphen '-' |
| 9–10 | Numeric (DD) |

**Migration note:** The COBOL program calls `CSUTLDTC` for calendar validation. Severity code must be `'0000'` or message number `'2513'` (a tolerated warning). The migrated system must replicate this exact validation logic.

**Source:** `COTRN02C.cbl:385-437` — Date validation block.

### US-TRN-03.6: Confirm before saving

> As a **bank operator**, I want to confirm (Y/N) before the transaction is saved, so that accidental submissions are prevented.

**Acceptance criteria:**

```gherkin
Scenario: Transaction saved after confirmation
  Given all fields are valid
  And the operator enters 'Y' (or 'y') in the confirmation field
  When the system processes the confirmation
  Then the transaction is written to the TRANSACT file
  And a success message is displayed

Scenario: Transaction not saved without confirmation
  Given all fields are valid
  And the confirmation field is empty, 'N', or 'n'
  When the system processes the confirmation
  Then the message "Confirm to add this transaction..." is displayed
  And the transaction is not saved

Scenario: Invalid confirmation value
  Given the confirmation field contains a value other than Y/y/N/n/space
  When the system processes the confirmation
  Then the message "Invalid value. Valid values are (Y/N)..." is displayed
```

**Source:** `COTRN02C.cbl:164-188` — Confirmation evaluation in PROCESS-ENTER-KEY.

### US-TRN-03.7: Sequential ID assignment

> As a **bank operator**, I want the system to assign a sequential transaction ID (last ID + 1), so that every transaction has a unique identifier.

**Acceptance criteria:**

```gherkin
Scenario: Sequential ID assigned
  Given transactions exist in the TRANSACT file
  When a new transaction is successfully added
  Then the new transaction ID equals the highest existing ID + 1
  And the message "Transaction added successfully. Your Tran ID is NNNN." is displayed

Scenario: Duplicate ID detected
  Given a concurrent write creates a duplicate transaction ID
  When the WRITE encounters DUPKEY or DUPREC
  Then the message "Tran ID already exist..." is displayed
  And no record is written
```

**Migration note:** The current COBOL approach (STARTBR with HIGH-VALUES → READPREV → increment) is not concurrency-safe. The migrated system should use a database sequence or equivalent atomic ID generation to eliminate the race condition.

**Source:** `COTRN02C.cbl:442-466` — Sequential ID generation. Lines 711–749 — TRANSACT file write with DUPREC handling.

### US-TRN-03.8: Copy last transaction template (PF5)

> As a **bank operator**, I want to copy data from the last transaction as a template, so that I can speed up data entry for similar transactions.

**Acceptance criteria:**

```gherkin
Scenario: Copy last transaction data
  Given the operator has entered a valid Account ID or Card Number
  When the operator presses PF5
  Then the last transaction's data fields are copied to the input form
  And the key fields (Account ID, Card Number) remain as entered
  And the confirmation field is cleared
  And the operator can modify any field before submitting

Scenario: No previous transaction exists
  Given the operator presses PF5
  And no transactions exist for the given account/card
  When the system attempts to copy the last transaction
  Then appropriate feedback is provided
```

**Source:** `COTRN02C.cbl:471-495` — Copy last transaction (PF5 handler).

## Main Flow

```
1. Operator navigates to the Transaction Add screen
2. System checks authentication (COMMAREA/EIBCALEN)
   - If not authenticated → redirect to sign-on screen
3. Operator enters Account ID OR Card Number
4. System validates the key field:
   a. If Account ID → look up in CXACAIX → populate Card Number
   b. If Card Number → look up in CCXREF → populate Account ID
   c. If neither → error: "Account or Card Number must be entered..."
5. Operator populates all 13 mandatory data fields
6. System validates each field (see US-TRN-03.3 through US-TRN-03.5)
   - On validation failure → display error, return to form
7. System prompts for confirmation
8. Operator enters 'Y' to confirm
9. System generates sequential transaction ID (last ID + 1)
10. System writes transaction record to TRANSACT file
11. System displays: "Transaction added successfully. Your Tran ID is NNNN."
```

## Alternative Flows

| ID | Trigger | Flow |
|---|---|---|
| **AF-1** | PF3 pressed | Return to calling program (transaction list) |
| **AF-2** | PF4 pressed | Clear all input fields |
| **AF-3** | PF5 pressed | Copy last transaction data as template (see US-TRN-03.8) |
| **AF-4** | Invalid key pressed | Display "Invalid key pressed" message |
| **AF-5** | Account ID not found | Display "Account ID NOT found..." after CXACAIX lookup fails |
| **AF-6** | Card Number not found | Display "Card Number NOT found..." after CCXREF lookup fails |
| **AF-7** | Confirmation = 'N' | Display "Confirm to add this transaction..." — no write |
| **AF-8** | DUPKEY/DUPREC on write | Display "Tran ID already exist..." — no record written |

## Validation Rules

| Field | Validation | Error Message |
|---|---|---|
| Account ID | Must be numeric; must exist in CXACAIX | "Account ID must be Numeric..." / "Account ID NOT found..." |
| Card Number | Must be numeric; must exist in CCXREF | "Card Number must be Numeric..." / "Card Number NOT found..." |
| Type CD | Cannot be empty; must be numeric | "Type CD can NOT be empty..." / "Type CD must be Numeric..." |
| Category CD | Cannot be empty; must be numeric | "Category CD can NOT be empty..." / "Category CD must be Numeric..." |
| Source | Cannot be empty | "Source can NOT be empty..." |
| Description | Cannot be empty | "Description can NOT be empty..." |
| Amount | Cannot be empty; format ±99999999.99 | "Amount can NOT be empty..." / "Amount should be in format -99999999.99" |
| Orig Date | Cannot be empty; YYYY-MM-DD; calendar valid | "Orig Date can NOT be empty..." / "Orig Date should be in format YYYY-MM-DD" / "Orig Date - Not a valid date..." |
| Proc Date | Cannot be empty; YYYY-MM-DD; calendar valid | "Proc Date can NOT be empty..." / "Proc Date should be in format YYYY-MM-DD" / "Proc Date - Not a valid date..." |
| Merchant ID | Cannot be empty; must be numeric | "Merchant ID can NOT be empty..." / "Merchant ID must be Numeric..." |
| Merchant Name | Cannot be empty | "Merchant Name can NOT be empty..." |
| Merchant City | Cannot be empty | "Merchant City can NOT be empty..." |
| Merchant Zip | Cannot be empty | "Merchant Zip can NOT be empty..." |
| Confirm | Must be 'Y'/'y' to save | "Confirm to add this transaction..." / "Invalid value. Valid values are (Y/N)..." |

## Cross-Reference Files

| File | Purpose | Lookup Direction |
|---|---|---|
| **CXACAIX** | Account alternate index | Account ID → Card Number |
| **CCXREF** | Card cross-reference | Card Number → Account ID |

## Regulatory Traceability

| Regulation | Article/Section | Requirement | How This Use Case Satisfies It |
|---|---|---|---|
| PSD2 | Art. 97 | Strong customer authentication | Transaction creation requires authenticated session (CICS/COMMAREA check); migrated system must enforce SCA via Azure AD |
| PSD2 | Art. 64 | Transaction data integrity | All 13 mandatory fields validated before write; amount precision enforced via NUMVAL-C equivalent |
| FSA FFFS 2014:5 | Ch. 7 | Financial record integrity | Sequential ID assignment ensures traceability; cross-reference validation ensures account-card linkage |
| AML 2017:11 | Para. 3 | Transaction monitoring prerequisites | Merchant information (ID, name, city, zip) captured for each transaction, enabling downstream AML screening |

## Migration Notes

1. **Financial precision**: The COBOL `NUMVAL-C` intrinsic function converts edited display format (`±99999999.99`) to numeric. The .NET equivalent must use `decimal.Parse` with identical format handling to prevent precision loss. Target type: `decimal(11,2)`.

2. **Sequential ID generation**: Replace COBOL's read-last-and-increment approach with a database sequence (`IDENTITY` column or `SEQUENCE` object in Azure SQL) to eliminate the race condition inherent in the current design.

3. **Date validation**: Replace `CSUTLDTC` utility call with .NET `DateTime.TryParseExact` using `"yyyy-MM-dd"` format. Investigate whether CSUTLDTC message `'2513'` (tolerated warning) has a .NET equivalent that should be preserved.

4. **Cross-reference lookup**: Replace VSAM file lookups (CXACAIX, CCXREF) with Azure SQL queries joining the Account and Card tables.

5. **Copy-last-transaction**: Implement as an API endpoint that retrieves the most recent transaction for the given account/card and returns it as a template for pre-populating the form.

6. **PF-key mapping**: Map CICS PF-key actions to REST API endpoints or UI button actions:
   - ENTER → Submit/Validate (POST)
   - PF3 → Navigate back (UI routing)
   - PF4 → Clear form (client-side)
   - PF5 → Copy last transaction (GET)

## Domain Expert Questions

The following questions require domain expert review before implementation:

1. Is the copy-last-transaction feature (PF5) still needed in the migrated system, or can it be replaced with a different UX pattern?
2. Should sequential ID generation be replaced with a database sequence (recommended)?
3. Are there business rules constraining which transaction type/category code combinations are valid?
4. The CSUTLDTC date validation tolerates message `'2513'` — what does this warning represent, and should it be preserved in the migrated system?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
