---
id: DS-BMS-ACTUP
title: "Account Update Screen"
copybook_name: "COACTUP.CPY"
domain: "ui-screens"
used_by_programs: [COACTUPC]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 10
---

# DS-BMS-ACTUP: Account Update Screen (COACTUP)

## Overview

The `COACTUP.CPY` copybook defines the **Account Update Screen**, a CICS BMS (Basic Mapping Support) map used for editing account and associated customer information. This is a full-form data entry screen presented on a 3270 terminal, allowing authorized users to modify account details (status, dates, credit limits, balances) as well as customer demographic data (name, address, phone, SSN, DOB, FICO score).

The copybook defines two symbolic map records:
- **CACTUPAI** -- Input record (data received from the terminal, with attribute bytes for field-level input control)
- **CACTUPAO** -- Output record (data sent to the terminal for display)

Both records share the same field layout but differ in their first byte per field: the input record includes attribute bytes (`DFHBMATTR`) for controlling field editability, color, and highlighting, while the output record uses character (`DFHBMCHAR`) output descriptors.

**Source file:** `COACTUP.CPY`
**BMS map name:** COACTUP
**Used by:** `COACTUPC` (Account Update CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `ACCTSID` | Display | 11 | Account ID (identifies the account being edited) |
| `APTS` | Input | 1 | Account active status (`Y`/`N`) |
| `APTS` | Display | 10 | Account open date (`YYYY-MM-DD`) |
| `ACSTAT` | Input | 1 | Account status flag |
| `OPTEFN` / `OPTELD` | Input | 10 | Account expiration date |
| `RETEFN` / `RETELD` | Input | 10 | Account reissue date |
| `CRLIM` | Input | 15 | Credit limit (edited numeric) |
| `CASHLT` | Input | 15 | Cash credit limit (edited numeric) |
| `CURBAL` | Input | 15 | Current balance (edited numeric) |
| `GRPID` | Input | 10 | Discount group ID |
| `FNAME` | Input | 25 | Customer first name |
| `MNAME` | Input | 25 | Customer middle name |
| `LNAME` | Input | 25 | Customer last name |
| `ADDR1` | Input | 50 | Address line 1 |
| `ADDR2` | Input | 50 | Address line 2 |
| `ADDR3` | Input | 50 | Address line 3 |
| `STTEFN` | Input | 2 | State/province code |
| `CTTEFN` | Input | 3 | Country code |
| `ZIPCD` | Input | 10 | ZIP/postal code |
| `PHON1` | Input | 15 | Primary phone number |
| `PHON2` | Input | 15 | Secondary phone number |
| `SSN` | Input | 9 | Social Security Number (sensitive PII) |
| `DOB` | Input | 10 | Date of birth |
| `TEFNS` | Input | 3 | FICO credit score |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Combined form:** This screen combines account-level data (ID, status, dates, financial fields) with customer-level data (name, address, contact info, SSN, DOB, FICO) into a single edit form. In the mainframe architecture, this reduces screen navigation; in the .NET architecture, these will likely be separate views or tabs.

2. **Field editability:** The BMS attribute bytes control which fields are editable versus display-only. The account ID is typically display-only (set before entering the screen), while status, dates, limits, and customer fields are editable.

3. **Monetary fields:** Credit limit, cash credit limit, and current balance use edited PIC formats (e.g., `+ZZZ,ZZZ,ZZZ.99`) for formatted display with sign, commas, and decimal point.

4. **PII fields:** SSN, DOB, phone numbers, and full name/address are GDPR-sensitive personal data. The .NET replacement must enforce appropriate access controls and audit logging for these fields.

5. **Validation:** Field-level validation is handled by the CICS program `COACTUPC`, not by the BMS map itself. The map only defines layout and basic field attributes (numeric-only, max length). Business rule validation occurs in the program logic.

6. **Navigation:** Users typically arrive at this screen from an account list or search screen. After saving, the program returns a confirmation message in the `INFOMSG` field or an error in `ERRMSG`.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COACTUP` on 3270 terminal | Blazor Server page or Razor Page: `AccountEdit.razor` / `AccountEdit.cshtml` |
| **Program** | `COACTUPC` (COBOL CICS program) | `AccountController.Edit` action or Blazor component with form binding |
| **Input validation** | COACTUPC program logic | Data annotations + FluentValidation on `AccountUpdateRequest` DTO |
| **Field formatting** | BMS edited PIC clauses | .NET format strings (`C2` for currency, `d` for dates) |
| **Navigation** | CICS SEND MAP / RECEIVE MAP | HTTP GET/POST or Blazor navigation |
| **Authorization** | CICS transaction security (RACF) | ASP.NET Authorization policies + Azure AD roles |
| **PII protection** | Terminal-level security | Field-level encryption, audit logging, GDPR consent tracking |

### Migration Considerations

- **Split into multiple views:** The combined account + customer form should be separated into distinct sections or pages in the .NET UI to follow modern UX patterns and support granular RBAC (e.g., only certain roles can edit financial fields vs. demographic fields).
- **SSN handling:** The SSN field must be masked in the UI (show only last 4 digits) and stored encrypted at rest per GDPR requirements. The COBOL screen displays the full SSN, which is not acceptable in the .NET replacement.
- **Audit trail:** Every field change must be captured in an audit log for FSA (FFFS 2014:5) Ch. 8 internal controls compliance. The COBOL program may not have had comprehensive field-level audit; the .NET implementation must add this.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
