---
id: DS-BMS-ACTVW
title: "Account View Screen"
copybook_name: "COACTVW.CPY"
domain: "ui-screens"
used_by_programs: [COACTVWC]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 11
---

# DS-BMS-ACTVW: Account View Screen (COACTVW)

## Overview

The `COACTVW.CPY` copybook defines the **Account View Screen**, a read-only CICS BMS screen for displaying account and customer information. This screen mirrors the field layout of the Account Update Screen (COACTUP) but all data fields are output-only -- the user cannot modify any values.

The copybook defines two symbolic map records:
- **CACTVWAI** -- Input record (receives only navigation keys and the account ID for lookup)
- **CACTVWAO** -- Output record (sends all account and customer fields for display)

Monetary amounts use edited PIC formats (`+ZZZ,ZZZ,ZZZ.99`) providing formatted display with sign indicators, thousands separators, and decimal points.

**Source file:** `COACTVW.CPY`
**BMS map name:** COACTVW
**Used by:** `COACTVWC` (Account View CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `ACCTSID` | Display | 11 | Account ID |
| `ACSTAT` | Display | 1 | Account active status (`Y`/`N`) |
| `OPTEFN` | Display | 10 | Account open date |
| `EXPTEFN` | Display | 10 | Account expiration date |
| `RETTEFN` | Display | 10 | Account reissue date |
| `CRLIM` | Display | 15 | Credit limit (formatted: `+ZZZ,ZZZ,ZZZ.99`) |
| `CASHLT` | Display | 15 | Cash credit limit (formatted) |
| `CURBAL` | Display | 15 | Current balance (formatted) |
| `GRPID` | Display | 10 | Discount group ID |
| `FNAME` | Display | 25 | Customer first name |
| `MNAME` | Display | 25 | Customer middle name |
| `LNAME` | Display | 25 | Customer last name |
| `ADDR1` | Display | 50 | Address line 1 |
| `ADDR2` | Display | 50 | Address line 2 |
| `ADDR3` | Display | 50 | Address line 3 |
| `STTEFN` | Display | 2 | State/province code |
| `CTTEFN` | Display | 3 | Country code |
| `ZIPCD` | Display | 10 | ZIP/postal code |
| `PHON1` | Display | 15 | Primary phone number |
| `PHON2` | Display | 15 | Secondary phone number |
| `SSN` | Display | 9 | Social Security Number (sensitive PII) |
| `DOB` | Display | 10 | Date of birth |
| `TEFNS` | Display | 3 | FICO credit score |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Read-only display:** All fields on this screen are output-only. The user views the information but cannot edit it. This is the "inquiry" counterpart to the Account Update Screen (COACTUP).

2. **Formatted monetary values:** Amounts are displayed using COBOL edited PIC clauses (`+ZZZ,ZZZ,ZZZ.99`), which produce human-readable formatted output with sign, thousands separators, and two decimal places. Zero-suppression replaces leading zeros with spaces.

3. **Combined account + customer data:** Like the update screen, this view combines account financial data with customer demographic data on a single 3270 screen. The .NET replacement should present this as a structured detail view with clear sections.

4. **PII visibility:** The screen displays full SSN, DOB, address, and phone numbers. The .NET replacement must apply PII masking rules (e.g., show only last 4 digits of SSN) based on the user's role and GDPR requirements.

5. **Navigation:** Users typically reach this screen from an account list or search. The only interactive elements are function keys for navigation (e.g., PF3 to return, PF12 to exit).

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COACTVW` on 3270 terminal | Blazor Server page or Razor Page: `AccountDetail.razor` / `AccountDetail.cshtml` |
| **Program** | `COACTVWC` (COBOL CICS program) | `AccountController.Detail` action or Blazor read-only component |
| **Data retrieval** | CICS READ from VSAM `ACCTDATA` + `CUSTDATA` | Repository pattern with Entity Framework query |
| **Field formatting** | BMS edited PIC clauses | .NET display format strings, `@Html.DisplayFor()` or Blazor `@bind` with format |
| **Navigation** | PF keys (PF3=Back, PF12=Exit) | Browser navigation, breadcrumbs, back button |
| **Authorization** | CICS transaction security (RACF) | ASP.NET Authorization policies + Azure AD roles |

### Migration Considerations

- **PII masking:** The .NET detail view must implement role-based field visibility. Standard users see masked SSN (***-**-1234); only authorized roles see full values. This is a GDPR requirement not present in the original COBOL implementation.
- **Separate from edit:** In the .NET architecture, the view and edit screens can share a component/partial with a read-only flag, reducing code duplication while maintaining the separation of read and write operations.
- **Responsive layout:** The 3270 fixed 80x24 layout should be replaced with a responsive design that works on various screen sizes, organizing the dense field layout into logical card/panel groupings (Account Info, Financial Summary, Customer Demographics, Contact Info).

---

**Template version:** 1.0
**Last updated:** 2026-02-15
