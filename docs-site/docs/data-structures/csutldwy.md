---
id: DS-UTLW-001
title: "Date Validation Working Storage"
copybook_name: "CSUTLDWY.cpy"
domain: "system"
used_by_programs: [COACTUPC, COCRDUPC, COTRN02C]
record_length: 0
status: "extracted"
target_schema: "N/A (utility)"
sidebar_position: 33
---

# DS-UTLW-001: Date Validation Working Storage (CSUTLDWY)

## Overview

The `CSUTLDWY.cpy` copybook defines the **Date Validation Working Storage**, the data area used by the date validation procedures in `CSUTLDPY.cpy`. This copybook declares all the working storage variables needed to validate dates in `CCYYMMDD` format, including individual date components (century, year, month, day), validation flags, and result fields.

This is a working storage companion copybook -- it declares variables that the procedure division copybook `CSUTLDPY` operates on. Together, they provide reusable date validation logic used across multiple programs in the system.

**Source file:** `CSUTLDWY.cpy`
**Record length:** N/A (working storage variables, not a stored record)
**Used by:** All programs using date validation (`COACTUPC`, `COCRDUPC`, `COTRN02C`)
**Companion:** `CSUTLDPY.cpy` (procedure division copybook)

## Source COBOL

```cobol
*--- Date to be validated ---
01  WS-EDIT-DATE-CCYYMMDD.
    05  WS-EDIT-DATE-CC           PIC 99.
        88  VALID-CENTURY          VALUE 19, 20.
    05  WS-EDIT-DATE-YY           PIC 99.
    05  WS-EDIT-DATE-MM           PIC 99.
        88  VALID-MONTH            VALUE 1 THRU 12.
    05  WS-EDIT-DATE-DD           PIC 99.
        88  VALID-DAY              VALUE 1 THRU 31.

01  WS-EDIT-DATE-BINARY           PIC S9(9) BINARY.

*--- Current date for DOB validation ---
01  WS-CURRENT-DATE               PIC X(8).
01  WS-CURRENT-DATE-NUM REDEFINES WS-CURRENT-DATE
                                  PIC 9(8).

*--- Validation flags ---
01  WS-EDIT-DATE-FLGS.
    05  WS-EDIT-YEAR-VALID        PIC X VALUE 'N'.
        88  YEAR-IS-VALID          VALUE 'Y'.
        88  YEAR-IS-INVALID        VALUE 'N'.
    05  WS-EDIT-MONTH-VALID       PIC X VALUE 'N'.
        88  MONTH-IS-VALID         VALUE 'Y'.
        88  MONTH-IS-INVALID       VALUE 'N'.
    05  WS-EDIT-DAY-VALID         PIC X VALUE 'N'.
        88  DAY-IS-VALID           VALUE 'Y'.
        88  DAY-IS-INVALID         VALUE 'N'.

*--- Date format and validation result ---
01  WS-DATE-FORMAT                PIC X(10).

01  WS-DATE-VALIDATION-RESULT.
    05  WS-DATE-VALID-SEVERITY    PIC X.
    05  WS-DATE-VALID-MESSAGE     PIC X(80).
    05  WS-DATE-VALID-RESULT      PIC X.
        88  DATE-IS-VALID          VALUE 'Y'.
        88  DATE-IS-INVALID        VALUE 'N'.
```

## Field Definitions

### Date Components (WS-EDIT-DATE-CCYYMMDD)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `WS-EDIT-DATE-CC` | `99` | 2 | Numeric | Century (19 or 20 only, per 88-level condition) |
| 2 | `WS-EDIT-DATE-YY` | `99` | 2 | Numeric | Year within century (00-99) |
| 3 | `WS-EDIT-DATE-MM` | `99` | 2 | Numeric | Month (1-12, per 88-level condition) |
| 4 | `WS-EDIT-DATE-DD` | `99` | 2 | Numeric | Day (1-31, per 88-level condition) |

### Binary Date

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 5 | `WS-EDIT-DATE-BINARY` | `S9(9) BINARY` | 4 | Binary integer | Date as integer for Language Environment validation |

### Current Date (for DOB validation)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 6 | `WS-CURRENT-DATE` | `X(8)` | 8 | Alphanumeric | Current date as string (CCYYMMDD) |
| 7 | `WS-CURRENT-DATE-NUM` | `9(8)` (REDEFINES) | 8 | Numeric | Numeric redefine for date comparison |

### Validation Flags (WS-EDIT-DATE-FLGS)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 8 | `WS-EDIT-YEAR-VALID` | `X` | 1 | Flag | Year validation result (`Y`/`N`) |
| 9 | `WS-EDIT-MONTH-VALID` | `X` | 1 | Flag | Month validation result (`Y`/`N`) |
| 10 | `WS-EDIT-DAY-VALID` | `X` | 1 | Flag | Day validation result (`Y`/`N`) |

### Validation Result (WS-DATE-VALIDATION-RESULT)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 11 | `WS-DATE-VALID-SEVERITY` | `X` | 1 | Alphanumeric | Severity level of validation failure |
| 12 | `WS-DATE-VALID-MESSAGE` | `X(80)` | 80 | Alphanumeric | Descriptive validation error message |
| 13 | `WS-DATE-VALID-RESULT` | `X` | 1 | Flag | Overall validation result (`Y` = valid, `N` = invalid) |

## Field Notes

1. **WS-EDIT-DATE-CCYYMMDD** -- The date to be validated is moved into this group item before calling the validation procedures in `CSUTLDPY`. The group is subdivided into century, year, month, and day components, each with 88-level conditions that define valid ranges. The century check is restrictive: only `19` and `20` are valid, meaning dates before 1900 or after 2099 are rejected.

2. **WS-EDIT-DATE-BINARY** -- A binary representation of the date used for Language Environment (LE) date validation via the `CSUTLDTC` service routine. The LE validation provides an additional cross-check beyond the component-level validation.

3. **WS-CURRENT-DATE / WS-CURRENT-DATE-NUM** -- The current system date is stored here for date-of-birth validation. The numeric REDEFINES allows direct numeric comparison (e.g., `IF WS-EDIT-DATE-CCYYMMDD > WS-CURRENT-DATE-NUM` to detect future dates). This is populated by the calling program using `FUNCTION CURRENT-DATE` or equivalent.

4. **WS-EDIT-DATE-FLGS** -- Individual validation flags that are set independently by each validation paragraph in `CSUTLDPY`. This allows the caller to determine exactly which component failed validation (year, month, or day) rather than just getting a pass/fail result.

5. **WS-DATE-VALIDATION-RESULT** -- The aggregate validation result. `WS-DATE-VALID-SEVERITY` indicates the severity (e.g., error vs. warning). `WS-DATE-VALID-MESSAGE` contains a human-readable error message (up to 80 characters). `WS-DATE-VALID-RESULT` is the final pass/fail flag.

## Business Rules Encoded in 88-Level Conditions

| Condition | Value(s) | Business Rule |
|-----------|----------|---------------|
| `VALID-CENTURY` | `19`, `20` | Only 19th and 20th century dates are valid (1900-2099) |
| `VALID-MONTH` | `1 THRU 12` | Month must be January through December |
| `VALID-DAY` | `1 THRU 31` | Day must be 1 through 31 (further refined by month in procedures) |
| `YEAR-IS-VALID` / `YEAR-IS-INVALID` | `Y` / `N` | Year validation result |
| `MONTH-IS-VALID` / `MONTH-IS-INVALID` | `Y` / `N` | Month validation result |
| `DAY-IS-VALID` / `DAY-IS-INVALID` | `Y` / `N` | Day validation result |
| `DATE-IS-VALID` / `DATE-IS-INVALID` | `Y` / `N` | Overall date validation result |

## Target Architecture Mapping

| Aspect | COBOL (Current) | .NET (Target) |
|--------|----------------|---------------|
| **Date validation** | Manual component checks (CC, YY, MM, DD) | `DateOnly.TryParseExact` or FluentValidation rules |
| **Century check** | 88-level `VALID-CENTURY VALUE 19, 20` | Range check: `year >= 1900 && year <= 2099` |
| **Month/day check** | 88-level conditions + paragraph logic | Built into `DateOnly` parsing (rejects invalid dates) |
| **Validation flags** | Individual PIC X flags per component | `ValidationResult` with per-field error messages |
| **Error messages** | `WS-DATE-VALID-MESSAGE PIC X(80)` | FluentValidation `WithMessage()` |
| **Current date** | `WS-CURRENT-DATE` populated manually | `DateOnly.FromDateTime(DateTime.UtcNow)` |
| **Binary date** | `S9(9) BINARY` for LE validation | Not needed (native date types) |

### .NET Equivalent

```csharp
public class DateValidator : AbstractValidator<DateOnly>
{
    public DateValidator()
    {
        RuleFor(d => d.Year)
            .InclusiveBetween(1900, 2099)
            .WithMessage("Century must be 19 or 20 (year 1900-2099)");

        // Month and day validation is inherent in DateOnly type
    }

    public static bool TryValidateDate(string ccyymmdd, out DateOnly result, out string errorMessage)
    {
        errorMessage = string.Empty;
        if (DateOnly.TryParseExact(ccyymmdd, "yyyyMMdd",
            CultureInfo.InvariantCulture, DateTimeStyles.None, out result))
        {
            if (result.Year < 1900 || result.Year > 2099)
            {
                errorMessage = "Century must be 19 or 20";
                return false;
            }
            return true;
        }
        errorMessage = "Invalid date format";
        return false;
    }
}
```

## Migration Notes

### Relationship to CSUTLDPY

This working storage copybook is always used together with `CSUTLDPY.cpy` (the procedure division copybook). The separation follows the COBOL convention of declaring data in the DATA DIVISION and logic in the PROCEDURE DIVISION. In .NET, these are unified into a single validation class.

| COBOL Component | Purpose | .NET Equivalent |
|----------------|---------|----------------|
| `CSUTLDWY.cpy` | Data declarations (this file) | Properties / local variables in validator class |
| `CSUTLDPY.cpy` | Validation logic (procedures) | Methods in validator class |

### Century Validation Consideration

The `VALID-CENTURY VALUE 19, 20` constraint limits dates to the range 1900-2099. This is adequate for the current banking system (customer dates of birth, account open dates, etc.) but should be reviewed if the system needs to handle historical dates before 1900 or dates beyond 2099.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** | Accurate date handling for financial transactions and regulatory reporting. | Date validation must be consistent across the system. Standardize on the `DateOnly` type and FluentValidation for all date inputs. |
| **GDPR** Art. 5(1)(d) | Accuracy principle -- personal data must be accurate. | Date of birth validation (uses `WS-CURRENT-DATE` to reject future dates) ensures accurate customer records. |
| **DORA** Art. 11 | ICT data integrity. | Parallel-run testing must verify that the COBOL date validation and .NET date validation accept and reject the same set of dates, including edge cases (leap years, century boundaries). |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
