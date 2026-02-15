---
id: DS-UTLP-001
title: "Date Validation Procedures"
copybook_name: "CSUTLDPY.cpy"
domain: "system"
used_by_programs: [COACTUPC, COCRDUPC, COTRN02C]
record_length: 0
status: "extracted"
target_schema: "N/A (business logic)"
sidebar_position: 34
---

# DS-UTLP-001: Date Validation Procedures (CSUTLDPY)

## Overview

The `CSUTLDPY.cpy` copybook is a **procedure division copybook** containing reusable date validation business rules. Unlike most copybooks that define data structures, this one defines executable paragraphs (subroutines) that validate dates in `CCYYMMDD` format. It works in conjunction with `CSUTLDWY.cpy`, which provides the working storage variables.

The validation logic includes century checks, month range checks, day range checks with month-specific constraints, leap year calculation, and date-of-birth future date rejection. These rules are included (via `COPY`) into multiple programs to ensure consistent date validation across the system.

**Source file:** `CSUTLDPY.cpy`
**Record length:** N/A (procedure division code, not a data structure)
**Used by:** All programs using date validation (`COACTUPC`, `COCRDUPC`, `COTRN02C`)
**Working storage companion:** `CSUTLDWY.cpy`

## Validation Paragraphs

### Paragraph Summary

| Paragraph Name | Purpose | Calls |
|---------------|---------|-------|
| `EDIT-DATE-CCYYMMDD` | Main entry point; orchestrates all validations | `EDIT-YEAR-CCYY`, `EDIT-MONTH`, `EDIT-DAY`, `EDIT-DAY-MONTH-YEAR` |
| `EDIT-YEAR-CCYY` | Validates century (19 or 20) and year | -- |
| `EDIT-MONTH` | Validates month (1-12) | -- |
| `EDIT-DAY` | Validates day (1-31 basic range) | -- |
| `EDIT-DAY-MONTH-YEAR` | Combined validation: month-specific day limits, February, leap year | -- |
| `EDIT-DATE-LE` | Language Environment date validation via `CSUTLDTC` | `CSUTLDTC` (external) |
| `EDIT-DATE-OF-BIRTH` | Validates that date is not in the future | -- |

### Validation Flow

```
EDIT-DATE-CCYYMMDD (main entry)
    |
    +-- EDIT-YEAR-CCYY
    |       Is century 19 or 20? Is year 00-99?
    |
    +-- EDIT-MONTH
    |       Is month 1-12?
    |
    +-- EDIT-DAY
    |       Is day 1-31?
    |
    +-- EDIT-DAY-MONTH-YEAR
    |       Month-specific day limits:
    |       - Jan/Mar/May/Jul/Aug/Oct/Dec: max 31
    |       - Apr/Jun/Sep/Nov: max 30
    |       - Feb: max 28 (or 29 if leap year)
    |           Leap year: divisible by 4
    |                      OR divisible by 400
    |
    +-- EDIT-DATE-LE (optional)
            Language Environment cross-check via CSUTLDTC
```

## Business Rules

### BR-DATE-001: Century Validation

**Paragraph:** `EDIT-YEAR-CCYY`
**Rule:** The century component (`CC` in `CCYYMMDD`) must be either `19` or `20`.
**Effect:** Dates before 1900-01-01 or after 2099-12-31 are rejected.

```
IF NOT VALID-CENTURY
    SET YEAR-IS-INVALID TO TRUE
    MOVE 'Invalid century - must be 19 or 20' TO WS-DATE-VALID-MESSAGE
    SET DATE-IS-INVALID TO TRUE
END-IF
```

### BR-DATE-002: Month Validation

**Paragraph:** `EDIT-MONTH`
**Rule:** The month component (`MM`) must be between 1 and 12 inclusive.

```
IF NOT VALID-MONTH
    SET MONTH-IS-INVALID TO TRUE
    MOVE 'Invalid month - must be 01-12' TO WS-DATE-VALID-MESSAGE
    SET DATE-IS-INVALID TO TRUE
END-IF
```

### BR-DATE-003: Day Validation (Basic Range)

**Paragraph:** `EDIT-DAY`
**Rule:** The day component (`DD`) must be between 1 and 31 inclusive.

```
IF NOT VALID-DAY
    SET DAY-IS-INVALID TO TRUE
    MOVE 'Invalid day - must be 01-31' TO WS-DATE-VALID-MESSAGE
    SET DATE-IS-INVALID TO TRUE
END-IF
```

### BR-DATE-004: Month-Specific Day Constraints

**Paragraph:** `EDIT-DAY-MONTH-YEAR`
**Rule:** The day must be valid for the specific month:

| Month(s) | Maximum Day | Notes |
|----------|------------|-------|
| January (01), March (03), May (05), July (07), August (08), October (10), December (12) | 31 | 31-day months |
| April (04), June (06), September (09), November (11) | 30 | 30-day months |
| February (02) | 28 or 29 | 28 normally, 29 in leap year |

### BR-DATE-005: Leap Year Calculation

**Paragraph:** `EDIT-DAY-MONTH-YEAR` (February branch)
**Rule:** A year is a leap year if:
- The year is divisible by 4, **OR**
- The year is divisible by 400

**Note:** The standard Gregorian leap year rule also excludes years divisible by 100 (unless also divisible by 400). The COBOL implementation should be verified for this edge case. For the years in the valid range (1900-2099), the only century year is 2000 (which is divisible by 400 and therefore a leap year) and 1900 (which is divisible by 100 but not 400 and therefore NOT a leap year). This distinction matters for dates like `1900-02-29`.

```
IF WS-EDIT-DATE-MM = 02
    IF FUNCTION MOD(WS-EDIT-DATE-CCYYMMDD-NUM, 4) = 0
       OR FUNCTION MOD(WS-EDIT-DATE-CCYYMMDD-NUM, 400) = 0
        IF WS-EDIT-DATE-DD > 29
            SET DAY-IS-INVALID TO TRUE
        END-IF
    ELSE
        IF WS-EDIT-DATE-DD > 28
            SET DAY-IS-INVALID TO TRUE
        END-IF
    END-IF
END-IF
```

### BR-DATE-006: Date of Birth Future Date Rejection

**Paragraph:** `EDIT-DATE-OF-BIRTH`
**Rule:** A date of birth cannot be in the future. The date being validated is compared against the current system date (`WS-CURRENT-DATE-NUM`).

```
IF WS-EDIT-DATE-CCYYMMDD > WS-CURRENT-DATE-NUM
    MOVE 'Date of birth cannot be in the future' TO WS-DATE-VALID-MESSAGE
    SET DATE-IS-INVALID TO TRUE
END-IF
```

### BR-DATE-007: Language Environment Cross-Check

**Paragraph:** `EDIT-DATE-LE`
**Rule:** As an additional validation step, the date is converted to a binary integer and passed to the IBM Language Environment date validation routine (`CSUTLDTC`). This provides a secondary validation that catches edge cases the manual checks might miss.

## Target Architecture Mapping

| Aspect | COBOL (Current) | .NET (Target) |
|--------|----------------|---------------|
| **Validation approach** | Manual paragraph-based checks | `DateOnly.TryParseExact` + FluentValidation |
| **Century check** | `IF NOT VALID-CENTURY` | `RuleFor(x => x.Year).InclusiveBetween(1900, 2099)` |
| **Month/day check** | Multiple paragraphs | Built into `DateOnly` type (rejects invalid dates) |
| **Leap year** | Manual modular arithmetic | Built into `DateOnly` / `DateTime.IsLeapYear(year)` |
| **DOB future check** | Numeric comparison with current date | `RuleFor(x => x.DateOfBirth).LessThanOrEqualTo(DateOnly.FromDateTime(DateTime.UtcNow))` |
| **LE cross-check** | `CSUTLDTC` call | Not needed (native date types handle this) |
| **Error messages** | `WS-DATE-VALID-MESSAGE PIC X(80)` | FluentValidation `.WithMessage()` |

### .NET Equivalent

```csharp
public class DateOfBirthValidator : AbstractValidator<string>
{
    public DateOfBirthValidator()
    {
        // BR-DATE-001 through BR-DATE-005: Date format and range validation
        RuleFor(dateString => dateString)
            .Must(BeAValidDate)
            .WithMessage("Invalid date format. Expected CCYYMMDD.")
            .DependentRules(() =>
            {
                // BR-DATE-001: Century check
                RuleFor(dateString => dateString)
                    .Must(HaveValidCentury)
                    .WithMessage("Century must be 19 or 20 (year 1900-2099)");

                // BR-DATE-006: Future date rejection
                RuleFor(dateString => dateString)
                    .Must(NotBeInTheFuture)
                    .WithMessage("Date of birth cannot be in the future");
            });
    }

    private static bool BeAValidDate(string dateString)
        => DateOnly.TryParseExact(dateString, "yyyyMMdd",
           CultureInfo.InvariantCulture, DateTimeStyles.None, out _);

    private static bool HaveValidCentury(string dateString)
    {
        if (!DateOnly.TryParseExact(dateString, "yyyyMMdd",
            CultureInfo.InvariantCulture, DateTimeStyles.None, out var date))
            return false;
        return date.Year >= 1900 && date.Year <= 2099;
    }

    private static bool NotBeInTheFuture(string dateString)
    {
        if (!DateOnly.TryParseExact(dateString, "yyyyMMdd",
            CultureInfo.InvariantCulture, DateTimeStyles.None, out var date))
            return false;
        return date <= DateOnly.FromDateTime(DateTime.UtcNow);
    }
}
```

## Migration Notes

### Procedure Division Copybook Pattern

In COBOL, a "procedure division copybook" is an unusual but valid pattern. The `COPY CSUTLDPY` statement in the PROCEDURE DIVISION of a program inserts the paragraphs directly into that program's code. This is COBOL's mechanism for code reuse -- essentially a textual include of shared subroutines.

In .NET, this pattern maps to a shared validation class or library that is referenced by multiple services. The validation logic is not duplicated; it lives in a single class.

### Leap Year Edge Case

The COBOL leap year logic (`divisible by 4 OR divisible by 400`) may not correctly handle years divisible by 100 but not by 400 (e.g., 1900, 2100). Within the valid century range (1900-2099), the only affected year is 1900 -- which is NOT a leap year. The .NET `DateTime.IsLeapYear(1900)` correctly returns `false`. This must be verified during parallel-run testing to ensure the COBOL and .NET implementations agree on edge cases.

### Validation Consolidation

| COBOL Copybook Pair | .NET Replacement |
|--------------------|-----------------|
| `CSUTLDWY.cpy` (data) + `CSUTLDPY.cpy` (logic) | Single `DateValidator` class with FluentValidation |
| `CODATECN.cpy` (conversion) | `DateOnly.ParseExact` / `ToString` |

All three date-related copybooks are consolidated into .NET's native date handling with a thin validation layer using FluentValidation.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** | Accurate date handling for financial records and regulatory filings. Date errors can cause incorrect interest calculations, statement dates, and regulatory report timing. | Standardize on `DateOnly` type. Apply FluentValidation consistently. Test edge cases (leap year, month boundaries, century boundaries). |
| **GDPR** Art. 5(1)(d) | Accuracy -- personal data (including date of birth) must be accurate and kept up to date. | The DOB future date rejection rule (BR-DATE-006) prevents obviously invalid birth dates. Additional reasonableness checks (e.g., DOB not before 1900) are enforced by the century validation. |
| **PSD2** Art. 97 | Strong Customer Authentication may involve date-based verification (card expiry). | Card expiration date validation should reuse the same date validation logic to ensure consistency. |
| **DORA** Art. 11 | ICT data integrity -- date validation is a critical control. | Parallel-run comparison must verify that COBOL and .NET date validation produce identical results for a comprehensive test set including: valid dates, invalid months/days, leap year boundaries (Feb 28/29), century boundaries (1899, 1900, 2099, 2100), and future DOB attempts. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
