---
id: DS-DTCN-001
title: "Date Conversion Structure"
copybook_name: "CODATECN.cpy"
domain: "system"
used_by_programs: [CODATECN]
record_length: 0
status: "extracted"
target_schema: "N/A (utility)"
sidebar_position: 32
---

# DS-DTCN-001: Date Conversion Structure (CODATECN)

## Overview

The `CODATECN.cpy` copybook defines the **Date Conversion Structure**, a parameter block used to convert dates between compact (`YYYYMMDD`) and ISO (`YYYY-MM-DD`) formats. This copybook is the interface to the `CODATECN` date conversion utility program, which accepts an input date in one format and produces the equivalent in the other format.

The structure uses COBOL `REDEFINES` to overlay different parsing layouts on the same 20-byte input and output areas, allowing the utility to handle multiple date formats without separate fields.

**Source file:** `CODATECN.cpy`
**Record length:** N/A (parameter block, not a stored record)
**Used by:** `CODATECN` (date conversion utility program)

## Source COBOL

```cobol
01  CODATECN-REC.
    05  CODATECN-IN-REC.
        10  CODATECN-TYPE             PIC X.
            88  YYYYMMDD-IN           VALUE "1".
            88  YYYY-MM-DD-IN         VALUE "2".
        10  CODATECN-INP-DATE         PIC X(20).
        10  CODATECN-1INP REDEFINES CODATECN-INP-DATE.
            15  CODATECN-1YYYY    PIC XXXX.
            15  CODATECN-1MM      PIC XX.
            15  CODATECN-1DD      PIC XX.
            15  CODATECN-1FIL     PIC X(12).
        10  CODATECN-2INP REDEFINES CODATECN-INP-DATE.
            15  CODATECN-1O-YYYY  PIC XXXX.
            15  CODATECN-1I-S1    PIC X.
            15  CODATECN-1MM      PIC XX.
            15  CODATECN-1I-S2    PIC X.
            15  CODATECN-2YY      PIC XX.
            15  CODATECN-2FIL     PIC X(10).
    05  CODATECN-OUT-REC.
        10  CODATECN-OUTTYPE          PIC X.
            88  YYYY-MM-DD-OP         VALUE "1".
            88  YYYYMMDD-OP           VALUE "2".
        10  CODATECN-0UT-DATE         PIC X(20).
        10  CODATECN-1OUT REDEFINES CODATECN-0UT-DATE.
            15  CODATECN-1O-YYYY  PIC XXXX.
            15  CODATECN-1O-S1    PIC X.
            15  CODATECN-1O-MM    PIC XX.
            15  CODATECN-1O-S2    PIC X.
            15  CODATECN-1O-DD    PIC XX.
            15  CODATECN-1OFIl    PIC X(10).
        10  CODATECN-2OUT REDEFINES CODATECN-0UT-DATE.
            15  CODATECN-2O-YYYY  PIC XXXX.
            15  CODATECN-2O-MM    PIC XX.
            15  CODATECN-2O-DD    PIC XX.
            15  CODATECN-2OFIl    PIC X(12).
    05  CODATECN-ERROR-MSG        PIC X(38).
```

## Field Definitions

### Input Record (CODATECN-IN-REC)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `CODATECN-TYPE` | `X` | 1 | Alphanumeric | Input format type: `"1"` = YYYYMMDD, `"2"` = YYYY-MM-DD |
| 2 | `CODATECN-INP-DATE` | `X(20)` | 20 | Alphanumeric | Raw input date string |
| 2a | `CODATECN-1INP` (REDEFINES) | -- | 20 | Overlay | Parses YYYYMMDD: YYYY(4) + MM(2) + DD(2) + FILLER(12) |
| 2b | `CODATECN-2INP` (REDEFINES) | -- | 20 | Overlay | Parses YYYY-MM-DD: YYYY(4) + separator(1) + MM(2) + separator(1) + DD(2) + FILLER(10) |

### Output Record (CODATECN-OUT-REC)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 3 | `CODATECN-OUTTYPE` | `X` | 1 | Alphanumeric | Output format type: `"1"` = YYYY-MM-DD, `"2"` = YYYYMMDD |
| 4 | `CODATECN-0UT-DATE` | `X(20)` | 20 | Alphanumeric | Raw output date string |
| 4a | `CODATECN-1OUT` (REDEFINES) | -- | 20 | Overlay | Produces YYYY-MM-DD: YYYY(4) + `-`(1) + MM(2) + `-`(1) + DD(2) + FILLER(10) |
| 4b | `CODATECN-2OUT` (REDEFINES) | -- | 20 | Overlay | Produces YYYYMMDD: YYYY(4) + MM(2) + DD(2) + FILLER(12) |

### Error Area

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 5 | `CODATECN-ERROR-MSG` | `X(38)` | 38 | Alphanumeric | Error message if conversion fails |

## Field Notes

1. **CODATECN-TYPE** -- 88-level conditions determine the input format. `"1"` indicates the input is in compact `YYYYMMDD` format (8 characters, no separators). `"2"` indicates the input is in ISO `YYYY-MM-DD` format (10 characters with hyphen separators). The program uses this flag to select the appropriate REDEFINES overlay for parsing.

2. **CODATECN-INP-DATE / REDEFINES** -- The 20-byte input area is shared by multiple overlays. `CODATECN-1INP` parses a compact date by extracting year (positions 1-4), month (5-6), and day (7-8). `CODATECN-2INP` parses a hyphenated date by extracting year (1-4), skipping the separator at position 5, month (6-7), skipping the separator at position 8, and day (9-10). The FILLER bytes account for the remaining unused space in each layout.

3. **CODATECN-OUTTYPE** -- Similar to the input type but controls the output format. `"1"` produces `YYYY-MM-DD` output, `"2"` produces `YYYYMMDD` output. The typical use case is converting compact dates to ISO format for display, or ISO format to compact dates for storage.

4. **CODATECN-0UT-DATE** -- Note the character `0` (zero) in `0UT` rather than the letter `O`. This is an original COBOL naming quirk that must be preserved for traceability. The REDEFINES overlays (`CODATECN-1OUT` and `CODATECN-2OUT`) provide structured access to the output date components.

5. **CODATECN-ERROR-MSG** -- Populated by the `CODATECN` program when conversion fails (e.g., invalid input date, unrecognized format type). A non-blank value in this field indicates a conversion error. The calling program should check this field after each call to the utility.

## REDEFINES Layout Diagram

```
Input (YYYYMMDD, Type "1"):
+------+----+----+------------+
| YYYY | MM | DD | FILLER(12) |
+------+----+----+------------+
  4      2    2     12          = 20 bytes

Input (YYYY-MM-DD, Type "2"):
+------+-+----+-+----+----------+
| YYYY |-| MM |-| DD | FILL(10) |
+------+-+----+-+----+----------+
  4     1  2   1  2     10       = 20 bytes

Output (YYYY-MM-DD, OutType "1"):
+------+-+----+-+----+----------+
| YYYY |-| MM |-| DD | FILL(10) |
+------+-+----+-+----+----------+
  4     1  2   1  2     10       = 20 bytes

Output (YYYYMMDD, OutType "2"):
+------+----+----+------------+
| YYYY | MM | DD | FILLER(12) |
+------+----+----+------------+
  4      2    2     12          = 20 bytes
```

## Target Architecture Mapping

| Aspect | COBOL (Current) | .NET (Target) |
|--------|----------------|---------------|
| **Purpose** | Convert between date string formats | `DateTime.ParseExact` / `DateTime.ToString` |
| **Input parsing** | REDEFINES overlays on PIC X(20) | `DateTime.ParseExact(input, "yyyyMMdd", ...)` or `DateTime.ParseExact(input, "yyyy-MM-dd", ...)` |
| **Output formatting** | Move to REDEFINES overlay fields | `dateTime.ToString("yyyy-MM-dd")` or `dateTime.ToString("yyyyMMdd")` |
| **Error handling** | `CODATECN-ERROR-MSG` populated on failure | Exception handling (`FormatException`) or `DateTime.TryParseExact` returning `bool` |
| **Utility program** | `CODATECN` called via `CALL` statement | Static helper method or extension method on `DateTime`/`DateOnly` |

### .NET Equivalent

The entire `CODATECN` utility program and its parameter copybook are replaced by standard .NET date parsing and formatting:

```csharp
// Equivalent of CODATECN utility in .NET
public static class DateConversion
{
    public static string ConvertDateFormat(string input, string inputFormat, string outputFormat)
    {
        // inputFormat: "yyyyMMdd" or "yyyy-MM-dd"
        // outputFormat: "yyyyMMdd" or "yyyy-MM-dd"
        var date = DateOnly.ParseExact(input.Trim(), inputFormat, CultureInfo.InvariantCulture);
        return date.ToString(outputFormat, CultureInfo.InvariantCulture);
    }
}
```

## Migration Notes

### Why This Copybook Exists

In COBOL, there are no built-in date formatting functions. Programs must manually parse date strings by extracting substrings at known positions. The `CODATECN` utility centralizes this logic so that each program does not need its own date parsing code. The REDEFINES technique is the COBOL equivalent of a union type or reinterpret cast.

### Migration Strategy

This copybook and its associated utility program (`CODATECN`) are **not migrated** as a separate component. The .NET `DateTime`, `DateOnly`, and `DateTimeOffset` types provide native date parsing and formatting that completely replaces this utility. All calling programs should be refactored to use standard .NET date methods directly.

### Data Type Mapping

| COBOL Concept | .NET Equivalent | Notes |
|--------------|----------------|-------|
| `YYYYMMDD` format (Type "1") | `DateOnly.ParseExact(s, "yyyyMMdd")` | Compact format without separators |
| `YYYY-MM-DD` format (Type "2") | `DateOnly.ParseExact(s, "yyyy-MM-dd")` | ISO 8601 format |
| `CODATECN-ERROR-MSG` | `FormatException` / `TryParseExact` | .NET exception model replaces error message field |
| REDEFINES overlays | Not needed | .NET handles format strings natively |

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** | Date fields in regulatory reports must use consistent formatting. | Standardize on ISO 8601 (`yyyy-MM-dd`) throughout the .NET system. Use `CultureInfo.InvariantCulture` for all date formatting to avoid locale-dependent behavior. |
| **DORA** Art. 11 | ICT data integrity -- date conversion must not introduce errors during migration. | Parallel-run testing must verify that date values converted by the COBOL utility produce identical results when parsed by .NET `DateOnly.ParseExact`. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
