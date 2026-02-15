---
id: DS-DATE-001
title: "Date/Time Structure"
copybook_name: "CSDAT01Y.cpy"
domain: "system"
used_by_programs: [COMEN01C, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, COBIL00C, CORPT00C, COADM01C, CBACT01C, CBACT02C, CBACT03C, CBTRN01C, CBTRN02C, CBTRN03C, CBTRN04C]
record_length: 0
status: "extracted"
target_schema: "N/A (utility)"
sidebar_position: 25
---

# DS-DATE-001: Date/Time Structure (CSDAT01Y)

## Overview

The `CSDAT01Y.cpy` copybook defines the **Date/Time Utility Structure**, a working storage area used by all CICS and batch programs to capture, format, and display the current date and time. The structure uses multiple `REDEFINES` clauses to provide different views of the same date/time data -- component fields (year, month, day, etc.) and formatted strings (MM/DD/YY, HH:MM:SS, full timestamp).

This is not a database record -- it is a utility data area populated at runtime by COBOL intrinsic functions (`FUNCTION CURRENT-DATE`) or CICS system calls (`EXEC CICS ASKTIME`). It provides date/time values for screen display, audit timestamps, and batch processing.

**Source file:** `CSDAT01Y.cpy`
**Used by:** ALL CICS and batch programs

## Source COBOL

```cobol
01 WS-DATE-TIME.
   05 WS-CURDATE.
      10 WS-CURDATE-YEAR              PIC 9(04).
      10 WS-CURDATE-MONTH             PIC 9(02).
      10 WS-CURDATE-DAY               PIC 9(02).
   05 WS-CURDATE-N REDEFINES WS-CURDATE
                                      PIC 9(08).
   05 WS-CURTIME.
      10 WS-CURTIME-HOURS             PIC 9(02).
      10 WS-CURTIME-MINUTES           PIC 9(02).
      10 WS-CURTIME-SECONDS           PIC 9(02).
      10 WS-CURTIME-MILSEC            PIC 9(02).
   05 WS-CURTIME-N REDEFINES WS-CURTIME
                                      PIC 9(08).
   05 WS-CURDATE-MM-DD-YY.
      10 WS-CURDATE-MM                PIC 9(02).
      10 FILLER                       PIC X(01) VALUE '/'.
      10 WS-CURDATE-DD                PIC 9(02).
      10 FILLER                       PIC X(01) VALUE '/'.
      10 WS-CURDATE-YY                PIC 9(02).
   05 WS-CURTIME-HH-MM-SS.
      10 WS-CURTIME-HH                PIC 9(02).
      10 FILLER                       PIC X(01) VALUE ':'.
      10 WS-CURTIME-MM                PIC 9(02).
      10 FILLER                       PIC X(01) VALUE ':'.
      10 WS-CURTIME-SS                PIC 9(02).
   05 WS-TIMESTAMP.
      10 WS-TS-YYYY                   PIC 9(04).
      10 FILLER                       PIC X(01) VALUE '-'.
      10 WS-TS-MM                     PIC 9(02).
      10 FILLER                       PIC X(01) VALUE '-'.
      10 WS-TS-DD                     PIC 9(02).
      10 FILLER                       PIC X(01) VALUE ' '.
      10 WS-TS-HH                     PIC 9(02).
      10 FILLER                       PIC X(01) VALUE ':'.
      10 WS-TS-MIN                    PIC 9(02).
      10 FILLER                       PIC X(01) VALUE ':'.
      10 WS-TS-SEC                    PIC 9(02).
      10 FILLER                       PIC X(01) VALUE '.'.
      10 WS-TS-MILSEC                 PIC 9(06).
```

## Field Definitions

### Date Component View (`WS-CURDATE`)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `WS-CURDATE-YEAR` | `9(04)` | 4 | Numeric | Four-digit year (YYYY) |
| 2 | `WS-CURDATE-MONTH` | `9(02)` | 2 | Numeric | Two-digit month (01-12) |
| 3 | `WS-CURDATE-DAY` | `9(02)` | 2 | Numeric | Two-digit day (01-31) |
| 4 | `WS-CURDATE-N` | `9(08)` | 8 | Numeric (REDEFINES) | Date as single numeric value (YYYYMMDD) |

### Time Component View (`WS-CURTIME`)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 5 | `WS-CURTIME-HOURS` | `9(02)` | 2 | Numeric | Hours (00-23) |
| 6 | `WS-CURTIME-MINUTES` | `9(02)` | 2 | Numeric | Minutes (00-59) |
| 7 | `WS-CURTIME-SECONDS` | `9(02)` | 2 | Numeric | Seconds (00-59) |
| 8 | `WS-CURTIME-MILSEC` | `9(02)` | 2 | Numeric | Hundredths of a second (00-99) |
| 9 | `WS-CURTIME-N` | `9(08)` | 8 | Numeric (REDEFINES) | Time as single numeric value (HHMMSSmm) |

### Formatted Date View (`WS-CURDATE-MM-DD-YY`)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 10 | `WS-CURDATE-MM` | `9(02)` | 2 | Numeric | Month for MM/DD/YY format |
| 11 | `WS-CURDATE-DD` | `9(02)` | 2 | Numeric | Day for MM/DD/YY format |
| 12 | `WS-CURDATE-YY` | `9(02)` | 2 | Numeric | Two-digit year for MM/DD/YY format |

### Formatted Time View (`WS-CURTIME-HH-MM-SS`)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 13 | `WS-CURTIME-HH` | `9(02)` | 2 | Numeric | Hours for HH:MM:SS format |
| 14 | `WS-CURTIME-MM` | `9(02)` | 2 | Numeric | Minutes for HH:MM:SS format |
| 15 | `WS-CURTIME-SS` | `9(02)` | 2 | Numeric | Seconds for HH:MM:SS format |

### Full Timestamp View (`WS-TIMESTAMP`)

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 16 | `WS-TS-YYYY` | `9(04)` | 4 | Numeric | Year component of timestamp |
| 17 | `WS-TS-MM` | `9(02)` | 2 | Numeric | Month component of timestamp |
| 18 | `WS-TS-DD` | `9(02)` | 2 | Numeric | Day component of timestamp |
| 19 | `WS-TS-HH` | `9(02)` | 2 | Numeric | Hour component of timestamp |
| 20 | `WS-TS-MIN` | `9(02)` | 2 | Numeric | Minute component of timestamp |
| 21 | `WS-TS-SEC` | `9(02)` | 2 | Numeric | Second component of timestamp |
| 22 | `WS-TS-MILSEC` | `9(06)` | 6 | Numeric | Microsecond component of timestamp |

## Field Notes

1. **REDEFINES pattern:** `WS-CURDATE-N` redefines `WS-CURDATE` as a single 8-digit numeric, allowing the date `2026-02-15` to be treated as the number `20260215` for arithmetic comparisons and date calculations. Similarly, `WS-CURTIME-N` redefines the time components as a single number.

2. **MM/DD/YY format:** The `WS-CURDATE-MM-DD-YY` view uses US date format with `/` separators. This is a display format only. In the .NET system for NordKredit (Swedish bank), dates should be displayed in ISO format (YYYY-MM-DD) or Swedish locale format (YYYY-MM-DD), not MM/DD/YY.

3. **Two-digit year:** The `WS-CURDATE-YY` field uses only 2 digits for the year, which is a Y2K-era concern. The full timestamp view (`WS-TIMESTAMP`) uses 4-digit years. The .NET replacement should exclusively use 4-digit years.

4. **Milliseconds vs. microseconds:** The time component view uses `WS-CURTIME-MILSEC` with `PIC 9(02)` (hundredths of a second), while the timestamp view uses `WS-TS-MILSEC` with `PIC 9(06)` (microseconds). The naming is inconsistent -- "MILSEC" suggests milliseconds but the PIC clauses indicate different precisions. The .NET `DateTimeOffset` type provides tick-level (100-nanosecond) precision, which exceeds both.

5. **Timestamp format:** The `WS-TIMESTAMP` field produces a format like `YYYY-MM-DD HH:MM:SS.SSSSSS`, which is compatible with ISO 8601. This maps directly to `DateTimeOffset.ToString("yyyy-MM-dd HH:mm:ss.ffffff")` in .NET.

6. **Population:** Programs populate this structure using either `FUNCTION CURRENT-DATE` (COBOL intrinsic) or `EXEC CICS ASKTIME` / `EXEC CICS FORMATTIME` (CICS system calls). The fields are then moved to screen display fields or used in record timestamps.

## Target Architecture Mapping

| Aspect | COBOL (Current) | .NET (Target) |
|--------|-----------------|---------------|
| **Date/time type** | Multiple COBOL fields with REDEFINES | `DateTimeOffset` (single type with timezone) |
| **Current time** | `FUNCTION CURRENT-DATE` or `EXEC CICS ASKTIME` | `DateTimeOffset.UtcNow` or `IClock.UtcNow` (for testability) |
| **Date formatting** | Manual field moves to formatted views | `DateTimeOffset.ToString("yyyy-MM-dd")` or culture-specific formatting |
| **Timestamp storage** | `PIC X(26)` string | `DATETIME2` or `DATETIMEOFFSET` in Azure SQL |
| **Timezone** | Implicit (server local time) | Explicit UTC storage with CET/CEST display conversion for Swedish users |

### .NET Utility (Conceptual)

```csharp
public interface IClock
{
    DateTimeOffset UtcNow { get; }
}

public class SystemClock : IClock
{
    public DateTimeOffset UtcNow => DateTimeOffset.UtcNow;
}

// Usage in services
public class TransactionService
{
    private readonly IClock _clock;

    public TransactionService(IClock clock)
    {
        _clock = clock;
    }

    public DateTimeOffset GetCurrentTimestamp()
    {
        return _clock.UtcNow;
    }

    public string FormatForDisplay(DateTimeOffset timestamp)
    {
        // Swedish locale: YYYY-MM-DD HH:mm
        var swedishTimeZone = TimeZoneInfo.FindSystemTimeZoneById("Europe/Stockholm");
        var local = TimeZoneInfo.ConvertTime(timestamp, swedishTimeZone);
        return local.ToString("yyyy-MM-dd HH:mm", CultureInfo.GetCultureInfo("sv-SE"));
    }
}
```

## Migration Notes

1. **Use `DateTimeOffset` everywhere:** The multiple COBOL date/time views (component, formatted, numeric) are all replaced by a single `DateTimeOffset` type in .NET. The various formats are produced at display time using format strings.

2. **UTC storage, local display:** The COBOL system uses implicit server-local time. The .NET system must store all timestamps in UTC and convert to CET/CEST (Europe/Stockholm timezone) for display. This is essential for correct behavior during daylight saving time transitions.

3. **Inject IClock for testability:** Rather than calling `DateTimeOffset.UtcNow` directly, inject an `IClock` interface. This allows unit tests to control time without mocking static methods.

4. **Locale-appropriate formatting:** Replace the US-centric MM/DD/YY format with ISO 8601 or Swedish locale formatting (YYYY-MM-DD). The .NET `CultureInfo` for `sv-SE` handles this automatically.

5. **Microsecond precision:** Azure SQL `DATETIME2` supports 100-nanosecond precision, which exceeds the COBOL microsecond precision. No loss of precision during migration.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Audit trail timestamps must be accurate and tamper-evident | All audit timestamps must use UTC with timezone offset recorded. Use database server time (`SYSUTCDATETIME()`) for audit columns, not application-provided time, to prevent timestamp manipulation. |
| **DORA** Art. 11 | ICT systems must maintain synchronized time sources | All application servers and database servers must synchronize to a common NTP source. Timestamp drift between systems must be monitored and alerted. |
| **PSD2** Art. 45 | Transaction timestamps for payment order recording | Payment transaction timestamps must record both origination and processing times with sufficient precision for dispute resolution. The microsecond precision in `WS-TIMESTAMP` is adequate. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
