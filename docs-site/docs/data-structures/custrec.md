---
id: DS-CUST-002
title: "Customer Record - Alternate"
copybook_name: "CUSTREC.cpy"
domain: "account-management"
used_by_programs: [COACTUPC, COACTVWC]
record_length: 500
status: "extracted"
target_schema: "dbo.Customer"
sidebar_position: 35
---

# DS-CUST-002: Customer Record - Alternate (CUSTREC)

## Overview

The `CUSTREC.cpy` copybook defines an **alternate Customer Record** that is structurally identical to the primary customer record copybook `CVCUS01Y.cpy` (DS-CUST-001), with one minor naming difference: the date of birth field is named `CUST-DOB-YYYYMMDD` instead of `CUST-DOB-YYYY-MM-DD`.

This alternate copybook exists as a separate file likely due to COBOL naming conventions or module-level preferences in different development teams. Both copybooks map to the same target table (`dbo.Customer`) and describe the same 500-byte VSAM record layout.

**Source file:** `CUSTREC.cpy`
**Record length:** 500 bytes
**Used by:** `COACTUPC`, `COACTVWC` (alternate copy)
**Primary equivalent:** `CVCUS01Y.cpy` (DS-CUST-001)
**Target table:** `dbo.Customer` (same as CVCUS01Y)

## Key Difference from CVCUS01Y

| Aspect | CVCUS01Y.cpy (DS-CUST-001) | CUSTREC.cpy (DS-CUST-002, this file) |
|--------|---------------------------|--------------------------------------|
| **DOB field name** | `CUST-DOB-YYYY-MM-DD` | `CUST-DOB-YYYYMMDD` |
| **DOB format implied** | Hyphenated ISO format (`YYYY-MM-DD`) | Compact format (`YYYYMMDD`) |
| **All other fields** | Identical | Identical |
| **Record length** | 500 bytes | 500 bytes |
| **Target table** | `dbo.Customer` | `dbo.Customer` |

## Source COBOL (Key Fields)

The record structure is identical to `CVCUS01Y.cpy`. The following shows the relevant field difference:

```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID                      PIC 9(09).
    05  CUST-FIRST-NAME              PIC X(25).
    05  CUST-MIDDLE-NAME             PIC X(25).
    05  CUST-LAST-NAME               PIC X(25).
    05  CUST-ADDR-LINE-1             PIC X(50).
    05  CUST-ADDR-LINE-2             PIC X(50).
    05  CUST-ADDR-LINE-3             PIC X(50).
    05  CUST-ADDR-STATE-CD           PIC X(02).
    05  CUST-ADDR-COUNTRY-CD         PIC X(03).
    05  CUST-ADDR-ZIP                PIC X(10).
    05  CUST-PHONE-NUM-1             PIC X(15).
    05  CUST-PHONE-NUM-2             PIC X(15).
    05  CUST-SSN                     PIC 9(09).
    05  CUST-GOVT-ISSUED-ID          PIC X(20).
    05  CUST-DOB-YYYYMMDD            PIC X(10).
*   ^^^ Note: Named CUST-DOB-YYYYMMDD here
*       vs CUST-DOB-YYYY-MM-DD in CVCUS01Y
    05  CUST-EFT-ACCOUNT-ID          PIC X(10).
    05  CUST-PRI-CARD-HOLDER-IND     PIC X(01).
    05  CUST-FICO-CREDIT-SCORE       PIC 9(03).
    05  FILLER                       PIC X(168).
```

## Field Notes

All field descriptions are identical to `CVCUS01Y.cpy` (DS-CUST-001). The only notable difference is:

1. **CUST-DOB-YYYYMMDD** -- Despite the field name suggesting a compact `YYYYMMDD` format, the PIC clause is `X(10)`, which is the same 10-byte field as in `CVCUS01Y`. The actual data format stored in VSAM is identical regardless of which copybook is used to read the record. The name difference is purely cosmetic at the COBOL source level. Both copybooks overlay the same byte positions in the same VSAM record.

2. **Format reconciliation** -- The naming difference must be reconciled during migration. Since both copybooks read the same physical data, the actual date format in the VSAM file is determined by how programs write the data, not by the copybook field name. Analysis of the programs that write customer records (`COACTUPC`) must confirm whether the DOB is stored as `YYYYMMDD` or `YYYY-MM-DD`.

## Migration Notes

### Reconciliation Strategy

Since both copybooks describe the same physical record:

1. **Choose one canonical representation** -- The primary copybook `CVCUS01Y.cpy` should be the canonical reference for the `dbo.Customer` table definition.
2. **Verify data format** -- Inspect actual VSAM data to determine whether DOB values contain hyphens (`YYYY-MM-DD`, 10 chars) or are compact (`YYYYMMDD`, 8 chars padded with spaces). The PIC X(10) clause accommodates both.
3. **Single migration path** -- Only one ETL process is needed for the Customer table, regardless of which copybook was used by the source program.

### Target DDL

The target DDL is identical to `CVCUS01Y.cpy` (DS-CUST-001). See that document for the complete `CREATE TABLE dbo.Customer` statement.

### Data Type Mapping (DOB Field Only)

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `CUST-DOB-YYYYMMDD` | `PIC X(10)` | `DATE` | `DateOnly?` | Parse with both `yyyyMMdd` and `yyyy-MM-dd` format strings |

### DOB Parsing Strategy

```csharp
// Handle both possible formats during migration
public static DateOnly? ParseCustomerDob(string rawValue)
{
    var trimmed = rawValue.Trim();
    if (string.IsNullOrEmpty(trimmed))
        return null;

    // Try ISO format first (YYYY-MM-DD)
    if (DateOnly.TryParseExact(trimmed, "yyyy-MM-dd",
        CultureInfo.InvariantCulture, DateTimeStyles.None, out var isoDate))
        return isoDate;

    // Try compact format (YYYYMMDD)
    if (DateOnly.TryParseExact(trimmed, "yyyyMMdd",
        CultureInfo.InvariantCulture, DateTimeStyles.None, out var compactDate))
        return compactDate;

    // Log warning and return null for unparseable values
    return null;
}
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **GDPR** Art. 5(1)(d) | Accuracy -- personal data must be accurate. | The naming inconsistency between copybooks must not cause data interpretation errors during migration. Parallel-run comparison must verify DOB values are parsed identically regardless of which copybook was used. |
| **GDPR** Art. 9 | Special categories of personal data. | Date of birth is personal data. Combined with SSN and name, it constitutes identifying information. Same GDPR protections apply as for `CVCUS01Y`. |
| **FSA (FFFS 2014:5)** | Data integrity for customer records. | The existence of two copybooks for the same record is a data integrity risk. The migration must ensure a single, consistent customer record definition. |
| **AML/KYC** (FFFS 2017:11) | Customer identification -- DOB is a KYC field. | Date of birth is required for identity verification. Parsing errors during migration could compromise KYC compliance. |
| **DORA** Art. 11 | ICT data integrity. | Migration validation must confirm zero discrepancies between customer records loaded via `CVCUS01Y` and `CUSTREC` copybook interpretations. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
