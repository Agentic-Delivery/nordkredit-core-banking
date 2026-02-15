---
id: DS-TTYP-001
title: "Transaction Type Record"
copybook_name: "CVTRA03Y.cpy"
domain: "transactions"
used_by_programs: [COTRN00C, COTRN01C, COTRN02C, CBTRN01C, CBTRN02C]
record_length: 60
status: "extracted"
target_schema: "dbo.TransactionType"
sidebar_position: 8
---

# DS-TTYP-001: Transaction Type Record (CVTRA03Y)

## Overview

The `CVTRA03Y.cpy` copybook defines the **Transaction Type Record**, a simple reference/lookup table that maps 2-character transaction type codes to their human-readable descriptions. This is one of the core reference data structures in the CardDemo transaction subsystem, used by both online CICS programs and batch processing programs to classify and describe transactions.

The record structure is straightforward: a 2-character code serves as the primary key, followed by a 50-character description.

**Source file:** `CVTRA03Y.cpy`
**Record length:** 60 bytes
**Used by:** `COTRN00C`, `COTRN01C`, `COTRN02C`, `CBTRN01C`, `CBTRN02C`
**VSAM file:** Transaction type reference file

## Source COBOL

```cobol
01  TRAN-TYPE-RECORD.
    05  TRAN-TYPE                               PIC X(02).
    05  TRAN-TYPE-DESC                          PIC X(50).
    05  FILLER                                  PIC X(08).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Offset | Length | Description |
|---|------------|-----------|------|--------|--------|-------------|
| 1 | `TRAN-TYPE` | `X(02)` | Alphanumeric | 0 | 2 | Transaction type code (primary key) |
| 2 | `TRAN-TYPE-DESC` | `X(50)` | Alphanumeric | 2 | 50 | Transaction type description |
| 3 | `FILLER` | `X(08)` | Filler | 52 | 8 | Reserved/unused space |

**Total record length:** 60 bytes

## Field Notes

1. **TRAN-TYPE** -- 2-character alphanumeric code that uniquely identifies a transaction type. This is the primary key. Examples include "SA" (Sales), "CR" (Credits), "PA" (Payments). This code is referenced by transaction records (CVTRA05Y), category records (CVTRA04Y), category balance records (CVTRA01Y), and discount group records (CVTRA02Y).

2. **TRAN-TYPE-DESC** -- 50-character description of the transaction type. Padded with trailing EBCDIC spaces. Trim trailing spaces during migration. Example: "Sales" followed by 45 spaces.

3. **FILLER** -- 8 bytes of reserved space for future expansion. Verify contents are consistently spaces or low-values before discarding.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration |
|-------|---------------------|
| `TRAN-TYPE` | Standard EBCDIC-to-UTF-8 character conversion. Preserve exact 2-character value including any spaces. |
| `TRAN-TYPE-DESC` | Standard EBCDIC-to-UTF-8 character conversion. Trim trailing EBCDIC spaces (`X'40'`). Check for Swedish characters (a-ring, a-umlaut, o-umlaut) which use different EBCDIC code points than ASCII/UTF-8. |
| `FILLER` | Ignore. Verify contents are EBCDIC spaces (`X'40'`) or low-values (`X'00'`). |

## Referential Integrity

```
TRAN-TYPE-RECORD (CVTRA03Y)
  ^
  |
  +-- Referenced by: CVTRA01Y.TRANCAT-TYPE-CD (Transaction Category Balance)
  +-- Referenced by: CVTRA02Y.DIS-TRAN-TYPE-CD (Discount Group)
  +-- Referenced by: CVTRA04Y.TRAN-TYPE-CD (Transaction Category)
  +-- Referenced by: CVTRA05Y.TRAN-TYPE-CD (Transaction Record)
  +-- Referenced by: CVTRA06Y.DALYTRAN-TYPE-CD (Daily Transaction Record)
```

> **Note:** This is a parent reference table. It must be loaded before any dependent tables during migration.

## Sample Data

| TRAN-TYPE | TRAN-TYPE-DESC |
|-----------|---------------|
| `SA` | `Sales` |
| `CR` | `Credits` |
| `PA` | `Payments` |
| `RF` | `Refunds` |
| `FE` | `Fees` |

> **Note:** Actual type codes and descriptions must be extracted from the production VSAM file. The examples above are representative.

## Migration Notes

### Target DDL

```sql
CREATE TABLE dbo.TransactionType (
    TransactionTypeCd    CHAR(2)        NOT NULL,
    TypeDescription      NVARCHAR(50)   NOT NULL,

    CONSTRAINT PK_TransactionType
        PRIMARY KEY (TransactionTypeCd)
);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `TRAN-TYPE` | `PIC X(02)` | `CHAR(2)` | `string` | Fixed-length 2-char primary key |
| `TRAN-TYPE-DESC` | `PIC X(50)` | `NVARCHAR(50)` | `string` | Use NVARCHAR for Unicode support (Swedish characters) |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS RowCount FROM dbo.TransactionType;

-- Verify all type codes are non-blank
SELECT * FROM dbo.TransactionType
WHERE TransactionTypeCd IS NULL OR LTRIM(RTRIM(TransactionTypeCd)) = '';

-- Verify descriptions are populated
SELECT * FROM dbo.TransactionType
WHERE TypeDescription IS NULL OR LTRIM(RTRIM(TypeDescription)) = '';

-- Cross-reference: ensure all type codes used in transactions exist
SELECT DISTINCT t.TransactionTypeCd
FROM dbo.[Transaction] t
LEFT JOIN dbo.TransactionType tt ON t.TransactionTypeCd = tt.TransactionTypeCd
WHERE tt.TransactionTypeCd IS NULL;
```

### Migration Load Order

This table must be loaded **before** the following dependent tables:
1. `dbo.TransactionCategory` (CVTRA04Y)
2. `dbo.TransactionCategoryBalance` (CVTRA01Y)
3. `dbo.DiscountGroup` (CVTRA02Y)
4. `dbo.Transaction` (CVTRA05Y)
5. `dbo.DailyTransaction` (CVTRA06Y)

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** | Transaction classification must be consistent and auditable for regulatory reporting. | Maintain complete type code reference data. Changes to type codes or descriptions must be versioned with effective dates. |
| **PSD2** | Payment transaction types must be clearly categorised for regulatory reporting (AISP/PISP). | Ensure type codes used for payment transactions are mapped to PSD2 payment instrument categories. |
| **AML/KYC** | Transaction type classification supports suspicious activity detection. | Type codes must be available to AML screening rules. New type codes require AML rule review. |
| **DORA** | Reference data integrity -- critical for correct transaction processing. | Include in data integrity monitoring. Changes require change management approval. Backup and recovery testing must cover reference data. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
