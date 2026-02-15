---
id: DS-TCAT-001
title: "Transaction Category Record"
copybook_name: "CVTRA04Y.cpy"
domain: "transactions"
used_by_programs: [COTRN00C, COTRN01C, COTRN02C, CBTRN01C]
record_length: 60
status: "extracted"
target_schema: "dbo.TransactionCategory"
sidebar_position: 9
---

# DS-TCAT-001: Transaction Category Record (CVTRA04Y)

## Overview

The `CVTRA04Y.cpy` copybook defines the **Transaction Category Record**, a reference/lookup table that provides descriptions for transaction categories within a given transaction type. Each category is identified by the composite key of transaction type code and category code, enabling hierarchical classification: a type (e.g., "Sales") contains multiple categories (e.g., "Retail", "E-Commerce", "Recurring").

This record is used by both online CICS transaction programs and batch processing to resolve category codes into human-readable descriptions for display and reporting.

**Source file:** `CVTRA04Y.cpy`
**Record length:** 60 bytes
**Used by:** `COTRN00C`, `COTRN01C`, `COTRN02C`, `CBTRN01C`
**VSAM file:** Transaction category reference file

## Source COBOL

```cobol
01  TRAN-CAT-RECORD.
    05  TRAN-CAT-KEY.
       10  TRAN-TYPE-CD                         PIC X(02).
       10  TRAN-CAT-CD                          PIC 9(04).
    05  TRAN-CAT-TYPE-DESC                      PIC X(50).
    05  FILLER                                  PIC X(04).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Offset | Length | Description |
|---|------------|-----------|------|--------|--------|-------------|
| 1 | `TRAN-TYPE-CD` | `X(02)` | Alphanumeric | 0 | 2 | Transaction type code (part of composite key) |
| 2 | `TRAN-CAT-CD` | `9(04)` | Numeric (display) | 2 | 4 | Transaction category code (part of composite key) |
| 3 | `TRAN-CAT-TYPE-DESC` | `X(50)` | Alphanumeric | 6 | 50 | Category description |
| 4 | `FILLER` | `X(04)` | Filler | 56 | 4 | Reserved/unused space |

**Total record length:** 60 bytes

## Field Notes

1. **TRAN-TYPE-CD** -- 2-character alphanumeric transaction type code. Part of the composite primary key. Foreign key to the Transaction Type reference table (CVTRA03Y). Each category belongs to exactly one type.

2. **TRAN-CAT-CD** -- 4-digit numeric category code. Part of the composite primary key. Together with `TRAN-TYPE-CD`, uniquely identifies a transaction category. Leading zeros are significant (e.g., `0001` is distinct from `1`).

3. **TRAN-CAT-TYPE-DESC** -- 50-character description of the transaction category within its parent type. Padded with trailing EBCDIC spaces. Trim trailing spaces during migration. Example: for type "SA" and category `5001`, the description might be "Retail Point-of-Sale".

4. **FILLER** -- 4 bytes of reserved space. Verify contents are consistently spaces or low-values before discarding.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration |
|-------|---------------------|
| `TRAN-TYPE-CD` | Standard EBCDIC-to-UTF-8 character conversion. Preserve exact 2-character value. |
| `TRAN-CAT-CD` | Zoned decimal (EBCDIC digits `F0`-`F9`). Convert to ASCII digits. Preserve leading zeros. |
| `TRAN-CAT-TYPE-DESC` | Standard EBCDIC-to-UTF-8 character conversion. Trim trailing EBCDIC spaces (`X'40'`). Check for Swedish characters (a-ring, a-umlaut, o-umlaut). |
| `FILLER` | Ignore. Verify contents are EBCDIC spaces (`X'40'`) or low-values (`X'00'`). |

## Referential Integrity

```
CVTRA03Y.TRAN-TYPE (Transaction Type)
  |
  v
TRAN-CAT-RECORD (CVTRA04Y)
  ^
  |
  +-- Referenced by: CVTRA01Y.TRANCAT-TYPE-CD + TRANCAT-CD (Transaction Category Balance)
  +-- Referenced by: CVTRA02Y.DIS-TRAN-TYPE-CD + DIS-TRAN-CAT-CD (Discount Group)
  +-- Referenced by: CVTRA05Y.TRAN-TYPE-CD + TRAN-CAT-CD (Transaction Record)
  +-- Referenced by: CVTRA06Y.DALYTRAN-TYPE-CD + DALYTRAN-CAT-CD (Daily Transaction Record)
```

> **Note:** This table depends on `dbo.TransactionType` and must be loaded after it. It is referenced by balance, discount, and transaction tables which must be loaded after it.

## Sample Data

| TRAN-TYPE-CD | TRAN-CAT-CD | TRAN-CAT-TYPE-DESC |
|-------------|-------------|-------------------|
| `SA` | `5001` | `Retail Point-of-Sale` |
| `SA` | `5002` | `E-Commerce Purchase` |
| `SA` | `5003` | `Recurring Subscription` |
| `CR` | `6001` | `Merchant Credit` |
| `CR` | `6002` | `Promotional Credit` |
| `PA` | `7001` | `Monthly Payment` |

> **Note:** Actual category codes and descriptions must be extracted from the production VSAM file. The examples above are representative.

## Migration Notes

### Target DDL

```sql
CREATE TABLE dbo.TransactionCategory (
    TransactionTypeCd     CHAR(2)        NOT NULL,
    TransactionCategoryCd INT            NOT NULL,
    CategoryDescription   NVARCHAR(50)   NOT NULL,

    CONSTRAINT PK_TransactionCategory
        PRIMARY KEY (TransactionTypeCd, TransactionCategoryCd),

    CONSTRAINT FK_TransCat_TransType
        FOREIGN KEY (TransactionTypeCd)
        REFERENCES dbo.TransactionType (TransactionTypeCd)
);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `TRAN-TYPE-CD` | `PIC X(02)` | `CHAR(2)` | `string` | Fixed-length 2-char FK to TransactionType |
| `TRAN-CAT-CD` | `PIC 9(04)` | `INT` | `int` | 4-digit numeric category code |
| `TRAN-CAT-TYPE-DESC` | `PIC X(50)` | `NVARCHAR(50)` | `string` | Use NVARCHAR for Unicode (Swedish characters) |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS RowCount FROM dbo.TransactionCategory;

-- Verify all categories have valid parent type codes
SELECT tc.* FROM dbo.TransactionCategory tc
LEFT JOIN dbo.TransactionType tt ON tc.TransactionTypeCd = tt.TransactionTypeCd
WHERE tt.TransactionTypeCd IS NULL;

-- Verify descriptions are populated
SELECT * FROM dbo.TransactionCategory
WHERE CategoryDescription IS NULL OR LTRIM(RTRIM(CategoryDescription)) = '';

-- Category count per type (for reconciliation with mainframe)
SELECT TransactionTypeCd, COUNT(*) AS CategoryCount
FROM dbo.TransactionCategory
GROUP BY TransactionTypeCd
ORDER BY TransactionTypeCd;
```

### Migration Load Order

**Depends on:** `dbo.TransactionType` (CVTRA03Y) -- must be loaded first.

**Must be loaded before:**
1. `dbo.TransactionCategoryBalance` (CVTRA01Y)
2. `dbo.DiscountGroup` (CVTRA02Y)
3. `dbo.Transaction` (CVTRA05Y)
4. `dbo.DailyTransaction` (CVTRA06Y)

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** | Transaction categorisation must be consistent for regulatory reporting. Categories used in FSA reports must be traceable. | Maintain version history for category descriptions. Map categories to FSA reporting codes where applicable. |
| **PSD2** | Payment categories must align with PSD2 payment instrument classifications. | Ensure payment-related categories map to PSD2 Annex I categories for regulatory reporting. |
| **AML/KYC** | Transaction categories support AML pattern detection (e.g., unusual category mix may indicate suspicious activity). | Category codes must be available to AML screening rules. New categories require AML rule review before activation. |
| **DORA** | Reference data integrity is critical for correct transaction processing and reporting. | Include in data integrity monitoring, change management, and disaster recovery testing. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
