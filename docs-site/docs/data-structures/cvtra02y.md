---
id: DS-DGRP-001
title: "Discount Group Record"
copybook_name: "CVTRA02Y.cpy"
domain: "transactions"
used_by_programs: [CBTRN01C, CBTRN02C]
record_length: 50
status: "extracted"
target_schema: "dbo.DiscountGroup"
sidebar_position: 7
---

# DS-DGRP-001: Discount Group Record (CVTRA02Y)

## Overview

The `CVTRA02Y.cpy` copybook defines the **Discount Group Record**, which links account groups to interest rates by transaction type and category. This structure supports differentiated pricing where groups of accounts receive specific interest rates based on the classification of their transactions.

The composite key (account group ID + transaction type code + transaction category code) enables the system to look up the applicable interest rate for any given group/type/category combination during batch processing and transaction posting.

**Source file:** `CVTRA02Y.cpy`
**Record length:** 50 bytes
**Used by:** `CBTRN01C`, `CBTRN02C`
**VSAM file:** Discount group file

## Source COBOL

```cobol
01  DIS-GROUP-RECORD.
    05  DIS-GROUP-KEY.
       10 DIS-ACCT-GROUP-ID                     PIC X(10).
       10 DIS-TRAN-TYPE-CD                      PIC X(02).
       10 DIS-TRAN-CAT-CD                       PIC 9(04).
    05  DIS-INT-RATE                            PIC S9(04)V99.
    05  FILLER                                  PIC X(28).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Offset | Length | Description |
|---|------------|-----------|------|--------|--------|-------------|
| 1 | `DIS-ACCT-GROUP-ID` | `X(10)` | Alphanumeric | 0 | 10 | Account group identifier |
| 2 | `DIS-TRAN-TYPE-CD` | `X(02)` | Alphanumeric | 10 | 2 | Transaction type code |
| 3 | `DIS-TRAN-CAT-CD` | `9(04)` | Numeric (display) | 12 | 4 | Transaction category code |
| 4 | `DIS-INT-RATE` | `S9(04)V99` | Signed numeric with 2 implied decimals | 16 | 6 | Interest rate |
| 5 | `FILLER` | `X(28)` | Filler | 22 | 28 | Reserved/unused space |

**Total record length:** 50 bytes

## Field Notes

1. **DIS-ACCT-GROUP-ID** -- 10-character alphanumeric account group identifier. Part of the composite key. This groups accounts that share the same interest rate schedule. Trailing spaces must be trimmed during migration.

2. **DIS-TRAN-TYPE-CD** -- 2-character transaction type code (e.g., "SA" for sales, "CR" for credits). Part of the composite key. Foreign key to the Transaction Type reference table (CVTRA03Y).

3. **DIS-TRAN-CAT-CD** -- 4-digit numeric transaction category code. Part of the composite key. Foreign key to the Transaction Category reference table (CVTRA04Y).

4. **DIS-INT-RATE** -- Signed numeric field with 2 implied decimal places. Stores the interest rate applicable to this group/type/category combination. The `S` prefix indicates the value can be negative (e.g., for credit adjustments). The `V` is an implied decimal point. Example: a stored value of `001250` with positive sign represents `12.50%`. Maximum value: `9999.99`.

5. **FILLER** -- 28 bytes of reserved space. Verify contents are consistently spaces or low-values before discarding during migration.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration |
|-------|---------------------|
| `DIS-ACCT-GROUP-ID` | Standard EBCDIC-to-UTF-8 character conversion. Trim trailing EBCDIC spaces (`X'40'`). |
| `DIS-TRAN-TYPE-CD` | Standard EBCDIC-to-UTF-8 character conversion. |
| `DIS-TRAN-CAT-CD` | Zoned decimal (EBCDIC digits `F0`-`F9`). Convert to ASCII digits. |
| `DIS-INT-RATE` | Signed zoned decimal. The sign is encoded in the last byte's zone nibble: `C` = positive, `D` = negative. Extract sign before numeric conversion. |
| `FILLER` | Ignore. Verify contents are EBCDIC spaces (`X'40'`) or low-values (`X'00'`). |

## Referential Integrity

```
DIS-GROUP-RECORD
  |
  +-- DIS-TRAN-TYPE-CD ----> CVTRA03Y.TRAN-TYPE (Transaction Type Record)
  |                          FK: dbo.DiscountGroup.TransactionTypeCd -> dbo.TransactionType.TransactionTypeCd
  |
  +-- DIS-TRAN-CAT-CD -----> CVTRA04Y.TRAN-CAT-CD (Transaction Category Record)
                              FK: dbo.DiscountGroup.TransactionCategoryCd -> dbo.TransactionCategory.TransactionCategoryCd
```

> **Note:** `DIS-ACCT-GROUP-ID` is a grouping key assigned to accounts. The relationship to individual accounts is maintained through the account record's group ID field, not through a direct FK from this table.

## Sample Data

| DIS-ACCT-GROUP-ID | DIS-TRAN-TYPE-CD | DIS-TRAN-CAT-CD | DIS-INT-RATE |
|-------------------|------------------|-----------------|--------------|
| `PREMIUM   ` | `SA` | `5001` | `+0019.99` |
| `STANDARD  ` | `SA` | `5001` | `+0024.99` |
| `PREMIUM   ` | `CR` | `6001` | `+0015.50` |
| `EMPLOYEE  ` | `SA` | `5001` | `+0009.99` |

> **Note:** The `V` (implied decimal) means the stored bytes are `001999` for the rate `19.99%`. The sign is encoded in the zone nibble of the last byte.

## Migration Notes

### Target DDL

```sql
CREATE TABLE dbo.DiscountGroup (
    AccountGroupId        VARCHAR(10)    NOT NULL,
    TransactionTypeCd     CHAR(2)        NOT NULL,
    TransactionCategoryCd INT            NOT NULL,
    InterestRate          DECIMAL(6,2)   NOT NULL,

    CONSTRAINT PK_DiscountGroup
        PRIMARY KEY (AccountGroupId, TransactionTypeCd, TransactionCategoryCd),

    CONSTRAINT FK_DiscGrp_TransType
        FOREIGN KEY (TransactionTypeCd)
        REFERENCES dbo.TransactionType (TransactionTypeCd),

    CONSTRAINT FK_DiscGrp_TransCategory
        FOREIGN KEY (TransactionTypeCd, TransactionCategoryCd)
        REFERENCES dbo.TransactionCategory (TransactionTypeCd, TransactionCategoryCd)
);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `DIS-ACCT-GROUP-ID` | `PIC X(10)` | `VARCHAR(10)` | `string` | Trim trailing spaces |
| `DIS-TRAN-TYPE-CD` | `PIC X(02)` | `CHAR(2)` | `string` | Fixed-length 2-char code |
| `DIS-TRAN-CAT-CD` | `PIC 9(04)` | `INT` | `int` | 4-digit category code |
| `DIS-INT-RATE` | `PIC S9(04)V99` | `DECIMAL(6,2)` | `decimal` | Signed rate with 2 decimal places |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS RowCount FROM dbo.DiscountGroup;

-- Verify no orphaned type codes
SELECT d.* FROM dbo.DiscountGroup d
LEFT JOIN dbo.TransactionType t ON d.TransactionTypeCd = t.TransactionTypeCd
WHERE t.TransactionTypeCd IS NULL;

-- Interest rate range check (sanity)
SELECT * FROM dbo.DiscountGroup
WHERE InterestRate < -99.99 OR InterestRate > 99.99;

-- Distinct group IDs for reconciliation
SELECT DISTINCT AccountGroupId FROM dbo.DiscountGroup ORDER BY AccountGroupId;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** | Interest rate transparency and fair pricing. Discount groups must be documented and auditable. | Maintain audit trail for all interest rate changes. Ensure rate changes are logged with effective dates, approver, and reason. |
| **FSA (FFFS 2014:5) Ch. 8** | Internal controls over financial parameters. Interest rates directly affect financial outcomes. | Implement maker-checker workflow for rate modifications. Rate changes require dual approval in the .NET system. |
| **PSD2** | Transparent pricing for payment services. | Ensure interest rate structures are available for regulatory reporting and customer disclosure. |
| **DORA** | ICT risk management -- configuration data integrity. | Interest rate tables are critical configuration data. Include in backup, recovery testing, and change management processes. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
