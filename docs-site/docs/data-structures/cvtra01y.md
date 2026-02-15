---
id: DS-TBAL-001
title: "Transaction Category Balance Record"
copybook_name: "CVTRA01Y.cpy"
domain: "transactions"
used_by_programs: [CBTRN01C, CBTRN02C, CBTRN03C]
record_length: 50
status: "extracted"
target_schema: "dbo.TransactionCategoryBalance"
sidebar_position: 6
---

# DS-TBAL-001: Transaction Category Balance Record (CVTRA01Y)

## Overview

The `CVTRA01Y.cpy` copybook defines the **Transaction Category Balance Record**, which stores aggregated balance amounts per account, transaction type, and transaction category. This record enables the system to maintain running balances by category, supporting balance inquiries, reporting, and reconciliation without scanning individual transaction records.

The composite key structure (account ID + type code + category code) allows efficient lookup of balances for specific transaction classifications within an account.

**Source file:** `CVTRA01Y.cpy`
**Record length:** 50 bytes
**Used by:** `CBTRN01C`, `CBTRN02C`, `CBTRN03C`
**VSAM file:** Transaction category balance file

## Source COBOL

```cobol
01  TRAN-CAT-BAL-RECORD.
    05  TRAN-CAT-KEY.
       10 TRANCAT-ACCT-ID                       PIC 9(11).
       10 TRANCAT-TYPE-CD                       PIC X(02).
       10 TRANCAT-CD                            PIC 9(04).
    05  TRAN-CAT-BAL                            PIC S9(09)V99.
    05  FILLER                                  PIC X(22).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Offset | Length | Description |
|---|------------|-----------|------|--------|--------|-------------|
| 1 | `TRANCAT-ACCT-ID` | `9(11)` | Numeric (display) | 0 | 11 | Account identifier |
| 2 | `TRANCAT-TYPE-CD` | `X(02)` | Alphanumeric | 11 | 2 | Transaction type code |
| 3 | `TRANCAT-CD` | `9(04)` | Numeric (display) | 13 | 4 | Transaction category code |
| 4 | `TRAN-CAT-BAL` | `S9(09)V99` | Signed numeric with 2 implied decimals | 17 | 11 | Category balance amount |
| 5 | `FILLER` | `X(22)` | Filler | 28 | 22 | Reserved/unused space |

**Total record length:** 50 bytes

## Field Notes

1. **TRANCAT-ACCT-ID** -- 11-digit numeric account identifier. Part of the composite key. Links to the Account master record (CVACT01Y). Leading zeros are significant and must be preserved during migration.

2. **TRANCAT-TYPE-CD** -- 2-character transaction type code (e.g., "SA" for sales, "CR" for credits). Part of the composite key. Foreign key to the Transaction Type reference table (CVTRA03Y).

3. **TRANCAT-CD** -- 4-digit numeric category code. Part of the composite key. Foreign key to the Transaction Category reference table (CVTRA04Y). Combined with TYPE-CD, it uniquely classifies a transaction category.

4. **TRAN-CAT-BAL** -- Signed numeric field with 2 implied decimal places. Stores the aggregated balance for this account/type/category combination. The `S` prefix indicates a signed value (positive or negative). The `V` is an implied decimal point -- no physical decimal separator is stored. Example: a stored value of `00012345678` with sign positive represents `123,456.78`.

5. **FILLER** -- 22 bytes of reserved space. Commonly used in COBOL for future expansion. Do not migrate; verify contents are consistently spaces or low-values before discarding.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration |
|-------|---------------------|
| `TRANCAT-ACCT-ID` | Zoned decimal (EBCDIC digits `F0`-`F9`). Convert to ASCII digits. |
| `TRANCAT-TYPE-CD` | Standard EBCDIC-to-UTF-8 character conversion. |
| `TRANCAT-CD` | Zoned decimal. Convert to ASCII digits. |
| `TRAN-CAT-BAL` | Signed zoned decimal. The sign is encoded in the last byte's zone nibble: `C` = positive, `D` = negative. Extract sign before numeric conversion. |
| `FILLER` | Ignore. Verify contents are EBCDIC spaces (`X'40'`) or low-values (`X'00'`). |

## Referential Integrity

```
TRAN-CAT-BAL-RECORD
  |
  +-- TRANCAT-ACCT-ID ----> CVACT01Y.ACCT-ID (Account Record)
  |                         FK: dbo.TransactionCategoryBalance.AccountId -> dbo.Account.AccountId
  |
  +-- TRANCAT-TYPE-CD ----> CVTRA03Y.TRAN-TYPE (Transaction Type Record)
  |                         FK: dbo.TransactionCategoryBalance.TransactionTypeCd -> dbo.TransactionType.TransactionTypeCd
  |
  +-- TRANCAT-CD ----------> CVTRA04Y.TRAN-CAT-CD (Transaction Category Record)
                             FK: dbo.TransactionCategoryBalance.TransactionCategoryCd -> dbo.TransactionCategory.TransactionCategoryCd
```

## Sample Data

| TRANCAT-ACCT-ID | TRANCAT-TYPE-CD | TRANCAT-CD | TRAN-CAT-BAL |
|-----------------|-----------------|------------|--------------|
| `00000000123` | `SA` | `5001` | `+000123456.78` |
| `00000000123` | `CR` | `6001` | `-000005432.10` |
| `00000000456` | `SA` | `5001` | `+000078900.00` |

> **Note:** The `V` (implied decimal) means the stored bytes are `00012345678` for the value `123,456.78`. The sign is encoded in the zone nibble of the last byte.

## Migration Notes

### Target DDL

```sql
CREATE TABLE dbo.TransactionCategoryBalance (
    AccountId            BIGINT         NOT NULL,
    TransactionTypeCd    CHAR(2)        NOT NULL,
    TransactionCategoryCd INT           NOT NULL,
    CategoryBalance      DECIMAL(11,2)  NOT NULL,

    CONSTRAINT PK_TransactionCategoryBalance
        PRIMARY KEY (AccountId, TransactionTypeCd, TransactionCategoryCd),

    CONSTRAINT FK_TransCatBal_Account
        FOREIGN KEY (AccountId)
        REFERENCES dbo.Account (AccountId),

    CONSTRAINT FK_TransCatBal_TransType
        FOREIGN KEY (TransactionTypeCd)
        REFERENCES dbo.TransactionType (TransactionTypeCd),

    CONSTRAINT FK_TransCatBal_TransCategory
        FOREIGN KEY (TransactionTypeCd, TransactionCategoryCd)
        REFERENCES dbo.TransactionCategory (TransactionTypeCd, TransactionCategoryCd)
);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `TRANCAT-ACCT-ID` | `PIC 9(11)` | `BIGINT` | `long` | 11-digit numeric fits in BIGINT |
| `TRANCAT-TYPE-CD` | `PIC X(02)` | `CHAR(2)` | `string` | Fixed-length 2-char code |
| `TRANCAT-CD` | `PIC 9(04)` | `INT` | `int` | 4-digit category code |
| `TRAN-CAT-BAL` | `PIC S9(09)V99` | `DECIMAL(11,2)` | `decimal` | Signed with 2 decimal places |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS RowCount FROM dbo.TransactionCategoryBalance;

-- Verify no orphaned records
SELECT t.* FROM dbo.TransactionCategoryBalance t
LEFT JOIN dbo.Account a ON t.AccountId = a.AccountId
WHERE a.AccountId IS NULL;

-- Balance checksum for reconciliation
SELECT SUM(CategoryBalance) AS TotalBalance FROM dbo.TransactionCategoryBalance;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** | Internal controls over financial reporting. Aggregated balances must be reconcilable to individual transactions. | Post-migration validation must verify that category balances match the sum of underlying transactions. Reconciliation queries must be run during parallel-run period. |
| **GDPR** | Account-level financial data is personal data when linked to customer identity. | Access controls on balance data. Right-to-erasure requires consideration of retention obligations. |
| **PSD2** | Transaction categorisation supports payment account information services (AISP). | Ensure category balance data is available for PSD2 account information API endpoints with appropriate consent. |
| **DORA** | ICT risk management requires data integrity controls. | Implement checksums and reconciliation during and after migration. Log all balance modifications for audit trail. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
