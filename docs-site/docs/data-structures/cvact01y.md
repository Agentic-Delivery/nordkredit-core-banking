---
id: DS-ACCT-001
title: "Account Record"
copybook_name: "CVACT01Y.cpy"
domain: "account-management"
used_by_programs: [COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, CBACT01C, CBACT02C, CBACT03C]
record_length: 300
status: "extracted"
target_schema: "dbo.Account"
sidebar_position: 1
---

# DS-ACCT-001: Account Record (CVACT01Y)

## Overview

The `CVACT01Y.cpy` copybook defines the **Account Record**, the primary data structure for customer accounts in the CardDemo system. This record is stored in the VSAM file `ACCTDATA` and contains account identification, status, financial balances, credit limits, key dates, and grouping information.

This is one of the core entity records in the system, referenced by nearly all online and batch programs that deal with account operations -- including account viewing, updating, credit limit management, and batch processing.

**Source file:** `CVACT01Y.cpy`
**VSAM file:** `ACCTDATA`
**Record length:** 300 bytes
**Used by:** `COACTUPC`, `COACTVWC`, `COCRDLIC`, `COCRDSLC`, `COCRDUPC`, `CBACT01C`, `CBACT02C`, `CBACT03C`

## Source COBOL

```cobol
01  ACCOUNT-RECORD.
    05  ACCT-ID                           PIC 9(11).
    05  ACCT-ACTIVE-STATUS                PIC X(01).
    05  ACCT-CURR-BAL                     PIC S9(10)V99.
    05  ACCT-CREDIT-LIMIT                 PIC S9(10)V99.
    05  ACCT-CASH-CREDIT-LIMIT            PIC S9(10)V99.
    05  ACCT-OPEN-DATE                    PIC X(10).
    05  ACCT-EXPIRAION-DATE               PIC X(10).
    05  ACCT-REISSUE-DATE                 PIC X(10).
    05  ACCT-CURR-CYC-CREDIT              PIC S9(10)V99.
    05  ACCT-CURR-CYC-DEBIT               PIC S9(10)V99.
    05  ACCT-ADDR-ZIP                     PIC X(10).
    05  ACCT-GROUP-ID                     PIC X(10).
    05  FILLER                            PIC X(178).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Nullable | Target Column |
|---|------------|-----------|--------|--------|------|-------------|----------|---------------|
| 1 | `ACCT-ID` | `9(11)` | 0 | 11 | Numeric (unsigned) | Unique account identifier | No | `AccountId` (BIGINT, PK) |
| 2 | `ACCT-ACTIVE-STATUS` | `X(01)` | 11 | 1 | Alphanumeric | Account active status flag (`Y`/`N`) | No | `IsActive` (BIT) |
| 3 | `ACCT-CURR-BAL` | `S9(10)V99` | 12 | 12 | Signed packed/display numeric | Current account balance with 2 implied decimal places | No | `CurrentBalance` (DECIMAL(12,2)) |
| 4 | `ACCT-CREDIT-LIMIT` | `S9(10)V99` | 24 | 12 | Signed packed/display numeric | Maximum credit limit | No | `CreditLimit` (DECIMAL(12,2)) |
| 5 | `ACCT-CASH-CREDIT-LIMIT` | `S9(10)V99` | 36 | 12 | Signed packed/display numeric | Cash advance credit limit | No | `CashCreditLimit` (DECIMAL(12,2)) |
| 6 | `ACCT-OPEN-DATE` | `X(10)` | 48 | 10 | Alphanumeric (date string) | Account opening date (`YYYY-MM-DD`) | No | `OpenDate` (DATE) |
| 7 | `ACCT-EXPIRAION-DATE` | `X(10)` | 58 | 10 | Alphanumeric (date string) | Account expiration date (`YYYY-MM-DD`) | Yes | `ExpirationDate` (DATE) |
| 8 | `ACCT-REISSUE-DATE` | `X(10)` | 68 | 10 | Alphanumeric (date string) | Account reissue date (`YYYY-MM-DD`) | Yes | `ReissueDate` (DATE NULL) |
| 9 | `ACCT-CURR-CYC-CREDIT` | `S9(10)V99` | 78 | 12 | Signed packed/display numeric | Current billing cycle total credits | No | `CurrentCycleCredit` (DECIMAL(12,2)) |
| 10 | `ACCT-CURR-CYC-DEBIT` | `S9(10)V99` | 90 | 12 | Signed packed/display numeric | Current billing cycle total debits | No | `CurrentCycleDebit` (DECIMAL(12,2)) |
| 11 | `ACCT-ADDR-ZIP` | `X(10)` | 102 | 10 | Alphanumeric | Account holder ZIP/postal code | Yes | `AddressZip` (NVARCHAR(10)) |
| 12 | `ACCT-GROUP-ID` | `X(10)` | 112 | 10 | Alphanumeric | Discount/account group identifier | Yes | `GroupId` (NVARCHAR(10)) |
| 13 | `FILLER` | `X(178)` | 122 | 178 | Filler | Reserved/unused space | N/A | Not migrated |

**Total record length:** 300 bytes

## Field Notes

1. **ACCT-ID** (`PIC 9(11)`): Numeric-only account identifier, zero-padded. This is the primary key for the account entity and is referenced as a foreign key from `CVACT02Y` (Card Record) and `CVACT03Y` (Cross-Reference Record). Stored as `BIGINT` in the target schema to support numeric indexing.

2. **ACCT-ACTIVE-STATUS** (`PIC X(01)`): Single-character flag indicating whether the account is active. Values: `Y` (active) or `N` (inactive/closed). Mapped to a `BIT` column in SQL (`Y` = 1, `N` = 0). Business rules in `COACTUPC` validate this field against allowed transitions.

3. **ACCT-CURR-BAL** (`PIC S9(10)V99`): Signed numeric with 2 implied decimal places. The `S` prefix indicates the field can hold negative values (e.g., credit balances). The `V` is an implied decimal point -- there is no physical decimal character in the COBOL data. The value `000012345678` represents `123456.78`. Maximum representable value: +/- 9,999,999,999.99.

4. **ACCT-CREDIT-LIMIT** (`PIC S9(10)V99`): Same format as `ACCT-CURR-BAL`. Represents the maximum credit extended to the account. Used by credit limit validation programs (`COCRDLIC`, `COCRDUPC`).

5. **ACCT-CASH-CREDIT-LIMIT** (`PIC S9(10)V99`): Cash advance limit, typically a subset of the overall credit limit. Same signed decimal format.

6. **ACCT-OPEN-DATE** (`PIC X(10)`): Date stored as a string in `YYYY-MM-DD` format. Must be validated during migration to ensure all values are valid dates. Spaces or low-values indicate no date set.

7. **ACCT-EXPIRAION-DATE** (`PIC X(10)`): Note the **typo** in the original COBOL source -- "EXPIRAION" instead of "EXPIRATION". This typo must be preserved in COBOL references for traceability but corrected in the .NET target column name (`ExpirationDate`). Same date string format as `ACCT-OPEN-DATE`.

8. **ACCT-REISSUE-DATE** (`PIC X(10)`): Date when the account was last reissued/renewed. May contain spaces if the account has never been reissued.

9. **ACCT-CURR-CYC-CREDIT / ACCT-CURR-CYC-DEBIT** (`PIC S9(10)V99`): Running totals for the current billing cycle. These are reset at cycle close by batch job `CBACT03C`. Critical for billing accuracy -- must be validated during parallel-run comparison testing.

10. **ACCT-ADDR-ZIP** (`PIC X(10)`): ZIP/postal code associated with the account. Supports both US 5-digit and extended 9-digit ZIP codes, as well as Swedish postal codes (5 digits with space, e.g., `123 45`).

11. **ACCT-GROUP-ID** (`PIC X(10)`): Links the account to a discount or rate group. Referenced by transaction processing programs.

12. **FILLER** (`PIC X(178)`): Reserved space to pad the record to 300 bytes. Not migrated to the target schema. May contain residual data from prior record layouts -- must be ignored during extraction.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration | Migration Action |
|-------|---------------------|-----------------|
| `ACCT-ID` | EBCDIC zones `F0`-`F9` for digits | Convert to ASCII digits; validate numeric content |
| `ACCT-ACTIVE-STATUS` | EBCDIC `C8` = `Y`, `D5` = `N` | Convert to ASCII; map `Y`/`N` to BIT 1/0 |
| `ACCT-CURR-BAL` (and all `S9(10)V99` fields) | Sign stored in last byte zone nibble (EBCDIC signed overpunch): positive `C0-C9`, negative `D0-D9` | Extract sign from zone nibble; convert to .NET `decimal`; insert implied decimal point |
| `ACCT-OPEN-DATE` (and all date fields) | EBCDIC alphanumeric, hyphen is `60` | Convert EBCDIC to UTF-8; parse as `YYYY-MM-DD`; validate date range |
| `ACCT-ADDR-ZIP` | May contain spaces (`40` in EBCDIC) and alphanumeric characters | Convert EBCDIC to UTF-8; trim trailing spaces |
| `ACCT-GROUP-ID` | Alphanumeric, may be all spaces | Convert EBCDIC to UTF-8; map all-spaces to NULL |
| `FILLER` | May contain any byte values | Discard entirely; do not migrate |

## Referential Integrity

```
CVACT01Y (ACCOUNT-RECORD)
    ACCT-ID (PK)
        |
        +--< CVACT02Y.CARD-ACCT-ID (FK) -- Card records linked to this account
        |
        +--< CVACT03Y.XREF-ACCT-ID (FK) -- Cross-reference records linking card/customer/account
```

| Relationship | Source Field | Target Entity | Target Field | Cardinality | Constraint |
|-------------|-------------|---------------|-------------|-------------|------------|
| Account -> Cards | `ACCT-ID` | `CVACT02Y` (Card Record) | `CARD-ACCT-ID` | 1:N | An account can have multiple cards |
| Account -> Cross-Ref | `ACCT-ID` | `CVACT03Y` (Cross-Reference) | `XREF-ACCT-ID` | 1:N | An account can appear in multiple cross-reference entries |

**Note:** In the VSAM file system, referential integrity is enforced by application logic (COBOL programs), not by the data store. The target Azure SQL schema must enforce these relationships with foreign key constraints.

## Sample Data

| Field | Example Value (Display) | Raw COBOL Value | Notes |
|-------|------------------------|----------------|-------|
| `ACCT-ID` | `00000000012` | `00000000012` | 11-digit zero-padded |
| `ACCT-ACTIVE-STATUS` | `Y` | `Y` | Active account |
| `ACCT-CURR-BAL` | `5,432.10` | `000054321{` | `{` = positive zero in zone; implied V99 |
| `ACCT-CREDIT-LIMIT` | `10,000.00` | `000100000{` | Signed, implied decimal |
| `ACCT-CASH-CREDIT-LIMIT` | `2,000.00` | `000020000{` | Subset of credit limit |
| `ACCT-OPEN-DATE` | `2019-03-15` | `2019-03-15` | ISO date string |
| `ACCT-EXPIRAION-DATE` | `2027-03-15` | `2027-03-15` | ISO date string |
| `ACCT-REISSUE-DATE` | `2024-03-15` | `2024-03-15` | Last reissue |
| `ACCT-CURR-CYC-CREDIT` | `1,250.00` | `000012500{` | Cycle credits |
| `ACCT-CURR-CYC-DEBIT` | `800.50` | `000008005{` | Cycle debits |
| `ACCT-ADDR-ZIP` | `114 55` | `114 55    ` | Swedish postal code, right-padded spaces |
| `ACCT-GROUP-ID` | `GRP001` | `GRP001    ` | Right-padded spaces |

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.Account (
    AccountId           BIGINT          NOT NULL,
    IsActive            BIT             NOT NULL DEFAULT 1,
    CurrentBalance      DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    CreditLimit         DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    CashCreditLimit     DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    OpenDate            DATE            NOT NULL,
    ExpirationDate      DATE            NULL,
    ReissueDate         DATE            NULL,
    CurrentCycleCredit  DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    CurrentCycleDebit   DECIMAL(12,2)   NOT NULL DEFAULT 0.00,
    AddressZip          NVARCHAR(10)    NULL,
    GroupId             NVARCHAR(10)    NULL,

    -- Audit columns (not in COBOL source)
    CreatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    UpdatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    CreatedBy           NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,
    UpdatedBy           NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,

    CONSTRAINT PK_Account PRIMARY KEY CLUSTERED (AccountId),
    CONSTRAINT CK_Account_CreditLimit CHECK (CreditLimit >= 0),
    CONSTRAINT CK_Account_CashCreditLimit CHECK (CashCreditLimit >= 0),
    CONSTRAINT CK_Account_CashLimitWithinCredit CHECK (CashCreditLimit <= CreditLimit)
);

-- Index for ZIP code lookups (regulatory reporting)
CREATE NONCLUSTERED INDEX IX_Account_AddressZip ON dbo.Account (AddressZip);

-- Index for group-based queries
CREATE NONCLUSTERED INDEX IX_Account_GroupId ON dbo.Account (GroupId);
```

### Post-Migration Validation Queries

```sql
-- Row count comparison
SELECT COUNT(*) AS AccountCount FROM dbo.Account;

-- Verify no NULL primary keys
SELECT COUNT(*) AS NullPKCount FROM dbo.Account WHERE AccountId IS NULL;

-- Financial field checksum comparison
SELECT
    SUM(CAST(CurrentBalance AS FLOAT)) AS TotalBalance,
    SUM(CAST(CreditLimit AS FLOAT)) AS TotalCreditLimit,
    SUM(CAST(CurrentCycleCredit AS FLOAT)) AS TotalCycleCredit,
    SUM(CAST(CurrentCycleDebit AS FLOAT)) AS TotalCycleDebit
FROM dbo.Account;

-- Verify date field validity
SELECT COUNT(*) AS InvalidDateCount
FROM dbo.Account
WHERE OpenDate > GETDATE()
   OR ExpirationDate < OpenDate;

-- Verify active status mapping
SELECT IsActive, COUNT(*) AS Cnt
FROM dbo.Account
GROUP BY IsActive;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls for account data integrity | All account modifications must be audit-logged with before/after values. The `CreatedAt`/`UpdatedAt`/`CreatedBy`/`UpdatedBy` columns provide basic audit trail; a separate `AccountAuditLog` table is recommended for field-level change history. |
| **FSA (FFFS 2014:5)** Ch. 4 | Risk management -- credit limit controls | `CreditLimit` and `CashCreditLimit` must be validated against institution-level policies. The CHECK constraints enforce non-negative limits and that cash limit does not exceed overall credit limit. |
| **PSD2** Art. 97 | Strong Customer Authentication (SCA) for accessing account information | Access to account data via the .NET API must be gated by SCA. The COBOL system relied on CICS terminal-level security (RACF); the .NET replacement must implement equivalent or stronger authentication via Azure AD B2C. |
| **GDPR** Art. 5(1)(e) | Storage limitation -- data kept no longer than necessary | Account records for closed accounts (`IsActive = 0`) must have a defined retention period. After retention expiry, non-essential fields should be anonymized while preserving financial records required by Swedish Bookkeeping Act (7 years). |
| **GDPR** Art. 25 | Data protection by design | `AddressZip` is indirect PII (location data). Consider whether ZIP code storage is necessary in the account record or should be retrieved from the customer record only when needed. |
| **AML/KYC** (FFFS 2017:11) | Ongoing monitoring of customer accounts | Account status changes, unusual balance patterns, and credit limit modifications must be flagged for AML review. Batch programs `CBACT01C`/`CBACT02C`/`CBACT03C` must integrate with the AML monitoring pipeline in the target system. |
| **DORA** Art. 11 | ICT data integrity in financial entities | Financial fields (`CurrentBalance`, `CreditLimit`, cycle totals) must be validated during and after migration with zero tolerance for rounding errors. Parallel-run comparison testing must confirm byte-exact equivalence of financial calculations. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
