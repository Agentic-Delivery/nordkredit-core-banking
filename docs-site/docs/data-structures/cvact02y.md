---
id: DS-CARD-001
title: "Card Record"
copybook_name: "CVACT02Y.cpy"
domain: "account-management"
used_by_programs: [COCRDLIC, COCRDSLC, COCRDUPC, CBTRN02C]
record_length: 150
status: "extracted"
target_schema: "dbo.Card"
sidebar_position: 2
---

# DS-CARD-001: Card Record (CVACT02Y)

## Overview

The `CVACT02Y.cpy` copybook defines the **Card Record**, the primary data structure for credit/debit card information in the CardDemo system. This record is stored in the VSAM file `CARDDATA` and contains the card number (PAN), associated account ID, CVV code, cardholder name, expiration date, and active status.

This record is central to all card management operations -- listing, viewing, updating, and transaction processing. It is subject to strict PCI-DSS requirements due to the presence of the Primary Account Number (PAN) and Card Verification Value (CVV).

**Source file:** `CVACT02Y.cpy`
**VSAM file:** `CARDDATA`
**Record length:** 150 bytes
**Used by:** `COCRDLIC`, `COCRDSLC`, `COCRDUPC`, `CBTRN02C`

## Source COBOL

```cobol
01  CARD-RECORD.
    05  CARD-NUM                          PIC X(16).
    05  CARD-ACCT-ID                      PIC 9(11).
    05  CARD-CVV-CD                       PIC 9(03).
    05  CARD-EMBOSSED-NAME                PIC X(50).
    05  CARD-EXPIRAION-DATE               PIC X(10).
    05  CARD-ACTIVE-STATUS                PIC X(01).
    05  FILLER                            PIC X(59).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Nullable | Target Column |
|---|------------|-----------|--------|--------|------|-------------|----------|---------------|
| 1 | `CARD-NUM` | `X(16)` | 0 | 16 | Alphanumeric | Primary Account Number (PAN) -- 16-digit card number | No | `CardNumberHash` (VARBINARY(64)) / `CardNumberLast4` (CHAR(4)) |
| 2 | `CARD-ACCT-ID` | `9(11)` | 16 | 11 | Numeric (unsigned) | Associated account identifier (FK to CVACT01Y) | No | `AccountId` (BIGINT, FK) |
| 3 | `CARD-CVV-CD` | `9(03)` | 27 | 3 | Numeric (unsigned) | Card Verification Value (CVV/CVC) | No | **Not stored** (PCI-DSS) |
| 4 | `CARD-EMBOSSED-NAME` | `X(50)` | 30 | 50 | Alphanumeric | Name embossed on physical card | No | `EmbossedName` (NVARCHAR(50)) |
| 5 | `CARD-EXPIRAION-DATE` | `X(10)` | 80 | 10 | Alphanumeric (date string) | Card expiration date (`YYYY-MM-DD`) | No | `ExpirationDate` (DATE) |
| 6 | `CARD-ACTIVE-STATUS` | `X(01)` | 90 | 1 | Alphanumeric | Card active status flag | No | `Status` (NVARCHAR(20)) |
| 7 | `FILLER` | `X(59)` | 91 | 59 | Filler | Reserved/unused space | N/A | Not migrated |

**Total record length:** 150 bytes

## Field Notes

1. **CARD-NUM** (`PIC X(16)`): The full 16-digit Primary Account Number (PAN). Defined as `X(16)` (alphanumeric) rather than `9(16)` to accommodate potential non-numeric characters, though in practice it contains only digits. **PCI-DSS Requirement 3.4**: The PAN must not be stored in clear text in the target system. The migration must tokenize or hash the full PAN; only the last 4 digits may be stored in the clear for display purposes.

2. **CARD-ACCT-ID** (`PIC 9(11)`): Foreign key linking this card to an account in `CVACT01Y`. A single account may have multiple cards (1:N relationship). This field must match an existing `ACCT-ID` in the account file.

3. **CARD-CVV-CD** (`PIC 9(03)`): The 3-digit Card Verification Value. **PCI-DSS Requirement 3.2**: Sensitive authentication data (including CVV) must **not** be stored after authorization, even if encrypted. This field exists in the COBOL source for historical reasons (pre-PCI compliance). During migration, this field must be **discarded entirely** -- it must not be migrated to the target database under any circumstances.

4. **CARD-EMBOSSED-NAME** (`PIC X(50)`): The cardholder name as embossed on the physical card. Typically formatted as `FIRSTNAME LASTNAME` or `LASTNAME/FIRSTNAME`. This is PII under GDPR and must be handled accordingly.

5. **CARD-EXPIRAION-DATE** (`PIC X(10)`): Note the **typo** in the original COBOL source -- "EXPIRAION" instead of "EXPIRATION". This typo must be preserved in COBOL references for traceability but corrected in the .NET target column name (`ExpirationDate`). Date stored as `YYYY-MM-DD` string.

6. **CARD-ACTIVE-STATUS** (`PIC X(01)`): Single-character status code. Known values: `Y` (active), `N` (inactive/cancelled). In the target system, this is expanded to a descriptive `NVARCHAR(20)` status enum (`Active`, `Inactive`, `Suspended`, `Cancelled`) to support richer state management.

7. **FILLER** (`PIC X(59)`): Reserved space. Not migrated. May contain residual data.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration | Migration Action |
|-------|---------------------|-----------------|
| `CARD-NUM` | EBCDIC digits `F0`-`F9` in `X(16)` field | Convert to ASCII; validate Luhn checksum after conversion |
| `CARD-ACCT-ID` | EBCDIC zoned decimal `F0`-`F9` | Convert to ASCII digits; validate numeric content |
| `CARD-CVV-CD` | EBCDIC zoned decimal | **Do not migrate** -- discard per PCI-DSS 3.2 |
| `CARD-EMBOSSED-NAME` | EBCDIC alphanumeric, may include special characters (`/`, `-`, `.`) | Convert EBCDIC to UTF-8; preserve special characters; trim trailing spaces |
| `CARD-EXPIRAION-DATE` | EBCDIC alphanumeric date string | Convert to UTF-8; parse as `YYYY-MM-DD`; validate date range |
| `CARD-ACTIVE-STATUS` | EBCDIC single character | Convert to ASCII; map to target status enum |
| `FILLER` | May contain any byte values | Discard entirely |

## Referential Integrity

```
CVACT01Y (ACCOUNT-RECORD)
    ACCT-ID (PK)
        |
        +--< CVACT02Y.CARD-ACCT-ID (FK)  -- This record
                |
                +--< CVACT03Y.XREF-CARD-NUM (FK) -- Cross-reference via card number
```

| Relationship | Source Field | Target Entity | Target Field | Cardinality | Constraint |
|-------------|-------------|---------------|-------------|-------------|------------|
| Card -> Account | `CARD-ACCT-ID` | `CVACT01Y` (Account Record) | `ACCT-ID` | N:1 | Every card must belong to an account |
| Card -> Cross-Ref | `CARD-NUM` | `CVACT03Y` (Cross-Reference) | `XREF-CARD-NUM` | 1:N | A card can appear in multiple cross-reference entries |

**Note:** The VSAM primary key for `CARDDATA` is the composite of `CARD-NUM` + `CARD-ACCT-ID`. In the target SQL schema, a surrogate key (`CardId`) is introduced with a unique constraint on the natural key.

## Sample Data

| Field | Example Value (Display) | Raw COBOL Value | Notes |
|-------|------------------------|----------------|-------|
| `CARD-NUM` | `4000123456789012` | `4000123456789012` | 16-digit PAN |
| `CARD-ACCT-ID` | `00000000012` | `00000000012` | 11-digit zero-padded |
| `CARD-CVV-CD` | `123` | `123` | 3-digit CVV (**not migrated**) |
| `CARD-EMBOSSED-NAME` | `ERIK LINDQVIST` | `ERIK LINDQVIST                                    ` | Right-padded to 50 chars |
| `CARD-EXPIRAION-DATE` | `2027-06-30` | `2027-06-30` | ISO date string |
| `CARD-ACTIVE-STATUS` | `Y` | `Y` | Active card |

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.Card (
    CardId              BIGINT IDENTITY(1,1) NOT NULL,
    CardNumberHash      VARBINARY(64)   NOT NULL,
    CardNumberLast4     CHAR(4)         NOT NULL,
    AccountId           BIGINT          NOT NULL,
    -- CARD-CVV-CD: NOT MIGRATED (PCI-DSS 3.2 prohibits storage post-authorization)
    EmbossedName        NVARCHAR(50)    NOT NULL,
    ExpirationDate      DATE            NOT NULL,
    Status              NVARCHAR(20)    NOT NULL DEFAULT 'Active',

    -- Audit columns (not in COBOL source)
    CreatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    UpdatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    CreatedBy           NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,
    UpdatedBy           NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,

    CONSTRAINT PK_Card PRIMARY KEY CLUSTERED (CardId),
    CONSTRAINT FK_Card_Account FOREIGN KEY (AccountId) REFERENCES dbo.Account (AccountId),
    CONSTRAINT UQ_Card_Hash UNIQUE (CardNumberHash),
    CONSTRAINT CK_Card_Last4 CHECK (LEN(CardNumberLast4) = 4),
    CONSTRAINT CK_Card_Status CHECK (Status IN ('Active', 'Inactive', 'Suspended', 'Cancelled'))
);

-- Index for account-based card lookups
CREATE NONCLUSTERED INDEX IX_Card_AccountId ON dbo.Card (AccountId);

-- Index for status-based queries (e.g., find all expired cards)
CREATE NONCLUSTERED INDEX IX_Card_Status_Expiration ON dbo.Card (Status, ExpirationDate);
```

### PAN Tokenization Strategy

The full PAN from `CARD-NUM` must be processed during migration as follows:

1. **Hash**: Compute SHA-256 hash of the PAN and store in `CardNumberHash` for lookup purposes.
2. **Last 4**: Extract last 4 digits and store in `CardNumberLast4` for display.
3. **Token**: If a tokenization service is available (e.g., Azure Key Vault or payment processor tokenization), replace PAN with a token for any runtime card-present operations.
4. **Discard**: The original PAN must not be stored in clear text anywhere in the target system.

### Post-Migration Validation Queries

```sql
-- Row count comparison
SELECT COUNT(*) AS CardCount FROM dbo.Card;

-- Verify no orphaned cards (all accounts exist)
SELECT c.CardId, c.AccountId
FROM dbo.Card c
LEFT JOIN dbo.Account a ON c.AccountId = a.AccountId
WHERE a.AccountId IS NULL;

-- Verify CVV was NOT migrated (column should not exist)
SELECT COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'Card' AND COLUMN_NAME LIKE '%CVV%';

-- Verify PAN is hashed (no clear text PANs)
SELECT COUNT(*) AS ClearPANCount
FROM dbo.Card
WHERE CardNumberHash IS NULL OR LEN(CardNumberLast4) <> 4;

-- Status distribution
SELECT Status, COUNT(*) AS Cnt FROM dbo.Card GROUP BY Status;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS 3.4** | Render PAN unreadable anywhere it is stored | Full PAN must be tokenized or hashed (`CardNumberHash`). Only last 4 digits stored in clear (`CardNumberLast4`). No clear-text PAN in logs, backups, or temporary tables. |
| **PCI-DSS 3.2** | Do not store sensitive authentication data after authorization (CVV, PIN) | `CARD-CVV-CD` must **not** be migrated. The target DDL intentionally omits any CVV column. Migration ETL must discard this field. |
| **PCI-DSS 7.1** | Restrict access to cardholder data by business need-to-know | Access to `dbo.Card` must be restricted via SQL Server roles. Application-level access to even the hashed PAN and last-4 must be logged. |
| **PCI-DSS 10.2** | Audit trails for all access to cardholder data | All SELECT/INSERT/UPDATE/DELETE operations on `dbo.Card` must be captured in an audit log (SQL Server Audit or Application Insights). |
| **GDPR** Art. 5(1)(c) | Data minimization | `EmbossedName` is PII. Evaluate whether it is needed in the card table or can be derived from the customer record at runtime. |
| **GDPR** Art. 17 | Right to erasure | When a data subject requests erasure, `EmbossedName` must be anonymized. `CardNumberHash` may be retained for financial record-keeping obligations (Swedish Bookkeeping Act) but the link to the individual must be severed. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls | Card status changes must be audit-logged. Expiration date management must be part of automated controls. |
| **DORA** Art. 11 | ICT data integrity | Card record migration must be validated with zero-tolerance comparison: every VSAM record must produce exactly one SQL row (minus the discarded CVV). |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
