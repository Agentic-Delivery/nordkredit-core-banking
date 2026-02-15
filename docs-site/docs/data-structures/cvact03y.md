---
id: DS-XREF-001
title: "Card Cross-Reference Record"
copybook_name: "CVACT03Y.cpy"
domain: "account-management"
used_by_programs: [COCRDLIC, COCRDSLC, COCRDUPC, COACTUPC, COACTVWC]
record_length: 50
status: "extracted"
target_schema: "dbo.CustomerAccountCardXref"
sidebar_position: 3
---

# DS-XREF-001: Card Cross-Reference Record (CVACT03Y)

## Overview

The `CVACT03Y.cpy` copybook defines the **Card Cross-Reference Record**, a linking/junction record that associates customers, accounts, and cards in the CardDemo system. This record is stored in the VSAM file `CARDXREF` and serves as the central relationship table enabling lookups between the three core entities: Customer, Account, and Card.

Without this cross-reference, there is no direct way to determine which customer owns which account or which card. It is used by virtually all online programs that need to resolve entity relationships -- card listing, card selection, card updates, account viewing, and account updates.

**Source file:** `CVACT03Y.cpy`
**VSAM file:** `CARDXREF`
**Record length:** 50 bytes
**Used by:** `COCRDLIC`, `COCRDSLC`, `COCRDUPC`, `COACTUPC`, `COACTVWC`

## Source COBOL

```cobol
01 CARD-XREF-RECORD.
    05  XREF-CARD-NUM                     PIC X(16).
    05  XREF-CUST-ID                      PIC 9(09).
    05  XREF-ACCT-ID                      PIC 9(11).
    05  FILLER                            PIC X(14).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Nullable | Target Column |
|---|------------|-----------|--------|--------|------|-------------|----------|---------------|
| 1 | `XREF-CARD-NUM` | `X(16)` | 0 | 16 | Alphanumeric | Card number (PAN) -- links to CVACT02Y | No | `CardNumberHash` (VARBINARY(64)) |
| 2 | `XREF-CUST-ID` | `9(09)` | 16 | 9 | Numeric (unsigned) | Customer identifier -- links to CVCUS01Y | No | `CustomerId` (BIGINT, FK) |
| 3 | `XREF-ACCT-ID` | `9(11)` | 25 | 11 | Numeric (unsigned) | Account identifier -- links to CVACT01Y | No | `AccountId` (BIGINT, FK) |
| 4 | `FILLER` | `X(14)` | 36 | 14 | Filler | Reserved/unused space | N/A | Not migrated |

**Total record length:** 50 bytes

## Field Notes

1. **XREF-CARD-NUM** (`PIC X(16)`): The full 16-digit Primary Account Number (PAN). This field links the cross-reference to a card record in `CVACT02Y`. As with all PAN storage, this is subject to PCI-DSS requirements. In the target schema, this must be stored as a hash (matching the `CardNumberHash` in `dbo.Card`) rather than in clear text. The VSAM key for `CARDXREF` is `XREF-CARD-NUM`, making card number the primary lookup path.

2. **XREF-CUST-ID** (`PIC 9(09)`): 9-digit numeric customer identifier linking to the customer record in `CVCUS01Y`. A single customer can have multiple cross-reference entries (multiple cards/accounts). This field enables the "find all cards for a customer" and "find all accounts for a customer" queries.

3. **XREF-ACCT-ID** (`PIC 9(11)`): 11-digit numeric account identifier linking to the account record in `CVACT01Y`. Combined with `XREF-CUST-ID`, this establishes the customer-account ownership relationship.

4. **FILLER** (`PIC X(14)`): Reserved space to pad the record to 50 bytes. Not migrated.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration | Migration Action |
|-------|---------------------|-----------------|
| `XREF-CARD-NUM` | EBCDIC digits `F0`-`F9` in `X(16)` field | Convert to ASCII; hash with SHA-256 to match `dbo.Card.CardNumberHash`; validate Luhn checksum before hashing |
| `XREF-CUST-ID` | EBCDIC zoned decimal `F0`-`F9` | Convert to ASCII digits; cast to BIGINT |
| `XREF-ACCT-ID` | EBCDIC zoned decimal `F0`-`F9` | Convert to ASCII digits; cast to BIGINT |
| `FILLER` | May contain any byte values | Discard entirely |

## Referential Integrity

```
CVCUS01Y (CUSTOMER-RECORD)         CVACT01Y (ACCOUNT-RECORD)         CVACT02Y (CARD-RECORD)
    CUST-ID (PK)                       ACCT-ID (PK)                      CARD-NUM (PK)
        \                                  |                                /
         \                                 |                               /
          +-----> CVACT03Y.XREF-CUST-ID   |   CVACT03Y.XREF-CARD-NUM <--+
                  CVACT03Y.XREF-ACCT-ID <-+
```

| Relationship | Source Field | Target Entity | Target Field | Cardinality | Constraint |
|-------------|-------------|---------------|-------------|-------------|------------|
| Xref -> Customer | `XREF-CUST-ID` | `CVCUS01Y` (Customer Record) | `CUST-ID` | N:1 | Every xref must reference a valid customer |
| Xref -> Account | `XREF-ACCT-ID` | `CVACT01Y` (Account Record) | `ACCT-ID` | N:1 | Every xref must reference a valid account |
| Xref -> Card | `XREF-CARD-NUM` | `CVACT02Y` (Card Record) | `CARD-NUM` | N:1 | Every xref must reference a valid card |

**Note:** This is a three-way junction table. In the VSAM system, referential integrity is enforced entirely by COBOL program logic. The target Azure SQL schema must enforce all three foreign key relationships with constraints. The natural key is `XREF-CARD-NUM` (each card appears in exactly one cross-reference entry).

## Sample Data

| Field | Example Value (Display) | Raw COBOL Value | Notes |
|-------|------------------------|----------------|-------|
| `XREF-CARD-NUM` | `4000123456789012` | `4000123456789012` | 16-digit PAN |
| `XREF-CUST-ID` | `000000001` | `000000001` | 9-digit zero-padded customer ID |
| `XREF-ACCT-ID` | `00000000012` | `00000000012` | 11-digit zero-padded account ID |

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.CustomerAccountCardXref (
    XrefId              BIGINT IDENTITY(1,1) NOT NULL,
    CardNumberHash      VARBINARY(64)   NOT NULL,
    CustomerId          BIGINT          NOT NULL,
    AccountId           BIGINT          NOT NULL,

    -- Audit columns (not in COBOL source)
    CreatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    UpdatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    CreatedBy           NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,
    UpdatedBy           NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,

    CONSTRAINT PK_CustomerAccountCardXref PRIMARY KEY CLUSTERED (XrefId),
    CONSTRAINT FK_Xref_Customer FOREIGN KEY (CustomerId) REFERENCES dbo.Customer (CustomerId),
    CONSTRAINT FK_Xref_Account FOREIGN KEY (AccountId) REFERENCES dbo.Account (AccountId),
    CONSTRAINT FK_Xref_Card FOREIGN KEY (CardNumberHash) REFERENCES dbo.Card (CardNumberHash),
    CONSTRAINT UQ_Xref_CardHash UNIQUE (CardNumberHash)
);

-- Index for customer-based lookups (find all cards/accounts for a customer)
CREATE NONCLUSTERED INDEX IX_Xref_CustomerId ON dbo.CustomerAccountCardXref (CustomerId);

-- Index for account-based lookups (find all cards/customers for an account)
CREATE NONCLUSTERED INDEX IX_Xref_AccountId ON dbo.CustomerAccountCardXref (AccountId);
```

### Post-Migration Validation Queries

```sql
-- Row count comparison
SELECT COUNT(*) AS XrefCount FROM dbo.CustomerAccountCardXref;

-- Verify no orphaned customer references
SELECT x.XrefId, x.CustomerId
FROM dbo.CustomerAccountCardXref x
LEFT JOIN dbo.Customer c ON x.CustomerId = c.CustomerId
WHERE c.CustomerId IS NULL;

-- Verify no orphaned account references
SELECT x.XrefId, x.AccountId
FROM dbo.CustomerAccountCardXref x
LEFT JOIN dbo.Account a ON x.AccountId = a.AccountId
WHERE a.AccountId IS NULL;

-- Verify no orphaned card references
SELECT x.XrefId, x.CardNumberHash
FROM dbo.CustomerAccountCardXref x
LEFT JOIN dbo.Card cd ON x.CardNumberHash = cd.CardNumberHash
WHERE cd.CardNumberHash IS NULL;

-- Verify uniqueness of card references (one xref per card)
SELECT CardNumberHash, COUNT(*) AS Cnt
FROM dbo.CustomerAccountCardXref
GROUP BY CardNumberHash
HAVING COUNT(*) > 1;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS 3.4** | Render PAN unreadable anywhere it is stored | `XREF-CARD-NUM` must be stored as a SHA-256 hash (`CardNumberHash`) matching the hash in `dbo.Card`. No clear-text PAN in the cross-reference table. |
| **PCI-DSS 1.2** | Restrict connections between untrusted networks and cardholder data | The cross-reference table links to cardholder data indirectly. Access must be restricted to the same PCI-DSS network segment as `dbo.Card`. |
| **GDPR** Art. 17 | Right to erasure | When a customer exercises right to erasure, all cross-reference entries for that customer must be identified and processed. The `CustomerId` index enables efficient lookup. Note: financial record retention obligations may require pseudonymization rather than deletion. |
| **GDPR** Art. 15 | Right of access | The cross-reference enables data subject access requests -- given a `CustomerId`, all associated accounts and cards can be identified. This lookup must be supported by the .NET API. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- relationship integrity | The three-way foreign key constraints ensure that no orphaned relationships exist. Any insert/update to this table must validate all three references. Changes must be audit-logged. |
| **AML/KYC** (FFFS 2017:11) | Customer due diligence -- identify all accounts per customer | The cross-reference is the primary mechanism for identifying all accounts and cards held by a customer. AML screening processes must use this table to build a complete customer profile. |
| **DORA** Art. 11 | ICT data integrity | Cross-reference integrity is critical to the entire system. Migration must include a three-way join validation to ensure every xref record resolves to valid customer, account, and card records in the target system. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
