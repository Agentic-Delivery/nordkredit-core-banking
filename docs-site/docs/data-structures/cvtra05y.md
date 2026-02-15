---
id: DS-TRAN-001
title: "Transaction Record"
copybook_name: "CVTRA05Y.cpy"
domain: "transactions"
used_by_programs: [COTRN00C, COTRN01C, COTRN02C, CBTRN01C, CBTRN02C, CBTRN03C, CORPT00C]
record_length: 350
status: "extracted"
target_schema: "dbo.Transaction"
sidebar_position: 10
---

# DS-TRAN-001: Transaction Record (CVTRA05Y)

## Overview

The `CVTRA05Y.cpy` copybook defines the **Transaction Record**, the core transactional entity in the CardDemo system. Each record represents a single financial transaction associated with a card, including the transaction identification, classification (type and category), financial amount, merchant details, and timestamps.

This is the most widely referenced data structure in the transaction subsystem, used by online CICS programs for transaction viewing, adding, and searching, by batch programs for transaction processing, and by report generation programs.

**Source file:** `CVTRA05Y.cpy`
**Record length:** 350 bytes
**Used by:** `COTRN00C`, `COTRN01C`, `COTRN02C`, `CBTRN01C`, `CBTRN02C`, `CBTRN03C`, `CORPT00C`
**VSAM file:** `TRANSACT`

## Source COBOL

```cobol
01  TRAN-RECORD.
    05  TRAN-ID                                 PIC X(16).
    05  TRAN-TYPE-CD                            PIC X(02).
    05  TRAN-CAT-CD                             PIC 9(04).
    05  TRAN-SOURCE                             PIC X(10).
    05  TRAN-DESC                               PIC X(100).
    05  TRAN-AMT                                PIC S9(09)V99.
    05  TRAN-MERCHANT-ID                        PIC 9(09).
    05  TRAN-MERCHANT-NAME                      PIC X(50).
    05  TRAN-MERCHANT-CITY                      PIC X(50).
    05  TRAN-MERCHANT-ZIP                       PIC X(10).
    05  TRAN-CARD-NUM                           PIC X(16).
    05  TRAN-ORIG-TS                            PIC X(26).
    05  TRAN-PROC-TS                            PIC X(26).
    05  FILLER                                  PIC X(20).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Offset | Length | Description |
|---|------------|-----------|------|--------|--------|-------------|
| 1 | `TRAN-ID` | `X(16)` | Alphanumeric | 0 | 16 | Transaction unique identifier |
| 2 | `TRAN-TYPE-CD` | `X(02)` | Alphanumeric | 16 | 2 | Transaction type code |
| 3 | `TRAN-CAT-CD` | `9(04)` | Numeric (display) | 18 | 4 | Transaction category code |
| 4 | `TRAN-SOURCE` | `X(10)` | Alphanumeric | 22 | 10 | Transaction source (e.g., POS, ATM, ONLINE) |
| 5 | `TRAN-DESC` | `X(100)` | Alphanumeric | 32 | 100 | Transaction description |
| 6 | `TRAN-AMT` | `S9(09)V99` | Signed numeric with 2 implied decimals | 132 | 11 | Transaction amount |
| 7 | `TRAN-MERCHANT-ID` | `9(09)` | Numeric (display) | 143 | 9 | Merchant identifier |
| 8 | `TRAN-MERCHANT-NAME` | `X(50)` | Alphanumeric | 152 | 50 | Merchant name |
| 9 | `TRAN-MERCHANT-CITY` | `X(50)` | Alphanumeric | 202 | 50 | Merchant city |
| 10 | `TRAN-MERCHANT-ZIP` | `X(10)` | Alphanumeric | 252 | 10 | Merchant ZIP/postal code |
| 11 | `TRAN-CARD-NUM` | `X(16)` | Alphanumeric | 262 | 16 | Card number (PAN) |
| 12 | `TRAN-ORIG-TS` | `X(26)` | Alphanumeric | 278 | 26 | Origination timestamp |
| 13 | `TRAN-PROC-TS` | `X(26)` | Alphanumeric | 304 | 26 | Processing timestamp |
| 14 | `FILLER` | `X(20)` | Filler | 330 | 20 | Reserved/unused space |

**Total record length:** 350 bytes

## Field Notes

1. **TRAN-ID** -- 16-character unique transaction identifier. Primary key. Likely system-generated (e.g., timestamp-based or sequential). Must be preserved exactly during migration for parallel-run reconciliation.

2. **TRAN-TYPE-CD** -- 2-character transaction type code. Foreign key to the Transaction Type reference table (CVTRA03Y). Examples: "SA" (Sales), "CR" (Credits).

3. **TRAN-CAT-CD** -- 4-digit numeric category code. Combined with `TRAN-TYPE-CD`, foreign key to the Transaction Category reference table (CVTRA04Y).

4. **TRAN-SOURCE** -- 10-character source identifier indicating how the transaction was initiated. Examples: "POS" (point-of-sale terminal), "ATM" (automated teller machine), "ONLINE" (e-commerce), "BATCH" (batch processing). Trailing spaces should be trimmed.

5. **TRAN-DESC** -- 100-character free-text description of the transaction. May contain merchant-supplied description or system-generated text. Trim trailing spaces.

6. **TRAN-AMT** -- Signed numeric field with 2 implied decimal places. The core financial amount of the transaction. The `S` prefix allows negative values (e.g., refunds, credits). The `V` is an implied decimal. Example: stored `00012345678` with positive sign = `123,456.78`. This field is critical for financial reconciliation and must maintain exact precision during migration.

7. **TRAN-MERCHANT-ID** -- 9-digit numeric merchant identifier. Links to the acquiring network's merchant record. Leading zeros significant.

8. **TRAN-MERCHANT-NAME** -- 50-character merchant name as received from the acquiring network. Trim trailing spaces. May contain Swedish characters.

9. **TRAN-MERCHANT-CITY** -- 50-character merchant city. Trim trailing spaces. May contain Swedish characters.

10. **TRAN-MERCHANT-ZIP** -- 10-character merchant postal code. Supports both Swedish (5-digit) and international postal codes.

11. **TRAN-CARD-NUM** -- 16-character card number (Primary Account Number / PAN). **PCI-DSS sensitive field.** This is the full, unmasked card number. In the .NET target system, this must be stored encrypted and masked in all displays except for PCI-authorized roles. See Compliance Considerations.

12. **TRAN-ORIG-TS** -- 26-character origination timestamp. Stored as a string, likely in ISO 8601 format (`YYYY-MM-DD-HH.MM.SS.FFFFFF`). This is when the transaction occurred at the merchant/terminal.

13. **TRAN-PROC-TS** -- 26-character processing timestamp. Stored as a string in the same format as `TRAN-ORIG-TS`. This is when the transaction was posted/processed by the system. May differ from origination for offline/batch-processed transactions.

14. **FILLER** -- 20 bytes of reserved space. Verify contents are consistently spaces or low-values before discarding.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration |
|-------|---------------------|
| `TRAN-ID` | Standard EBCDIC-to-UTF-8 conversion. Preserve exact value (no trimming). |
| `TRAN-TYPE-CD` | Standard EBCDIC-to-UTF-8 conversion. |
| `TRAN-CAT-CD` | Zoned decimal. Convert EBCDIC digits to ASCII. |
| `TRAN-SOURCE` | Standard EBCDIC-to-UTF-8 conversion. Trim trailing spaces. |
| `TRAN-DESC` | EBCDIC-to-UTF-8. May contain Swedish characters (a-ring `X'5B'`, a-umlaut `X'4F'`, o-umlaut `X'7C'` in Swedish EBCDIC CCSID 278). Trim trailing spaces. |
| `TRAN-AMT` | Signed zoned decimal. Sign in last byte zone nibble: `C` = positive, `D` = negative. |
| `TRAN-MERCHANT-ID` | Zoned decimal. Convert EBCDIC digits to ASCII. Preserve leading zeros. |
| `TRAN-MERCHANT-NAME` | EBCDIC-to-UTF-8. Swedish characters possible. Trim trailing spaces. |
| `TRAN-MERCHANT-CITY` | EBCDIC-to-UTF-8. Swedish characters possible. Trim trailing spaces. |
| `TRAN-MERCHANT-ZIP` | Standard EBCDIC-to-UTF-8 conversion. Trim trailing spaces. |
| `TRAN-CARD-NUM` | Standard EBCDIC-to-UTF-8 conversion. **PCI-DSS: handle in secure memory during conversion.** |
| `TRAN-ORIG-TS` | Standard EBCDIC-to-UTF-8 conversion. Parse to `DATETIME2` after conversion. |
| `TRAN-PROC-TS` | Standard EBCDIC-to-UTF-8 conversion. Parse to `DATETIME2` after conversion. |
| `FILLER` | Ignore. Verify EBCDIC spaces or low-values. |

## Referential Integrity

```
TRAN-RECORD (CVTRA05Y)
  |
  +-- TRAN-TYPE-CD --------> CVTRA03Y.TRAN-TYPE (Transaction Type)
  |                          FK: dbo.Transaction.TransactionTypeCd -> dbo.TransactionType.TransactionTypeCd
  |
  +-- TRAN-TYPE-CD +
  |   TRAN-CAT-CD ---------> CVTRA04Y.TRAN-CAT-KEY (Transaction Category)
  |                          FK: dbo.Transaction.(TransactionTypeCd, TransactionCategoryCd)
  |                              -> dbo.TransactionCategory.(TransactionTypeCd, TransactionCategoryCd)
  |
  +-- TRAN-CARD-NUM -------> CVACT02Y.ACCT-CARD-NUM (Card Record)
                              FK: dbo.Transaction.CardNumber -> dbo.Card.CardNumber
```

## Sample Data

| TRAN-ID | TRAN-TYPE-CD | TRAN-CAT-CD | TRAN-SOURCE | TRAN-DESC | TRAN-AMT | TRAN-MERCHANT-ID | TRAN-MERCHANT-NAME | TRAN-MERCHANT-CITY | TRAN-MERCHANT-ZIP | TRAN-CARD-NUM | TRAN-ORIG-TS | TRAN-PROC-TS |
|---------|-------------|-------------|-------------|-----------|----------|-----------------|-------------------|-------------------|------------------|--------------|-------------|-------------|
| `0000000000000001` | `SA` | `5001` | `POS` | `Grocery purchase` | `+000000125.50` | `000123456` | `ICA Maxi` | `Stockholm` | `11120` | `4000123456789010` | `2026-01-15-10.30.45.000000` | `2026-01-15-14.22.10.000000` |
| `0000000000000002` | `CR` | `6001` | `ONLINE` | `Merchant credit - return` | `-000000045.00` | `000789012` | `Elgiganten` | `Goteborg` | `41101` | `4000123456789010` | `2026-01-16-09.15.00.000000` | `2026-01-16-12.00.30.000000` |

> **Note:** `TRAN-CARD-NUM` shown here is sample data. In production, this field contains real PANs and must be handled per PCI-DSS requirements. The `V` (implied decimal) in `TRAN-AMT` means no physical decimal separator is stored.

## Migration Notes

### Target DDL

```sql
CREATE TABLE dbo.[Transaction] (
    TransactionId         VARCHAR(16)    NOT NULL,
    TransactionTypeCd     CHAR(2)        NOT NULL,
    TransactionCategoryCd INT            NOT NULL,
    TransactionSource     VARCHAR(10)    NULL,
    TransactionDesc       NVARCHAR(100)  NULL,
    TransactionAmount     DECIMAL(11,2)  NOT NULL,
    MerchantId            BIGINT         NULL,
    MerchantName          NVARCHAR(50)   NULL,
    MerchantCity          NVARCHAR(50)   NULL,
    MerchantZip           VARCHAR(10)    NULL,
    CardNumber            VARBINARY(256) NOT NULL,  -- Encrypted PAN (PCI-DSS)
    OriginationTimestamp  DATETIME2(6)   NOT NULL,
    ProcessingTimestamp   DATETIME2(6)   NOT NULL,

    CONSTRAINT PK_Transaction
        PRIMARY KEY (TransactionId),

    CONSTRAINT FK_Trans_TransType
        FOREIGN KEY (TransactionTypeCd)
        REFERENCES dbo.TransactionType (TransactionTypeCd),

    CONSTRAINT FK_Trans_TransCategory
        FOREIGN KEY (TransactionTypeCd, TransactionCategoryCd)
        REFERENCES dbo.TransactionCategory (TransactionTypeCd, TransactionCategoryCd),

    CONSTRAINT FK_Trans_Card
        FOREIGN KEY (CardNumber)
        REFERENCES dbo.Card (CardNumberEncrypted)
);

-- Index for card-based lookups
CREATE INDEX IX_Transaction_CardNumber
    ON dbo.[Transaction] (CardNumber);

-- Index for date-range queries (reporting)
CREATE INDEX IX_Transaction_ProcTimestamp
    ON dbo.[Transaction] (ProcessingTimestamp);

-- Index for type/category lookups
CREATE INDEX IX_Transaction_TypeCategory
    ON dbo.[Transaction] (TransactionTypeCd, TransactionCategoryCd);
```

> **Note:** The `CardNumber` FK reference assumes the Card table also stores encrypted PANs. The exact FK design depends on the encryption strategy (column-level encryption, Always Encrypted, or application-level encryption with a hash for lookups).

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `TRAN-ID` | `PIC X(16)` | `VARCHAR(16)` | `string` | Primary key, preserve exactly |
| `TRAN-TYPE-CD` | `PIC X(02)` | `CHAR(2)` | `string` | FK to TransactionType |
| `TRAN-CAT-CD` | `PIC 9(04)` | `INT` | `int` | FK (with type) to TransactionCategory |
| `TRAN-SOURCE` | `PIC X(10)` | `VARCHAR(10)` | `string` | Trim trailing spaces |
| `TRAN-DESC` | `PIC X(100)` | `NVARCHAR(100)` | `string` | NVARCHAR for Swedish characters |
| `TRAN-AMT` | `PIC S9(09)V99` | `DECIMAL(11,2)` | `decimal` | Signed, 2 implied decimals |
| `TRAN-MERCHANT-ID` | `PIC 9(09)` | `BIGINT` | `long` | 9-digit numeric |
| `TRAN-MERCHANT-NAME` | `PIC X(50)` | `NVARCHAR(50)` | `string` | NVARCHAR for Swedish characters |
| `TRAN-MERCHANT-CITY` | `PIC X(50)` | `NVARCHAR(50)` | `string` | NVARCHAR for Swedish characters |
| `TRAN-MERCHANT-ZIP` | `PIC X(10)` | `VARCHAR(10)` | `string` | Postal codes |
| `TRAN-CARD-NUM` | `PIC X(16)` | `VARBINARY(256)` | `byte[]` | Encrypted PAN (PCI-DSS) |
| `TRAN-ORIG-TS` | `PIC X(26)` | `DATETIME2(6)` | `DateTime` | Parse from string to datetime |
| `TRAN-PROC-TS` | `PIC X(26)` | `DATETIME2(6)` | `DateTime` | Parse from string to datetime |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS RowCount FROM dbo.[Transaction];

-- Transaction amount checksum
SELECT SUM(TransactionAmount) AS TotalAmount FROM dbo.[Transaction];

-- Verify no orphaned type/category references
SELECT t.TransactionId, t.TransactionTypeCd, t.TransactionCategoryCd
FROM dbo.[Transaction] t
LEFT JOIN dbo.TransactionCategory tc
    ON t.TransactionTypeCd = tc.TransactionTypeCd
    AND t.TransactionCategoryCd = tc.TransactionCategoryCd
WHERE tc.TransactionTypeCd IS NULL;

-- Verify timestamp parsing (no NULLs where source had values)
SELECT COUNT(*) AS NullOrigTs FROM dbo.[Transaction]
WHERE OriginationTimestamp IS NULL;

SELECT COUNT(*) AS NullProcTs FROM dbo.[Transaction]
WHERE ProcessingTimestamp IS NULL;

-- Date range sanity check
SELECT MIN(OriginationTimestamp) AS EarliestTran,
       MAX(OriginationTimestamp) AS LatestTran
FROM dbo.[Transaction];
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS** | Cardholder data (PAN) must be encrypted at rest and masked in display. Requirement 3: Protect stored cardholder data. | `TRAN-CARD-NUM` must be encrypted using AES-256 or Azure SQL Always Encrypted. Display masked as `**** **** **** 1234`. Access to full PAN requires PCI-authorized role and audit logging. Truncated PAN (first 6 / last 4) stored separately for non-sensitive lookups. |
| **PCI-DSS** | Requirement 10: Track and monitor all access to cardholder data. | All queries that decrypt or access `CardNumber` must be logged with user identity, timestamp, and business justification. |
| **GDPR** | Transaction data linked to a card (and therefore to a customer) is personal data. Right to erasure (Art. 17) and data minimisation (Art. 5) apply. | Retention policy required. Implement pseudonymisation for analytics. Right-to-erasure must consider FSA retention requirements (typically 7 years for financial transactions). |
| **FSA (FFFS 2014:5)** | Ch. 4: Transaction records must be retained for regulatory reporting. Ch. 8: Internal controls require complete audit trail. | Minimum 7-year retention for transaction records. Immutable audit log for all modifications. Reconciliation with mainframe during parallel run. |
| **PSD2** | Art. 97: Strong Customer Authentication for electronic payment transactions. Art. 45: Transaction records for dispute resolution. | Ensure transaction source field captures SCA method used. Retain records for PSD2 dispute resolution timeline (13 months + extensions). |
| **AML/KYC** | Suspicious transaction monitoring. Transaction patterns used for AML screening. | Transaction records must be available to AML screening batch jobs. Merchant data supports geographic risk analysis. Transaction amounts and patterns feed into suspicious activity detection rules. |
| **DORA** | ICT risk management. Transaction processing is a critical business function. | Transaction data integrity monitoring. Disaster recovery must ensure zero transaction loss. Include in business continuity testing. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
