---
id: DS-AUTH-DTL-001
title: "IMS Pending Authorization Detail"
copybook_name: "CIPAUDTY.cpy"
domain: "authorization"
used_by_programs: [CCPAUTH1, COPAUS0C]
record_length: 0
status: "extracted"
target_schema: "dbo.PendingAuthorizationDetail"
sidebar_position: 39
---

# DS-AUTH-DTL-001: IMS Pending Authorization Detail (CIPAUDTY)

## Overview

The `CIPAUDTY.cpy` copybook defines the **IMS Pending Authorization Detail Record**, an IMS database segment that stores individual pending card authorization transactions. This record tracks the full lifecycle of an authorization from initial request through matching, decline, or expiration, including fraud indicators.

This is one of the most complex data structures in the authorization subsystem, featuring COMP-3 (packed decimal) fields for dates, times, and financial amounts, a match status state machine, and fraud tracking fields. It is subject to PCI-DSS (card data), AML/KYC (fraud indicators), and PSD2 (authorization lifecycle) requirements.

**Source file:** `CIPAUDTY.cpy`
**Used by:** `CCPAUTH1` (IMS authorization program), `COPAUS0C` (pending authorization status program)

## Source COBOL (Key Fields)

```cobol
     05  PA-AUTHORIZATION-KEY.
         10  PA-AUTH-DATE-9C          PIC S9(07)  COMP-3.
         10  PA-AUTH-TIME-9C          PIC S9(07)  COMP-3.

     05  PA-AUTH-ID-CODE              PIC  X(06).
     05  PA-AUTH-RESP-CODE            PIC  X(02).
     05  PA-AUTH-RESP-REASON          PIC  X(04).
     05  PA-AUTH-TYPE                 PIC  X(02).

     05  PA-CARD-NUM                  PIC  X(16).
     05  PA-CARD-EXPIRY               PIC  X(04).

     05  PA-MERCHANT-CATG-CD          PIC  X(04).
     05  PA-MERCHANT-ID               PIC  X(15).
     05  PA-MERCHANT-NAME             PIC  X(25).
     05  PA-MERCHANT-CITY             PIC  X(13).
     05  PA-MERCHANT-STATE            PIC  X(02).
     05  PA-MERCHANT-ZIP              PIC  X(10).

     05  PA-TRANSACTION-AMT           PIC S9(09)V99  COMP-3.
     05  PA-APPROVED-AMT              PIC S9(09)V99  COMP-3.

     05  PA-MATCH-STATUS              PIC  X(01).
         88  PA-STATUS-PENDING        VALUE 'P'.
         88  PA-STATUS-DECLINED       VALUE 'D'.
         88  PA-STATUS-EXPIRED        VALUE 'E'.
         88  PA-STATUS-MATCHED        VALUE 'M'.

     05  PA-FRAUD-IND                 PIC  X(01).
         88  PA-FRAUD-CONFIRMED       VALUE 'F'.
         88  PA-FRAUD-REMOVED         VALUE 'R'.
     05  PA-FRAUD-REPORT-DATE         PIC  X(08).

     05  PA-TRANSACTION-ID            PIC  X(15).
```

## Field Definitions

| # | Field Name | PIC Clause | Type | Description | Target Column |
|---|------------|-----------|------|-------------|---------------|
| 1 | `PA-AUTH-DATE-9C` | `S9(07) COMP-3` | Packed decimal (4 bytes) | Authorization date as packed integer | `AuthorizationTimestamp` (DATETIME2) |
| 2 | `PA-AUTH-TIME-9C` | `S9(07) COMP-3` | Packed decimal (4 bytes) | Authorization time as packed integer | `AuthorizationTimestamp` (DATETIME2) |
| 3 | `PA-AUTH-ID-CODE` | `X(06)` | Alphanumeric | Authorization identification code | `AuthorizationIdCode` (NVARCHAR(6)) |
| 4 | `PA-AUTH-RESP-CODE` | `X(02)` | Alphanumeric | Response code from authorization | `ResponseCode` (CHAR(2)) |
| 5 | `PA-AUTH-RESP-REASON` | `X(04)` | Alphanumeric | Response reason code | `ResponseReason` (NVARCHAR(4)) |
| 6 | `PA-AUTH-TYPE` | `X(02)` | Alphanumeric | Authorization type | `AuthorizationType` (CHAR(2)) |
| 7 | `PA-CARD-NUM` | `X(16)` | Alphanumeric | Primary Account Number (PAN) | `CardNumberHash` / `CardNumberLast4` |
| 8 | `PA-CARD-EXPIRY` | `X(04)` | Alphanumeric | Card expiration date | **Not stored** (PCI-DSS) |
| 9 | `PA-MERCHANT-CATG-CD` | `X(04)` | Alphanumeric | Merchant Category Code | `MerchantCategoryCode` (CHAR(4)) |
| 10 | `PA-MERCHANT-ID` | `X(15)` | Alphanumeric | Merchant identifier | `MerchantId` (NVARCHAR(15)) |
| 11 | `PA-MERCHANT-NAME` | `X(25)` | Alphanumeric | Merchant name | `MerchantName` (NVARCHAR(25)) |
| 12 | `PA-MERCHANT-CITY` | `X(13)` | Alphanumeric | Merchant city | `MerchantCity` (NVARCHAR(13)) |
| 13 | `PA-MERCHANT-STATE` | `X(02)` | Alphanumeric | Merchant state | `MerchantState` (NVARCHAR(2)) |
| 14 | `PA-MERCHANT-ZIP` | `X(10)` | Alphanumeric | Merchant postal code | `MerchantZip` (NVARCHAR(10)) |
| 15 | `PA-TRANSACTION-AMT` | `S9(09)V99 COMP-3` | Packed decimal (6 bytes) | Requested transaction amount | `TransactionAmount` (DECIMAL(11,2)) |
| 16 | `PA-APPROVED-AMT` | `S9(09)V99 COMP-3` | Packed decimal (6 bytes) | Approved amount | `ApprovedAmount` (DECIMAL(11,2)) |
| 17 | `PA-MATCH-STATUS` | `X(01)` | Alphanumeric | Authorization lifecycle status | `MatchStatus` (NVARCHAR(20)) |
| 18 | `PA-FRAUD-IND` | `X(01)` | Alphanumeric | Fraud indicator | `FraudIndicator` (NVARCHAR(20)) |
| 19 | `PA-FRAUD-REPORT-DATE` | `X(08)` | Alphanumeric | Date fraud was reported (`CCYYMMDD`) | `FraudReportDate` (DATE) |
| 20 | `PA-TRANSACTION-ID` | `X(15)` | Alphanumeric | Unique transaction identifier | `TransactionId` (NVARCHAR(15)) |

## Field Notes

### COMP-3 Packed Decimal Fields

The authorization key and financial amount fields use COMP-3 (packed decimal) encoding, which stores two digits per byte with the sign in the last nibble. This is more storage-efficient than display (zoned) decimal but requires special parsing during migration.

| Field | PIC Clause | Byte Length | Value Range | Example |
|-------|-----------|------------|-------------|---------|
| `PA-AUTH-DATE-9C` | `S9(07) COMP-3` | 4 bytes | 0000000-9999999 | `0260215` = 2026-02-15 (CYYMMDD) |
| `PA-AUTH-TIME-9C` | `S9(07) COMP-3` | 4 bytes | 0000000-9999999 | `0143025` = 14:30:25 (HHMMSSC) |
| `PA-TRANSACTION-AMT` | `S9(09)V99 COMP-3` | 6 bytes | -999,999,999.99 to +999,999,999.99 | Implied V99 decimal |
| `PA-APPROVED-AMT` | `S9(09)V99 COMP-3` | 6 bytes | -999,999,999.99 to +999,999,999.99 | May differ from request amount |

### Match Status State Machine

The `PA-MATCH-STATUS` field tracks the authorization lifecycle:

```
    [Initial]
        |
        v
    P (Pending) -----> M (Matched)     -- Settlement matched
        |
        +------------> D (Declined)    -- Authorization declined
        |
        +------------> E (Expired)     -- Authorization expired (no settlement)
```

| Status | 88-Level Name | Value | Description |
|--------|--------------|-------|-------------|
| Pending | `PA-STATUS-PENDING` | `P` | Authorization approved, awaiting settlement match |
| Declined | `PA-STATUS-DECLINED` | `D` | Authorization was declined |
| Expired | `PA-STATUS-EXPIRED` | `E` | Authorization expired without settlement |
| Matched | `PA-STATUS-MATCHED` | `M` | Authorization matched to settlement transaction |

### Fraud Indicators

| Indicator | 88-Level Name | Value | Description |
|-----------|--------------|-------|-------------|
| Confirmed Fraud | `PA-FRAUD-CONFIRMED` | `F` | Transaction confirmed as fraudulent |
| Fraud Removed | `PA-FRAUD-REMOVED` | `R` | Fraud flag was removed (false positive) |
| No Indicator | (spaces) | ` ` | No fraud flag on this transaction |

When `PA-FRAUD-IND` is set to `F`, the `PA-FRAUD-REPORT-DATE` records when the fraud was reported.

## COMP-3 Parsing Notes

```
COMP-3 packed decimal encoding:
  Each byte stores 2 digits (one in each nibble)
  Last nibble is the sign: C = positive, D = negative, F = unsigned

  Example: PA-TRANSACTION-AMT = 12345.67
  Stored as: 00 01 23 45 67 0C  (6 bytes, S9(09)V99 COMP-3)

  byte 1: 00 (digits 0, 0)
  byte 2: 01 (digits 0, 1)
  byte 3: 23 (digits 2, 3)
  byte 4: 45 (digits 4, 5)
  byte 5: 67 (digits 6, 7)
  byte 6: 0C (digit 0, sign C=positive)

  Result: +000012345.67 (with implied V99 decimal)
```

## Target Architecture Mapping

| Aspect | COBOL/IMS (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Storage** | IMS hierarchical database segment | Azure SQL relational table |
| **Key** | `PA-AUTHORIZATION-KEY` (packed date/time) | `PendingAuthorizationDetailId` (BIGINT IDENTITY) + unique constraint on `TransactionId` |
| **Packed decimals** | COMP-3 encoding | `decimal` type (native) |
| **Status tracking** | Single-character flag with 88-levels | Enum type or status column with CHECK constraint |
| **Fraud tracking** | Single-character indicator | Enum + nullable date |
| **IMS navigation** | GU/GN/GHU calls on segment | Entity Framework LINQ queries |

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.PendingAuthorizationDetail (
    PendingAuthDetailId     BIGINT IDENTITY(1,1) NOT NULL,
    AuthorizationTimestamp  DATETIME2            NOT NULL,
    AuthorizationIdCode     NVARCHAR(6)          NULL,
    ResponseCode            CHAR(2)              NOT NULL,
    ResponseReason          NVARCHAR(4)          NULL,
    AuthorizationType       CHAR(2)              NOT NULL,
    CardNumberHash          VARBINARY(64)        NOT NULL,
    CardNumberLast4         CHAR(4)              NOT NULL,
    -- PA-CARD-EXPIRY: NOT STORED (PCI-DSS 3.2)
    MerchantCategoryCode    CHAR(4)              NULL,
    MerchantId              NVARCHAR(15)         NULL,
    MerchantName            NVARCHAR(25)         NULL,
    MerchantCity            NVARCHAR(13)         NULL,
    MerchantState           NVARCHAR(2)          NULL,
    MerchantZip             NVARCHAR(10)         NULL,
    TransactionAmount       DECIMAL(11,2)        NOT NULL,
    ApprovedAmount          DECIMAL(11,2)        NOT NULL DEFAULT 0.00,
    MatchStatus             NVARCHAR(20)         NOT NULL DEFAULT 'Pending',
    FraudIndicator          NVARCHAR(20)         NULL,
    FraudReportDate         DATE                 NULL,
    TransactionId           NVARCHAR(15)         NOT NULL,

    -- Audit columns
    CreatedAt               DATETIME2            NOT NULL DEFAULT SYSUTCDATETIME(),
    UpdatedAt               DATETIME2            NOT NULL DEFAULT SYSUTCDATETIME(),

    CONSTRAINT PK_PendingAuthDetail PRIMARY KEY CLUSTERED (PendingAuthDetailId),
    CONSTRAINT UQ_PendingAuthDetail_TxnId UNIQUE (TransactionId),
    CONSTRAINT CK_PendingAuthDetail_Status CHECK (MatchStatus IN ('Pending', 'Declined', 'Expired', 'Matched')),
    CONSTRAINT CK_PendingAuthDetail_Fraud CHECK (FraudIndicator IS NULL OR FraudIndicator IN ('Confirmed', 'Removed'))
);

-- Index for pending authorizations (settlement matching)
CREATE NONCLUSTERED INDEX IX_PendingAuth_Status
    ON dbo.PendingAuthorizationDetail (MatchStatus)
    WHERE MatchStatus = 'Pending';

-- Index for card-based lookups
CREATE NONCLUSTERED INDEX IX_PendingAuth_CardHash
    ON dbo.PendingAuthorizationDetail (CardNumberHash);

-- Index for fraud analysis
CREATE NONCLUSTERED INDEX IX_PendingAuth_Fraud
    ON dbo.PendingAuthorizationDetail (FraudIndicator)
    WHERE FraudIndicator IS NOT NULL;
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `PA-AUTH-DATE-9C` + `PA-AUTH-TIME-9C` | `S9(07) COMP-3` x 2 | `DATETIME2` | `DateTime` | Unpack COMP-3, combine date + time |
| `PA-CARD-NUM` | `X(16)` | `VARBINARY(64)` / `CHAR(4)` | `byte[]` / `string` | Hash + last 4 |
| `PA-CARD-EXPIRY` | `X(04)` | **Not stored** | -- | PCI-DSS: discard |
| `PA-TRANSACTION-AMT` | `S9(09)V99 COMP-3` | `DECIMAL(11,2)` | `decimal` | Unpack COMP-3 |
| `PA-APPROVED-AMT` | `S9(09)V99 COMP-3` | `DECIMAL(11,2)` | `decimal` | Unpack COMP-3 |
| `PA-MATCH-STATUS` | `X(01)` | `NVARCHAR(20)` | `string` (enum) | Map P/D/E/M to descriptive names |
| `PA-FRAUD-IND` | `X(01)` | `NVARCHAR(20)` | `string?` (enum) | Map F/R to descriptive names |
| `PA-FRAUD-REPORT-DATE` | `X(08)` | `DATE` | `DateOnly?` | Parse CCYYMMDD |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS DetailCount FROM dbo.PendingAuthorizationDetail;

-- Match status distribution
SELECT MatchStatus, COUNT(*) AS Cnt
FROM dbo.PendingAuthorizationDetail
GROUP BY MatchStatus;

-- Financial amount checksums
SELECT
    SUM(TransactionAmount) AS TotalRequested,
    SUM(ApprovedAmount) AS TotalApproved
FROM dbo.PendingAuthorizationDetail;

-- Fraud indicator distribution
SELECT FraudIndicator, COUNT(*) AS Cnt
FROM dbo.PendingAuthorizationDetail
WHERE FraudIndicator IS NOT NULL
GROUP BY FraudIndicator;

-- Verify card expiry NOT stored
SELECT COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'PendingAuthorizationDetail' AND COLUMN_NAME LIKE '%Expiry%';
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS 3.4** | Render PAN unreadable. | `PA-CARD-NUM` hashed; only last 4 stored in clear. |
| **PCI-DSS 3.2** | Do not store sensitive authentication data post-authorization. | `PA-CARD-EXPIRY` must NOT be migrated or stored. |
| **PSD2** Art. 97 | SCA for electronic payments. | Authorization lifecycle (match status) documents the SCA outcome and subsequent settlement. |
| **AML/KYC** (FFFS 2017:11) | Transaction monitoring and fraud detection. | `PA-FRAUD-IND` and `PA-FRAUD-REPORT-DATE` are core AML/KYC fields. Fraud-confirmed transactions must feed the SAR (Suspicious Activity Report) pipeline. Match status patterns (high decline rates, expired authorizations) may indicate suspicious activity. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls. | Match status transitions must be audit-logged. Fraud indicator changes require dual authorization. |
| **DORA** Art. 11 | ICT data integrity. | COMP-3 unpacking must be validated with zero tolerance for financial amount discrepancies. Parallel-run comparison must confirm identical packed decimal interpretation. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
