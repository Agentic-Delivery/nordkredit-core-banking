---
id: DS-AUTH-REQ-001
title: "Authorization Request"
copybook_name: "CCPAURQY.cpy"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 0
status: "extracted"
target_schema: "dbo.AuthorizationRequest"
sidebar_position: 38
---

# DS-AUTH-REQ-001: Authorization Request (CCPAURQY)

## Overview

The `CCPAURQY.cpy` copybook defines the **Authorization Request Record**, the input message structure for the card authorization process. This record contains all the data needed to make an authorization decision: card identification, transaction details, merchant information, and point-of-sale context.

This record is central to the PSD2 Strong Customer Authentication (SCA) flow and contains PCI-DSS sensitive data (PAN and card expiry date). The merchant data fields are used for transaction routing and fraud detection.

**Source file:** `CCPAURQY.cpy`
**Used by:** `CCPAUTH1` (IMS authorization program)

## Source COBOL

```cobol
     05  PA-RQ-DATE                   PIC  X(08).
     05  PA-RQ-TIME                   PIC  X(06).
     05  PA-RQ-CARD-NUM               PIC  X(16).
     05  PA-RQ-AUTH-TYPE              PIC  X(02).
     05  PA-RQ-CARD-EXPIRY            PIC  X(04).
     05  PA-RQ-MSG-TYPE               PIC  X(04).
     05  PA-RQ-MSG-SOURCE             PIC  X(08).
     05  PA-RQ-PROCESSING-CODE        PIC  X(06).
     05  PA-RQ-TRANSACTION-AMT        PIC +9(10).99.
     05  PA-RQ-MERCHANT-CATG-CD       PIC  X(04).
     05  PA-RQ-ACQUIRER-COUNTRY-CD    PIC  X(03).
     05  PA-RQ-POS-ENTRY-MODE         PIC  X(03).
     05  PA-RQ-MERCHANT-ID            PIC  X(15).
     05  PA-RQ-MERCHANT-NAME          PIC  X(25).
     05  PA-RQ-MERCHANT-CITY          PIC  X(13).
     05  PA-RQ-MERCHANT-STATE         PIC  X(02).
     05  PA-RQ-MERCHANT-ZIP           PIC  X(10).
     05  PA-RQ-TRANSACTION-ID         PIC  X(15).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description | Target Column |
|---|------------|-----------|--------|------|-------------|---------------|
| 1 | `PA-RQ-DATE` | `X(08)` | 8 | Alphanumeric | Request date (`CCYYMMDD`) | `RequestTimestamp` (DATETIME2, combined with TIME) |
| 2 | `PA-RQ-TIME` | `X(06)` | 6 | Alphanumeric | Request time (`HHMMSS`) | `RequestTimestamp` (DATETIME2, combined with DATE) |
| 3 | `PA-RQ-CARD-NUM` | `X(16)` | 16 | Alphanumeric | Primary Account Number (PAN) | `CardNumberHash` (VARBINARY(64)) / `CardNumberLast4` (CHAR(4)) |
| 4 | `PA-RQ-AUTH-TYPE` | `X(02)` | 2 | Alphanumeric | Authorization type code | `AuthorizationType` (CHAR(2)) |
| 5 | `PA-RQ-CARD-EXPIRY` | `X(04)` | 4 | Alphanumeric | Card expiration (`MMYY` or `YYMM`) | `CardExpiryHash` (VARBINARY(64)) |
| 6 | `PA-RQ-MSG-TYPE` | `X(04)` | 4 | Alphanumeric | Message type indicator (ISO 8583) | `MessageType` (CHAR(4)) |
| 7 | `PA-RQ-MSG-SOURCE` | `X(08)` | 8 | Alphanumeric | Message source identifier | `MessageSource` (NVARCHAR(8)) |
| 8 | `PA-RQ-PROCESSING-CODE` | `X(06)` | 6 | Alphanumeric | Processing code (ISO 8583 field 3) | `ProcessingCode` (CHAR(6)) |
| 9 | `PA-RQ-TRANSACTION-AMT` | `+9(10).99` | 14 | Edited numeric | Transaction amount with explicit sign and decimal | `TransactionAmount` (DECIMAL(12,2)) |
| 10 | `PA-RQ-MERCHANT-CATG-CD` | `X(04)` | 4 | Alphanumeric | Merchant Category Code (MCC) | `MerchantCategoryCode` (CHAR(4)) |
| 11 | `PA-RQ-ACQUIRER-COUNTRY-CD` | `X(03)` | 3 | Alphanumeric | Acquirer country code (ISO 3166) | `AcquirerCountryCode` (CHAR(3)) |
| 12 | `PA-RQ-POS-ENTRY-MODE` | `X(03)` | 3 | Alphanumeric | Point-of-Sale entry mode | `PosEntryMode` (CHAR(3)) |
| 13 | `PA-RQ-MERCHANT-ID` | `X(15)` | 15 | Alphanumeric | Merchant identifier | `MerchantId` (NVARCHAR(15)) |
| 14 | `PA-RQ-MERCHANT-NAME` | `X(25)` | 25 | Alphanumeric | Merchant name | `MerchantName` (NVARCHAR(25)) |
| 15 | `PA-RQ-MERCHANT-CITY` | `X(13)` | 13 | Alphanumeric | Merchant city | `MerchantCity` (NVARCHAR(13)) |
| 16 | `PA-RQ-MERCHANT-STATE` | `X(02)` | 2 | Alphanumeric | Merchant state/province | `MerchantState` (NVARCHAR(2)) |
| 17 | `PA-RQ-MERCHANT-ZIP` | `X(10)` | 10 | Alphanumeric | Merchant postal code | `MerchantZip` (NVARCHAR(10)) |
| 18 | `PA-RQ-TRANSACTION-ID` | `X(15)` | 15 | Alphanumeric | Unique transaction identifier | `TransactionId` (NVARCHAR(15)) |

## Field Notes

1. **PA-RQ-DATE** (`PIC X(08)`) -- Request date in `CCYYMMDD` format (8 characters). Combined with `PA-RQ-TIME` to form the request timestamp. Unlike `ERR-DATE` in the error log (which uses 2-digit year), this field uses a full 4-digit year.

2. **PA-RQ-TIME** (`PIC X(06)`) -- Request time in `HHMMSS` format (24-hour clock, 6 characters). Combined with `PA-RQ-DATE` for the complete timestamp.

3. **PA-RQ-CARD-NUM** (`PIC X(16)`) -- The full 16-digit PAN. **PCI-DSS Requirement 3.4**: Must be hashed before storage. Only last 4 digits may be stored in clear text for display purposes.

4. **PA-RQ-AUTH-TYPE** (`PIC X(02)`) -- Identifies the type of authorization (e.g., `01` = purchase, `02` = cash advance, `03` = pre-authorization). Used for routing logic and limit checks.

5. **PA-RQ-CARD-EXPIRY** (`PIC X(04)`) -- Card expiration date in 4-character format (either `MMYY` or `YYMM`). **PCI-DSS sensitive authentication data**: Must not be stored after authorization. This field must be hashed or discarded during migration. It is used only for real-time validation.

6. **PA-RQ-MSG-TYPE** (`PIC X(04)`) -- ISO 8583 message type indicator. Common values:
   - `0100` = Authorization request
   - `0110` = Authorization response
   - `0400` = Reversal request

7. **PA-RQ-MSG-SOURCE** (`PIC X(08)`) -- Identifies the system or channel that originated the request (e.g., POS terminal, ATM, online banking).

8. **PA-RQ-PROCESSING-CODE** (`PIC X(06)`) -- ISO 8583 field 3. Describes the effect of the transaction on the account (e.g., `000000` = purchase, `010000` = cash withdrawal).

9. **PA-RQ-TRANSACTION-AMT** (`PIC +9(10).99`) -- Edited numeric with explicit sign and decimal point. Format: `+NNNNNNNNNN.NN` (14 characters). Same parsing considerations as `PA-RL-APPROVED-AMT` in the authorization response.

10. **PA-RQ-MERCHANT-CATG-CD** (`PIC X(04)`) -- Merchant Category Code (MCC) per ISO 18245. Used for:
    - Transaction routing
    - Spending category reporting
    - AML/KYC risk scoring (high-risk MCC codes)
    - PSD2 SCA exemption evaluation

11. **PA-RQ-ACQUIRER-COUNTRY-CD** (`PIC X(03)`) -- ISO 3166-1 numeric country code of the acquiring bank. Used for cross-border transaction detection and regulatory reporting.

12. **PA-RQ-POS-ENTRY-MODE** (`PIC X(03)`) -- How the card data was captured:
    - `010` = Manual entry (keyed)
    - `050` = Chip (ICC)
    - `070` = Contactless chip
    - `080` = Contactless magnetic stripe
    - `090` = Magnetic stripe

    This field is critical for **fraud detection** (manual entry is higher risk) and **PSD2 SCA** (chip transactions may qualify for SCA exemption).

13. **PA-RQ-MERCHANT-ID through PA-RQ-MERCHANT-ZIP** -- Merchant identification and location data. Used for transaction routing, fraud detection (unusual merchant locations), and customer statement descriptions.

14. **PA-RQ-TRANSACTION-ID** (`PIC X(15)`) -- Unique transaction identifier. Serves as the correlation key across authorization request, response, and pending authorization records.

## Target Architecture Mapping

| Aspect | COBOL/IMS (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Request intake** | IMS message queue | REST API endpoint / Azure Service Bus message |
| **PAN handling** | Clear text in IMS message | Tokenized at API gateway, hashed at rest |
| **Amount parsing** | Edited numeric `PIC +9(10).99` | `decimal` parsed from string or JSON |
| **MCC lookup** | In-memory table | Reference data service / lookup table |
| **POS entry mode** | Raw code in message | Enum type with fraud risk scoring |
| **Correlation** | `PA-RQ-TRANSACTION-ID` | Distributed tracing Activity ID + TransactionId |

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.AuthorizationRequest (
    RequestId               BIGINT IDENTITY(1,1)  NOT NULL,
    RequestTimestamp         DATETIME2             NOT NULL,
    CardNumberHash          VARBINARY(64)         NOT NULL,
    CardNumberLast4         CHAR(4)               NOT NULL,
    AuthorizationType       CHAR(2)               NOT NULL,
    -- PA-RQ-CARD-EXPIRY: NOT STORED (PCI-DSS 3.2 - sensitive auth data)
    MessageType             CHAR(4)               NOT NULL,
    MessageSource           NVARCHAR(8)           NULL,
    ProcessingCode          CHAR(6)               NOT NULL,
    TransactionAmount       DECIMAL(12,2)         NOT NULL,
    MerchantCategoryCode    CHAR(4)               NULL,
    AcquirerCountryCode     CHAR(3)               NULL,
    PosEntryMode            CHAR(3)               NULL,
    MerchantId              NVARCHAR(15)          NULL,
    MerchantName            NVARCHAR(25)          NULL,
    MerchantCity            NVARCHAR(13)          NULL,
    MerchantState           NVARCHAR(2)           NULL,
    MerchantZip             NVARCHAR(10)          NULL,
    TransactionId           NVARCHAR(15)          NOT NULL,

    -- Audit columns
    CreatedAt               DATETIME2             NOT NULL DEFAULT SYSUTCDATETIME(),

    CONSTRAINT PK_AuthorizationRequest PRIMARY KEY CLUSTERED (RequestId),
    CONSTRAINT UQ_AuthReq_TransactionId UNIQUE (TransactionId)
);

-- Index for card-based lookups
CREATE NONCLUSTERED INDEX IX_AuthReq_CardHash
    ON dbo.AuthorizationRequest (CardNumberHash);

-- Index for merchant analysis
CREATE NONCLUSTERED INDEX IX_AuthReq_MerchantCatg
    ON dbo.AuthorizationRequest (MerchantCategoryCode, RequestTimestamp DESC);

-- Index for time-based queries
CREATE NONCLUSTERED INDEX IX_AuthReq_Timestamp
    ON dbo.AuthorizationRequest (RequestTimestamp DESC);
```

### Data Type Mapping

| COBOL Field | COBOL Type | SQL Type | C# Type | Notes |
|------------|-----------|---------|---------|-------|
| `PA-RQ-DATE` + `PA-RQ-TIME` | `X(08)` + `X(06)` | `DATETIME2` | `DateTime` | Combined into single timestamp |
| `PA-RQ-CARD-NUM` | `X(16)` | `VARBINARY(64)` / `CHAR(4)` | `byte[]` / `string` | Hashed + last 4 (PCI-DSS) |
| `PA-RQ-CARD-EXPIRY` | `X(04)` | **Not stored** | -- | PCI-DSS 3.2: discard after auth |
| `PA-RQ-TRANSACTION-AMT` | `+9(10).99` | `DECIMAL(12,2)` | `decimal` | Parse edited numeric |
| All other `X(n)` fields | Alphanumeric | `NVARCHAR(n)` or `CHAR(n)` | `string` | Trim trailing spaces |

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS RequestCount FROM dbo.AuthorizationRequest;

-- Verify PAN is hashed
SELECT COUNT(*) AS ClearPANCount
FROM dbo.AuthorizationRequest
WHERE CardNumberHash IS NULL OR LEN(CardNumberLast4) <> 4;

-- Verify card expiry is NOT stored
SELECT COLUMN_NAME
FROM INFORMATION_SCHEMA.COLUMNS
WHERE TABLE_NAME = 'AuthorizationRequest' AND COLUMN_NAME LIKE '%Expiry%';

-- Transaction amount checksum
SELECT SUM(TransactionAmount) AS TotalRequested
FROM dbo.AuthorizationRequest;

-- POS entry mode distribution (fraud analysis baseline)
SELECT PosEntryMode, COUNT(*) AS Cnt
FROM dbo.AuthorizationRequest
GROUP BY PosEntryMode;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS 3.4** | Render PAN unreadable anywhere it is stored. | `PA-RQ-CARD-NUM` must be hashed. No clear-text PAN in logs, databases, or temporary storage. |
| **PCI-DSS 3.2** | Do not store sensitive authentication data after authorization. | `PA-RQ-CARD-EXPIRY` must **not** be persisted to the target database. Used only for real-time validation, then discarded. |
| **PSD2** Art. 97 | Strong Customer Authentication for electronic payment transactions. | Authorization requests must be evaluated for SCA requirements. `PA-RQ-POS-ENTRY-MODE` determines whether SCA was applied (chip = SCA, contactless under limit = exemption). The `PA-RQ-MERCHANT-CATG-CD` feeds into SCA exemption evaluation (e.g., low-value payments, trusted beneficiaries). |
| **PSD2** Art. 98 | Regulatory technical standards on SCA. | The authorization request data must support the RTS on SCA, including transaction risk analysis based on amount, merchant, and POS entry mode. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls over payment authorization. | All authorization requests must be logged and retained for audit. The `TransactionId` provides end-to-end traceability. |
| **AML/KYC** (FFFS 2017:11) | Transaction monitoring and suspicious activity detection. | High-risk MCC codes, cross-border transactions (`AcquirerCountryCode`), and unusual POS entry modes must be flagged. Authorization request data feeds the AML monitoring pipeline. |
| **GDPR** Art. 5(1)(c) | Data minimization. | Merchant data is not personal data, but PAN is. Ensure PAN is minimized (hash + last 4 only). Card expiry is discarded post-authorization. |
| **DORA** Art. 11 | ICT data integrity for financial transactions. | Authorization request amounts must be validated during migration with zero tolerance. Parallel-run comparison must confirm identical authorization decisions for identical inputs. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
