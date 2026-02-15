---
id: DS-AUTH-RSP-001
title: "Authorization Response"
copybook_name: "CCPAURLY.cpy"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 0
status: "extracted"
target_schema: "dbo.AuthorizationResponse"
sidebar_position: 37
---

# DS-AUTH-RSP-001: Authorization Response (CCPAURLY)

## Overview

The `CCPAURLY.cpy` copybook defines the **Authorization Response Record**, the structure returned by the authorization decision engine after evaluating a card authorization request. This record contains the card number, transaction identifier, authorization ID code, response code, response reason, and approved amount.

This record contains the Primary Account Number (PAN), making it subject to PCI-DSS data protection requirements. The approved amount uses an edited numeric format with an explicit sign and decimal point.

**Source file:** `CCPAURLY.cpy`
**Used by:** `CCPAUTH1` (IMS authorization program)

## Source COBOL

```cobol
     05  PA-RL-CARD-NUM               PIC  X(16).
     05  PA-RL-TRANSACTION-ID         PIC  X(15).
     05  PA-RL-AUTH-ID-CODE           PIC  X(06).
     05  PA-RL-AUTH-RESP-CODE         PIC  X(02).
     05  PA-RL-AUTH-RESP-REASON       PIC  X(04).
     05  PA-RL-APPROVED-AMT           PIC +9(10).99.
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description | Target Column |
|---|------------|-----------|--------|------|-------------|---------------|
| 1 | `PA-RL-CARD-NUM` | `X(16)` | 16 | Alphanumeric | Primary Account Number (PAN) | `CardNumberHash` (VARBINARY(64)) / `CardNumberLast4` (CHAR(4)) |
| 2 | `PA-RL-TRANSACTION-ID` | `X(15)` | 15 | Alphanumeric | Unique transaction identifier | `TransactionId` (NVARCHAR(15)) |
| 3 | `PA-RL-AUTH-ID-CODE` | `X(06)` | 6 | Alphanumeric | Authorization identification code | `AuthorizationIdCode` (NVARCHAR(6)) |
| 4 | `PA-RL-AUTH-RESP-CODE` | `X(02)` | 2 | Alphanumeric | Authorization response code | `ResponseCode` (CHAR(2)) |
| 5 | `PA-RL-AUTH-RESP-REASON` | `X(04)` | 4 | Alphanumeric | Reason code for the authorization decision | `ResponseReason` (NVARCHAR(4)) |
| 6 | `PA-RL-APPROVED-AMT` | `+9(10).99` | 14 | Edited numeric | Approved transaction amount with explicit sign and decimal | `ApprovedAmount` (DECIMAL(12,2)) |

## Field Notes

1. **PA-RL-CARD-NUM** (`PIC X(16)`) -- The full 16-digit Primary Account Number (PAN). **PCI-DSS Requirement 3.4**: This must not be stored in clear text in the target system. During migration, the PAN must be hashed (SHA-256) for `CardNumberHash` and truncated to last 4 digits for `CardNumberLast4`.

2. **PA-RL-TRANSACTION-ID** (`PIC X(15)`) -- Unique identifier for the transaction being authorized. This serves as the correlation key between the authorization request, response, and pending authorization records.

3. **PA-RL-AUTH-ID-CODE** (`PIC X(06)`) -- The authorization identification code assigned by the issuer when the transaction is approved. This code is included in settlement records and must be preserved for reconciliation.

4. **PA-RL-AUTH-RESP-CODE** (`PIC X(02)`) -- Standard authorization response code:

   | Code | Meaning | Action |
   |------|---------|--------|
   | `00` | Approved | Transaction proceeds |
   | `05` | Declined | Do not honor |
   | `51` | Insufficient funds | Decline transaction |
   | `54` | Expired card | Decline transaction |
   | `61` | Exceeds withdrawal limit | Decline transaction |
   | `91` | Issuer unavailable | Retry or decline |

5. **PA-RL-AUTH-RESP-REASON** (`PIC X(04)`) -- Provides additional detail beyond the response code. Used for internal reporting and fraud analysis.

6. **PA-RL-APPROVED-AMT** (`PIC +9(10).99`) -- An **edited numeric** field. Unlike standard COBOL numeric fields (which use implied decimals), this field contains an explicit `+` or `-` sign character and an explicit `.` decimal point. The display format is `+NNNNNNNNNN.NN` (14 characters total: 1 sign + 10 digits + 1 decimal point + 2 digits). Example: `+0000012500.00` represents a positive approved amount of 12,500.00. The sign and decimal are physical characters in the data, not implied.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration | Migration Action |
|-------|---------------------|-----------------|
| `PA-RL-CARD-NUM` | EBCDIC alphanumeric | Convert to ASCII; hash for storage (PCI-DSS) |
| `PA-RL-TRANSACTION-ID` | EBCDIC alphanumeric | Convert to UTF-8; trim spaces |
| `PA-RL-AUTH-ID-CODE` | EBCDIC alphanumeric | Convert to UTF-8; trim spaces |
| `PA-RL-AUTH-RESP-CODE` | EBCDIC alphanumeric | Convert to ASCII; preserve leading zeros |
| `PA-RL-AUTH-RESP-REASON` | EBCDIC alphanumeric | Convert to UTF-8 |
| `PA-RL-APPROVED-AMT` | Edited numeric with explicit sign/decimal | Parse sign character (`+`/`-`), remove leading zeros, parse as decimal. EBCDIC `+` = `X'4E'`, `-` = `X'60'`, `.` = `X'4B'`. |

## Target Architecture Mapping

| Aspect | COBOL/IMS (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Response record** | IMS message segment | C# record/DTO class |
| **PAN handling** | Clear text in memory | Hashed at rest, tokenized in transit |
| **Edited numeric** | `PIC +9(10).99` with physical sign/decimal | `decimal` type parsed from string |
| **Response codes** | Free-text PIC X(02) | Enum or lookup table |

### .NET Equivalent

```csharp
public record AuthorizationResponse
{
    public string CardNumberHash { get; init; } = default!;
    public string CardNumberLast4 { get; init; } = default!;
    public string TransactionId { get; init; } = default!;
    public string AuthorizationIdCode { get; init; } = default!;
    public string ResponseCode { get; init; } = default!;
    public string ResponseReason { get; init; } = default!;
    public decimal ApprovedAmount { get; init; }
}
```

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.AuthorizationResponse (
    ResponseId              BIGINT IDENTITY(1,1) NOT NULL,
    CardNumberHash          VARBINARY(64)       NOT NULL,
    CardNumberLast4         CHAR(4)             NOT NULL,
    TransactionId           NVARCHAR(15)        NOT NULL,
    AuthorizationIdCode     NVARCHAR(6)         NULL,
    ResponseCode            CHAR(2)             NOT NULL,
    ResponseReason          NVARCHAR(4)         NULL,
    ApprovedAmount          DECIMAL(12,2)       NOT NULL DEFAULT 0.00,

    -- Audit columns
    CreatedAt               DATETIME2           NOT NULL DEFAULT SYSUTCDATETIME(),

    CONSTRAINT PK_AuthorizationResponse PRIMARY KEY CLUSTERED (ResponseId),
    CONSTRAINT UQ_AuthResp_TransactionId UNIQUE (TransactionId)
);

-- Index for card-based lookups
CREATE NONCLUSTERED INDEX IX_AuthResp_CardHash
    ON dbo.AuthorizationResponse (CardNumberHash);

-- Index for response code analysis
CREATE NONCLUSTERED INDEX IX_AuthResp_ResponseCode
    ON dbo.AuthorizationResponse (ResponseCode, CreatedAt DESC);
```

### Edited Numeric Parsing

The `PIC +9(10).99` field is an edited numeric, which is unusual in data records (more common in report layouts). Parsing requires:

```csharp
// Parse COBOL edited numeric: "+0000012500.00" -> 12500.00m
public static decimal ParseEditedNumeric(string editedValue)
{
    var trimmed = editedValue.Trim();
    return decimal.Parse(trimmed, NumberStyles.AllowLeadingSign | NumberStyles.AllowDecimalPoint,
        CultureInfo.InvariantCulture);
}
```

### Post-Migration Validation

```sql
-- Row count comparison
SELECT COUNT(*) AS ResponseCount FROM dbo.AuthorizationResponse;

-- Verify PAN is hashed (no clear text)
SELECT COUNT(*) AS ClearPANCount
FROM dbo.AuthorizationResponse
WHERE CardNumberHash IS NULL OR LEN(CardNumberLast4) <> 4;

-- Response code distribution
SELECT ResponseCode, COUNT(*) AS Cnt
FROM dbo.AuthorizationResponse
GROUP BY ResponseCode
ORDER BY Cnt DESC;

-- Approved amount checksum
SELECT SUM(ApprovedAmount) AS TotalApproved
FROM dbo.AuthorizationResponse
WHERE ResponseCode = '00';
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS 3.4** | Render PAN unreadable anywhere it is stored. | `PA-RL-CARD-NUM` must be hashed (SHA-256) before storage. Only last 4 digits stored in clear. No clear-text PAN in logs or temporary storage. |
| **PCI-DSS 10.2** | Audit trails for all access to cardholder data. | All access to `dbo.AuthorizationResponse` must be logged. |
| **PSD2** Art. 97 | Strong Customer Authentication for electronic payments. | Authorization response codes document the SCA outcome. Response records must be retained for regulatory examination. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls over authorization decisions. | Authorization responses must be immutable and retained per FSA requirements. The `ResponseCode` and `ResponseReason` provide the audit trail for each authorization decision. |
| **AML/KYC** (FFFS 2017:11) | Transaction monitoring for suspicious activity. | Patterns of declined authorizations, unusual approved amounts, and response reason codes feed into AML monitoring. |
| **DORA** Art. 11 | ICT data integrity. | Authorization response records must be validated during migration with zero tolerance for amount discrepancies. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
