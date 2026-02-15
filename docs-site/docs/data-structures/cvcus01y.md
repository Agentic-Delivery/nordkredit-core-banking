---
id: DS-CUST-001
title: "Customer Record"
copybook_name: "CVCUS01Y.cpy"
domain: "account-management"
used_by_programs: [COACTUPC, COACTVWC, COCRDLIC, COCRDSLC]
record_length: 500
status: "extracted"
target_schema: "dbo.Customer"
sidebar_position: 5
---

# DS-CUST-001: Customer Record (CVCUS01Y)

## Overview

The `CVCUS01Y.cpy` copybook defines the **Customer Record**, the primary data structure for customer personal information in the CardDemo system. This record is stored in the VSAM file `CUSTDATA` and contains the customer's identity, contact information, government-issued identification, date of birth, credit score, and payment preferences.

This is the most PII-dense record in the system and is subject to the strictest GDPR, AML/KYC, and data protection requirements. It is referenced by account management programs that need customer demographic data for display, update, and regulatory compliance purposes.

**Source file:** `CVCUS01Y.cpy`
**VSAM file:** `CUSTDATA`
**Record length:** 500 bytes
**Used by:** `COACTUPC`, `COACTVWC`, `COCRDLIC`, `COCRDSLC`

## Source COBOL

```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID                                 PIC 9(09).
    05  CUST-FIRST-NAME                         PIC X(25).
    05  CUST-MIDDLE-NAME                        PIC X(25).
    05  CUST-LAST-NAME                          PIC X(25).
    05  CUST-ADDR-LINE-1                        PIC X(50).
    05  CUST-ADDR-LINE-2                        PIC X(50).
    05  CUST-ADDR-LINE-3                        PIC X(50).
    05  CUST-ADDR-STATE-CD                      PIC X(02).
    05  CUST-ADDR-COUNTRY-CD                    PIC X(03).
    05  CUST-ADDR-ZIP                           PIC X(10).
    05  CUST-PHONE-NUM-1                        PIC X(15).
    05  CUST-PHONE-NUM-2                        PIC X(15).
    05  CUST-SSN                                PIC 9(09).
    05  CUST-GOVT-ISSUED-ID                     PIC X(20).
    05  CUST-DOB-YYYY-MM-DD                     PIC X(10).
    05  CUST-EFT-ACCOUNT-ID                     PIC X(10).
    05  CUST-PRI-CARD-HOLDER-IND                PIC X(01).
    05  CUST-FICO-CREDIT-SCORE                  PIC 9(03).
    05  FILLER                                  PIC X(168).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Nullable | Target Column |
|---|------------|-----------|--------|--------|------|-------------|----------|---------------|
| 1 | `CUST-ID` | `9(09)` | 0 | 9 | Numeric (unsigned) | Unique customer identifier | No | `CustomerId` (BIGINT, PK) |
| 2 | `CUST-FIRST-NAME` | `X(25)` | 9 | 25 | Alphanumeric | Customer first name | No | `FirstName` (NVARCHAR(25)) |
| 3 | `CUST-MIDDLE-NAME` | `X(25)` | 34 | 25 | Alphanumeric | Customer middle name | Yes | `MiddleName` (NVARCHAR(25) NULL) |
| 4 | `CUST-LAST-NAME` | `X(25)` | 59 | 25 | Alphanumeric | Customer last name | No | `LastName` (NVARCHAR(25)) |
| 5 | `CUST-ADDR-LINE-1` | `X(50)` | 84 | 50 | Alphanumeric | Address line 1 (street) | No | `AddressLine1` (NVARCHAR(50)) |
| 6 | `CUST-ADDR-LINE-2` | `X(50)` | 134 | 50 | Alphanumeric | Address line 2 (apartment, suite, etc.) | Yes | `AddressLine2` (NVARCHAR(50) NULL) |
| 7 | `CUST-ADDR-LINE-3` | `X(50)` | 184 | 50 | Alphanumeric | Address line 3 (additional address info) | Yes | `AddressLine3` (NVARCHAR(50) NULL) |
| 8 | `CUST-ADDR-STATE-CD` | `X(02)` | 234 | 2 | Alphanumeric | State/province code (e.g., `AB` for Alberta, `SE` for Sweden) | Yes | `StateCode` (NCHAR(2) NULL) |
| 9 | `CUST-ADDR-COUNTRY-CD` | `X(03)` | 236 | 3 | Alphanumeric | ISO 3166-1 alpha-3 country code (e.g., `SWE`, `USA`) | No | `CountryCode` (NCHAR(3)) |
| 10 | `CUST-ADDR-ZIP` | `X(10)` | 239 | 10 | Alphanumeric | ZIP/postal code | Yes | `PostalCode` (NVARCHAR(10) NULL) |
| 11 | `CUST-PHONE-NUM-1` | `X(15)` | 249 | 15 | Alphanumeric | Primary phone number | Yes | `PhoneNumber1` (NVARCHAR(15) NULL) |
| 12 | `CUST-PHONE-NUM-2` | `X(15)` | 264 | 15 | Alphanumeric | Secondary phone number | Yes | `PhoneNumber2` (NVARCHAR(15) NULL) |
| 13 | `CUST-SSN` | `9(09)` | 279 | 9 | Numeric (unsigned) | Social Security Number / Swedish Personal Identity Number (personnummer) | No | `SsnEncrypted` (VARBINARY(256)) |
| 14 | `CUST-GOVT-ISSUED-ID` | `X(20)` | 288 | 20 | Alphanumeric | Government-issued ID number (passport, national ID) | Yes | `GovernmentIdEncrypted` (VARBINARY(256) NULL) |
| 15 | `CUST-DOB-YYYY-MM-DD` | `X(10)` | 308 | 10 | Alphanumeric (date string) | Date of birth (`YYYY-MM-DD`) | No | `DateOfBirth` (DATE) |
| 16 | `CUST-EFT-ACCOUNT-ID` | `X(10)` | 318 | 10 | Alphanumeric | EFT (Electronic Funds Transfer) linked account ID | Yes | `EftAccountId` (NVARCHAR(10) NULL) |
| 17 | `CUST-PRI-CARD-HOLDER-IND` | `X(01)` | 328 | 1 | Alphanumeric | Primary cardholder indicator (`Y`/`N`) | No | `IsPrimaryCardHolder` (BIT) |
| 18 | `CUST-FICO-CREDIT-SCORE` | `9(03)` | 329 | 3 | Numeric (unsigned) | FICO credit score (300-850 range) | Yes | `CreditScore` (SMALLINT NULL) |
| 19 | `FILLER` | `X(168)` | 332 | 168 | Filler | Reserved/unused space | N/A | Not migrated |

**Total record length:** 500 bytes

## Field Notes

1. **CUST-ID** (`PIC 9(09)`): 9-digit numeric customer identifier, zero-padded. This is the primary key for the customer entity. Referenced as a foreign key from `CVACT03Y` (Cross-Reference Record) via `XREF-CUST-ID`. Stored as `BIGINT` in the target schema.

2. **CUST-FIRST-NAME** / **CUST-MIDDLE-NAME** / **CUST-LAST-NAME** (`PIC X(25)` each): Customer name fields. Right-padded with spaces in COBOL. Trailing spaces must be trimmed during migration. Middle name may be all spaces (map to NULL). These are PII fields subject to GDPR right to erasure.

3. **CUST-ADDR-LINE-1** through **CUST-ADDR-LINE-3** (`PIC X(50)` each): Multi-line address. Lines 2 and 3 may be all spaces (map to NULL). Swedish addresses typically use Line 1 for street, Line 2 for c/o or floor, Line 3 is rarely used.

4. **CUST-ADDR-STATE-CD** (`PIC X(02)`): Two-character state/province code. For Swedish customers, this may contain a county code (lan) or be blank, as Sweden does not use state codes in the same way as the US.

5. **CUST-ADDR-COUNTRY-CD** (`PIC X(03)`): ISO 3166-1 alpha-3 country code. NordKredit customers are primarily Swedish (`SWE`). This field is critical for GDPR data residency compliance -- all customer data for EU residents must remain within EU data centers.

6. **CUST-ADDR-ZIP** (`PIC X(10)`): Postal code. Swedish format: 5 digits, optionally with a space after the third digit (e.g., `114 55` or `11455`). US format: 5 or 9 digits (e.g., `12345` or `12345-6789`).

7. **CUST-PHONE-NUM-1** / **CUST-PHONE-NUM-2** (`PIC X(15)` each): Phone numbers stored as strings. May include country code prefix (e.g., `+46701234567` for Swedish mobile). No format enforcement in COBOL -- validation must be added in the .NET layer.

8. **CUST-SSN** (`PIC 9(09)`): Social Security Number (US) or Swedish Personal Identity Number (personnummer, 10 or 12 digits -- the 9-digit field may store the core digits without the century prefix). **This is the most sensitive field in the entire system.** Must be encrypted at rest in the target database (Azure SQL Always Encrypted or Transparent Data Encryption with column-level encryption). Must never appear in logs, error messages, or API responses.

9. **CUST-GOVT-ISSUED-ID** (`PIC X(20)`): Government-issued identification number (passport number, national ID card number). This is a secondary identity verification field used for AML/KYC compliance. Must be encrypted at rest in the target system.

10. **CUST-DOB-YYYY-MM-DD** (`PIC X(10)`): Date of birth stored as a string in `YYYY-MM-DD` format. PII under GDPR. Used for age verification and identity confirmation. Combined with name, this is a quasi-identifier that could re-identify anonymized data.

11. **CUST-EFT-ACCOUNT-ID** (`PIC X(10)`): External bank account identifier for electronic funds transfers (e.g., direct debit, payment disbursement). May reference a Bankgirot or SWIFT account. May be all spaces if no EFT account is linked.

12. **CUST-PRI-CARD-HOLDER-IND** (`PIC X(01)`): Indicates whether this customer is the primary cardholder (`Y`) or an authorized user/supplementary cardholder (`N`). This distinction affects liability and communication preferences.

13. **CUST-FICO-CREDIT-SCORE** (`PIC 9(03)`): 3-digit integer representing the customer's credit score. Valid range is typically 300-850. A value of `000` may indicate "not scored" or "score unavailable" -- map to NULL in the target system. This is used for credit limit decisions and risk assessment.

14. **FILLER** (`PIC X(168)`): Reserved space to pad the record to 500 bytes. Not migrated.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration | Migration Action |
|-------|---------------------|-----------------|
| `CUST-ID` | EBCDIC zoned decimal `F0`-`F9` | Convert to ASCII digits; cast to BIGINT |
| `CUST-FIRST-NAME`, `CUST-MIDDLE-NAME`, `CUST-LAST-NAME` | EBCDIC alphanumeric; Swedish characters (a, o, a with diacritics) may use EBCDIC codepage 278 (Swedish/Finnish) | Convert using EBCDIC codepage 278 to UTF-8; preserve Swedish characters (a-ring, a-diaeresis, o-diaeresis); trim trailing spaces |
| `CUST-ADDR-LINE-1` through `CUST-ADDR-LINE-3` | EBCDIC alphanumeric with potential Swedish characters | Convert using EBCDIC codepage 278 to UTF-8; trim trailing spaces; map all-spaces to NULL |
| `CUST-ADDR-STATE-CD`, `CUST-ADDR-COUNTRY-CD` | EBCDIC uppercase letters | Convert to ASCII; validate against ISO code lists |
| `CUST-ADDR-ZIP` | EBCDIC alphanumeric | Convert to ASCII; trim trailing spaces |
| `CUST-PHONE-NUM-1`, `CUST-PHONE-NUM-2` | EBCDIC alphanumeric (digits, `+`, `-`, spaces) | Convert to ASCII; trim trailing spaces; map all-spaces to NULL |
| `CUST-SSN` | EBCDIC zoned decimal | Convert to ASCII digits; **encrypt immediately** after conversion -- do not store in clear text at any intermediate stage |
| `CUST-GOVT-ISSUED-ID` | EBCDIC alphanumeric | Convert to UTF-8; **encrypt immediately** after conversion |
| `CUST-DOB-YYYY-MM-DD` | EBCDIC date string | Convert to ASCII; parse as `YYYY-MM-DD`; validate date range (no future dates, reasonable birth year) |
| `CUST-EFT-ACCOUNT-ID` | EBCDIC alphanumeric | Convert to ASCII; trim trailing spaces; map all-spaces to NULL |
| `CUST-PRI-CARD-HOLDER-IND` | EBCDIC single character | Convert to ASCII; map `Y`/`N` to BIT 1/0 |
| `CUST-FICO-CREDIT-SCORE` | EBCDIC zoned decimal | Convert to ASCII digits; cast to SMALLINT; map `000` to NULL |
| `FILLER` | May contain any byte values | Discard entirely |

**Critical note on codepage:** NordKredit's mainframe uses EBCDIC codepage 278 (Swedish/Finnish EBCDIC). This codepage maps Swedish characters differently from the standard US EBCDIC codepage 037. The migration ETL must use codepage 278 for all text field conversions to preserve characters such as:
- `a` with ring above (a-ring)
- `a` with diaeresis (a-umlaut)
- `o` with diaeresis (o-umlaut)

## Referential Integrity

```
CVCUS01Y (CUSTOMER-RECORD)
    CUST-ID (PK)
        |
        +--< CVACT03Y.XREF-CUST-ID (FK) -- Cross-reference records linking customer to accounts/cards
```

| Relationship | Source Field | Target Entity | Target Field | Cardinality | Constraint |
|-------------|-------------|---------------|-------------|-------------|------------|
| Customer -> Cross-Ref | `CUST-ID` | `CVACT03Y` (Cross-Reference) | `XREF-CUST-ID` | 1:N | A customer can have multiple cross-reference entries (multiple accounts/cards) |

**Note:** The customer record does not directly reference accounts or cards -- the relationship is established through the cross-reference record (`CVACT03Y`). This design allows a customer to have multiple accounts and multiple cards.

## Sample Data

| Field | Example Value (Display) | Raw COBOL Value | Notes |
|-------|------------------------|----------------|-------|
| `CUST-ID` | `000000001` | `000000001` | 9-digit zero-padded |
| `CUST-FIRST-NAME` | `ERIK` | `ERIK                     ` | Right-padded to 25 chars |
| `CUST-MIDDLE-NAME` | `ANDERS` | `ANDERS                   ` | Right-padded to 25 chars |
| `CUST-LAST-NAME` | `LINDQVIST` | `LINDQVIST                ` | Right-padded to 25 chars |
| `CUST-ADDR-LINE-1` | `STORGATAN 15` | `STORGATAN 15                                      ` | Swedish street address |
| `CUST-ADDR-LINE-2` | `LGH 1204` | `LGH 1204                                          ` | Apartment number |
| `CUST-ADDR-LINE-3` | *(empty)* | `                                                  ` | All spaces = NULL |
| `CUST-ADDR-STATE-CD` | *(empty)* | `  ` | Not used for Swedish addresses |
| `CUST-ADDR-COUNTRY-CD` | `SWE` | `SWE` | ISO 3166-1 alpha-3 |
| `CUST-ADDR-ZIP` | `114 55` | `114 55    ` | Swedish postal code |
| `CUST-PHONE-NUM-1` | `+46701234567` | `+46701234567   ` | Swedish mobile |
| `CUST-PHONE-NUM-2` | `+4686543210` | `+4686543210    ` | Swedish landline |
| `CUST-SSN` | `198503125` | `198503125` | **Sensitive** -- shown for documentation only |
| `CUST-GOVT-ISSUED-ID` | `SE-PP-12345678` | `SE-PP-12345678      ` | Swedish passport number |
| `CUST-DOB-YYYY-MM-DD` | `1985-03-12` | `1985-03-12` | ISO date string |
| `CUST-EFT-ACCOUNT-ID` | `BGC1234567` | `BGC1234567` | Bankgirot clearing number |
| `CUST-PRI-CARD-HOLDER-IND` | `Y` | `Y` | Primary cardholder |
| `CUST-FICO-CREDIT-SCORE` | `742` | `742` | Good credit score |

## Migration Notes

### Target DDL (Azure SQL)

```sql
CREATE TABLE dbo.Customer (
    CustomerId              BIGINT          NOT NULL,
    FirstName               NVARCHAR(25)    NOT NULL,
    MiddleName              NVARCHAR(25)    NULL,
    LastName                NVARCHAR(25)    NOT NULL,
    AddressLine1            NVARCHAR(50)    NOT NULL,
    AddressLine2            NVARCHAR(50)    NULL,
    AddressLine3            NVARCHAR(50)    NULL,
    StateCode               NCHAR(2)        NULL,
    CountryCode             NCHAR(3)        NOT NULL DEFAULT 'SWE',
    PostalCode              NVARCHAR(10)    NULL,
    PhoneNumber1            NVARCHAR(15)    NULL,
    PhoneNumber2            NVARCHAR(15)    NULL,
    SsnEncrypted            VARBINARY(256)  NOT NULL,
    SsnLast4                CHAR(4)         NOT NULL,
    GovernmentIdEncrypted   VARBINARY(256)  NULL,
    DateOfBirth             DATE            NOT NULL,
    EftAccountId            NVARCHAR(10)    NULL,
    IsPrimaryCardHolder     BIT             NOT NULL DEFAULT 1,
    CreditScore             SMALLINT        NULL,

    -- Audit columns (not in COBOL source)
    CreatedAt               DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    UpdatedAt               DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    CreatedBy               NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,
    UpdatedBy               NVARCHAR(100)   NOT NULL DEFAULT SYSTEM_USER,

    CONSTRAINT PK_Customer PRIMARY KEY CLUSTERED (CustomerId),
    CONSTRAINT CK_Customer_CreditScore CHECK (CreditScore IS NULL OR (CreditScore >= 300 AND CreditScore <= 850)),
    CONSTRAINT CK_Customer_CountryCode CHECK (LEN(CountryCode) = 3)
);

-- Index for name-based searches (customer lookup)
CREATE NONCLUSTERED INDEX IX_Customer_LastName_FirstName
    ON dbo.Customer (LastName, FirstName);

-- Index for SSN lookup (encrypted -- used with Always Encrypted deterministic encryption)
CREATE NONCLUSTERED INDEX IX_Customer_SsnEncrypted
    ON dbo.Customer (SsnEncrypted);

-- Index for country-based queries (regulatory reporting)
CREATE NONCLUSTERED INDEX IX_Customer_CountryCode
    ON dbo.Customer (CountryCode);
```

### Column-Level Encryption Strategy

The following fields require encryption at rest using Azure SQL Always Encrypted:

| Field | Encryption Type | Rationale |
|-------|----------------|-----------|
| `SsnEncrypted` | Deterministic | SSN must be encrypted but needs equality lookup capability for KYC checks |
| `GovernmentIdEncrypted` | Randomized | Government ID needs stronger protection; equality lookups not required |

**Implementation notes:**
- Use Azure Key Vault to manage Column Master Keys (CMK).
- The application (ASP.NET) handles encryption/decryption via the Always Encrypted driver -- the database server never sees clear-text values.
- `SsnLast4` stores the last 4 digits of the SSN in clear text for display purposes (similar to PAN masking for cards).

### Post-Migration Validation Queries

```sql
-- Row count comparison
SELECT COUNT(*) AS CustomerCount FROM dbo.Customer;

-- Verify no NULL primary keys or required fields
SELECT COUNT(*) AS NullRequiredCount
FROM dbo.Customer
WHERE CustomerId IS NULL
   OR FirstName IS NULL
   OR LastName IS NULL
   OR SsnEncrypted IS NULL
   OR DateOfBirth IS NULL;

-- Verify credit score range
SELECT COUNT(*) AS InvalidScoreCount
FROM dbo.Customer
WHERE CreditScore IS NOT NULL
  AND (CreditScore < 300 OR CreditScore > 850);

-- Verify country code distribution
SELECT CountryCode, COUNT(*) AS Cnt
FROM dbo.Customer
GROUP BY CountryCode
ORDER BY Cnt DESC;

-- Verify primary cardholder distribution
SELECT IsPrimaryCardHolder, COUNT(*) AS Cnt
FROM dbo.Customer
GROUP BY IsPrimaryCardHolder;

-- Verify no orphaned customers (every customer has at least one xref)
SELECT c.CustomerId
FROM dbo.Customer c
LEFT JOIN dbo.CustomerAccountCardXref x ON c.CustomerId = x.CustomerId
WHERE x.CustomerId IS NULL;

-- Verify date of birth validity (no future dates, reasonable range)
SELECT COUNT(*) AS InvalidDobCount
FROM dbo.Customer
WHERE DateOfBirth > GETDATE()
   OR DateOfBirth < '1900-01-01';
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **GDPR** Art. 5(1)(f) | Integrity and confidentiality of personal data | All PII fields (name, address, phone, SSN, DOB, government ID) must be protected with appropriate technical measures. SSN and government ID require encryption at rest. Access to customer data must be logged. |
| **GDPR** Art. 17 | Right to erasure (right to be forgotten) | When a customer exercises right to erasure, all personal data must be anonymized or deleted. Financial records required by the Swedish Bookkeeping Act (7 years) must be retained but de-identified: replace name with "ANONYMIZED", zero out address/phone/DOB, retain only the encrypted SSN hash for audit linkage. |
| **GDPR** Art. 15 | Right of access (data subject access request) | The system must be able to export all personal data for a given `CustomerId` in a structured, machine-readable format. This includes the customer record and all related records via the cross-reference table. |
| **GDPR** Art. 20 | Right to data portability | Customer data must be exportable in a common format (JSON, CSV) on request. The .NET API must provide a data export endpoint gated by SCA. |
| **GDPR** Art. 44-49 | Transfer of personal data to third countries | `CountryCode` and data residency must be checked. Customer data for EU residents must not be stored outside the EU. Azure SQL must be deployed in an EU region (e.g., North Europe, West Europe). |
| **AML/KYC** (FFFS 2017:11) | Customer due diligence | `CUST-SSN`, `CUST-GOVT-ISSUED-ID`, `CUST-DOB-YYYY-MM-DD`, and name/address are required for KYC verification. These fields must be complete and validated for all customers. Missing or invalid KYC data must trigger an alert. |
| **AML/KYC** (FFFS 2017:11) | Ongoing monitoring | Changes to customer identity fields (name, SSN, government ID, DOB) must be flagged for AML review. The audit trail must capture before/after values for all identity field modifications. |
| **FSA (FFFS 2014:5)** Ch. 4 | Risk management -- credit scoring | `CUST-FICO-CREDIT-SCORE` is used for credit risk assessment. Changes to credit scores must be tracked and the scoring methodology must be documented for regulatory review. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls | All customer record modifications must be audit-logged with field-level change tracking, authenticated user identity, and timestamp. |
| **PSD2** Art. 97 | Strong Customer Authentication | Access to customer personal data via the .NET API must require SCA. Read access to sensitive fields (SSN, government ID) must require elevated authentication. |
| **DORA** Art. 11 | ICT data integrity | Customer data migration must be validated field-by-field. Swedish character preservation (EBCDIC codepage 278 to UTF-8) must be verified for every name and address field. Zero tolerance for data corruption in identity fields. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
