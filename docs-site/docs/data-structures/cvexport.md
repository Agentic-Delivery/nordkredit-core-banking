---
id: DS-EXPRT-001
title: "Export Record"
copybook_name: "CVEXPORT.cpy"
domain: "account-management"
used_by_programs: [CBEXP01C]
record_length: 500
status: "extracted"
target_schema: "N/A (export format)"
sidebar_position: 13
---

# DS-EXPRT-001: Export Record (CVEXPORT)

## Overview

The `CVEXPORT.cpy` copybook defines the **Export Record**, a multi-record type structure used for branch migration data exports. This is the most complex copybook in the CardDemo system, using COBOL `REDEFINES` clauses to overlay multiple record layouts onto the same storage area. The `EXPORT-REC-TYPE` field determines which layout applies for a given record.

The export structure supports five record types:
1. **Customer data** -- demographic and identity information
2. **Account data** -- account financial details
3. **Transaction data** -- transaction records
4. **Card cross-reference data** -- card-to-account mappings
5. **Card data** -- card details

This structure also contains `COMP` (binary) and `COMP-3` (packed decimal) fields, which require special handling during EBCDIC-to-Unicode conversion due to endianness and BCD encoding differences.

**Source file:** `CVEXPORT.cpy`
**Record length:** 500 bytes
**Used by:** `CBEXP01C`
**Output:** Branch migration export file

## Source COBOL

### Base Record (Header)

```cobol
01  EXPORT-RECORD.
    05  EXPORT-REC-TYPE                         PIC X(01).
       88  EXPORT-REC-CUSTOMER                  VALUE 'C'.
       88  EXPORT-REC-ACCOUNT                   VALUE 'A'.
       88  EXPORT-REC-TRANSACTION               VALUE 'T'.
       88  EXPORT-REC-CARD-XREF                 VALUE 'X'.
       88  EXPORT-REC-CARD                      VALUE 'D'.
    05  EXPORT-TIMESTAMP.
       10  EXPORT-TIMESTAMP-STR                 PIC X(26).
    05  EXPORT-TIMESTAMP-R REDEFINES EXPORT-TIMESTAMP.
       10  EXPORT-TS-YYYY                       PIC 9(04).
       10  EXPORT-TS-SEP1                       PIC X(01).
       10  EXPORT-TS-MM                         PIC 9(02).
       10  EXPORT-TS-SEP2                       PIC X(01).
       10  EXPORT-TS-DD                         PIC 9(02).
       10  EXPORT-TS-SEP3                       PIC X(01).
       10  EXPORT-TS-HH                         PIC 9(02).
       10  EXPORT-TS-SEP4                       PIC X(01).
       10  EXPORT-TS-MI                         PIC 9(02).
       10  EXPORT-TS-SEP5                       PIC X(01).
       10  EXPORT-TS-SS                         PIC 9(02).
       10  EXPORT-TS-SEP6                       PIC X(01).
       10  EXPORT-TS-FFFFFF                     PIC 9(06).
    05  EXPORT-SEQUENCE-NUM                     PIC 9(09) COMP.
    05  EXPORT-BRANCH-ID                        PIC X(04).
    05  EXPORT-REGION-CODE                      PIC X(05).
    05  EXPORT-RECORD-DATA                      PIC X(460).
```

### Customer Data (REDEFINES EXPORT-RECORD-DATA)

```cobol
    05  EXPORT-CUSTOMER-DATA
            REDEFINES EXPORT-RECORD-DATA.
       10  EXPORT-CUST-ID                       PIC 9(09) COMP.
       10  EXPORT-CUST-SSN                      PIC X(09).
       10  EXPORT-CUST-FIRST-NAME               PIC X(25).
       10  EXPORT-CUST-MIDDLE-NAME              PIC X(25).
       10  EXPORT-CUST-LAST-NAME                PIC X(25).
       10  EXPORT-CUST-ADDR-LINE-1              PIC X(50).
       10  EXPORT-CUST-ADDR-LINE-2              PIC X(50).
       10  EXPORT-CUST-ADDR-CITY                PIC X(50).
       10  EXPORT-CUST-ADDR-STATE               PIC X(02).
       10  EXPORT-CUST-ADDR-COUNTRY             PIC X(03).
       10  EXPORT-CUST-ADDR-ZIP                 PIC X(10).
       10  EXPORT-CUST-PHONE-1                  PIC X(15).
       10  EXPORT-CUST-PHONE-2                  PIC X(15).
       10  EXPORT-CUST-DOB                      PIC X(10).
       10  EXPORT-CUST-FICO-SCORE               PIC 9(03) COMP-3.
       10  EXPORT-CUST-EFF-DATE                 PIC X(10).
       10  EXPORT-CUST-EXP-DATE                 PIC X(10).
       10  FILLER                               PIC X(109).
```

### Account Data (REDEFINES EXPORT-RECORD-DATA)

```cobol
    05  EXPORT-ACCOUNT-DATA
            REDEFINES EXPORT-RECORD-DATA.
       10  EXPORT-ACCT-ID                       PIC 9(11) COMP-3.
       10  EXPORT-ACCT-STATUS                   PIC X(01).
       10  EXPORT-ACCT-CREDIT-LIMIT             PIC S9(09)V99 COMP-3.
       10  EXPORT-ACCT-CASH-LIMIT               PIC S9(09)V99 COMP-3.
       10  EXPORT-ACCT-CURRENT-BAL              PIC S9(09)V99 COMP-3.
       10  EXPORT-ACCT-OPEN-DATE                PIC X(10).
       10  EXPORT-ACCT-EXP-DATE                 PIC X(10).
       10  EXPORT-ACCT-REISSUE-DATE             PIC X(10).
       10  EXPORT-ACCT-GROUP-ID                 PIC X(10).
       10  EXPORT-ACCT-CUST-ID                  PIC 9(09) COMP.
       10  FILLER                               PIC X(393).
```

### Transaction Data (REDEFINES EXPORT-RECORD-DATA)

```cobol
    05  EXPORT-TRANSACTION-DATA
            REDEFINES EXPORT-RECORD-DATA.
       10  EXPORT-TRAN-ID                       PIC X(16).
       10  EXPORT-TRAN-TYPE-CD                  PIC X(02).
       10  EXPORT-TRAN-CAT-CD                   PIC 9(04) COMP-3.
       10  EXPORT-TRAN-SOURCE                   PIC X(10).
       10  EXPORT-TRAN-DESC                     PIC X(100).
       10  EXPORT-TRAN-AMT                      PIC S9(09)V99 COMP-3.
       10  EXPORT-TRAN-MERCHANT-ID              PIC 9(09) COMP.
       10  EXPORT-TRAN-MERCHANT-NAME            PIC X(50).
       10  EXPORT-TRAN-MERCHANT-CITY            PIC X(50).
       10  EXPORT-TRAN-MERCHANT-ZIP             PIC X(10).
       10  EXPORT-TRAN-CARD-NUM                 PIC X(16).
       10  EXPORT-TRAN-ORIG-TS                  PIC X(26).
       10  EXPORT-TRAN-PROC-TS                  PIC X(26).
       10  FILLER                               PIC X(137).
```

### Card Cross-Reference Data (REDEFINES EXPORT-RECORD-DATA)

```cobol
    05  EXPORT-CARD-XREF-DATA
            REDEFINES EXPORT-RECORD-DATA.
       10  EXPORT-XREF-CARD-NUM                 PIC X(16).
       10  EXPORT-XREF-ACCT-ID                  PIC 9(11) COMP.
       10  EXPORT-XREF-CUST-ID                  PIC 9(09) COMP.
       10  FILLER                               PIC X(429).
```

### Card Data (REDEFINES EXPORT-RECORD-DATA)

```cobol
    05  EXPORT-CARD-DATA
            REDEFINES EXPORT-RECORD-DATA.
       10  EXPORT-CARD-NUM                      PIC X(16).
       10  EXPORT-CARD-ACCT-ID                  PIC 9(11) COMP.
       10  EXPORT-CARD-CVV-CD                   PIC 9(03) COMP.
       10  EXPORT-CARD-STATUS                   PIC X(01).
       10  EXPORT-CARD-EMBOSSED-NAME            PIC X(50).
       10  EXPORT-CARD-EXP-DATE                 PIC X(10).
       10  EXPORT-CARD-ACTIVE-DATE              PIC X(10).
       10  FILLER                               PIC X(357).
```

## Field Definitions

### Base Record (Header) -- All Record Types

| # | Field Name | PIC Clause | Type | Offset | Length | Description |
|---|------------|-----------|------|--------|--------|-------------|
| 1 | `EXPORT-REC-TYPE` | `X(01)` | Alphanumeric | 0 | 1 | Record type discriminator (C/A/T/X/D) |
| 2 | `EXPORT-TIMESTAMP-STR` | `X(26)` | Alphanumeric | 1 | 26 | Export timestamp (ISO-like format) |
| 3 | `EXPORT-SEQUENCE-NUM` | `9(09) COMP` | Binary (4 bytes) | 27 | 4 | Sequence number within export |
| 4 | `EXPORT-BRANCH-ID` | `X(04)` | Alphanumeric | 31 | 4 | Branch identifier |
| 5 | `EXPORT-REGION-CODE` | `X(05)` | Alphanumeric | 35 | 5 | Region code |
| 6 | `EXPORT-RECORD-DATA` | `X(460)` | Alphanumeric | 40 | 460 | Record data area (layout depends on REC-TYPE) |

### Customer Data (REC-TYPE = 'C') -- Within EXPORT-RECORD-DATA

| # | Field Name | PIC Clause | Type | Offset* | Length | Description |
|---|------------|-----------|------|---------|--------|-------------|
| 1 | `EXPORT-CUST-ID` | `9(09) COMP` | Binary (4 bytes) | 0 | 4 | Customer identifier |
| 2 | `EXPORT-CUST-SSN` | `X(09)` | Alphanumeric | 4 | 9 | Social Security Number (PII) |
| 3 | `EXPORT-CUST-FIRST-NAME` | `X(25)` | Alphanumeric | 13 | 25 | First name |
| 4 | `EXPORT-CUST-MIDDLE-NAME` | `X(25)` | Alphanumeric | 38 | 25 | Middle name |
| 5 | `EXPORT-CUST-LAST-NAME` | `X(25)` | Alphanumeric | 63 | 25 | Last name |
| 6 | `EXPORT-CUST-ADDR-LINE-1` | `X(50)` | Alphanumeric | 88 | 50 | Address line 1 |
| 7 | `EXPORT-CUST-ADDR-LINE-2` | `X(50)` | Alphanumeric | 138 | 50 | Address line 2 |
| 8 | `EXPORT-CUST-ADDR-CITY` | `X(50)` | Alphanumeric | 188 | 50 | City |
| 9 | `EXPORT-CUST-ADDR-STATE` | `X(02)` | Alphanumeric | 238 | 2 | State/province code |
| 10 | `EXPORT-CUST-ADDR-COUNTRY` | `X(03)` | Alphanumeric | 240 | 3 | Country code |
| 11 | `EXPORT-CUST-ADDR-ZIP` | `X(10)` | Alphanumeric | 243 | 10 | ZIP/postal code |
| 12 | `EXPORT-CUST-PHONE-1` | `X(15)` | Alphanumeric | 253 | 15 | Primary phone |
| 13 | `EXPORT-CUST-PHONE-2` | `X(15)` | Alphanumeric | 268 | 15 | Secondary phone |
| 14 | `EXPORT-CUST-DOB` | `X(10)` | Alphanumeric | 283 | 10 | Date of birth |
| 15 | `EXPORT-CUST-FICO-SCORE` | `9(03) COMP-3` | Packed decimal (2 bytes) | 293 | 2 | FICO credit score |
| 16 | `EXPORT-CUST-EFF-DATE` | `X(10)` | Alphanumeric | 295 | 10 | Effective date |
| 17 | `EXPORT-CUST-EXP-DATE` | `X(10)` | Alphanumeric | 305 | 10 | Expiration date |
| 18 | `FILLER` | `X(109)` | Filler | 315 | 109 | Reserved |

*Offsets are relative to the start of `EXPORT-RECORD-DATA` (byte 40 of the full record).

### Account Data (REC-TYPE = 'A') -- Within EXPORT-RECORD-DATA

| # | Field Name | PIC Clause | Type | Offset* | Length | Description |
|---|------------|-----------|------|---------|--------|-------------|
| 1 | `EXPORT-ACCT-ID` | `9(11) COMP-3` | Packed decimal (6 bytes) | 0 | 6 | Account identifier |
| 2 | `EXPORT-ACCT-STATUS` | `X(01)` | Alphanumeric | 6 | 1 | Account status |
| 3 | `EXPORT-ACCT-CREDIT-LIMIT` | `S9(09)V99 COMP-3` | Packed decimal (6 bytes) | 7 | 6 | Credit limit |
| 4 | `EXPORT-ACCT-CASH-LIMIT` | `S9(09)V99 COMP-3` | Packed decimal (6 bytes) | 13 | 6 | Cash credit limit |
| 5 | `EXPORT-ACCT-CURRENT-BAL` | `S9(09)V99 COMP-3` | Packed decimal (6 bytes) | 19 | 6 | Current balance |
| 6 | `EXPORT-ACCT-OPEN-DATE` | `X(10)` | Alphanumeric | 25 | 10 | Account open date |
| 7 | `EXPORT-ACCT-EXP-DATE` | `X(10)` | Alphanumeric | 35 | 10 | Account expiration date |
| 8 | `EXPORT-ACCT-REISSUE-DATE` | `X(10)` | Alphanumeric | 45 | 10 | Reissue date |
| 9 | `EXPORT-ACCT-GROUP-ID` | `X(10)` | Alphanumeric | 55 | 10 | Discount group identifier |
| 10 | `EXPORT-ACCT-CUST-ID` | `9(09) COMP` | Binary (4 bytes) | 65 | 4 | Customer identifier (FK) |
| 11 | `FILLER` | `X(393)` | Filler | 69 | 393 | Reserved |

### Transaction Data (REC-TYPE = 'T') -- Within EXPORT-RECORD-DATA

| # | Field Name | PIC Clause | Type | Offset* | Length | Description |
|---|------------|-----------|------|---------|--------|-------------|
| 1 | `EXPORT-TRAN-ID` | `X(16)` | Alphanumeric | 0 | 16 | Transaction identifier |
| 2 | `EXPORT-TRAN-TYPE-CD` | `X(02)` | Alphanumeric | 16 | 2 | Transaction type code |
| 3 | `EXPORT-TRAN-CAT-CD` | `9(04) COMP-3` | Packed decimal (3 bytes) | 18 | 3 | Transaction category code |
| 4 | `EXPORT-TRAN-SOURCE` | `X(10)` | Alphanumeric | 21 | 10 | Transaction source |
| 5 | `EXPORT-TRAN-DESC` | `X(100)` | Alphanumeric | 31 | 100 | Transaction description |
| 6 | `EXPORT-TRAN-AMT` | `S9(09)V99 COMP-3` | Packed decimal (6 bytes) | 131 | 6 | Transaction amount |
| 7 | `EXPORT-TRAN-MERCHANT-ID` | `9(09) COMP` | Binary (4 bytes) | 137 | 4 | Merchant identifier |
| 8 | `EXPORT-TRAN-MERCHANT-NAME` | `X(50)` | Alphanumeric | 141 | 50 | Merchant name |
| 9 | `EXPORT-TRAN-MERCHANT-CITY` | `X(50)` | Alphanumeric | 191 | 50 | Merchant city |
| 10 | `EXPORT-TRAN-MERCHANT-ZIP` | `X(10)` | Alphanumeric | 241 | 10 | Merchant ZIP |
| 11 | `EXPORT-TRAN-CARD-NUM` | `X(16)` | Alphanumeric | 251 | 16 | Card number (PAN) |
| 12 | `EXPORT-TRAN-ORIG-TS` | `X(26)` | Alphanumeric | 267 | 26 | Origination timestamp |
| 13 | `EXPORT-TRAN-PROC-TS` | `X(26)` | Alphanumeric | 293 | 26 | Processing timestamp |
| 14 | `FILLER` | `X(137)` | Filler | 319 | 137 | Reserved |

### Card Cross-Reference Data (REC-TYPE = 'X') -- Within EXPORT-RECORD-DATA

| # | Field Name | PIC Clause | Type | Offset* | Length | Description |
|---|------------|-----------|------|---------|--------|-------------|
| 1 | `EXPORT-XREF-CARD-NUM` | `X(16)` | Alphanumeric | 0 | 16 | Card number (PAN) |
| 2 | `EXPORT-XREF-ACCT-ID` | `9(11) COMP` | Binary (4 bytes) | 16 | 4 | Account identifier |
| 3 | `EXPORT-XREF-CUST-ID` | `9(09) COMP` | Binary (4 bytes) | 20 | 4 | Customer identifier |
| 4 | `FILLER` | `X(429)` | Filler | 24 | 429 | Reserved |

### Card Data (REC-TYPE = 'D') -- Within EXPORT-RECORD-DATA

| # | Field Name | PIC Clause | Type | Offset* | Length | Description |
|---|------------|-----------|------|---------|--------|-------------|
| 1 | `EXPORT-CARD-NUM` | `X(16)` | Alphanumeric | 0 | 16 | Card number (PAN) |
| 2 | `EXPORT-CARD-ACCT-ID` | `9(11) COMP` | Binary (4 bytes) | 16 | 4 | Account identifier |
| 3 | `EXPORT-CARD-CVV-CD` | `9(03) COMP` | Binary (2 bytes) | 20 | 2 | CVV code |
| 4 | `EXPORT-CARD-STATUS` | `X(01)` | Alphanumeric | 22 | 1 | Card status |
| 5 | `EXPORT-CARD-EMBOSSED-NAME` | `X(50)` | Alphanumeric | 23 | 50 | Embossed cardholder name |
| 6 | `EXPORT-CARD-EXP-DATE` | `X(10)` | Alphanumeric | 73 | 10 | Card expiration date |
| 7 | `EXPORT-CARD-ACTIVE-DATE` | `X(10)` | Alphanumeric | 83 | 10 | Card activation date |
| 8 | `FILLER` | `X(357)` | Filler | 93 | 357 | Reserved |

## Field Notes

1. **EXPORT-REC-TYPE** -- Single-character record type discriminator. The 88-level condition names define the valid values: `C` = Customer, `A` = Account, `T` = Transaction, `X` = Card Cross-Reference, `D` = Card. The REDEFINES structure means the interpretation of `EXPORT-RECORD-DATA` depends entirely on this field.

2. **EXPORT-TIMESTAMP / EXPORT-TIMESTAMP-R** -- The timestamp is stored as a 26-character string and can also be accessed via the REDEFINES as individual date/time components (YYYY-MM-DD-HH.MI.SS.FFFFFF). The REDEFINES provides convenient access to individual components without substring operations.

3. **EXPORT-SEQUENCE-NUM** -- `COMP` (binary) 4-byte field. On z/OS, this is big-endian. Must be converted to little-endian for .NET/Azure SQL. Value range: 0 to 999,999,999.

4. **COMP fields** -- Binary integer fields. On IBM z/OS: `PIC 9(09) COMP` = 4 bytes big-endian unsigned; `PIC 9(11) COMP` = 4 bytes (though 11 digits exceeds 4-byte unsigned range, verify actual storage). These require endian conversion during migration.

5. **COMP-3 fields** -- Packed decimal (BCD) fields. Each digit occupies a half-byte (nibble), with the sign in the last nibble. `PIC S9(09)V99 COMP-3` occupies 6 bytes: 11 digits + 1 sign nibble = 12 nibbles = 6 bytes. The `V` is an implied decimal; no physical decimal separator exists.

6. **EXPORT-CUST-SSN** -- Swedish personal identity number (personnummer). **GDPR sensitive PII.** Must be encrypted in transit and at rest.

7. **EXPORT-TRAN-CARD-NUM / EXPORT-XREF-CARD-NUM / EXPORT-CARD-NUM** -- Card PANs appear in three record types. **PCI-DSS sensitive.** Must be encrypted in the export file and during transmission.

8. **EXPORT-CARD-CVV-CD** -- Card Verification Value. **PCI-DSS: CVV must never be stored after authorization.** This field's presence in the export suggests it is used only for initial card provisioning. The .NET system must NOT persist CVV data after card activation.

## EBCDIC Encoding Notes

| Field Type | EBCDIC Consideration |
|-----------|---------------------|
| `PIC X` fields | Standard EBCDIC-to-UTF-8 conversion. Swedish CCSID 278 for text fields (a-ring, a-umlaut, o-umlaut). |
| `PIC 9 COMP` | **Big-endian binary integer.** Must convert from big-endian (z/OS) to little-endian (.NET). Example: `EXPORT-SEQUENCE-NUM` stored as `00 00 00 2A` (big-endian) = 42 decimal. |
| `PIC 9 COMP-3` | **Packed decimal (BCD).** Each byte contains 2 digits (one per nibble). Last nibble is sign: `C` = positive, `D` = negative, `F` = unsigned. Example: `EXPORT-ACCT-CREDIT-LIMIT` value `12345.67` stored as `01 23 45 67 0C` (6 bytes with sign). |
| Timestamp | EBCDIC character timestamp. Convert characters, then parse to `DATETIME2`. |
| SSN | EBCDIC characters. Convert to UTF-8. **Encrypt immediately after conversion.** |
| PAN fields | EBCDIC characters. Convert to UTF-8. **Encrypt immediately after conversion. Do not log or write plaintext PAN.** |

### COMP-3 Conversion Algorithm

```
For each byte in the COMP-3 field:
  - High nibble (bits 7-4): first digit (0-9)
  - Low nibble (bits 3-0): second digit (0-9), OR sign nibble for last byte

Last byte low nibble:
  - C (1100) = positive
  - D (1101) = negative
  - F (1111) = unsigned positive

Apply implied decimal point at the position indicated by V in the PIC clause.
```

### COMP Conversion Algorithm

```
Read bytes in big-endian order (most significant byte first).
Convert to little-endian for .NET:
  - Reverse byte order
  - Or use: BitConverter.ToInt32(bytes.Reverse().ToArray(), 0)
  - Or use: BinaryPrimitives.ReadInt32BigEndian(bytes)
```

## Referential Integrity

```
EXPORT-RECORD (CVEXPORT)
  |
  +-- Type 'C' (Customer):
  |     EXPORT-CUST-ID ---------> dbo.Customer.CustomerId
  |
  +-- Type 'A' (Account):
  |     EXPORT-ACCT-ID ---------> dbo.Account.AccountId
  |     EXPORT-ACCT-CUST-ID ----> dbo.Customer.CustomerId
  |     EXPORT-ACCT-GROUP-ID ---> dbo.DiscountGroup.AccountGroupId
  |
  +-- Type 'T' (Transaction):
  |     EXPORT-TRAN-ID ---------> dbo.Transaction.TransactionId
  |     EXPORT-TRAN-TYPE-CD ----> dbo.TransactionType.TransactionTypeCd
  |     EXPORT-TRAN-CAT-CD -----> dbo.TransactionCategory.TransactionCategoryCd
  |     EXPORT-TRAN-CARD-NUM ---> dbo.Card.CardNumber
  |
  +-- Type 'X' (Card Xref):
  |     EXPORT-XREF-CARD-NUM ---> dbo.Card.CardNumber
  |     EXPORT-XREF-ACCT-ID ----> dbo.Account.AccountId
  |     EXPORT-XREF-CUST-ID ----> dbo.Customer.CustomerId
  |
  +-- Type 'D' (Card):
        EXPORT-CARD-NUM ---------> dbo.Card.CardNumber
        EXPORT-CARD-ACCT-ID -----> dbo.Account.AccountId
```

> **Note:** The export file is a denormalized extract. Records within a single export should be internally consistent (e.g., account records reference valid customer IDs within the same export).

## Sample Data

### Export File Sequence Example

| Seq | REC-TYPE | Description | Key Fields |
|-----|---------|-------------|------------|
| 1 | `C` | Customer record | CUST-ID: 000000001, SSN: 198501159876 |
| 2 | `A` | Account record | ACCT-ID: 00000000123, CUST-ID: 000000001 |
| 3 | `D` | Card record | CARD-NUM: 4000123456789010, ACCT-ID: 00000000123 |
| 4 | `X` | Cross-reference | CARD-NUM: 4000123456789010, ACCT-ID: 123, CUST-ID: 1 |
| 5 | `T` | Transaction | TRAN-ID: 0000000000000001, CARD: 4000123456789010 |
| 6 | `T` | Transaction | TRAN-ID: 0000000000000002, CARD: 4000123456789010 |
| 7 | `C` | Next customer | CUST-ID: 000000002 |
| ... | ... | ... | ... |

> **Note:** Records are typically ordered: all data for one customer/branch, then the next. The sequence number provides ordering within the export file. PAN and SSN values shown are samples only.

## Migration Notes

### Target Architecture

The export record does **not** map to a single database table. It is a serialization format used for branch data migration. In the .NET target system, this maps to:

| COBOL Construct | .NET Target | Notes |
|----------------|-------------|-------|
| Export file reader (CBEXP01C) | Import/export service | C# service that reads/writes export format |
| REDEFINES structure | Discriminated union / polymorphic deserialization | Use record type to select DTO |
| COMP fields | `int` / `long` with endian conversion | `BinaryPrimitives.ReadInt32BigEndian()` |
| COMP-3 fields | `decimal` with BCD conversion | Custom COMP-3 decoder or library |
| Export file | Azure Blob Storage | Encrypted at rest, short retention |

### Import Service DTO Example (C#)

```csharp
public abstract record ExportRecord(
    char RecordType,
    DateTime Timestamp,
    int SequenceNumber,
    string BranchId,
    string RegionCode
);

public record CustomerExportRecord(
    char RecordType, DateTime Timestamp, int SequenceNumber,
    string BranchId, string RegionCode,
    int CustomerId,
    string Ssn,           // Encrypted at rest
    string FirstName,
    string MiddleName,
    string LastName,
    string AddressLine1,
    string AddressLine2,
    string City,
    string State,
    string Country,
    string ZipCode,
    string Phone1,
    string Phone2,
    DateTime DateOfBirth,
    int FicoScore,
    DateTime EffectiveDate,
    DateTime ExpirationDate
) : ExportRecord(RecordType, Timestamp, SequenceNumber, BranchId, RegionCode);

public record AccountExportRecord(
    char RecordType, DateTime Timestamp, int SequenceNumber,
    string BranchId, string RegionCode,
    long AccountId,
    char Status,
    decimal CreditLimit,
    decimal CashLimit,
    decimal CurrentBalance,
    DateTime OpenDate,
    DateTime ExpirationDate,
    DateTime ReissueDate,
    string GroupId,
    int CustomerId
) : ExportRecord(RecordType, Timestamp, SequenceNumber, BranchId, RegionCode);

// ... similar records for Transaction, CardXref, Card
```

### COMP / COMP-3 Conversion Helpers (C#)

```csharp
public static class CobolFieldConverter
{
    /// <summary>
    /// Convert big-endian COMP (binary) to int.
    /// </summary>
    public static int ReadComp(ReadOnlySpan<byte> bytes)
        => BinaryPrimitives.ReadInt32BigEndian(bytes);

    /// <summary>
    /// Convert COMP-3 (packed decimal) to decimal.
    /// </summary>
    public static decimal ReadComp3(ReadOnlySpan<byte> bytes, int impliedDecimals)
    {
        long result = 0;
        bool isNegative = false;

        for (int i = 0; i < bytes.Length; i++)
        {
            byte b = bytes[i];
            int highNibble = (b >> 4) & 0x0F;
            int lowNibble = b & 0x0F;

            if (i == bytes.Length - 1)
            {
                result = result * 10 + highNibble;
                isNegative = lowNibble == 0x0D;
            }
            else
            {
                result = result * 10 + highNibble;
                result = result * 10 + lowNibble;
            }
        }

        decimal value = result;
        for (int i = 0; i < impliedDecimals; i++)
            value /= 10m;

        return isNegative ? -value : value;
    }
}
```

### Post-Migration Validation

```sql
-- After importing an export file, validate record counts by type
SELECT 'Customers' AS RecordType, COUNT(*) FROM dbo.Customer WHERE BranchId = @BranchId
UNION ALL
SELECT 'Accounts', COUNT(*) FROM dbo.Account WHERE BranchId = @BranchId
UNION ALL
SELECT 'Transactions', COUNT(*) FROM dbo.[Transaction] WHERE BranchId = @BranchId;

-- Verify financial totals from account records
SELECT SUM(CurrentBalance) AS TotalBalance,
       SUM(CreditLimit) AS TotalCreditLimit
FROM dbo.Account WHERE BranchId = @BranchId;

-- Verify cross-reference integrity
SELECT x.CardNumber FROM dbo.CustomerAccountCardXref x
LEFT JOIN dbo.Card c ON x.CardNumber = c.CardNumber
WHERE c.CardNumber IS NULL AND x.BranchId = @BranchId;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS** | Requirement 3: Protect stored cardholder data. PAN (card numbers) and CVV must be protected. | Card numbers (`EXPORT-CARD-NUM`, `EXPORT-XREF-CARD-NUM`, `EXPORT-TRAN-CARD-NUM`) must be encrypted in the export file and during transmission. **CVV (`EXPORT-CARD-CVV-CD`) must NEVER be stored after card authorization** -- if this export is for initial provisioning only, CVV must be purged after use. |
| **PCI-DSS** | Requirement 4: Encrypt transmission of cardholder data across open, public networks. | Export files must be transmitted over encrypted channels (TLS 1.2+). Azure Blob Storage with server-side encryption. |
| **GDPR** | Art. 5: Data minimisation. Art. 17: Right to erasure. Art. 32: Security of processing. | Customer PII (SSN, name, address, DOB, phone) must be encrypted at rest and in transit. Export files containing PII must have defined retention periods and be securely deleted after import. Data must remain within EU (Azure region selection). |
| **GDPR** | Art. 30: Records of processing activities. | Branch data export/import constitutes data processing. Document in the GDPR processing register with purpose, legal basis, and retention period. |
| **FSA (FFFS 2014:5)** | Ch. 8: Internal controls. Branch migration data must be complete and accurate. | Implement checksum validation for export files. Sequence number gaps must be detected. Financial totals must reconcile before and after import. |
| **AML/KYC** | Customer due diligence data must be migrated completely. | Export must include all KYC-relevant customer fields. Verify no customer records are lost during branch migration. AML screening status must be preserved. |
| **DORA** | Art. 11: ICT-related incident management. Data migration is a critical ICT operation. | Branch migration must follow DORA change management procedures. Rollback plan required. Monitor for data integrity incidents during migration. |
| **EBA Outsourcing** | Material outsourcing requires data portability. | The export format itself demonstrates data portability capability. Document as evidence for EBA outsourcing compliance. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
