---
title: Data Structures
sidebar_position: 1
---

# Data Structures

COBOL copybook definitions and their mapped .NET equivalents for the NordKredit core banking migration.

## Overview

The NordKredit mainframe system uses approximately 300 COBOL copybooks to define data contracts
across all domains. This section documents the key copybooks that define the primary data structures
for the domains currently in scope (Card Management, Transactions, Account Management).

Each copybook document includes:
- **Field definitions** — COBOL PIC clauses, offsets, and lengths
- **.NET class mapping** — target C# types and SQL column types
- **EBCDIC-to-Unicode conversion** — field-specific conversion methods using `EbcdicConverter`
- **Referencing programs** — COBOL programs that include each copybook
- **Regulatory traceability** — applicable FSA, PSD2, GDPR, and AML/KYC requirements

## Copybook Index

### Account Domain

| Copybook   | Record Name         | Length | VSAM File | .NET Class | Status |
|------------|---------------------|--------|-----------|------------|--------|
| [CVACT01Y](./cvact01y-account-master) | ACCOUNT-RECORD | 300 bytes | ACCTFILE | `Account` | Reconstructed from .NET model |

### Card Management Domain

| Copybook   | Record Name         | Length | VSAM File | .NET Class | Status |
|------------|---------------------|--------|-----------|------------|--------|
| [CVACT02Y](./cvact02y-card-record) | CARD-RECORD | 150 bytes | CARDDAT | `Card` | In repo |
| [CVACT03Y](./cvact03y-card-cross-reference) | CARD-XREF-RECORD | 50 bytes | CXREF | `CardCrossReference` | In repo |
| [CVCRD01Y](./cvcrd01y-card-work-area) | CC-WORK-AREAS | Variable | N/A (working storage) | *(no direct mapping)* | In repo |

### Transaction Domain

| Copybook   | Record Name         | Length | VSAM File | .NET Class | Status |
|------------|---------------------|--------|-----------|------------|--------|
| [CVTRA01Y](./cvtra01y-transaction-category-balance) | TRAN-CAT-BAL-RECORD | 50 bytes | TCATBALF | `TransactionCategoryBalance` | Reconstructed from .NET model |
| [CVTRA03Y](./cvtra03y-transaction-type) | TRAN-TYPE-RECORD | ~52 bytes | TRANTYPE | `TransactionType` | Reconstructed from .NET model |
| [CVTRA04Y](./cvtra04y-transaction-category) | TRAN-CAT-RECORD | ~56 bytes | TRANCATG | `TransactionCategory` | Reconstructed from .NET model |
| [CVTRA05Y](./cvtra05y-transaction-record) | TRAN-RECORD | 350 bytes | TRANSACT | `Transaction` | Reconstructed from .NET model |
| [CVTRA06Y](./cvtra06y-daily-transaction) | DALYTRAN-RECORD | 350 bytes | DALYTRAN | `DailyTransaction` | Reconstructed from .NET model |

### Infrastructure (CICS)

| Copybook   | Record Name         | Length | VSAM File | .NET Class | Status |
|------------|---------------------|--------|-----------|------------|--------|
| [COCOM01Y](./cocom01y-commarea) | Application COMMAREA | Up to 2,000 bytes | N/A (CICS COMMAREA) | *(REST API layer)* | Reconstructed from program references |
| [CSUSR01Y](./csusr01y-user-data) | Signed-on User Data | Variable | N/A (CICS security) | *(Azure AD claims)* | Reconstructed from program references |

## COBOL PIC Clause Quick Reference

| PIC Clause        | Meaning                  | .NET Type   | SQL Type         | Conversion Method                    |
|-------------------|--------------------------|-------------|------------------|--------------------------------------|
| PIC X(n)          | Alphanumeric, n chars    | `string`    | `nvarchar(n)`    | `EbcdicConverter.ConvertToUnicode()` |
| PIC 9(n)          | Unsigned numeric, n digits | `int`/`long` | `int`/`bigint` | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` |
| PIC S9(n)V99      | Signed decimal, 2 places | `decimal`   | `decimal(n+2,2)` | `EbcdicConverter.ConvertZonedDecimal(scale: 2)` |
| PIC S9(n) COMP-3  | Packed decimal (BCD)     | `decimal`   | `decimal`        | `EbcdicConverter.ConvertPackedDecimal()` |
| PIC S9(n) COMP    | Binary integer           | `int`/`long`| `int`/`bigint`   | Direct binary interpretation         |

## EBCDIC Conversion

All text fields use IBM Code Page 1143 (Swedish/Finnish EBCDIC variant) for conversion.
The `EbcdicConverter` utility class in `NordKredit.Infrastructure.DataMigration` handles:

- **Text fields** (`PIC X`) — `ConvertToUnicode()` / `ConvertToEbcdic()` for bidirectional conversion
- **Packed decimal** (`COMP-3`) — `ConvertPackedDecimal(scale)` with nibble-pair digit extraction
- **Zoned decimal** (`PIC 9`, `PIC S9`) — `ConvertZonedDecimal(scale)` with zone/digit nibble separation

Swedish characters (Å, Ä, Ö) require Code Page 1143 specifically — generic EBCDIC code pages will produce incorrect mappings.

## Coverage Status

- **Documented**: 11 copybooks (key data structures for Card Management, Transactions, Account, and CICS infrastructure)
- **In repo**: 3 copybooks (CVACT02Y, CVACT03Y, CVCRD01Y) with full field-level documentation from source
- **Reconstructed**: 8 copybooks documented from .NET domain models and EF Core configuration
- **Pending**: Remaining ~289 copybooks (tracked in issue #125 for copybook acquisition)
