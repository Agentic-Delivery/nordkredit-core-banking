---
title: CVTRA01Y — Transaction Category Balance
sidebar_position: 8
---

# CVTRA01Y — Transaction Category Balance

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVTRA01Y.cpy                             |
| **Record Name**   | TRAN-CAT-BAL-RECORD                     |
| **Record Length**  | 50 bytes                                 |
| **VSAM File**     | TCATBALF (KSDS, composite key = TRAN-CAT-KEY) |
| **Domain**        | Transactions                             |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Tracks the accumulated balance for each account/transaction-type/category combination.
Used by batch posting (CBTRN02C) to maintain running totals for transaction categorization
and by reporting programs for category-level balance queries. The composite key structure
enables efficient lookups by account and transaction classification.

## Field Definitions

| # | Field Name          | PIC Clause       | Offset | Length | Description                              |
|---|---------------------|------------------|--------|--------|------------------------------------------|
| 1 | TRANCAT-ACCT-ID    | PIC 9(11)        | 0      | 11     | Account identifier                       |
| 2 | TRANCAT-TYPE-CD    | PIC X(02)        | 11     | 2      | Transaction type code (e.g., "DB", "CR") |
| 3 | TRANCAT-CD         | PIC 9(04)        | 13     | 4      | Category code (e.g., 1001–9999)          |
| 4 | TRAN-CAT-BAL       | PIC S9(09)V99    | 17     | 11     | Category balance (signed, 2 decimal places) |
| 5 | FILLER             | PIC X(22)        | 28     | 22     | Reserved / padding to 50 bytes           |

**Composite key:** `TRAN-CAT-KEY` = `TRANCAT-ACCT-ID` + `TRANCAT-TYPE-CD` + `TRANCAT-CD` (17 bytes)

**Note:** Offsets are reconstructed from the .NET domain model. Verify against actual copybook when available.

## .NET Class Mapping

**Target class:** `NordKredit.Domain.Transactions.TransactionCategoryBalance`

| COBOL Field        | C# Property  | C# Type   | SQL Column Type  | Notes                          |
|--------------------|---------------|-----------|------------------|--------------------------------|
| TRANCAT-ACCT-ID    | AccountId    | `string`  | `nvarchar(11)`   | Composite PK part 1           |
| TRANCAT-TYPE-CD    | TypeCode     | `string`  | `nvarchar(2)`    | Composite PK part 2           |
| TRANCAT-CD         | CategoryCode | `int`     | `int`            | Composite PK part 3           |
| TRAN-CAT-BAL       | Balance      | `decimal` | `decimal(11,2)`  | Never use float/double         |

### Design Decisions

- **Composite key**: EF Core maps `HasKey(e => new { e.AccountId, e.TypeCode, e.CategoryCode })` — preserves the COBOL VSAM key structure exactly.
- **TRAN-CAT-BAL**: Uses `decimal(11,2)` matching the COBOL `PIC S9(09)V99` precision (9 integer digits + 2 decimal).

## EBCDIC-to-Unicode Conversion Notes

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |
| PIC 9 (zoned)     | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` | Unsigned integer                  |
| PIC S9 V99        | `EbcdicConverter.ConvertZonedDecimal(scale: 2)` | Signed decimal with 2 places     |

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| CBTRN02C    | Updates category balances during transaction posting |
| CBTRN03C    | Reads category balances for transaction reporting |

## Regulatory Traceability

| Regulation          | Requirement                                |
|---------------------|--------------------------------------------|
| FSA FFFS 2014:5 Ch.7 | Financial reporting — accurate category-level balances |
| PSD2 Art. 94        | Record keeping — transaction classification |
