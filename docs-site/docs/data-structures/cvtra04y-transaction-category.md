---
title: CVTRA04Y — Transaction Category
sidebar_position: 10
---

# CVTRA04Y — Transaction Category

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVTRA04Y.cpy                             |
| **Record Name**   | TRAN-CAT-RECORD                          |
| **Record Length**  | ~56 bytes                                |
| **VSAM File**     | TRANCATG (KSDS, composite key = TRAN-TYPE + TRAN-CAT-CD) |
| **Domain**        | Transactions                             |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Lookup table mapping the combination of transaction type code and category code to
a human-readable description. Provides finer-grained classification than the type code alone
(e.g., type "DB" + category 1001 = "Retail Purchase"). Used by reporting programs for
transaction enrichment.

## Field Definitions

| # | Field Name          | PIC Clause   | Offset | Length | Description                              |
|---|---------------------|--------------|--------|--------|------------------------------------------|
| 1 | TRAN-TYPE           | PIC X(02)    | 0      | 2      | Transaction type code (composite PK part 1) |
| 2 | TRAN-CAT-CD         | PIC 9(04)    | 2      | 4      | Category code (composite PK part 2)      |
| 3 | TRAN-CAT-DESC       | PIC X(50)    | 6      | 50     | Category description                     |

**Composite key:** `TRAN-TYPE` + `TRAN-CAT-CD` (6 bytes)

**Note:** Field definitions are reconstructed from the .NET domain model. Verify against actual copybook when available.

## .NET Class Mapping

**Target class:** `NordKredit.Domain.Transactions.TransactionCategory`

| COBOL Field      | C# Property  | C# Type   | SQL Column Type  | Notes                          |
|------------------|---------------|-----------|------------------|--------------------------------|
| TRAN-TYPE        | TypeCode     | `string`  | `nvarchar(2)`    | Composite PK part 1           |
| TRAN-CAT-CD      | CategoryCode | `int`     | `int`            | Composite PK part 2           |
| TRAN-CAT-DESC    | Description  | `string`  | `nvarchar(50)`   |                                |

### Design Decisions

- **TRAN-CAT-CD**: Mapped from `PIC 9(04)` (4-digit numeric string) to `int` since category codes are pure numeric identifiers without meaningful leading zeros.

## EBCDIC-to-Unicode Conversion Notes

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |
| PIC 9 (zoned)     | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` | Unsigned integer                  |

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| CBTRN03C    | Transaction detail report — enriches output with category descriptions |

## Regulatory Traceability

| Regulation            | Requirement                              |
|-----------------------|------------------------------------------|
| FSA FFFS 2014:5 Ch.7  | Financial reporting — granular transaction classification |
| PSD2 Art. 94          | Accessibility — detailed transaction categorization |
