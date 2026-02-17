---
title: CVTRA05Y — Transaction Record
sidebar_position: 11
---

# CVTRA05Y — Transaction Record

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVTRA05Y.cpy                             |
| **Record Name**   | TRAN-RECORD                              |
| **Record Length**  | 350 bytes                                |
| **VSAM File**     | TRANSACT (KSDS, key = TRAN-ID)           |
| **Domain**        | Transactions                             |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Defines the posted transaction record — the final, committed state of a transaction after
it passes all validation checks. Contains transaction identifiers, classification codes,
monetary amount, merchant information, card linkage, and timestamps. This is the primary
transaction data structure and the target of the daily batch posting process (CBTRN02C).

## Field Definitions

| # | Field Name            | PIC Clause       | Offset | Length | Description                              |
|---|-----------------------|------------------|--------|--------|------------------------------------------|
| 1 | TRAN-ID              | PIC X(16)        | 0      | 16     | Transaction ID (primary key)             |
| 2 | TRAN-TYPE-CD         | PIC X(02)        | 16     | 2      | Type code (FK to TRANTYPE: "DB", "CR")   |
| 3 | TRAN-CAT-CD          | PIC 9(04)        | 18     | 4      | Category code (FK to TRANCATG: 1001–9999)|
| 4 | TRAN-SOURCE          | PIC X(10)        | 22     | 10     | Source system ("ONLINE", "BATCH")        |
| 5 | TRAN-DESC            | PIC X(100)       | 32     | 100    | Transaction description                  |
| 6 | TRAN-AMT             | PIC S9(09)V99    | 132    | 11     | Amount (signed, 2 decimal places)        |
| 7 | TRAN-MERCHANT-ID     | PIC 9(09)        | 143    | 9      | Merchant identifier                      |
| 8 | TRAN-MERCHANT-NAME   | PIC X(50)        | 152    | 50     | Merchant name                            |
| 9 | TRAN-MERCHANT-CITY   | PIC X(50)        | 202    | 50     | Merchant city                            |
| 10| TRAN-MERCHANT-ZIP    | PIC X(10)        | 252    | 10     | Merchant postal code                     |
| 11| TRAN-CARD-NUM        | PIC X(16)        | 262    | 16     | Card number (FK to CARDDAT)              |
| 12| TRAN-ORIG-TS         | PIC X(26)        | 278    | 26     | Origination timestamp                    |
| 13| TRAN-PROC-TS         | PIC X(26)        | 304    | 26     | Processing timestamp                     |
| 14| FILLER               | PIC X(20)        | 330    | 20     | Reserved / padding to 350 bytes          |

**Note:** Offsets are reconstructed from field lengths. Verify against actual copybook when available.

## .NET Class Mapping

**Target class:** `NordKredit.Domain.Transactions.Transaction`

| COBOL Field          | C# Property            | C# Type      | SQL Column Type  | Notes                              |
|----------------------|-------------------------|--------------|------------------|------------------------------------|
| TRAN-ID              | Id                     | `string`     | `nvarchar(16)`   | Primary key                        |
| TRAN-TYPE-CD         | TypeCode               | `string`     | `nvarchar(2)`    |                                    |
| TRAN-CAT-CD          | CategoryCode           | `int`        | `int`            |                                    |
| TRAN-SOURCE          | Source                 | `string`     | `nvarchar(10)`   |                                    |
| TRAN-DESC            | Description            | `string`     | `nvarchar(100)`  |                                    |
| TRAN-AMT             | Amount                 | `decimal`    | `decimal(11,2)`  | Never use float/double             |
| TRAN-MERCHANT-ID     | MerchantId             | `int`        | `int`            |                                    |
| TRAN-MERCHANT-NAME   | MerchantName           | `string`     | `nvarchar(50)`   |                                    |
| TRAN-MERCHANT-CITY   | MerchantCity           | `string`     | `nvarchar(50)`   |                                    |
| TRAN-MERCHANT-ZIP    | MerchantZip            | `string`     | `nvarchar(10)`   |                                    |
| TRAN-CARD-NUM        | CardNumber             | `string`     | `nvarchar(16)`   | Indexed for card-based queries     |
| TRAN-ORIG-TS         | OriginationTimestamp   | `DateTime`   | `datetime2`      | Parsed from COBOL PIC X(26)        |
| TRAN-PROC-TS         | ProcessingTimestamp    | `DateTime`   | `datetime2`      | Parsed from COBOL PIC X(26)        |

### Design Decisions

- **Timestamps**: COBOL stores timestamps as `PIC X(26)` strings (format: `YYYY-MM-DD-HH.MM.SS.FFFFFF`). .NET maps to `DateTime` with parsing during data migration.
- **TRAN-AMT**: `decimal(11,2)` exactly matches `PIC S9(09)V99` — 9 integer digits + 2 implied decimal places.
- **CardNumber index**: An index on `CardNumber` replaces the VSAM alternate index for transaction lookups by card.

## EBCDIC-to-Unicode Conversion Notes

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |
| PIC 9 (zoned)     | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` | Unsigned integer                  |
| PIC S9 V99        | `EbcdicConverter.ConvertZonedDecimal(scale: 2)` | Signed decimal with 2 places     |
| PIC X(26) timestamp | `EbcdicConverter.ConvertToUnicode()` then `DateTime.ParseExact()` | Two-step conversion |

**Special attention:** Merchant names and descriptions may contain Swedish characters (Å, Ä, Ö) and must use Code Page 1143 for correct conversion.

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| CBTRN02C    | Transaction posting — writes TRAN-RECORD to TRANSACT file |
| CBTRN03C    | Transaction reporting — reads and enriches with type/category descriptions |

## Regulatory Traceability

| Regulation            | Requirement                              |
|-----------------------|------------------------------------------|
| PSD2 Art. 94          | Record keeping — complete transaction audit trail |
| FSA FFFS 2014:5 Ch.7  | Financial reporting — accurate transaction records |
| AML/KYC               | Transaction monitoring — merchant and amount data |
| GDPR Art. 5(1)(d)    | Data accuracy during migration            |
