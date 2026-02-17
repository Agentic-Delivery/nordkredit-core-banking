---
title: CVTRA06Y — Daily Transaction
sidebar_position: 12
---

# CVTRA06Y — Daily Transaction

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVTRA06Y.cpy                             |
| **Record Name**   | DALYTRAN-RECORD                          |
| **Record Length**  | 350 bytes                                |
| **VSAM File**     | DALYTRAN (sequential daily input file)   |
| **Domain**        | Transactions (batch processing)          |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Defines the daily transaction input record — unposted transactions received from external
systems (card networks, online banking) for batch processing. Has the same field layout as
the posted Transaction record (CVTRA05Y) but is sourced from a sequential daily input file
rather than the VSAM transaction store. Processed by the nightly batch cycle: verified by
CBTRN01C, then posted by CBTRN02C (or rejected to DailyRejects).

## Field Definitions

| # | Field Name              | PIC Clause       | Offset | Length | Description                              |
|---|-------------------------|------------------|--------|--------|------------------------------------------|
| 1 | DALYTRAN-ID            | PIC X(16)        | 0      | 16     | Transaction ID                           |
| 2 | DALYTRAN-TYPE-CD       | PIC X(02)        | 16     | 2      | Type code ("DB", "CR")                   |
| 3 | DALYTRAN-CAT-CD        | PIC 9(04)        | 18     | 4      | Category code (1001–9999)                |
| 4 | DALYTRAN-SOURCE        | PIC X(10)        | 22     | 10     | Source system                            |
| 5 | DALYTRAN-DESC          | PIC X(100)       | 32     | 100    | Transaction description                  |
| 6 | DALYTRAN-AMT           | PIC S9(09)V99    | 132    | 11     | Amount (signed, 2 decimal places)        |
| 7 | DALYTRAN-MERCHANT-ID   | PIC 9(09)        | 143    | 9      | Merchant identifier                      |
| 8 | DALYTRAN-MERCHANT-NAME | PIC X(50)        | 152    | 50     | Merchant name                            |
| 9 | DALYTRAN-MERCHANT-CITY | PIC X(50)        | 202    | 50     | Merchant city                            |
| 10| DALYTRAN-MERCHANT-ZIP  | PIC X(10)        | 252    | 10     | Merchant postal code                     |
| 11| DALYTRAN-CARD-NUM      | PIC X(16)        | 262    | 16     | Card number                              |
| 12| DALYTRAN-ORIG-TS       | PIC X(26)        | 278    | 26     | Origination timestamp                    |
| 13| DALYTRAN-PROC-TS       | PIC X(26)        | 304    | 26     | Processing timestamp                     |
| 14| FILLER                 | PIC X(20)        | 330    | 20     | Reserved / padding to 350 bytes          |

**Note:** Field layout mirrors CVTRA05Y (TRAN-RECORD) with `DALYTRAN-` prefix instead of `TRAN-`.

## .NET Class Mapping

**Target class:** `NordKredit.Domain.Transactions.DailyTransaction`

| COBOL Field             | C# Property            | C# Type      | SQL Column Type  | Notes                              |
|-------------------------|-------------------------|--------------|------------------|------------------------------------|
| DALYTRAN-ID             | Id                     | `string`     | `nvarchar(16)`   | Primary key                        |
| DALYTRAN-TYPE-CD        | TypeCode               | `string`     | `nvarchar(2)`    |                                    |
| DALYTRAN-CAT-CD         | CategoryCode           | `int`        | `int`            |                                    |
| DALYTRAN-SOURCE         | Source                 | `string`     | `nvarchar(10)`   |                                    |
| DALYTRAN-DESC           | Description            | `string`     | `nvarchar(100)`  |                                    |
| DALYTRAN-AMT            | Amount                 | `decimal`    | `decimal(11,2)`  | Never use float/double             |
| DALYTRAN-MERCHANT-ID    | MerchantId             | `int`        | `int`            |                                    |
| DALYTRAN-MERCHANT-NAME  | MerchantName           | `string`     | `nvarchar(50)`   |                                    |
| DALYTRAN-MERCHANT-CITY  | MerchantCity           | `string`     | `nvarchar(50)`   |                                    |
| DALYTRAN-MERCHANT-ZIP   | MerchantZip            | `string`     | `nvarchar(10)`   |                                    |
| DALYTRAN-CARD-NUM       | CardNumber             | `string`     | `nvarchar(16)`   | Indexed for card-based queries     |
| DALYTRAN-ORIG-TS        | OriginationTimestamp   | `DateTime`   | `datetime2`      |                                    |
| DALYTRAN-PROC-TS        | ProcessingTimestamp    | `DateTime`   | `datetime2`      |                                    |

### Design Decisions

- **Same schema as Transaction**: The DailyTransaction table mirrors the Transaction table schema. In COBOL, both copybooks share the same layout with different field prefixes. In .NET, both classes have identical properties but are separate entities for domain clarity.
- **Batch lifecycle**: Daily transactions are input → verified (CBTRN01C) → posted as Transactions (CBTRN02C) or rejected as DailyRejects. In .NET, this maps to an Azure Function timer trigger processing the DailyTransactions table.

## EBCDIC-to-Unicode Conversion Notes

Same conversion rules as CVTRA05Y (Transaction Record):

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |
| PIC 9 (zoned)     | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` | Unsigned integer                  |
| PIC S9 V99        | `EbcdicConverter.ConvertZonedDecimal(scale: 2)` | Signed decimal with 2 places     |
| PIC X(26) timestamp | `EbcdicConverter.ConvertToUnicode()` then `DateTime.ParseExact()` | Two-step conversion |

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| CBTRN01C    | Verification — validates card, cross-reference, account for each daily transaction |
| CBTRN02C    | Posting — reads verified daily transactions, posts to TRANSACT file, rejects invalid ones |

## Regulatory Traceability

| Regulation            | Requirement                              |
|-----------------------|------------------------------------------|
| PSD2 Art. 97          | SCA validation during transaction verification |
| FFFS 2014:5 Ch.4 §3   | Credit risk checks before posting        |
| EBA Guidelines        | Creditworthiness assessment during posting |
| GDPR Art. 5(1)(d)    | Data accuracy of incoming transaction data |
