---
title: CVACT01Y — Account Master Record
sidebar_position: 2
---

# CVACT01Y — Account Master Record

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVACT01Y.cpy                             |
| **Record Name**   | ACCOUNT-RECORD                           |
| **Record Length**  | 300 bytes                                |
| **VSAM File**     | ACCTFILE (KSDS, key = ACCT-ID)           |
| **Domain**        | Transactions / Account Management        |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Defines the account master record used across transaction processing and account management.
Contains account identifiers, balance fields, credit limits, and cycle accumulators.
This is the central data structure for all balance-related operations on the mainframe.

## Field Definitions

| # | Field Name              | PIC Clause        | Offset | Length | Description                              |
|---|-------------------------|--------------------|--------|--------|------------------------------------------|
| 1 | ACCT-ID                | PIC 9(11)          | 0      | 11     | Account identifier (primary key)         |
| 2 | ACCT-ACTIVE-STATUS     | PIC X(01)          | 11     | 1      | Active status: 'A' = active, 'D' = dormant |
| 3 | ACCT-CURR-BAL          | PIC S9(10)V99      | 12     | 12     | Current balance (signed, 2 decimal places) |
| 4 | ACCT-CREDIT-LIMIT      | PIC S9(10)V99      | 24     | 12     | Credit limit                             |
| 5 | ACCT-CASH-CREDIT-LIMIT | PIC S9(10)V99      | 36     | 12     | Cash advance credit limit                |
| 6 | ACCT-CURR-CYC-CREDIT   | PIC S9(10)V99      | 48     | 12     | Current cycle credit total               |
| 7 | ACCT-CURR-CYC-DEBIT    | PIC S9(10)V99      | 60     | 12     | Current cycle debit total                |
| 8 | ACCT-EXPIRAION-DATE    | PIC X(10)          | 72     | 10     | Expiration date (note: typo in COBOL source) |
| 9 | FILLER                 | PIC X(218)         | 82     | 218    | Reserved / future use                    |

**Note:** Offsets are approximate — the full copybook is not yet available in the repository (tracked in issue #125). Field definitions are reconstructed from the .NET domain model `Account.cs` and EF Core configuration in `NordKreditDbContext.cs`.

## .NET Class Mapping

**Target class:** `NordKredit.Domain.Transactions.Account`

| COBOL Field            | C# Property        | C# Type      | SQL Column Type  | Notes                                |
|------------------------|---------------------|-------------- |------------------|--------------------------------------|
| ACCT-ID                | Id                  | `string`      | `nvarchar(11)`   | Primary key                          |
| ACCT-ACTIVE-STATUS     | ActiveStatus        | `string`      | `nvarchar(1)`    | 'A' or 'D'                          |
| ACCT-CURR-BAL          | CurrentBalance      | `decimal`     | `decimal(12,2)`  | Never use float/double               |
| ACCT-CREDIT-LIMIT      | CreditLimit         | `decimal`     | `decimal(12,2)`  |                                      |
| ACCT-CASH-CREDIT-LIMIT | CashCreditLimit     | `decimal`     | `decimal(12,2)`  |                                      |
| ACCT-CURR-CYC-CREDIT   | CurrentCycleCredit  | `decimal`     | `decimal(12,2)`  |                                      |
| ACCT-CURR-CYC-DEBIT    | CurrentCycleDebit   | `decimal`     | `decimal(12,2)`  |                                      |
| ACCT-EXPIRAION-DATE    | ExpirationDate      | `DateTime?`   | `datetime2`      | Nullable — null means never expires  |

## EBCDIC-to-Unicode Conversion Notes

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |
| PIC S9 V99 (signed decimal) | `EbcdicConverter.ConvertZonedDecimal(scale: 2)` | Sign in last byte zone nibble |
| PIC 9 (unsigned)  | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` | Unsigned zoned decimal        |

If the COBOL source uses `COMP-3` for any balance fields, use `EbcdicConverter.ConvertPackedDecimal()` instead. Verify against actual copybook when available.

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| CBTRN02C    | Account expiration validation (lines 414–420), balance updates during transaction posting |
| COCRDSLC    | Referenced (commented out `*COPY CVACT01Y`) for card detail context |
| COCRDUPC    | Referenced (commented out `*COPY CVACT01Y`) for card update context |
| CBTRN01C    | Account lookup during transaction verification |

## Regulatory Traceability

| Regulation            | Requirement                              |
|-----------------------|------------------------------------------|
| FFFS 2014:5 Ch.4 §3  | Credit limit enforcement for risk management |
| PSD2 Art. 97          | Account validation during SCA flows      |
| GDPR Art. 5(1)(d)    | Accuracy of balance data during migration |
