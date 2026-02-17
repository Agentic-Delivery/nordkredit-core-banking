---
title: CVTRA03Y — Transaction Type
sidebar_position: 9
---

# CVTRA03Y — Transaction Type

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVTRA03Y.cpy                             |
| **Record Name**   | TRAN-TYPE-RECORD                         |
| **Record Length**  | ~52 bytes                                |
| **VSAM File**     | TRANTYPE (KSDS, key = TRAN-TYPE)         |
| **Domain**        | Transactions                             |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Lookup table mapping transaction type codes to human-readable descriptions.
Used by reporting programs (CBTRN03C) to enrich transaction reports with type descriptions.
Type codes such as "DB" (debit) and "CR" (credit) are the primary classification for all transactions.

## Field Definitions

| # | Field Name          | PIC Clause   | Offset | Length | Description                              |
|---|---------------------|--------------|--------|--------|------------------------------------------|
| 1 | TRAN-TYPE           | PIC X(02)    | 0      | 2      | Transaction type code (primary key)      |
| 2 | TRAN-TYPE-DESC      | PIC X(50)    | 2      | 50     | Type description                         |

**Note:** Field definitions are reconstructed from the .NET domain model. Verify against actual copybook when available.

## .NET Class Mapping

**Target class:** `NordKredit.Domain.Transactions.TransactionType`

| COBOL Field      | C# Property  | C# Type   | SQL Column Type  | Notes                          |
|------------------|---------------|-----------|------------------|--------------------------------|
| TRAN-TYPE        | TypeCode     | `string`  | `nvarchar(2)`    | Primary key                    |
| TRAN-TYPE-DESC   | Description  | `string`  | `nvarchar(50)`   |                                |

## EBCDIC-to-Unicode Conversion Notes

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |

Both fields are text — straightforward EBCDIC-to-Unicode conversion. Description field may contain Swedish characters in localized deployments.

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| CBTRN03C    | Transaction detail report — enriches output with type descriptions |

## Regulatory Traceability

| Regulation            | Requirement                              |
|-----------------------|------------------------------------------|
| FSA FFFS 2014:5 Ch.7  | Financial reporting — clear transaction type classification |
| PSD2 Art. 94          | Accessibility — human-readable transaction information |
