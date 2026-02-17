---
title: CVACT02Y — Card Record
sidebar_position: 3
---

# CVACT02Y — Card Record

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVACT02Y.cpy                             |
| **Record Name**   | CARD-RECORD                              |
| **Record Length**  | 150 bytes                                |
| **VSAM File**     | CARDDAT (KSDS, key = CARD-NUM), CARDAIX (alternate index on CARD-ACCT-ID) |
| **Domain**        | Card Management                          |
| **Status**        | In repo (`docs/cobol-source/cpy/CVACT02Y.cpy`) |

## Purpose

Defines the card entity record stored in the CARDDAT VSAM file. Contains the card number,
linked account, CVV, cardholder name, expiration date, and active status. This is the primary
data structure for card lifecycle management (list, view, update operations).

## Field Definitions

| # | Field Name              | PIC Clause   | Offset | Length | Description                              |
|---|-------------------------|--------------|--------|--------|------------------------------------------|
| 1 | CARD-NUM               | PIC X(16)    | 0      | 16     | Card number (primary key)                |
| 2 | CARD-ACCT-ID           | PIC 9(11)    | 16     | 11     | Account identifier (foreign key)         |
| 3 | CARD-CVV-CD            | PIC 9(03)    | 27     | 3      | Card verification value                  |
| 4 | CARD-EMBOSSED-NAME     | PIC X(50)    | 30     | 50     | Cardholder name as embossed on card      |
| 5 | CARD-EXPIRAION-DATE    | PIC X(10)    | 80     | 10     | Expiration date (YYYY-MM-DD format)      |
| 6 | CARD-ACTIVE-STATUS     | PIC X(01)    | 90     | 1      | Status: 'Y' = active, 'N' = inactive    |
| 7 | FILLER                 | PIC X(59)    | 91     | 59     | Reserved / padding to 150 bytes          |

## .NET Class Mapping

**Target class:** `NordKredit.Domain.CardManagement.Card`

| COBOL Field          | C# Property      | C# Type     | SQL Column Type  | Notes                                  |
|----------------------|-------------------|-------------|------------------|----------------------------------------|
| CARD-NUM             | CardNumber        | `string`    | `nvarchar(16)`   | Primary key                            |
| CARD-ACCT-ID         | AccountId         | `string`    | `nvarchar(11)`   | Foreign key to Accounts table          |
| CARD-CVV-CD          | CvvCode           | `string`    | `nvarchar(3)`    | PCI-DSS review required for storage    |
| CARD-EMBOSSED-NAME   | EmbossedName      | `string`    | `nvarchar(50)`   | Supports Swedish characters (Å, Ä, Ö) |
| CARD-EXPIRAION-DATE  | ExpirationDate    | `DateOnly`  | `date`           | Parsed from COBOL string format        |
| CARD-ACTIVE-STATUS   | ActiveStatus      | `char`      | `nchar(1)`       | 'Y' or 'N'                            |
| *(not in COBOL)*     | RowVersion        | `byte[]`    | `rowversion`     | Optimistic concurrency (replaces COBOL field comparison) |

### Design Decisions

- **CARD-ACCT-ID**: COBOL uses `PIC 9(11)` (numeric), but .NET maps to `string` to preserve leading zeros in the account identifier.
- **CARD-CVV-CD**: Stored as `string` rather than `int` to preserve leading zeros. PCI-DSS compliance review is needed post-migration to determine whether CVV storage should continue.
- **CARD-EXPIRAION-DATE**: The COBOL field name contains a typo (`EXPIRAION` instead of `EXPIRATION`). The .NET property uses the corrected spelling.
- **RowVersion**: Added for optimistic concurrency control, replacing the COBOL pattern of reading/comparing all fields before update (see COCRDUPC.cbl).

## EBCDIC-to-Unicode Conversion Notes

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |
| PIC 9 (zoned)     | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` | Unsigned integer via zoned decimal |

**Special attention:** `CARD-EMBOSSED-NAME` contains Swedish characters (Å, Ä, Ö) which require IBM Code Page 1143 for correct conversion. The target column uses `nvarchar` to preserve Unicode.

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| COCRDLIC    | Card list display — reads CARDDAT via STARTBR/READNEXT |
| COCRDSLC    | Card detail display — reads individual card by CARD-NUM |
| COCRDUPC    | Card update — reads, modifies, and rewrites CARD-RECORD |

## Regulatory Traceability

| Regulation          | Requirement                                |
|---------------------|--------------------------------------------|
| GDPR Art. 5(1)(c)  | Data minimization — only store necessary card fields |
| GDPR Art. 5(1)(d)  | Accuracy — cardholder name must match embossed card |
| PSD2 Art. 97       | Strong Customer Authentication for card operations |
| PCI-DSS            | CVV storage review required post-migration  |
