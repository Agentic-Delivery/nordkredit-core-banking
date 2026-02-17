---
title: CVACT03Y — Card Cross-Reference
sidebar_position: 4
---

# CVACT03Y — Card Cross-Reference

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVACT03Y.cpy                             |
| **Record Name**   | CARD-XREF-RECORD                         |
| **Record Length**  | 50 bytes                                 |
| **VSAM File**     | CXREF (KSDS, key = XREF-CARD-NUM)       |
| **Domain**        | Card Management / Transactions           |
| **Status**        | In repo (`docs/cobol-source/cpy/CVACT03Y.cpy`) |

## Purpose

Establishes the three-way relationship between cards, customers, and accounts.
This cross-reference is used during transaction verification (CBTRN01C) to resolve a card number
to its owning account and customer. It is a lookup-only structure — updates are managed through
card issuance processes.

## Field Definitions

| # | Field Name       | PIC Clause   | Offset | Length | Description                              |
|---|------------------|--------------|--------|--------|------------------------------------------|
| 1 | XREF-CARD-NUM   | PIC X(16)    | 0      | 16     | Card number (primary key, FK to CARDDAT) |
| 2 | XREF-CUST-ID    | PIC 9(09)    | 16     | 9      | Customer identifier                      |
| 3 | XREF-ACCT-ID    | PIC 9(11)    | 25     | 11     | Account identifier (FK to ACCTFILE)      |
| 4 | FILLER          | PIC X(14)    | 36     | 14     | Reserved / padding to 50 bytes           |

## .NET Class Mapping

**Target classes:**
- `NordKredit.Domain.CardManagement.CardCrossReference`
- `NordKredit.Domain.Transactions.CardCrossReference`

Both domains define their own cross-reference class with the same fields, following domain isolation principles.

| COBOL Field    | C# Property  | C# Type   | SQL Column Type  | Notes                          |
|----------------|---------------|-----------|------------------|--------------------------------|
| XREF-CARD-NUM  | CardNumber   | `string`  | `nvarchar(16)`   | Primary key                    |
| XREF-CUST-ID   | CustomerId   | `int`     | `int`            | Numeric customer ID            |
| XREF-ACCT-ID   | AccountId    | `string`  | `nvarchar(11)`   | Preserves leading zeros        |

### Design Decisions

- **Dual domain classes**: Both `CardManagement` and `Transactions` domains have their own `CardCrossReference` entity to maintain bounded context isolation. They map to the same database table (`CardCrossReferences`).
- **XREF-CUST-ID**: Mapped to `int` (not `string`) because customer IDs do not have meaningful leading zeros in this system.

## EBCDIC-to-Unicode Conversion Notes

| Field Type        | Conversion Method                          | Notes                                    |
|-------------------|--------------------------------------------|------------------------------------------|
| PIC X (text)      | `EbcdicConverter.ConvertToUnicode()`       | IBM Code Page 1143 (Swedish/Finnish)     |
| PIC 9 (zoned)     | `EbcdicConverter.ConvertZonedDecimal(scale: 0)` | Unsigned integer via zoned decimal |

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| CBTRN01C    | Card-to-account lookup during transaction verification |
| CBTRN02C    | Account resolution for transaction posting |
| COCRDSLC    | Referenced (commented out) for card detail context |
| COCRDUPC    | Referenced (commented out) for card update context |

## Regulatory Traceability

| Regulation          | Requirement                                |
|---------------------|--------------------------------------------|
| GDPR Art. 5(1)(c)  | Data minimization — only card/customer/account linkage |
| AML/KYC             | Card-to-customer linkage for suspicious activity monitoring |
