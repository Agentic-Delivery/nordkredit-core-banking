---
title: CVCRD01Y — Card Work Area
sidebar_position: 5
---

# CVCRD01Y — Card Work Area

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CVCRD01Y.cpy                             |
| **Record Name**   | CC-WORK-AREAS                            |
| **Record Length**  | Variable (working storage, not a file record) |
| **VSAM File**     | N/A — working storage only               |
| **Domain**        | Card Management (CICS UI infrastructure) |
| **Status**        | In repo (`docs/cobol-source/cpy/CVCRD01Y.cpy`) |

## Purpose

Defines common working storage areas shared across card management CICS programs.
Contains the AID key handler (terminal keyboard input), navigation control fields
(next program, next map), message buffers, and reusable account/card/customer ID fields.
This copybook is UI infrastructure for the CICS terminal interface — it does not represent
a persistent data record.

## Field Definitions

| # | Field Name          | PIC Clause   | Offset | Length | Description                              |
|---|---------------------|--------------|--------|--------|------------------------------------------|
| 1 | CCARD-AID           | PIC X(5)     | 0      | 5      | Terminal AID key value                   |
|   | *88 CCARD-AID-ENTER*  |            |        |        | Value 'ENTER' — enter key pressed        |
|   | *88 CCARD-AID-CLEAR*  |            |        |        | Value 'CLEAR' — clear key pressed        |
|   | *88 CCARD-AID-PA1*    |            |        |        | Value 'PA1  ' — PA1 key                 |
|   | *88 CCARD-AID-PA2*    |            |        |        | Value 'PA2  ' — PA2 key                 |
|   | *88 CCARD-AID-PFK01–PFK12* |       |        |        | Values 'PFK01'–'PFK12' — function keys  |
| 2 | CCARD-NEXT-PROG     | PIC X(8)     | 5      | 8      | Next program to invoke via XCTL          |
| 3 | CCARD-NEXT-MAPSET   | PIC X(7)     | 13     | 7      | Next BMS mapset name                     |
| 4 | CCARD-NEXT-MAP      | PIC X(7)     | 20     | 7      | Next BMS map name                        |
| 5 | CCARD-ERROR-MSG     | PIC X(75)    | 27     | 75     | Error message buffer                     |
| 6 | CCARD-RETURN-MSG    | PIC X(75)    | 102    | 75     | Return/status message buffer             |
|   | *88 CCARD-RETURN-MSG-OFF* |        |        |        | Value LOW-VALUES — no message            |
| 7 | CC-ACCT-ID          | PIC X(11)    | 177    | 11     | Account ID (alphanumeric form)           |
|   | CC-ACCT-ID-N        | *REDEFINES* CC-ACCT-ID PIC 9(11) |  |  | Account ID (numeric form)          |
| 8 | CC-CARD-NUM         | PIC X(16)    | 188    | 16     | Card number (alphanumeric form)          |
|   | CC-CARD-NUM-N       | *REDEFINES* CC-CARD-NUM PIC 9(16) |  |  | Card number (numeric form)         |
| 9 | CC-CUST-ID          | PIC X(09)    | 204    | 9      | Customer ID (alphanumeric form)          |
|   | CC-CUST-ID-N        | *REDEFINES* CC-CUST-ID PIC 9(9)  |  |  | Customer ID (numeric form)         |

## .NET Class Mapping

This copybook does **not** map to a persistent .NET entity. It represents CICS terminal UI infrastructure
that is replaced by the REST API layer (`NordKredit.Api`).

| COBOL Concept           | .NET Replacement                          |
|-------------------------|-------------------------------------------|
| CCARD-AID (key handler) | HTTP request method + route               |
| CCARD-NEXT-PROG (XCTL) | Controller action routing                 |
| CCARD-NEXT-MAPSET/MAP   | Razor/Blazor views or SPA routing         |
| CCARD-ERROR-MSG         | `ProblemDetails` error response            |
| CCARD-RETURN-MSG        | HTTP response body / success message       |
| CC-ACCT-ID / CC-CARD-NUM / CC-CUST-ID | Request DTO properties    |

## EBCDIC-to-Unicode Conversion Notes

Since this is working storage (not persisted data), EBCDIC conversion is only relevant if
COMMAREA data is exchanged during parallel-run. All PIC X fields use `EbcdicConverter.ConvertToUnicode()`
with IBM Code Page 1143.

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| COCRDLIC    | Card list — AID key handling, navigation, card/account IDs |
| COCRDSLC    | Card detail — AID key handling, card lookup parameters |
| COCRDUPC    | Card update — AID key handling, card/account/customer context |

## Migration Notes

The 88-level condition names (e.g., `CCARD-AID-ENTER`) demonstrate COBOL's built-in pattern for
named constants / enums. In .NET, equivalent patterns include:
- `enum CicsAidKey { Enter, Clear, PA1, PA2, PFK01, ... }` for the AID keys
- Constants or configuration for program names and mapset names

Since the CICS UI is being replaced entirely by a REST API, this copybook's fields are not directly
migrated. The concepts are absorbed into the API controller layer.
