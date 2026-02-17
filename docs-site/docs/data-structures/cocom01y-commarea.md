---
title: COCOM01Y — Application COMMAREA
sidebar_position: 6
---

# COCOM01Y — Application COMMAREA

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | COCOM01Y.cpy                             |
| **Record Name**   | Application COMMAREA                     |
| **Record Length**  | Up to 2,000 bytes (WS-COMMAREA PIC X(2000)) |
| **VSAM File**     | N/A — CICS COMMAREA (inter-program communication) |
| **Domain**        | Cross-cutting (CICS infrastructure)      |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Defines the CICS Communication Area (COMMAREA) structure used for passing data between
programs during pseudo-conversational transactions. Every CICS program in the CardDemo
application includes this copybook to access shared context (calling program, transaction ID)
and program-specific data appended after the common prefix.

In the CICS model, COMMAREA is the primary mechanism for maintaining state across
pseudo-conversational interactions (where the program ends after each screen display and
restarts when the user responds).

## Field Definitions

The exact field layout of COCOM01Y is not yet available in the repository (tracked in #125).
Based on usage in COCRDSLC.cbl and COCRDUPC.cbl, the common COMMAREA prefix includes:

| # | Field Name          | PIC Clause   | Offset | Length | Description                              |
|---|---------------------|--------------|--------|--------|------------------------------------------|
| 1 | *(common prefix)*   |              | 0      | varies | Shared COMMAREA fields defined in COCOM01Y |

Each program appends its own `WS-THIS-PROGCOMMAREA` after the common prefix:

**COCRDSLC (Card Detail):**

| # | Field Name          | PIC Clause   | Length | Description                              |
|---|---------------------|--------------|--------|------------------------------------------|
| 1 | CA-FROM-PROGRAM     | PIC X(08)    | 8      | Calling program name                     |
| 2 | CA-FROM-TRANID      | PIC X(04)    | 4      | Calling transaction ID                   |

**COCRDUPC (Card Update):**

| # | Field Name             | PIC Clause   | Length | Description                              |
|---|------------------------|--------------|--------|------------------------------------------|
| 1 | CCUP-CHANGE-ACTION     | PIC X(1)     | 1      | Change state machine flag                |
| 2 | CCUP-OLD-DETAILS       | (group)      | 80     | Original card data for comparison        |
| 3 | CCUP-NEW-DETAILS       | (group)      | 80     | Modified card data from user input       |
| 4 | CARD-UPDATE-RECORD     | (group)      | 150    | Complete card record for VSAM write      |

The `WS-COMMAREA` field (`PIC X(2000)`) is the raw byte buffer into which the COMMAREA content is placed.

## .NET Class Mapping

The CICS COMMAREA pattern is replaced entirely in the .NET architecture:

| COBOL COMMAREA Concept     | .NET Replacement                          |
|----------------------------|-------------------------------------------|
| COMMAREA byte buffer       | HTTP request/response bodies (JSON)       |
| Pseudo-conversational state | Stateless REST API + client-side state   |
| CA-FROM-PROGRAM            | HTTP `Referer` header / route context     |
| CA-FROM-TRANID             | Correlation ID / `X-Request-ID` header    |
| CCUP-CHANGE-ACTION         | HTTP method semantics (GET = view, PUT = update) |
| CCUP-OLD/NEW-DETAILS       | Optimistic concurrency via `RowVersion`   |

No direct .NET entity maps to COCOM01Y. The concepts are distributed across:
- **Request DTOs** (e.g., `CardUpdateRequest`)
- **Response DTOs** (e.g., `CardDetailResponse`)
- **HTTP infrastructure** (ASP.NET Core middleware)

## EBCDIC-to-Unicode Conversion Notes

COMMAREA data is relevant during parallel-run if CICS transactions are intercepted and forwarded to the .NET system. All PIC X fields use `EbcdicConverter.ConvertToUnicode()` with IBM Code Page 1143.

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| COCRDLIC    | Card list — receives/sends COMMAREA for pagination state |
| COCRDSLC    | Card detail — receives card number in COMMAREA |
| COCRDUPC    | Card update — maintains old/new card data in COMMAREA |
| *(all CICS programs)* | Every CICS program in the system includes COCOM01Y |

## Migration Notes

The COMMAREA pattern represents one of the most significant architectural differences between
the mainframe and .NET systems:

1. **State management**: CICS programs are pseudo-conversational — they terminate after each
   screen send and must reconstruct state from COMMAREA on restart. REST APIs are stateless
   by design, so this concern disappears.
2. **Data serialization**: COMMAREA uses fixed-length EBCDIC byte buffers. REST APIs use
   variable-length JSON with UTF-8 encoding.
3. **Program linkage**: `EXEC CICS XCTL` / `LINK` with COMMAREA becomes HTTP redirects or
   service-to-service calls.
