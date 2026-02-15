---
id: DS-CRDWK-001
title: "Card Work Area"
copybook_name: "CVCRD01Y.cpy"
domain: "account-management"
used_by_programs: [COCRDLIC, COCRDSLC, COCRDUPC, COACTUPC, COACTVWC, COTRN00C, COTRN01C, COTRN02C]
record_length: 0
status: "extracted"
target_schema: "N/A (session context)"
sidebar_position: 4
---

# DS-CRDWK-001: Card Work Area (CVCRD01Y)

## Overview

The `CVCRD01Y.cpy` copybook defines the **Card Work Area** (`CC-WORK-AREAS`), a COBOL working storage structure used as shared context across CICS online transaction programs. Unlike the other data record copybooks in this documentation set, this is **not** a database/VSAM record -- it is a program communication area (commarea) passed between CICS programs via `EXEC CICS XCTL` and `EXEC CICS LINK` calls.

This structure carries navigation state (which program/map to display next), error/return messages, and key entity identifiers (account ID, card number, customer ID) that are needed across multiple screens in the CardDemo application. It is the CICS equivalent of session state in a web application.

**Source file:** `CVCRD01Y.cpy`
**Record type:** Working storage / CICS commarea (not a VSAM record)
**Record length:** N/A (working storage, not a fixed-length data record)
**Used by:** `COCRDLIC`, `COCRDSLC`, `COCRDUPC`, `COACTUPC`, `COACTVWC`, `COTRN00C`, `COTRN01C`, `COTRN02C`

## Source COBOL

```cobol
01  CC-WORK-AREAS.
   05 CC-WORK-AREA.
      10 CCARD-AID                         PIC X(5).
         88  CCARD-AID-ENTER                VALUE 'ENTER'.
         88  CCARD-AID-CLEAR                VALUE 'CLEAR'.
         88  CCARD-AID-PA1                  VALUE 'PA1  '.
         88  CCARD-AID-PA2                  VALUE 'PA2  '.
         88  CCARD-AID-PFK01                VALUE 'PFK01'.
         88  CCARD-AID-PFK02                VALUE 'PFK02'.
         88  CCARD-AID-PFK03                VALUE 'PFK03'.
         88  CCARD-AID-PFK04                VALUE 'PFK04'.
         88  CCARD-AID-PFK05                VALUE 'PFK05'.
         88  CCARD-AID-PFK06                VALUE 'PFK06'.
         88  CCARD-AID-PFK07                VALUE 'PFK07'.
         88  CCARD-AID-PFK08                VALUE 'PFK08'.
         88  CCARD-AID-PFK09                VALUE 'PFK09'.
         88  CCARD-AID-PFK10                VALUE 'PFK10'.
         88  CCARD-AID-PFK11                VALUE 'PFK11'.
         88  CCARD-AID-PFK12                VALUE 'PFK12'.
      10  CCARD-NEXT-PROG                  PIC X(8).
      10  CCARD-NEXT-MAPSET                PIC X(7).
      10  CCARD-NEXT-MAP                   PIC X(7).
      10  CCARD-ERROR-MSG                  PIC X(75).
      10  CCARD-RETURN-MSG                 PIC X(75).
        88  CCARD-RETURN-MSG-OFF           VALUE LOW-VALUES.
      10 CC-ACCT-ID                        PIC X(11) VALUE SPACES.
      10 CC-ACCT-ID-N REDEFINES CC-ACCT-ID PIC 9(11).
      10 CC-CARD-NUM                       PIC X(16) VALUE SPACES.
      10 CC-CARD-NUM-N REDEFINES CC-CARD-NUM PIC 9(16).
      10 CC-CUST-ID                        PIC X(09) VALUE SPACES.
      10 CC-CUST-ID-N REDEFINES CC-CUST-ID PIC 9(9).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Nullable | Target Mapping |
|---|------------|-----------|--------|--------|------|-------------|----------|----------------|
| 1 | `CCARD-AID` | `X(5)` | 0 | 5 | Alphanumeric | CICS Attention Identifier (AID) key pressed by user | No | HTTP action / button click event |
| 2 | `CCARD-NEXT-PROG` | `X(8)` | 5 | 8 | Alphanumeric | Name of next CICS program to execute | No | Route/controller name |
| 3 | `CCARD-NEXT-MAPSET` | `X(7)` | 13 | 7 | Alphanumeric | Name of next BMS mapset to display | No | View/page name |
| 4 | `CCARD-NEXT-MAP` | `X(7)` | 20 | 7 | Alphanumeric | Name of next BMS map within the mapset | No | View component name |
| 5 | `CCARD-ERROR-MSG` | `X(75)` | 27 | 75 | Alphanumeric | Error message to display on next screen | Yes | `TempData["Error"]` or validation result |
| 6 | `CCARD-RETURN-MSG` | `X(75)` | 102 | 75 | Alphanumeric | Success/return message to display | Yes | `TempData["Success"]` or notification |
| 7 | `CC-ACCT-ID` | `X(11)` | 177 | 11 | Alphanumeric (alpha view) | Account ID (alphanumeric view) | Yes | `SessionState.AccountId` (string) |
| 8 | `CC-ACCT-ID-N` | `9(11)` | 177 | 11 | Numeric (REDEFINES) | Account ID (numeric view, same storage) | Yes | `SessionState.AccountId` (long) |
| 9 | `CC-CARD-NUM` | `X(16)` | 188 | 16 | Alphanumeric (alpha view) | Card number (alphanumeric view) | Yes | `SessionState.CardNumber` (string, masked) |
| 10 | `CC-CARD-NUM-N` | `9(16)` | 188 | 16 | Numeric (REDEFINES) | Card number (numeric view, same storage) | Yes | `SessionState.CardNumber` (long) |
| 11 | `CC-CUST-ID` | `X(09)` | 204 | 9 | Alphanumeric (alpha view) | Customer ID (alphanumeric view) | Yes | `SessionState.CustomerId` (string) |
| 12 | `CC-CUST-ID-N` | `9(9)` | 204 | 9 | Numeric (REDEFINES) | Customer ID (numeric view, same storage) | Yes | `SessionState.CustomerId` (long) |

**Note:** `CC-ACCT-ID-N`, `CC-CARD-NUM-N`, and `CC-CUST-ID-N` are COBOL `REDEFINES` of their alphanumeric counterparts. They occupy the same memory -- `REDEFINES` provides a numeric lens over the same bytes. In .NET, this is handled by type parsing (e.g., `long.Parse(accountIdString)`).

## Field Notes

1. **CCARD-AID** (`PIC X(5)` with 88-level items): Captures the CICS Attention Identifier (AID) key pressed by the terminal operator. The 88-level condition names define symbolic names for each key:
   - `ENTER` -- Submit/confirm action
   - `CLEAR` -- Clear screen / cancel
   - `PA1`, `PA2` -- Program Attention keys (typically unused or system functions)
   - `PFK01` through `PFK12` -- Program Function Keys (navigation and actions)

   In the .NET target, these map to HTTP request methods, button click events, or keyboard shortcuts. The navigation logic that checks `CCARD-AID` must be preserved in the controller/routing layer.

2. **CCARD-NEXT-PROG** (`PIC X(8)`): The CICS program name to transfer control to via `EXEC CICS XCTL`. Examples: `COCRDLIC` (card list), `COCRDUPC` (card update). In .NET, this maps to a route redirect or controller action selection.

3. **CCARD-NEXT-MAPSET** / **CCARD-NEXT-MAP** (`PIC X(7)` each): The BMS mapset and map to display on the next screen send. In .NET, these map to view/page names in the routing configuration.

4. **CCARD-ERROR-MSG** / **CCARD-RETURN-MSG** (`PIC X(75)` each): Message fields passed between programs for display on the next screen. `CCARD-RETURN-MSG` has an 88-level `CCARD-RETURN-MSG-OFF` set to `LOW-VALUES` (all binary zeros), which indicates "no message". In .NET, these map to `TempData`, flash messages, or a notification service.

5. **CC-ACCT-ID** / **CC-ACCT-ID-N** (`PIC X(11)` / `PIC 9(11)` REDEFINES): The account identifier carried in session context. The alphanumeric view (`X(11)`) is used for string operations (comparison, display), while the numeric view (`9(11)`) is used for arithmetic or formatted output. Default value is `SPACES`, indicating no account is selected.

6. **CC-CARD-NUM** / **CC-CARD-NUM-N** (`PIC X(16)` / `PIC 9(16)` REDEFINES): The card number carried in session context. **PCI-DSS note:** Even in working storage / session state, the full PAN should not be stored in the .NET replacement. The target implementation should carry only a card token or the card surrogate key (`CardId`), never the full PAN.

7. **CC-CUST-ID** / **CC-CUST-ID-N** (`PIC X(09)` / `PIC 9(9)` REDEFINES): The customer identifier carried in session context. Default value is `SPACES`.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration | Migration Action |
|-------|---------------------|-----------------|
| `CCARD-AID` | EBCDIC character values for AID key names | Map to .NET action/event names; no data migration needed |
| `CCARD-NEXT-PROG` | EBCDIC program names (uppercase A-Z, 0-9) | Map to .NET route/controller names; no data migration needed |
| `CCARD-NEXT-MAPSET` / `CCARD-NEXT-MAP` | EBCDIC map names | Map to .NET view names; no data migration needed |
| `CCARD-ERROR-MSG` / `CCARD-RETURN-MSG` | EBCDIC text messages | Convert message text to UTF-8 for resource files; `LOW-VALUES` maps to null/empty |
| `CC-ACCT-ID` / `CC-CARD-NUM` / `CC-CUST-ID` | EBCDIC digits in alphanumeric fields; `SPACES` = `0x40` | No data migration needed (session state is transient) |

**Note:** Since this is a working storage structure and not a persisted data record, EBCDIC encoding is only relevant for understanding the COBOL program logic during migration. No data conversion pipeline is needed for this copybook.

## Referential Integrity

This structure does not define persistent relationships. However, the entity identifiers it carries reference the following data records:

| Field | References | Entity |
|-------|-----------|--------|
| `CC-ACCT-ID` / `CC-ACCT-ID-N` | `CVACT01Y.ACCT-ID` | Account Record |
| `CC-CARD-NUM` / `CC-CARD-NUM-N` | `CVACT02Y.CARD-NUM` | Card Record |
| `CC-CUST-ID` / `CC-CUST-ID-N` | `CVCUS01Y.CUST-ID` | Customer Record |

In the .NET application, the session context must validate that these IDs reference existing entities before using them for data operations.

## Sample Data

| Field | Example Value | Notes |
|-------|--------------|-------|
| `CCARD-AID` | `ENTER` | User pressed Enter key |
| `CCARD-NEXT-PROG` | `COCRDSLC` | Transfer to Card Select program |
| `CCARD-NEXT-MAPSET` | `COCRDSL` | Display Card Select mapset |
| `CCARD-NEXT-MAP` | `COCRDSL` | Display Card Select map |
| `CCARD-ERROR-MSG` | `INVALID ACCOUNT NUMBER` | Error from validation |
| `CCARD-RETURN-MSG` | `UPDATE SUCCESSFUL` | Success confirmation |
| `CC-ACCT-ID` | `00000000012` | Selected account (alpha view) |
| `CC-ACCT-ID-N` | `00000000012` | Same bytes, numeric view |
| `CC-CARD-NUM` | `4000123456789012` | Selected card (alpha view) |
| `CC-CARD-NUM-N` | `4000123456789012` | Same bytes, numeric view |
| `CC-CUST-ID` | `000000001` | Selected customer (alpha view) |
| `CC-CUST-ID-N` | `000000001` | Same bytes, numeric view |

## Migration Notes

### Target Architecture Mapping

This copybook does **not** map to a database table. Instead, it maps to session/context state in the .NET application:

| COBOL Concept | .NET Equivalent |
|--------------|-----------------|
| CICS commarea (passed via XCTL/LINK) | ASP.NET Session State, `TempData`, or a scoped DI service (`CardDemoSessionContext`) |
| `CCARD-AID` (AID key detection) | HTTP method + route + button/action parameter |
| `CCARD-NEXT-PROG` / `CCARD-NEXT-MAPSET` / `CCARD-NEXT-MAP` | ASP.NET routing / `RedirectToAction` / Blazor navigation |
| `CCARD-ERROR-MSG` / `CCARD-RETURN-MSG` | `TempData["Error"]` / `TempData["Success"]` or notification service |
| `CC-ACCT-ID` / `CC-CARD-NUM` / `CC-CUST-ID` | Strongly-typed session context object or route parameters |
| 88-level AID conditions | C# enum or constants class |
| REDEFINES (alpha/numeric views) | Type parsing: `long.Parse()` / `.ToString()` |

### Suggested .NET Implementation

```csharp
/// <summary>
/// Session context replacing CVCRD01Y CC-WORK-AREAS commarea.
/// Scoped per-request via DI.
/// </summary>
public class CardDemoSessionContext
{
    // Navigation state (replaces CCARD-NEXT-PROG, CCARD-NEXT-MAPSET, CCARD-NEXT-MAP)
    public string? NextRoute { get; set; }
    public string? ErrorMessage { get; set; }      // CCARD-ERROR-MSG
    public string? ReturnMessage { get; set; }      // CCARD-RETURN-MSG

    // Entity identifiers (replaces CC-ACCT-ID, CC-CARD-NUM, CC-CUST-ID)
    public long? AccountId { get; set; }            // CC-ACCT-ID-N
    public long? CardId { get; set; }               // Surrogate key (NOT full PAN)
    public long? CustomerId { get; set; }           // CC-CUST-ID-N
}

/// <summary>
/// AID key mappings (replaces 88-level conditions on CCARD-AID).
/// </summary>
public static class CardDemoActions
{
    public const string Enter = "Enter";
    public const string Clear = "Clear";
    public const string PageUp = "PFK07";           // PF7 = page up
    public const string PageDown = "PFK08";          // PF8 = page down
    // ... additional PFK mappings as needed
}
```

### No DDL Required

This copybook does not require a database table. The session context is transient and stored in-memory (ASP.NET Session or scoped service). No `CREATE TABLE` DDL is needed.

### Post-Migration Validation

Validation for this copybook is behavioral rather than data-based:

- Verify that navigation between screens preserves entity context (account ID, card number, customer ID) across HTTP requests.
- Verify that error and success messages are correctly passed between controller actions.
- Verify that PF key equivalents (PFK07 = page up, PFK08 = page down) trigger the correct actions in the .NET UI.
- Verify that the "Clear" action resets the session context (equivalent to `CCARD-AID-CLEAR`).

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS 3.4** | Render PAN unreadable anywhere it is stored | The COBOL commarea carries the full PAN in `CC-CARD-NUM`. The .NET session context must **not** carry the full PAN. Use the `CardId` surrogate key or a tokenized reference instead. Session storage must not contain clear-text PAN even transiently. |
| **PCI-DSS 4.1** | Use strong cryptography to safeguard cardholder data during transmission | If session state is stored server-side (recommended), PAN exposure is limited. If any card data flows through cookies or client-side state, it must be encrypted. |
| **GDPR** Art. 5(1)(f) | Integrity and confidentiality of personal data | Session state containing customer IDs and account IDs must be protected against unauthorized access. ASP.NET session cookies must use `HttpOnly`, `Secure`, and `SameSite` flags. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- access logging | Navigation between screens (which effectively represents user actions on financial data) must be logged for audit purposes. The .NET equivalent should log controller action invocations with the authenticated user context. |
| **DORA** Art. 9 | ICT security -- session management | Session timeouts must be enforced (equivalent to CICS terminal timeout). Idle sessions must be terminated to prevent unauthorized access to financial data. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
