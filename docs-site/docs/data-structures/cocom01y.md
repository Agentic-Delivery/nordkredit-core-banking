---
id: DS-COMM-001
title: "Common Communication Area"
copybook_name: "COCOM01Y.cpy"
domain: "system"
used_by_programs: [COMEN01C, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, COBIL00C, CORPT00C, COADM01C]
record_length: 0
status: "extracted"
target_schema: "N/A (session state)"
sidebar_position: 20
---

# DS-COMM-001: Common Communication Area (COCOM01Y)

## Overview

The `COCOM01Y.cpy` copybook defines the **CICS COMMAREA** (Communication Area), the primary data structure used to pass context between CICS programs in the CardDemo application. In the CICS transaction processing model, the COMMAREA is the mechanism by which one program passes state to the next -- it carries user identity, navigation context, customer/account/card selection, and last-screen tracking information.

This copybook is included by **every CICS online program** in the CardDemo system. It is not a database record; it is an in-memory structure that exists only for the duration of a CICS pseudo-conversation.

**Source file:** `COCOM01Y.cpy`
**CICS usage:** COMMAREA passed via `EXEC CICS XCTL` / `EXEC CICS RETURN TRANSID`
**Record length:** Variable (not a VSAM record)
**Used by:** ALL CICS programs (COMEN01C, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, COBIL00C, CORPT00C, COADM01C)

## Source COBOL

```cobol
01 CARDDEMO-COMMAREA.
   05 CDEMO-GENERAL-INFO.
      10 CDEMO-FROM-TRANID             PIC X(04).
      10 CDEMO-FROM-PROGRAM            PIC X(08).
      10 CDEMO-TO-TRANID               PIC X(04).
      10 CDEMO-TO-PROGRAM              PIC X(08).
      10 CDEMO-USER-ID                 PIC X(08).
      10 CDEMO-USER-TYPE               PIC X(01).
         88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
         88 CDEMO-USRTYP-USER          VALUE 'U'.
      10 CDEMO-PGM-CONTEXT             PIC 9(01).
         88 CDEMO-PGM-ENTER            VALUE 0.
         88 CDEMO-PGM-REENTER          VALUE 1.
   05 CDEMO-CUSTOMER-INFO.
      10 CDEMO-CUST-ID                 PIC 9(09).
      10 CDEMO-CUST-FNAME              PIC X(25).
      10 CDEMO-CUST-MNAME              PIC X(25).
      10 CDEMO-CUST-LNAME              PIC X(25).
   05 CDEMO-ACCOUNT-INFO.
      10 CDEMO-ACCT-ID                 PIC 9(11).
      10 CDEMO-ACCT-STATUS             PIC X(01).
   05 CDEMO-CARD-INFO.
      10 CDEMO-CARD-NUM                PIC 9(16).
   05 CDEMO-MORE-INFO.
      10  CDEMO-LAST-MAP               PIC X(7).
      10  CDEMO-LAST-MAPSET            PIC X(7).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `CDEMO-FROM-TRANID` | `X(04)` | 4 | Alphanumeric | CICS transaction ID of the calling program |
| 2 | `CDEMO-FROM-PROGRAM` | `X(08)` | 8 | Alphanumeric | Program name of the calling program |
| 3 | `CDEMO-TO-TRANID` | `X(04)` | 4 | Alphanumeric | CICS transaction ID of the target program |
| 4 | `CDEMO-TO-PROGRAM` | `X(08)` | 8 | Alphanumeric | Program name of the target program |
| 5 | `CDEMO-USER-ID` | `X(08)` | 8 | Alphanumeric | Authenticated user identifier |
| 6 | `CDEMO-USER-TYPE` | `X(01)` | 1 | Alphanumeric | User type flag: `A` = Admin, `U` = User |
| 7 | `CDEMO-PGM-CONTEXT` | `9(01)` | 1 | Numeric | Program context: `0` = initial entry, `1` = re-entry |
| 8 | `CDEMO-CUST-ID` | `9(09)` | 9 | Numeric | Selected customer identifier |
| 9 | `CDEMO-CUST-FNAME` | `X(25)` | 25 | Alphanumeric | Customer first name |
| 10 | `CDEMO-CUST-MNAME` | `X(25)` | 25 | Alphanumeric | Customer middle name |
| 11 | `CDEMO-CUST-LNAME` | `X(25)` | 25 | Alphanumeric | Customer last name |
| 12 | `CDEMO-ACCT-ID` | `9(11)` | 11 | Numeric | Selected account identifier |
| 13 | `CDEMO-ACCT-STATUS` | `X(01)` | 1 | Alphanumeric | Account active status flag |
| 14 | `CDEMO-CARD-NUM` | `9(16)` | 16 | Numeric | Selected card number (full PAN) |
| 15 | `CDEMO-LAST-MAP` | `X(7)` | 7 | Alphanumeric | Last BMS map name displayed |
| 16 | `CDEMO-LAST-MAPSET` | `X(7)` | 7 | Alphanumeric | Last BMS mapset name used |

## Field Notes

1. **CDEMO-FROM-TRANID / CDEMO-FROM-PROGRAM**: These fields implement a "breadcrumb" navigation pattern. Each program records where it came from so that it can return to the previous screen when the user presses PF3 (back). In the .NET architecture, this is replaced by standard HTTP routing, browser history, or navigation state management.

2. **CDEMO-TO-TRANID / CDEMO-TO-PROGRAM**: The target program identifiers. Set before issuing `EXEC CICS XCTL` to transfer control. In .NET, this maps to route parameters or controller action selection.

3. **CDEMO-USER-ID** (`PIC X(08)`): Carries the authenticated user's ID throughout the session. Set by `COSGN00C` upon successful login. Limited to 8 characters (CICS/RACF constraint). In .NET, replaced by claims from Azure AD / Azure AD B2C JWT tokens.

4. **CDEMO-USER-TYPE** (`PIC X(01)`): Determines authorization level. The 88-level conditions `CDEMO-USRTYP-ADMIN` (`A`) and `CDEMO-USRTYP-USER` (`U`) are used throughout the application to gate access to admin-only features (user management, transaction type configuration). In .NET, this maps to role-based authorization (Azure AD roles or claims).

5. **CDEMO-PGM-CONTEXT** (`PIC 9(01)`): Distinguishes between first-time program entry (`0`) and re-entry after user input (`1`). This is fundamental to the CICS pseudo-conversational programming model: the program ends after sending a screen, and restarts when the user responds. The context flag tells the program whether to initialize or process input. In .NET, this is handled naturally by HTTP request/response or Blazor component lifecycle.

6. **CDEMO-CUST-ID / CDEMO-CUST-FNAME / CDEMO-CUST-MNAME / CDEMO-CUST-LNAME**: Customer context fields. When a user selects a customer from a list, these fields are populated and carried to downstream screens (account view, card list, etc.). The name fields provide display-only context without requiring a re-read of the customer file.

7. **CDEMO-ACCT-ID / CDEMO-ACCT-STATUS**: Selected account context. Populated when a user navigates to an account-level operation.

8. **CDEMO-CARD-NUM** (`PIC 9(16)`): The full 16-digit card number (Primary Account Number / PAN). This field carries the selected card's PAN between programs. **PCI-DSS concern:** The full PAN is transmitted in-memory between CICS programs. In the .NET architecture, the full PAN must not be stored in session state or transmitted in URLs. Use tokenization or reference IDs instead, displaying only the last 4 digits in the UI.

9. **CDEMO-LAST-MAP / CDEMO-LAST-MAPSET**: Tracks which BMS screen was last displayed. Used for screen restoration and navigation logic. No direct .NET equivalent needed; replaced by routing/navigation state.

## Target Architecture Mapping

| Aspect | CICS (Current) | .NET (Target) |
|--------|----------------|---------------|
| **State mechanism** | CICS COMMAREA (in-memory, per-transaction) | ASP.NET session state, JWT claims, or Blazor circuit state |
| **User identity** | `CDEMO-USER-ID` + `CDEMO-USER-TYPE` | Azure AD / Azure AD B2C JWT token with role claims |
| **Navigation context** | `FROM-TRANID/PROGRAM`, `TO-TRANID/PROGRAM` | ASP.NET routing, browser navigation, URL parameters |
| **Entity selection** | `CDEMO-CUST-ID`, `CDEMO-ACCT-ID`, `CDEMO-CARD-NUM` | Route parameters, query strings, or view state |
| **Program lifecycle** | `CDEMO-PGM-CONTEXT` (enter/re-enter) | HTTP GET/POST distinction or Blazor component state |
| **Screen tracking** | `CDEMO-LAST-MAP`, `CDEMO-LAST-MAPSET` | Not needed (browser handles page history) |

### .NET Session Context (Conceptual)

```csharp
public class CardDemoSessionContext
{
    // User identity (from Azure AD claims)
    public string UserId { get; set; }
    public UserType UserType { get; set; } // Admin, User

    // Selected entity context
    public long? CustomerId { get; set; }
    public string CustomerDisplayName { get; set; }
    public long? AccountId { get; set; }
    public string AccountStatus { get; set; }
    public string CardTokenReference { get; set; } // Tokenized, NOT raw PAN
}

public enum UserType
{
    User,
    Admin
}
```

## Migration Notes

### Key Migration Decisions

1. **Eliminate COMMAREA pattern:** The CICS COMMAREA is a workaround for the pseudo-conversational model. In .NET, standard HTTP session management, JWT tokens, and route parameters replace this entirely. There is no need to replicate the COMMAREA as a single monolithic object.

2. **Card number tokenization:** The `CDEMO-CARD-NUM` field carries the full PAN between programs. In the .NET system, card numbers must be tokenized. The session context should carry a token reference, not the raw PAN. Display should be masked (e.g., `**** **** **** 1234`).

3. **User type to roles:** The simple `A`/`U` user type flag is replaced by Azure AD role-based access control (RBAC). Roles should be more granular than the binary admin/user split (e.g., `AccountViewer`, `AccountEditor`, `CardManager`, `UserAdmin`, `ReportViewer`).

4. **Program context not needed:** The `CDEMO-PGM-CONTEXT` enter/re-enter flag is an artifact of CICS pseudo-conversational programming. HTTP naturally distinguishes GET (initial load) from POST (form submission), and Blazor components have explicit lifecycle methods.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **PCI-DSS** v4.0 Req. 3 | Protect stored cardholder data | `CDEMO-CARD-NUM` carries full PAN in COMMAREA. The .NET replacement must NOT store the full PAN in session state. Use tokenization (Azure Key Vault or payment processor tokens). Display only last 4 digits. |
| **GDPR** Art. 5(1)(c) | Data minimization | The COMMAREA carries customer name fields (`FNAME`, `MNAME`, `LNAME`) for display convenience. In .NET, avoid storing PII in session state; retrieve from the data layer only when needed for the current view. |
| **GDPR** Art. 32 | Security of processing | CICS COMMAREA is in-memory only and not persisted. If .NET session state is server-side (e.g., Redis), ensure encryption at rest and appropriate TTL expiry. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- access management | `CDEMO-USER-TYPE` controls authorization. The .NET replacement must implement role-based authorization with Azure AD, with audit logging of all authorization decisions for sensitive operations. |
| **PSD2** Art. 97 | Strong Customer Authentication | The COMMAREA carries user identity from sign-on. The .NET system must ensure that session tokens are bound to an SCA-authenticated session (Azure AD B2C with MFA / BankID). |
| **DORA** Art. 9 | ICT security policies | Session state management must follow secure coding practices: session timeout, token rotation, and protection against session fixation/hijacking. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
