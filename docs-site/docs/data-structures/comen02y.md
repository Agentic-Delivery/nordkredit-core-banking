---
id: DS-MENU-001
title: "Main Menu Options"
copybook_name: "COMEN02Y.cpy"
domain: "system"
used_by_programs: [COMEN01C]
record_length: 0
status: "extracted"
target_schema: "N/A (configuration)"
sidebar_position: 22
---

# DS-MENU-001: Main Menu Options (COMEN02Y)

## Overview

The `COMEN02Y.cpy` copybook defines the **Main Menu Options**, the primary navigation structure for the CardDemo CICS application. It defines 11 menu items covering all major application functions (account management, credit card operations, transactions, reports, and bill payment), each with a user type authorization flag.

This is not a database record -- it is a compile-time constant embedded in the CICS program `COMEN01C`. The menu items define navigation targets and include per-item authorization via the `OPT-USRTYPE` field.

**Source file:** `COMEN02Y.cpy`
**Used by:** `COMEN01C` (Main Menu CICS program)

## Source COBOL

```cobol
01 CARDDEMO-MAIN-MENU-OPTIONS.
   05 FILLER PIC X(46) VALUE '01Account View                      COACTVWCU'.
   05 FILLER PIC X(46) VALUE '02Account Update                    COACTUPC '.
   05 FILLER PIC X(46) VALUE '03Credit Card List                  COCRDLICU'.
   05 FILLER PIC X(46) VALUE '04Credit Card View                  COCRDSLCU'.
   05 FILLER PIC X(46) VALUE '05Credit Card Update                COCRDUPC '.
   05 FILLER PIC X(46) VALUE '06Transaction List                  COTRN00CU'.
   05 FILLER PIC X(46) VALUE '07Transaction View                  COTRN01CU'.
   05 FILLER PIC X(46) VALUE '08Transaction Add                   COTRN02C '.
   05 FILLER PIC X(46) VALUE '09Reports                           CORPT00CU'.
   05 FILLER PIC X(46) VALUE '10Bill Payment                      COBIL00CU'.
   05 FILLER PIC X(46) VALUE '11Pending Authorization View        COPND00CU'.
   05 FILLER PIC X(46) VALUE SPACES.
01 CARDDEMO-MAIN-MENU-OPT-TABLE REDEFINES
   CARDDEMO-MAIN-MENU-OPTIONS.
   05 CDEMO-MENU-OPT-ENTRY OCCURS 12 TIMES.
      10 CDEMO-OPT-NUM                  PIC 9(02).
      10 CDEMO-OPT-NAME                 PIC X(35).
      10 CDEMO-OPT-PGMNAME             PIC X(08).
      10 CDEMO-OPT-USRTYPE             PIC X(01).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `CDEMO-OPT-NUM` | `9(02)` | 2 | Numeric | Menu option number (01-11, 00 for unused slots) |
| 2 | `CDEMO-OPT-NAME` | `X(35)` | 35 | Alphanumeric | Display label for the menu option |
| 3 | `CDEMO-OPT-PGMNAME` | `X(08)` | 8 | Alphanumeric | Target CICS program name to `XCTL` to |
| 4 | `CDEMO-OPT-USRTYPE` | `X(01)` | 1 | Alphanumeric | Required user type: `U` = all users, space = admin only |

**Entry size:** 46 bytes per option
**Array size:** 12 entries (11 populated + 1 blank/reserved)

## Field Notes

1. **REDEFINES pattern:** Same pattern as `COADM02Y` -- hardcoded `FILLER` strings redefined as an array of structured entries.

2. **Menu options defined:**

| Option | Name | Target Program | User Type | Access |
|--------|------|---------------|-----------|--------|
| 01 | Account View | COACTVWC | U | All users |
| 02 | Account Update | COACTUPC | (space) | Admin only |
| 03 | Credit Card List | COCRDLIC | U | All users |
| 04 | Credit Card View | COCRDSLC | U | All users |
| 05 | Credit Card Update | COCRDUPC | (space) | Admin only |
| 06 | Transaction List | COTRN00C | U | All users |
| 07 | Transaction View | COTRN01C | U | All users |
| 08 | Transaction Add | COTRN02C | (space) | Admin only |
| 09 | Reports | CORPT00C | U | All users |
| 10 | Bill Payment | COBIL00C | U | All users |
| 11 | Pending Authorization View | COPND00C | U | All users |

3. **OPT-USRTYPE authorization:** The `U` flag indicates the option is visible to regular users. Options without the `U` flag (spaces) are visible only to admin users (`CDEMO-USER-TYPE = 'A'`). The `COMEN01C` program filters the displayed menu based on the current user's type from the COMMAREA.

4. **Authorization pattern:** View operations (Account View, Card List/View, Transaction List/View, Reports) are available to all users. Modify operations (Account Update, Card Update, Transaction Add) are restricted to admin users. This is a basic RBAC pattern that should be expanded in the .NET migration.

5. **Unused slot:** Entry 12 is blank (spaces), providing one slot for expansion.

## Target Architecture Mapping

| Aspect | CICS (Current) | .NET (Target) |
|--------|----------------|---------------|
| **Menu structure** | Hardcoded COBOL `FILLER` values | `appsettings.json` or component-based navigation |
| **Navigation** | `EXEC CICS XCTL PROGRAM(pgmname)` | ASP.NET routing / Blazor `NavigationManager` |
| **Authorization** | `OPT-USRTYPE` check against COMMAREA `USER-TYPE` | `[Authorize(Roles = "...")]` or policy-based authorization |
| **Menu rendering** | BMS map with numbered options | Sidebar/navbar component with role-filtered links |

### .NET Navigation Configuration (Conceptual)

```csharp
public class MainMenuConfiguration
{
    public List<MainMenuItem> Items { get; set; } = new()
    {
        new("Account View",               "/accounts/view",             "User,Admin"),
        new("Account Update",             "/accounts/edit",             "Admin"),
        new("Credit Card List",           "/cards",                     "User,Admin"),
        new("Credit Card View",           "/cards/view",                "User,Admin"),
        new("Credit Card Update",         "/cards/edit",                "Admin"),
        new("Transaction List",           "/transactions",              "User,Admin"),
        new("Transaction View",           "/transactions/view",         "User,Admin"),
        new("Transaction Add",            "/transactions/add",          "Admin"),
        new("Reports",                    "/reports",                   "User,Admin"),
        new("Bill Payment",               "/payments/bill",             "User,Admin"),
        new("Pending Authorization View", "/authorizations/pending",    "User,Admin"),
    };
}

public record MainMenuItem(string Name, string Route, string RequiredRoles);
```

## Migration Notes

1. **Externalize configuration:** The hardcoded menu should become externalized configuration or a role-aware navigation component. This allows menu changes and new features to be added without recompilation.

2. **Granular RBAC:** The binary Admin/User authorization should be expanded. For example, a `TransactionEntry` role could allow transaction adds without granting full admin access. Azure AD application roles should map to specific business capabilities, not a single admin flag.

3. **Read vs. write separation:** The existing pattern already separates view (all users) from modify (admin only) operations. This aligns well with CQRS principles and can be reinforced with separate authorization policies for read and write operations in .NET.

4. **Pending Authorization View (option 11):** This option references `COPND00C`, which may not be fully implemented in the source system. Verify during migration whether this program exists and is functional.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- role-based access | Menu authorization must enforce least-privilege access. The .NET system should implement granular roles rather than the binary admin/user model. Access control decisions must be logged. |
| **PSD2** Art. 97 | Strong Customer Authentication for payment operations | Bill Payment (option 10) and Transaction Add (option 08) require SCA before execution. The menu navigation itself does not require SCA, but the target operations must enforce it. |
| **GDPR** Art. 25 | Data protection by design | Menu visibility should follow need-to-know principles. Users should only see menu items they are authorized to access (the existing pattern already does this). |
| **DORA** Art. 9 | ICT access control | Administrative menu items (Account Update, Card Update, Transaction Add) must be protected by role-based access controls with audit logging of access attempts. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
