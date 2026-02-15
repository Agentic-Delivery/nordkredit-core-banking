---
id: DS-ADMN-001
title: "Admin Menu Options"
copybook_name: "COADM02Y.cpy"
domain: "system"
used_by_programs: [COADM01C]
record_length: 0
status: "extracted"
target_schema: "N/A (configuration)"
sidebar_position: 21
---

# DS-ADMN-001: Admin Menu Options (COADM02Y)

## Overview

The `COADM02Y.cpy` copybook defines the **Admin Menu Options**, a static configuration structure that populates the administrator menu screen in the CardDemo CICS application. It defines 6 admin-only operations (user management and transaction type maintenance), stored as a COBOL table structure using `REDEFINES`.

This is not a database record -- it is a compile-time constant embedded in the CICS program `COADM01C`. The menu items define the navigation targets for administrative functions.

**Source file:** `COADM02Y.cpy`
**Used by:** `COADM01C` (Admin Menu CICS program)

## Source COBOL

```cobol
01 CARDDEMO-ADMIN-MENU-OPTIONS.
   05 FILLER PIC X(45) VALUE '01User List                        COUSR00C'.
   05 FILLER PIC X(45) VALUE '02User Add                         COUSR01C'.
   05 FILLER PIC X(45) VALUE '03User Update                      COUSR02C'.
   05 FILLER PIC X(45) VALUE '04User Delete                      COUSR03C'.
   05 FILLER PIC X(45) VALUE '05Transaction Type List/Update      COTRN04C'.
   05 FILLER PIC X(45) VALUE '06Transaction Type Maintenance      COTRN05C'.
   05 FILLER PIC X(45) VALUE SPACES.
   05 FILLER PIC X(45) VALUE SPACES.
   05 FILLER PIC X(45) VALUE SPACES.
01 CARDDEMO-ADMIN-MENU-OPT-TABLE REDEFINES
   CARDDEMO-ADMIN-MENU-OPTIONS.
   05 CDEMO-ADMIN-MENU-OPT-ENTRY OCCURS 9 TIMES.
      10 CDEMO-ADMIN-OPT-NUM            PIC 9(02).
      10 CDEMO-ADMIN-OPT-NAME           PIC X(35).
      10 CDEMO-ADMIN-OPT-PGMNAME        PIC X(08).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `CDEMO-ADMIN-OPT-NUM` | `9(02)` | 2 | Numeric | Menu option number (01-06, 00 for unused slots) |
| 2 | `CDEMO-ADMIN-OPT-NAME` | `X(35)` | 35 | Alphanumeric | Display label for the menu option |
| 3 | `CDEMO-ADMIN-OPT-PGMNAME` | `X(08)` | 8 | Alphanumeric | Target CICS program name to `XCTL` to |

**Entry size:** 45 bytes per option
**Array size:** 9 entries (6 populated + 3 blank/reserved)

## Field Notes

1. **REDEFINES pattern:** The COBOL source defines the menu data as a series of `FILLER` lines with hardcoded string values, then `REDEFINES` the block as an array of 9 structured entries. This is a common COBOL pattern for defining static lookup tables without a file.

2. **Menu options defined:**

| Option | Name | Target Program | Function |
|--------|------|---------------|----------|
| 01 | User List | COUSR00C | List all system users |
| 02 | User Add | COUSR01C | Create a new user account |
| 03 | User Update | COUSR02C | Modify an existing user |
| 04 | User Delete | COUSR03C | Delete a user account |
| 05 | Transaction Type List/Update | COTRN04C | View and update transaction types |
| 06 | Transaction Type Maintenance | COTRN05C | Maintain transaction type definitions |

3. **Unused slots:** Entries 7-9 are blank (filled with spaces), providing expansion room without recompilation. The program logic skips entries where `OPT-NUM` is zero or spaces.

4. **Admin-only access:** This menu is only accessible to users with `CDEMO-USER-TYPE = 'A'` (Admin). The `COADM01C` program checks user type before displaying this menu.

## Target Architecture Mapping

| Aspect | CICS (Current) | .NET (Target) |
|--------|----------------|---------------|
| **Menu structure** | Hardcoded COBOL `FILLER` values | `appsettings.json` or database-driven navigation configuration |
| **Navigation** | `EXEC CICS XCTL PROGRAM(pgmname)` | ASP.NET routing / Blazor `NavigationManager` |
| **Authorization** | `CDEMO-USER-TYPE = 'A'` check in program | `[Authorize(Roles = "Admin")]` attribute or policy-based authorization |
| **Menu rendering** | BMS map with option numbers | Sidebar/navbar component with role-filtered links |

### .NET Navigation Configuration (Conceptual)

```csharp
public class AdminMenuConfiguration
{
    public List<AdminMenuItem> Items { get; set; } = new()
    {
        new("User List", "/admin/users", "Admin"),
        new("User Add", "/admin/users/add", "Admin"),
        new("User Update", "/admin/users/edit", "Admin"),
        new("User Delete", "/admin/users/delete", "Admin"),
        new("Transaction Type List", "/admin/transaction-types", "Admin"),
        new("Transaction Type Maintenance", "/admin/transaction-types/maintain", "Admin"),
    };
}

public record AdminMenuItem(string Name, string Route, string RequiredRole);
```

## Migration Notes

1. **Configuration, not code:** The hardcoded COBOL menu should become externalized configuration in the .NET system (e.g., `appsettings.json`, database-driven menu, or a navigation component with role-based filtering). This allows menu changes without redeployment.

2. **Granular RBAC:** The binary Admin/User type should be expanded to granular Azure AD roles (e.g., `UserManager`, `TransactionTypeAdmin`) so that admin privileges can be assigned independently per function. Not all administrators need access to all 6 functions.

3. **Audit trail:** All admin operations (user CRUD, transaction type changes) must be audit-logged per FSA (FFFS 2014:5) Ch. 8 internal controls. The menu navigation itself does not require logging, but the target operations do.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Segregation of duties in administrative access | Admin menu access must be restricted to authorized personnel. Azure AD roles should enforce least-privilege access. Each admin function (user management, transaction type config) should have independent role assignments. |
| **DORA** Art. 9 | ICT access control policies | Administrative functions must require additional authentication verification (step-up authentication). Session timeout for admin operations should be shorter than for regular user operations. |
| **GDPR** Art. 32 | Security of processing -- access control | User management functions handle PII (user names, IDs). Admin access must be logged and monitored. The .NET system must implement comprehensive audit trails for all admin operations. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
