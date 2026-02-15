---
id: DS-BMS-ADM01
title: "Admin Menu Screen"
copybook_name: "COADM01.CPY"
domain: "ui-screens"
used_by_programs: [COADM01C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 12
---

# DS-BMS-ADM01: Admin Menu Screen (COADM01)

## Overview

The `COADM01.CPY` copybook defines the **Admin Menu Screen**, a CICS BMS navigation menu restricted to administrative users. This screen presents up to 12 menu options linking to user management, transaction type configuration, and other administrative functions. The user selects an option by entering its number and pressing Enter.

The copybook defines two symbolic map records:
- **CADM01AI** -- Input record (receives the option selection from the terminal)
- **CADM01AO** -- Output record (sends the menu option labels and messages to the terminal)

**Source file:** `COADM01.CPY`
**BMS map name:** COADM01
**Used by:** `COADM01C` (Admin Menu CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (current CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `OPTEFN01` - `OPTEFN12` | Display | 40 | Menu option labels (12 slots) |
| `OPTION` | Input | 2 | User's menu selection (numeric option number) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Menu structure:** The screen displays up to 12 labeled menu options in a vertical list. Each option slot (`OPTEFN01` through `OPTEFN12`) contains a descriptive label. Unused slots are typically blank or contain spaces.

2. **Option selection:** The user types a numeric option number (1-12) into the `OPTION` field and presses Enter. The program `COADM01C` validates the selection and routes to the appropriate sub-screen via CICS `XCTL` (transfer control).

3. **Admin-only access:** This menu is only accessible to users with administrative privileges. The CICS program checks user authorization before displaying the menu. Typical admin functions include:
   - User list / add / update / delete
   - Transaction type management
   - System configuration

4. **Header fields:** The screen header displays the transaction name, program name, current date, and current time, following the standard CardDemo screen header pattern.

5. **Message areas:** `INFOMSG` displays informational feedback (e.g., "User created successfully"), while `ERRMSG` displays validation or authorization errors (e.g., "Invalid option selected").

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COADM01` on 3270 terminal | Admin section in Blazor/Razor: `AdminDashboard.razor` or dedicated admin area |
| **Program** | `COADM01C` (COBOL CICS program) | ASP.NET navigation/routing with `[Authorize(Roles = "Admin")]` |
| **Navigation** | Numeric option entry + Enter key | Sidebar navigation menu, clickable links/cards |
| **Authorization** | CICS RACF security check | Azure AD role-based access, ASP.NET Authorization policies |
| **Menu options** | Fixed 12-slot BMS fields | Dynamic navigation menu driven by user permissions |

### Migration Considerations

- **Replace with navigation component:** The 3270 numbered menu pattern should be replaced with a modern admin dashboard or sidebar navigation. Menu items should be dynamically rendered based on the authenticated user's Azure AD roles and permissions.
- **Role granularity:** The COBOL program likely has a single "admin" check. The .NET implementation should support more granular roles (e.g., `UserAdmin`, `ConfigAdmin`) to follow the principle of least privilege per FSA (FFFS 2014:5) Ch. 8 internal controls.
- **Audit logging:** All administrative actions initiated from this menu should be logged for compliance. The .NET implementation should log menu access and navigation events.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
