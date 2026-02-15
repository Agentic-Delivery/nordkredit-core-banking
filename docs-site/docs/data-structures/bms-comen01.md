---
id: DS-BMS-MEN01
title: "Main Menu Screen"
copybook_name: "COMEN01.CPY"
domain: "ui-screens"
used_by_programs: [COMEN01C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 17
---

# DS-BMS-MEN01: Main Menu Screen (COMEN01)

## Overview

The `COMEN01.CPY` copybook defines the **Main Menu Screen**, the primary navigation hub for the CardDemo CICS application. After successful sign-on, users are presented with this menu, which provides access to all major application functions: account management, card management, transaction processing, reporting, bill payment, and authorization operations.

The copybook defines two symbolic map records:
- **CMEN01AI** -- Input record (receives the option selection from the terminal)
- **CMEN01AO** -- Output record (sends the menu option labels and messages to the terminal)

**Source file:** `COMEN01.CPY`
**BMS map name:** COMEN01
**Used by:** `COMEN01C` (Main Menu CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `OPTEFN01` - `OPTEFN12` | Display | 40 | Menu option labels (12 slots) |
| `OPTION` | Input | 2 | User's menu selection (numeric option number) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Central navigation:** This is the first screen users see after login. It serves as the top-level menu from which all other functions are accessed. The typical menu options include:
   - Account list / view / update
   - Card list / view / update
   - Transaction list / view / add
   - Bill payment
   - Report generation
   - Authorization functions
   - Admin menu (visible to admin users only)

2. **Menu structure:** Up to 12 option labels (`OPTEFN01` through `OPTEFN12`) are displayed in a numbered vertical list. The user enters a number in the `OPTION` field and presses Enter. The program routes to the selected function via CICS `XCTL`.

3. **Role-based options:** The `COMEN01C` program may conditionally display or hide certain menu options based on the user's authorization level (e.g., admin-only options). Non-admin users see a subset of the full menu.

4. **Standard header:** Like all CardDemo screens, the header displays the transaction name, program name, current date, and time.

5. **Session context:** The program maintains user session context in the CICS COMMAREA, carrying the authenticated user ID and role information to subsequent screens.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COMEN01` on 3270 terminal | Application shell with navigation: sidebar, top nav, or dashboard |
| **Program** | `COMEN01C` (COBOL CICS program) | ASP.NET layout/shell component with role-based navigation |
| **Navigation** | Numeric option entry + Enter key | Clickable menu items, sidebar links, dashboard cards |
| **Role filtering** | Conditional menu display in COBOL | `AuthorizeView` components or policy-based menu rendering |
| **Session** | CICS COMMAREA | ASP.NET authentication middleware + claims-based identity |

### Migration Considerations

- **Replace with modern navigation:** The numbered menu pattern is replaced by a persistent navigation component (sidebar or top nav bar) that is always visible across the application. This eliminates the need to return to a "main menu" screen.
- **Role-based rendering:** Menu items should be dynamically rendered based on the user's Azure AD roles and claims. Unauthorized menu items should not be visible (not just disabled), following security-by-design principles.
- **Dashboard approach:** Consider replacing the static menu with a dashboard showing summary widgets (account counts, pending transactions, alerts) alongside navigation links, providing operational awareness at a glance.
- **Deep linking:** Unlike the 3270 sequential navigation model, the .NET application should support direct URL-based navigation to any authorized screen, eliminating the requirement to pass through the main menu.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
