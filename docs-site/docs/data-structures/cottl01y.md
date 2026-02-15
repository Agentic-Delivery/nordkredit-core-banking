---
id: DS-TITL-001
title: "Screen Title"
copybook_name: "COTTL01Y.cpy"
domain: "system"
used_by_programs: [COMEN01C, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, COBIL00C, CORPT00C, COADM01C]
record_length: 0
status: "extracted"
target_schema: "N/A (UI constant)"
sidebar_position: 24
---

# DS-TITL-001: Screen Title (COTTL01Y)

## Overview

The `COTTL01Y.cpy` copybook defines the **Screen Title Constants**, a set of static text values displayed at the top of every CICS screen in the CardDemo application. These constants provide application branding and a standard thank-you/goodbye message.

This is not a data record -- it contains compile-time string constants used for UI display. Every CICS screen program includes this copybook to display the application title consistently.

**Source file:** `COTTL01Y.cpy`
**Used by:** ALL CICS screen programs

## Source COBOL

```cobol
01 CCDA-SCREEN-TITLE.
  05 CCDA-TITLE01    PIC X(40) VALUE '      AWS Mainframe Modernization       '.
  05 CCDA-TITLE02    PIC X(40) VALUE '              CardDemo                  '.
  05 CCDA-THANK-YOU  PIC X(40) VALUE 'Thank you for using CCDA application... '.
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `CCDA-TITLE01` | `X(40)` | 40 | Alphanumeric (constant) | Application title line 1 |
| 2 | `CCDA-TITLE02` | `X(40)` | 40 | Alphanumeric (constant) | Application title line 2 (product name) |
| 3 | `CCDA-THANK-YOU` | `X(40)` | 40 | Alphanumeric (constant) | Sign-off/thank-you message |

## Field Notes

1. **CCDA-TITLE01:** Displays "AWS Mainframe Modernization" -- this is the original demo application branding. For NordKredit AB, this will be replaced with the bank's application name and branding.

2. **CCDA-TITLE02:** Displays "CardDemo" -- the application name. This will be replaced with the NordKredit-specific application name in the .NET system.

3. **CCDA-THANK-YOU:** Displayed on the sign-off screen when users log out. Maps to a logout confirmation page or message in the .NET application.

4. **Fixed-width formatting:** The strings are padded with spaces to exactly 40 characters for 3270 screen alignment. In the .NET web UI, fixed-width formatting is not needed; the values become simple string constants or localized resources.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Title display** | `MOVE CCDA-TITLE01 TO screen-title-field` in every program | Shared layout component (`_Layout.cshtml` or `MainLayout.razor`) with application name from configuration |
| **Branding** | Hardcoded COBOL string constants | `appsettings.json` configuration or resource files |
| **Thank-you message** | Displayed on sign-off screen | Logout confirmation page with localized message |
| **Localization** | English only, hardcoded | .NET resource files (`.resx`) supporting Swedish and English |

### .NET Configuration (Conceptual)

```json
{
  "Application": {
    "Name": "NordKredit Card Management",
    "Title": "NordKredit AB - Core Banking",
    "LogoutMessage": "Thank you for using NordKredit Card Management."
  }
}
```

## Migration Notes

1. **Replace branding:** The "AWS Mainframe Modernization" and "CardDemo" titles must be replaced with NordKredit AB branding. This is a configuration change, not a code change.

2. **Localization:** The COBOL system is English-only. The .NET application should support Swedish (primary) and English using .NET resource files for all UI text, including titles and messages.

3. **Shared layout:** Instead of including a title copybook in every program, the .NET application uses a shared layout component that renders the application header consistently across all pages. This is a natural consolidation of the repeated `COPY COTTL01Y` in every COBOL program.

4. **No migration of values:** The actual string values in this copybook are demo-specific and will not be migrated. Only the pattern (centralized application branding) is carried forward.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
