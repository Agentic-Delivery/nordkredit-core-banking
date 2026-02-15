---
id: DS-MSG1-001
title: "Common Messages"
copybook_name: "CSMSG01Y.cpy"
domain: "system"
used_by_programs: [COMEN01C, COACTUPC, COACTVWC, COCRDLIC, COCRDSLC, COCRDUPC, COTRN00C, COTRN01C, COTRN02C, COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C, COBIL00C, CORPT00C, COADM01C]
record_length: 0
status: "extracted"
target_schema: "N/A (UI messages)"
sidebar_position: 27
---

# DS-MSG1-001: Common Messages (CSMSG01Y)

## Overview

The `CSMSG01Y.cpy` copybook defines the **Common Messages**, a set of standard UI message constants shared across all CICS programs in the CardDemo application. These messages provide consistent user feedback for common situations (sign-off acknowledgment, invalid key press).

This is not a database record -- it contains compile-time string constants used for display on CICS 3270 screens. Every CICS program includes this copybook to access the shared message pool.

**Source file:** `CSMSG01Y.cpy`
**Used by:** ALL CICS programs

## Source COBOL

```cobol
01 CCDA-COMMON-MESSAGES.
  05 CCDA-MSG-THANK-YOU         PIC X(50) VALUE
     'Thank you for using CardDemo application...      '.
  05 CCDA-MSG-INVALID-KEY       PIC X(50) VALUE
     'Invalid key pressed. Please see below...         '.
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `CCDA-MSG-THANK-YOU` | `X(50)` | 50 | Alphanumeric (constant) | Sign-off/thank-you message displayed on logout |
| 2 | `CCDA-MSG-INVALID-KEY` | `X(50)` | 50 | Alphanumeric (constant) | Error message for invalid key press (wrong PF key or AID) |

## Field Notes

1. **CCDA-MSG-THANK-YOU:** Displayed when the user exits the application (typically by pressing PF3 from the main menu or signing off). In the .NET system, this maps to a logout confirmation message or redirect page.

2. **CCDA-MSG-INVALID-KEY:** Displayed when the user presses a function key that is not valid for the current screen context. On a 3270 terminal, users can press any of 24 PF keys plus PA keys and Enter/Clear. Each screen only responds to a subset of these keys; pressing an unsupported key triggers this message. In the .NET web UI, this concept is largely eliminated -- users click buttons or links, and invalid actions are either hidden (disabled buttons) or prevented by UI design.

3. **Fixed-width padding:** Both messages are padded with trailing spaces to exactly 50 characters for 3270 screen field alignment. In .NET, messages are variable-length strings.

4. **Message placement:** These messages are typically moved to the `ERRMSG` or `INFOMSG` fields of the BMS screen map before sending the screen to the terminal.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Message storage** | Hardcoded COBOL `VALUE` clauses | .NET resource files (`.resx`) for localization support |
| **Message display** | `MOVE msg TO screen-field`, `SEND MAP` | Toast notifications, alert components, or validation summary |
| **Invalid input handling** | "Invalid key pressed" error message | Disabled/hidden UI elements, client-side validation, HTTP 400 responses |
| **Sign-off message** | Displayed on 3270 screen | Logout redirect page or toast notification |

### .NET Resource File (Conceptual)

```xml
<!-- Resources/Messages.resx (English) -->
<data name="ThankYouMessage" xml:space="preserve">
  <value>Thank you for using NordKredit Card Management.</value>
</data>
<data name="InvalidInputMessage" xml:space="preserve">
  <value>Invalid input. Please review the form and try again.</value>
</data>

<!-- Resources/Messages.sv-SE.resx (Swedish) -->
<data name="ThankYouMessage" xml:space="preserve">
  <value>Tack for att du anvander NordKredit Korthantering.</value>
</data>
<data name="InvalidInputMessage" xml:space="preserve">
  <value>Ogiltig inmatning. Kontrollera formularet och forsok igen.</value>
</data>
```

## Migration Notes

1. **Localization:** The COBOL system uses English-only hardcoded messages. The .NET system must support Swedish (primary) and English using .NET resource files (`.resx`). All user-facing messages should be externalized to resource files from the start.

2. **Replace "Invalid key" concept:** The "invalid key pressed" paradigm is specific to 3270 terminal interaction. In a web UI, the equivalent is client-side form validation, disabled buttons for unauthorized actions, and server-side validation error messages. The generic "invalid key" message should be replaced with specific, actionable error messages per field or action.

3. **Expand message catalog:** The COBOL system has only 2 common messages. The .NET system should define a comprehensive message catalog covering:
   - Validation errors (field-specific)
   - Success confirmations
   - Warning messages
   - System error messages
   - Session timeout notifications

4. **No migration of values:** The actual message text is demo-specific ("CardDemo application") and will be replaced with NordKredit branding. Only the pattern (centralized, shared messages) is carried forward.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
