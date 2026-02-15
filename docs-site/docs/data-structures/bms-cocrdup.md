---
id: DS-BMS-CRDUP
title: "Card Update Screen"
copybook_name: "COCRDUP.CPY"
domain: "ui-screens"
used_by_programs: [COCRDUPC]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 16
---

# DS-BMS-CRDUP: Card Update Screen (COCRDUP)

## Overview

The `COCRDUP.CPY` copybook defines the **Card Update Screen**, a CICS BMS screen for modifying card record details. The screen allows authorized users to edit the cardholder name, card status, and expiration date fields for an existing card.

The copybook defines two symbolic map records:
- **CCRDUPAI** -- Input record (receives edited card data from the terminal)
- **CCRDUPAO** -- Output record (sends current card data for display/editing)

**Source file:** `COCRDUP.CPY`
**BMS map name:** COCRDUP
**Used by:** `COCRDUPC` (Card Update CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `ACTITEFN` | Display | 11 | Account ID (display-only, set from prior screen) |
| `CARTEFN` | Display | 16 | Card number (display-only, set from prior screen) |
| `CRDNAME` | Input | 50 | Cardholder name (editable) |
| `CRDSTCD` | Input | 1 | Card status code (editable) |
| `EXPMON` | Input | 2 | Expiration month (MM, editable) |
| `EXPYEAR` | Input | 4 | Expiration year (YYYY, editable) |
| `EXPDAY` | Input | 2 | Expiration day (DD, editable) |
| `CONFIRM` | Input | 1 | Confirmation flag (`Y`/`N`) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Edit form:** The account ID and card number are display-only (identifying the card being edited), while the cardholder name, status, and expiration date fields are editable.

2. **Expiration date:** The expiration date is split into three separate input fields: month (MM), year (YYYY), and day (DD). This differs from the standard card expiration format (MM/YY) because the system stores the full date internally.

3. **Status codes:** The `CRDSTCD` field accepts a single character representing the card status. Valid values are defined by the program logic in `COCRDUPC` (e.g., `A` = Active, `C` = Closed, `S` = Suspended, `L` = Lost, `T` = Stolen).

4. **Confirmation:** The `CONFIRM` field requires the user to enter `Y` to save changes. This prevents accidental modifications to card records.

5. **Validation:** The CICS program validates the status code against allowed values, ensures the expiration date is valid, and checks that the cardholder name is not blank. Error messages appear in `ERRMSG`.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COCRDUP` on 3270 terminal | Blazor/Razor page: `CardEdit.razor` / `CardEdit.cshtml` |
| **Program** | `COCRDUPC` (COBOL CICS program) | `CardController.Edit` action or Blazor form component |
| **Input validation** | COCRDUPC program logic | Data annotations + FluentValidation on `CardUpdateRequest` DTO |
| **Confirmation** | `Y`/`N` text field | Submit button with confirmation modal |
| **Status selection** | Free-text single character | Dropdown/select list with descriptive labels |
| **Date entry** | Three separate fields (MM, YYYY, DD) | Date picker component |

### Migration Considerations

- **PCI-DSS compliance:** Card numbers are displayed on the edit screen. In the .NET replacement, the card number must remain masked unless the user has explicit PCI-authorized access. Editing card numbers directly should not be permitted through this screen.
- **Status transition rules:** The COBOL program may enforce business rules about valid status transitions (e.g., a card cannot go from "Closed" back to "Active" without reissue). These rules must be extracted and implemented as domain logic in the .NET system, not embedded in the UI controller.
- **Audit trail:** Every card modification must be logged with the old values, new values, user, and timestamp for FSA compliance. The COBOL program may use a simple log; the .NET implementation should use a structured audit event system.
- **Expiration date:** Replace the three separate date fields with a standard date picker component. Validate that the new expiration date is in the future.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
