---
id: DS-BMS-CRDSL
title: "Card Select/View Screen"
copybook_name: "COCRDSL.CPY"
domain: "ui-screens"
used_by_programs: [COCRDSLC]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 15
---

# DS-BMS-CRDSL: Card Select/View Screen (COCRDSL)

## Overview

The `COCRDSL.CPY` copybook defines the **Card Select/View Screen**, a CICS BMS screen used to display card details and allow the user to select a card for further operations (view, update). The screen shows account and card identifiers, cardholder name, card status, and expiration date.

The copybook defines two symbolic map records:
- **CCRDSLAI** -- Input record (receives card/account selection data from the terminal)
- **CCRDSLAO** -- Output record (sends card detail data and messages to the terminal)

**Source file:** `COCRDSL.CPY`
**BMS map name:** COCRDSL
**Used by:** `COCRDSLC` (Card Select CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `ACTITEFN` | Display/Input | 11 | Account ID |
| `CARTEFN` | Display/Input | 16 | Card number |
| `CRDNAME` | Display | 50 | Cardholder name |
| `CRDSTCD` | Display | 1 | Card status code |
| `EXPMON` | Display | 2 | Expiration month (MM) |
| `EXPYEAR` | Display | 4 | Expiration year (YYYY) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Selection/detail hybrid:** This screen serves a dual purpose. When navigated to from the card list, it displays the selected card's details. The user can also manually enter an account ID and card number to look up a specific card.

2. **Card identification:** The screen displays both the account ID and the full 16-digit card number. These two fields together uniquely identify a card in the CardDemo system.

3. **Cardholder name:** The name is displayed as a single combined field, typically formatted as the full cardholder name from the customer record.

4. **Status code:** The `CRDSTCD` field shows a single-character status code (e.g., `A` for active, `C` for closed, `S` for suspended). The display meaning is typically shown via a screen label or known to the operator.

5. **Expiration date:** Shown as separate month and year fields, matching the card expiration format (MM/YYYY).

6. **Action routing:** From this screen, the user can navigate to the Card Update screen (COCRDUP) to modify the card, or return to the Card List screen (COCRDLI).

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COCRDSL` on 3270 terminal | Blazor/Razor page: `CardDetail.razor` / `CardDetail.cshtml` |
| **Program** | `COCRDSLC` (COBOL CICS program) | `CardController.Detail` action or Blazor component |
| **Card lookup** | Manual account/card ID entry | URL route parameter (`/cards/{cardId}`) or search |
| **Navigation** | PF keys to route to update/list | Action buttons ("Edit", "Back to List") |
| **Status display** | Single character code | Human-readable status label with color badge |

### Migration Considerations

- **PCI-DSS card masking:** The full card number must be masked in the .NET detail view (e.g., `**** **** **** 1234`). Only authorized PCI-compliant roles should have access to view full card numbers, and such access must be logged.
- **Combine with update view:** In the .NET architecture, the separate select/view and update screens can be combined into a single detail page with a read-only mode that can be toggled to edit mode, reducing navigation steps.
- **Status display:** Replace single-character status codes with descriptive labels and visual indicators (e.g., green badge for "Active", red for "Closed", yellow for "Suspended").

---

**Template version:** 1.0
**Last updated:** 2026-02-15
