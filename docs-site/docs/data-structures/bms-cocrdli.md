---
id: DS-BMS-CRDLI
title: "Card List Screen"
copybook_name: "COCRDLI.CPY"
domain: "ui-screens"
used_by_programs: [COCRDLIC]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 14
---

# DS-BMS-CRDLI: Card List Screen (COCRDLI)

## Overview

The `COCRDLI.CPY` copybook defines the **Card List Screen**, a CICS BMS screen that displays a paginated list of cards. The screen shows up to 7 card rows per page, each with a selection field, account number, card number, and card status. Users can page through results using PF7 (backward) and PF8 (forward) function keys, and select a card for further action.

The copybook defines two symbolic map records:
- **CCRDLIAI** -- Input record (receives row selection inputs and pagination keys)
- **CCRDLIAO** -- Output record (sends card list data and messages to the terminal)

**Source file:** `COCRDLI.CPY`
**BMS map name:** COCRDLI
**Used by:** `COCRDLIC` (Card List CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `ACTITEFN` | Input | 11 | Account ID filter (optional, to filter cards by account) |
| `SEL0001` - `SEL0007` | Input | 1 | Row selection field (enter `S` or action code to select) |
| `ACCT0001` - `ACCT0007` | Display | 11 | Account number for each row |
| `CARD0001` - `CARD0007` | Display | 16 | Card number for each row |
| `CSTS0001` - `CSTS0007` | Display | 1 | Card status for each row |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **List layout:** The screen displays 7 card records in a tabular format. Each row contains a selection field, account number, card number, and status code. The fixed 7-row limit is a 3270 screen size constraint (80 columns x 24 rows, minus header, footer, and message areas).

2. **Pagination:** PF7 scrolls backward (previous page) and PF8 scrolls forward (next page). The program maintains cursor position across pages using a COMMAREA (communication area) to track the current page offset.

3. **Row selection:** The user types a selection character (e.g., `S` for select) in the `SEL` field of the desired row and presses Enter. The program reads the selection, identifies the corresponding card, and routes to the appropriate detail or action screen.

4. **Account filter:** The `ACTITEFN` field allows the user to filter the card list by account ID. When populated, only cards belonging to that account are displayed.

5. **Card number display:** Card numbers (16 digits) are displayed in full on the 3270 screen. The .NET replacement must mask card numbers per PCI-DSS requirements (show only last 4 digits in list views).

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COCRDLI` on 3270 terminal | Blazor/Razor page: `CardList.razor` / `CardList.cshtml` with data grid |
| **Program** | `COCRDLIC` (COBOL CICS program) | `CardController.Index` action with pagination parameters |
| **Pagination** | PF7/PF8 keys, COMMAREA state | Server-side pagination with page/size query parameters |
| **Row selection** | `S` character in selection field | Clickable row links or action buttons |
| **Filtering** | Account ID input field | Search/filter bar with multiple filter criteria |
| **Data grid** | Fixed 7-row BMS layout | Responsive data table (e.g., Blazor DataGrid, MudBlazor Table) |

### Migration Considerations

- **PCI-DSS compliance:** Card numbers must be masked in the list view (show only last 4 digits: `**** **** **** 1234`). The COBOL screen displays full card numbers, which is not acceptable in the .NET replacement. Only authorized roles should be able to reveal full card numbers.
- **Flexible pagination:** Replace the fixed 7-row page size with configurable page sizes (10, 25, 50 rows) and modern pagination controls (page numbers, total count display).
- **Search and filter:** Expand beyond the single account ID filter to support multi-criteria search (card status, date range, cardholder name) with server-side filtering for performance.
- **Bulk actions:** Consider adding multi-select capability for bulk operations (e.g., bulk status change) that was not feasible in the 3270 single-select model.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
