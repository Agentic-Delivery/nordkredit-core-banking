---
id: DS-BMS-TRN00
title: "Transaction List Screen"
copybook_name: "COTRN00.CPY"
domain: "ui-screens"
used_by_programs: [COTRN00C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 20
---

# DS-BMS-TRN00: Transaction List Screen (COTRN00)

## Overview

The `COTRN00.CPY` copybook defines the **Transaction List Screen**, a CICS BMS screen that displays a paginated list of transactions. The screen shows up to 10 transaction rows per page, each with a selection field, transaction ID, date, description, and amount. Users page through results using PF7 (backward) and PF8 (forward) function keys.

The copybook defines two symbolic map records:
- **CTRN00AI** -- Input record (receives row selection inputs and pagination keys)
- **CTRN00AO** -- Output record (sends transaction list data and messages to the terminal)

**Source file:** `COTRN00.CPY`
**BMS map name:** COTRN00
**Used by:** `COTRN00C` (Transaction List CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `ACTITEFN` | Input | 11 | Account ID filter |
| `SEL0001` - `SEL0010` | Input | 1 | Row selection field (action code to select) |
| `TRID0001` - `TRID0010` | Display | 16 | Transaction ID for each row |
| `TRDT0001` - `TRDT0010` | Display | 10 | Transaction date for each row |
| `TRDS0001` - `TRDS0010` | Display | 26 | Transaction description for each row |
| `TRAM0001` - `TRAM0010` | Display | 12 | Transaction amount for each row (formatted) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **List layout:** The screen displays up to 10 transaction records in a tabular format. Each row contains a selection field, transaction ID, date, description, and amount. The 10-row capacity is a 3270 screen constraint.

2. **Pagination:** PF7 scrolls backward and PF8 scrolls forward through the transaction list. The program maintains the current page position in the CICS COMMAREA.

3. **Row selection:** The user enters a selection character in the `SEL` field of a row and presses Enter to navigate to the transaction detail screen (COTRN01).

4. **Account filter:** The `ACTITEFN` field allows filtering transactions by account ID. When populated, only transactions associated with that account (via card linkage) are shown.

5. **Amount display:** Transaction amounts use formatted numeric output with sign indicator, showing credits and debits clearly.

6. **Sort order:** Transactions are typically displayed in reverse chronological order (most recent first), matching the VSAM key sequence or browse order.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COTRN00` on 3270 terminal | Blazor/Razor page: `TransactionList.razor` with data grid component |
| **Program** | `COTRN00C` (COBOL CICS program) | `TransactionController.Index` action with pagination/filter parameters |
| **Pagination** | PF7/PF8 keys, COMMAREA state | Server-side pagination with configurable page size |
| **Row selection** | Selection character in field | Clickable row links or view buttons |
| **Filtering** | Account ID input field | Multi-criteria filter panel (account, date range, amount range, type) |
| **Data grid** | Fixed 10-row BMS layout | Responsive data table with sorting, filtering, export |

### Migration Considerations

- **Enhanced filtering:** Expand beyond the single account ID filter to support date range, amount range, transaction type, and description search. Provide both quick filters and an advanced filter panel.
- **Configurable pagination:** Replace the fixed 10-row page with configurable page sizes (25, 50, 100) and include total record count, page numbers, and jump-to-page controls.
- **Export capability:** Add the ability to export filtered transaction lists to CSV or Excel format for reconciliation and reporting purposes.
- **Real-time updates:** Consider implementing real-time transaction list updates using SignalR for operational monitoring scenarios, replacing the manual refresh required on the 3270.
- **AML relevance:** Transaction listing is a key function for AML screening and suspicious activity review. The .NET implementation should support AML analyst workflows with flagging and annotation capabilities.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
