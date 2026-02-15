---
id: DS-BMS-RPT00
title: "Report Request Screen"
copybook_name: "CORPT00.CPY"
domain: "ui-screens"
used_by_programs: [CORPT00C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 18
---

# DS-BMS-RPT00: Report Request Screen (CORPT00)

## Overview

The `CORPT00.CPY` copybook defines the **Report Request Screen**, a CICS BMS screen for initiating report generation. The screen allows the user to select a report period type (monthly, yearly, or custom date range), specify start and end dates, and confirm the report request.

The copybook defines two symbolic map records:
- **CRPT00AI** -- Input record (receives report parameters and confirmation from the terminal)
- **CRPT00AO** -- Output record (sends current selections and messages to the terminal)

**Source file:** `CORPT00.CPY`
**BMS map name:** CORPT00
**Used by:** `CORPT00C` (Report Request CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `MONTTEFN` | Input | 1 | Monthly report selection flag |
| `YEARTEFN` | Input | 1 | Yearly report selection flag |
| `CUSTEFN` | Input | 1 | Custom date range selection flag |
| `SDTEFN` | Input | 10 | Start date (`YYYY-MM-DD`) |
| `EDTEFN` | Input | 10 | End date (`YYYY-MM-DD`) |
| `CONFIRM` | Input | 1 | Confirmation flag (`Y`/`N`) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Report type selection:** The user selects one of three report period types by marking the corresponding field:
   - **Monthly** (`MONTTEFN`): Generates a report for a specific month
   - **Yearly** (`YEARTEFN`): Generates a report for a full year
   - **Custom** (`CUSTEFN`): Uses the start/end date fields for a custom range

2. **Date range:** The `SDTEFN` (start date) and `EDTEFN` (end date) fields accept dates in `YYYY-MM-DD` format. For monthly/yearly selections, the program may auto-populate these dates based on the current period. For custom selection, the user must enter both dates.

3. **Validation:** The program validates that exactly one report type is selected, dates are valid, and the end date is not before the start date. Errors are displayed in `ERRMSG`.

4. **Confirmation:** The `CONFIRM` field requires `Y` to submit the report request. This prevents accidental report generation, which may be resource-intensive.

5. **Asynchronous processing:** Report generation is typically a batch-oriented process. The CICS program may queue the report request for batch processing rather than generating it inline, given the potentially large data volumes involved.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `CORPT00` on 3270 terminal | Blazor/Razor page: `ReportRequest.razor` / `ReportRequest.cshtml` |
| **Program** | `CORPT00C` (COBOL CICS program) | `ReportController.Create` action or Blazor form component |
| **Report type** | Single-character flag fields (radio-like) | Radio button group or dropdown |
| **Date entry** | Text fields (`YYYY-MM-DD`) | Date picker components with calendar UI |
| **Confirmation** | `Y`/`N` text field | Submit button with loading indicator |
| **Report generation** | Batch job (JCL) or CICS background task | Azure Functions timer trigger or on-demand Function with queue |

### Migration Considerations

- **Asynchronous with status tracking:** Replace the fire-and-forget report request with an async pattern: submit request to Azure Service Bus queue, process via Azure Function, notify user when complete. Provide a "Report Status" page showing pending/completed reports.
- **Report output format:** The mainframe likely generates printed reports or spool output. The .NET replacement should generate downloadable PDF/Excel reports and optionally display interactive report views in the browser.
- **Regulatory reporting:** If any reports serve FSA regulatory reporting requirements (e.g., FFFS 2014:5 reporting obligations), the migration must ensure the .NET reports produce equivalent output that meets the regulatory calendar SLAs.
- **Date validation:** Use proper date picker components with built-in range validation. Prevent selection of future date ranges for historical reports.
- **Report templates:** Consider implementing a report template system that allows new report types to be added without code changes, supporting the evolving regulatory reporting landscape.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
