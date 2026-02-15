---
id: DS-BMS-TRN01
title: "Transaction View Screen"
copybook_name: "COTRN01.CPY"
domain: "ui-screens"
used_by_programs: [COTRN01C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 21
---

# DS-BMS-TRN01: Transaction View Screen (COTRN01)

## Overview

The `COTRN01.CPY` copybook defines the **Transaction View Screen**, a CICS BMS screen for displaying the full details of a single transaction. This read-only screen shows all transaction fields including identification, classification, financial details, dates, and merchant information.

The copybook defines two symbolic map records:
- **CTRN01AI** -- Input record (receives transaction selection/navigation from the terminal)
- **CTRN01AO** -- Output record (sends full transaction detail data to the terminal)

**Source file:** `COTRN01.CPY`
**BMS map name:** COTRN01
**Used by:** `COTRN01C` (Transaction View CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `TRTEFN` | Display | 16 | Transaction ID |
| `CARDTEFN` | Display | 16 | Card number associated with the transaction |
| `TTYTEFN` | Display | 2 | Transaction type code |
| `TCATEFN` | Display | 4 | Transaction category code |
| `TRSTEFN` | Display | 2 | Transaction source (e.g., POS, ATM, online) |
| `TRDSTEFN` | Display | 100 | Transaction description |
| `TRAMTEFN` | Display | 15 | Transaction amount (formatted) |
| `TRPTEFN` | Display | 10 | Transaction processing date |
| `TORTEFN` | Display | 10 | Transaction origination date |
| `MCHTEFN` | Display | 50 | Merchant name |
| `MCTEFN` | Display | 20 | Merchant city |
| `MZTEFN` | Display | 10 | Merchant ZIP/postal code |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Full transaction detail:** This screen displays all available information for a single transaction, organized into logical sections: identification (transaction ID, card), classification (type, category, source), financial (amount), dates (processing, origination), and merchant information.

2. **Read-only display:** All fields are output-only. The user views the transaction details but cannot modify them. This is the detail view accessed from the Transaction List Screen (COTRN00).

3. **Card number display:** The full 16-digit card number is displayed. The .NET replacement must mask this per PCI-DSS requirements.

4. **Type and category codes:** The `TTYTEFN` (type) and `TCATEFN` (category) fields display raw codes. The operator is expected to know the code meanings. The .NET replacement should display descriptive labels alongside or instead of raw codes.

5. **Dual dates:** The screen shows both the processing date (when the transaction was posted) and the origination date (when the transaction occurred at the merchant). These can differ for offline/batch-processed transactions.

6. **Merchant information:** Merchant name, city, and ZIP are displayed for point-of-sale and e-commerce transactions. This data originates from the acquiring network and is stored in the transaction record.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COTRN01` on 3270 terminal | Blazor/Razor page: `TransactionDetail.razor` / `TransactionDetail.cshtml` |
| **Program** | `COTRN01C` (COBOL CICS program) | `TransactionController.Detail` action or Blazor component |
| **Data retrieval** | CICS READ from VSAM transaction file | Repository pattern with Entity Framework query |
| **Code resolution** | Operator knowledge of codes | Lookup joins to type/category reference tables; display labels |
| **Navigation** | PF3=Back to list | Breadcrumb navigation, browser back button |
| **Card display** | Full 16-digit card number | Masked card number (`**** **** **** 1234`) per PCI-DSS |

### Migration Considerations

- **Code-to-label resolution:** Replace raw type and category codes with human-readable labels by joining to reference data tables. Show both the code and label (e.g., "SA - Sales") for traceability during the parallel-run period.
- **PCI-DSS card masking:** The card number must be masked in the detail view. Only PCI-authorized roles should have a "reveal full number" option, and access must be logged.
- **Structured layout:** Organize the dense transaction data into collapsible sections or tabs (Summary, Merchant Details, Dates, Audit Info) for better readability compared to the flat 3270 layout.
- **AML annotation:** Add the ability for AML analysts to flag or annotate suspicious transactions directly from the detail view, a capability not present in the COBOL system.
- **Audit trail link:** Provide a link to the transaction's audit history showing all system events related to this transaction (posting, adjustments, reversals).

---

**Template version:** 1.0
**Last updated:** 2026-02-15
