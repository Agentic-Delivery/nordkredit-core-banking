---
id: DS-BMS-BIL00
title: "Bill Payment Screen"
copybook_name: "COBIL00.CPY"
domain: "ui-screens"
used_by_programs: [COBIL00C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 13
---

# DS-BMS-BIL00: Bill Payment Screen (COBIL00)

## Overview

The `COBIL00.CPY` copybook defines the **Bill Payment Screen**, a CICS BMS screen for processing bill payments against a card account. The screen allows the user to enter an account identifier, view the current balance, and confirm payment processing.

The copybook defines two symbolic map records:
- **CBIL00AI** -- Input record (receives account ID and confirmation from the terminal)
- **CBIL00AO** -- Output record (sends balance display and messages to the terminal)

**Source file:** `COBIL00.CPY`
**BMS map name:** COBIL00
**Used by:** `COBIL00C` (Bill Payment CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `ACTITEFN` | Input | 11 | Account ID for the payment |
| `CURBAL` | Display | 15 | Current account balance (formatted) |
| `CONFIRM` | Input | 1 | Confirmation flag (`Y`/`N`) to process payment |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Workflow:** The user enters an account ID and presses Enter. The program retrieves and displays the current balance. The user then confirms the payment by entering `Y` in the confirm field and pressing Enter again. This two-step interaction prevents accidental payment submissions.

2. **Balance display:** The current balance field uses formatted numeric output to show the account balance with sign, thousands separators, and decimal places.

3. **Validation:** The CICS program `COBIL00C` validates the account ID exists and is active before displaying the balance. If the account is invalid or inactive, an error message is displayed in `ERRMSG`.

4. **Confirmation pattern:** The `CONFIRM` field implements a simple confirmation gate. Only when set to `Y` does the program proceed with payment processing. This is a common CICS pattern for destructive/financial operations.

5. **Payment processing:** Upon confirmation, the program updates the account balance and creates a transaction record. The payment details (amount, date) are derived from the account's current billing cycle data.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COBIL00` on 3270 terminal | Blazor/Razor page: `BillPayment.razor` / `BillPayment.cshtml` |
| **Program** | `COBIL00C` (COBOL CICS program) | `BillPaymentController` or Blazor component with API call |
| **Confirmation** | `Y`/`N` text field | Confirmation modal dialog or dedicated review step |
| **Balance display** | BMS edited PIC field | .NET currency formatting (`ToString("C2")`) |
| **Transaction safety** | CICS syncpoint | Database transaction with `IUnitOfWork` pattern |
| **Authorization** | CICS transaction security | ASP.NET Authorization + Azure AD roles |

### Migration Considerations

- **PSD2 SCA requirement:** Bill payments in the .NET system must implement Strong Customer Authentication (SCA) per PSD2 Art. 97. The simple `Y`/`N` confirmation in the COBOL system does not meet SCA requirements. The .NET replacement must integrate with the bank's authentication framework (e.g., BankID for Swedish customers) for payment authorization.
- **Payment amount entry:** The current screen derives payment from the billing cycle. The .NET replacement should allow partial payment amounts to provide greater flexibility.
- **Idempotency:** The .NET payment endpoint must be idempotent to prevent duplicate payments from network retries or double-clicks. The COBOL program achieves this through CICS pseudo-conversational design; the .NET equivalent should use idempotency keys.
- **Audit trail:** Every payment attempt (successful or failed) must be logged for FSA and AML compliance.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
