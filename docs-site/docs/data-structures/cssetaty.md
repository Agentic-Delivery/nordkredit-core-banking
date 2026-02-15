---
id: DS-SETA-001
title: "Set Attribute Macro"
copybook_name: "CSSETATY.cpy"
domain: "system"
used_by_programs: [COACTUPC, COCRDUPC]
record_length: 0
status: "extracted"
target_schema: "N/A (UI pattern)"
sidebar_position: 29
---

# DS-SETA-001: Set Attribute Macro (CSSETATY)

## Overview

The `CSSETATY.cpy` copybook defines the **Set Attribute Macro**, a COBOL template/macro pattern for dynamically setting BMS (Basic Mapping Support) field attributes based on validation results. It uses placeholder variable names (`TESTVAR1`, `SCRNVAR2`, `MAPNAME3`) that are replaced via `COPY ... REPLACING` when the copybook is included in a program.

This is not a data structure in the traditional sense -- it is a reusable code fragment (procedure division copybook) that implements a UI validation display pattern: highlighting fields in red when they contain invalid data, and resetting them to the default color when they are valid.

**Source file:** `CSSETATY.cpy`
**Used by:** `COACTUPC` (Account Update), `COCRDUPC` (Card Update)

## Source COBOL

```cobol
IF TESTVAR1 = SPACES OR LOW-VALUES
    MOVE DFHBMFSE TO SCRNVAR2
        OF MAPNAME3
    MOVE DFHRED   TO SCRNVAR2
        OF MAPNAME3
ELSE
    MOVE DFHBMFSE TO SCRNVAR2
        OF MAPNAME3
    MOVE DFHGRN   TO SCRNVAR2
        OF MAPNAME3
END-IF
```

**Note:** The actual copybook uses `REPLACING` syntax at the `COPY` statement:
```cobol
COPY CSSETATY REPLACING
    ==TESTVAR1==  BY ==WS-CUST-FNAME==
    ==SCRNVAR2==  BY ==FNAMEA==
    ==MAPNAME3==  BY ==CACTUPAI==.
```

## Field Definitions

This copybook does not define data fields. It defines a reusable procedure division code fragment with the following placeholders:

| Placeholder | Purpose | Replaced With |
|-------------|---------|---------------|
| `TESTVAR1` | Working storage variable to validate | The data field being checked (e.g., `WS-CUST-FNAME`) |
| `SCRNVAR2` | BMS attribute byte field to set | The screen field's attribute byte (e.g., `FNAMEA`) |
| `MAPNAME3` | BMS map record name | The BMS map input record (e.g., `CACTUPAI`) |

## BMS Attribute Constants Used

| Constant | Value | Meaning |
|----------|-------|---------|
| `DFHBMFSE` | `X'0C'` | Field attribute: unprotected, MDT (Modified Data Tag) set, fset |
| `DFHRED` | `X'F2'` | Extended attribute: red color |
| `DFHGRN` | `X'F4'` | Extended attribute: green color |

## Pattern Notes

1. **Validation display logic:** The macro implements a simple binary validation pattern:
   - If the field value is `SPACES` or `LOW-VALUES` (empty/missing) -> set field color to **red** (error)
   - If the field value is non-empty -> set field color to **green** (valid)

2. **COPY REPLACING:** COBOL's `COPY ... REPLACING` mechanism provides a primitive form of macro expansion or generics. The copybook is included once per field that needs validation highlighting, with different placeholder values each time. This is COBOL's answer to code reuse for repetitive patterns.

3. **BMS attributes:** On a 3270 terminal, each field has attribute bytes that control its appearance (color, intensity, protection, cursor position). `DFHBMFSE` sets the field to unprotected/modifiable, while `DFHRED`/`DFHGRN` control the display color. This is the 3270 equivalent of CSS classes.

4. **Limitation -- binary validation only:** The macro only checks for empty vs. non-empty. It does not perform format validation, range checking, or cross-field validation. More complex validation is implemented inline in the CICS programs.

5. **Used in data entry screens:** This macro is used in `COACTUPC` (Account Update) and `COCRDUPC` (Card Update) -- the two screens with the most data entry fields requiring validation feedback.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Validation display** | BMS attribute bytes (DFHRED/DFHGRN) | CSS classes (`is-invalid` / `is-valid` in Bootstrap) |
| **Validation logic** | `IF field = SPACES` (empty check only) | FluentValidation or Data Annotations with multiple rules per field |
| **Reuse pattern** | `COPY CSSETATY REPLACING ...` | Shared validation component, tag helpers, or Blazor component |
| **Error indication** | Red field color on 3270 | Red border, error icon, error message text below field |
| **Valid indication** | Green field color on 3270 | Green border, checkmark icon (optional) |

### .NET Validation Display (Conceptual)

```csharp
// FluentValidation replaces the simple empty-check pattern
public class AccountUpdateValidator : AbstractValidator<AccountUpdateRequest>
{
    public AccountUpdateValidator()
    {
        RuleFor(x => x.FirstName)
            .NotEmpty().WithMessage("First name is required");

        RuleFor(x => x.LastName)
            .NotEmpty().WithMessage("Last name is required");

        RuleFor(x => x.PostalCode)
            .NotEmpty().WithMessage("Postal code is required")
            .Matches(@"^\d{3}\s?\d{2}$").WithMessage("Invalid Swedish postal code format");
    }
}
```

```html
<!-- Razor/Blazor equivalent of the attribute-setting macro -->
<div class="mb-3">
    <label for="firstName" class="form-label">First Name</label>
    <input asp-for="FirstName"
           class="form-control @(ViewData.ModelState["FirstName"]?.Errors.Any() == true
               ? "is-invalid" : "is-valid")" />
    <span asp-validation-for="FirstName" class="invalid-feedback"></span>
</div>
```

## Migration Notes

1. **Replace with standard validation framework:** The `COPY REPLACING` macro pattern is replaced by FluentValidation (server-side) and client-side validation (JavaScript/Blazor validation). The empty-check-only logic should be expanded to include format validation, range validation, and cross-field validation.

2. **Richer error feedback:** The COBOL macro only changes the field color (red/green). The .NET UI should provide:
   - Visual indicator (red border/icon for errors, green for valid)
   - Descriptive error message text below or beside the field
   - Validation summary at the top of the form
   - Client-side validation for immediate feedback

3. **No code to migrate:** This copybook contains procedure division code (not data definitions) with placeholder variables. There is no data to migrate. The pattern (validation display logic) is implemented natively by .NET validation frameworks.

4. **Accessibility:** The COBOL system relies solely on color (red/green) to indicate validation state, which is not accessible to colorblind users. The .NET UI must use additional indicators (icons, text messages, ARIA attributes) per WCAG 2.1 AA standards.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
