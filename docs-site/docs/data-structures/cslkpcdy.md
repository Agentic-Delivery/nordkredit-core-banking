---
id: DS-LKUP-001
title: "Lookup Code Repository"
copybook_name: "CSLKPCDY.cpy"
domain: "system"
used_by_programs: [COACTUPC, COCRDUPC]
record_length: 0
status: "extracted"
target_schema: "N/A (validation rules)"
sidebar_position: 26
---

# DS-LKUP-001: Lookup Code Repository (CSLKPCDY)

## Overview

The `CSLKPCDY.cpy` copybook defines the **Lookup Code Repository**, a comprehensive validation data structure containing three sets of lookup codes used for input validation in the CardDemo application:

1. **US Phone Area Codes** -- NANPA (North American Numbering Plan Administration) area codes for phone number validation
2. **US State Codes** -- Two-letter state and territory codes
3. **US State-ZIP Code Combinations** -- Valid first-two-digit ZIP code ranges per state

This is not a database record -- it is a compile-time validation table embedded in COBOL programs that perform data entry validation. The validation is implemented using COBOL 88-level condition names (equivalent to enumeration/set membership checks).

**Source file:** `CSLKPCDY.cpy`
**Used by:** `COACTUPC` (Account Update), `COCRDUPC` (Card Update)

## Source COBOL (Abbreviated)

```cobol
01 WS-EDIT-PHONE-AREA-CODE.
   05 WS-US-PHONE-AREA-CODE-TO-EDIT    PIC XXX.
      88 VALID-PHONE-AREA-CODE          VALUE
         '201' '202' '203' '204' '205' '206' '207' '208'
         '209' '210' '212' '213' '214' '215' '216' '217'
         ... (hundreds of valid NANPA area codes) ...
         '989'.
      88 VALID-GENERAL-PURP-CODE        VALUE
         '201' '202' '203' ...
      88 VALID-EASY-RECOG-AREA-CODE     VALUE
         '211' '311' '411' '511' '611' '711' '811' '911'.

01 WS-EDIT-US-STATE-CODE.
   05 US-STATE-CODE-TO-EDIT             PIC X(2).
      88 VALID-US-STATE-CODE            VALUE
         'AL' 'AK' 'AZ' 'AR' 'CA' 'CO' 'CT' 'DE' 'FL' 'GA'
         'HI' 'ID' 'IL' 'IN' 'IA' 'KS' 'KY' 'LA' 'ME' 'MD'
         'MA' 'MI' 'MN' 'MS' 'MO' 'MT' 'NE' 'NV' 'NH' 'NJ'
         'NM' 'NY' 'NC' 'ND' 'OH' 'OK' 'OR' 'PA' 'RI' 'SC'
         'SD' 'TN' 'TX' 'UT' 'VT' 'VA' 'WA' 'WV' 'WI' 'WY'
         'DC' 'PR' 'VI' 'GU' 'AS' 'MP'.

01 WS-EDIT-US-STATE-ZIP.
   05 US-STATE-ZIPCODE-TO-EDIT.
      10 US-STATE-AND-FIRST-ZIP2        PIC X(4).
         88 VALID-US-STATE-ZIP-CD2-COMBO VALUE
            'AL35' 'AL36' 'AK99' 'AZ85' 'AZ86'
            ... (valid state + first-2-digits-of-ZIP combinations) ...
```

## Field Definitions

### Phone Area Code Validation

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `WS-US-PHONE-AREA-CODE-TO-EDIT` | `XXX` | 3 | Alphanumeric | Phone area code to validate |
| 2 | `VALID-PHONE-AREA-CODE` | 88-level | N/A | Condition | TRUE if code is a valid NANPA area code |
| 3 | `VALID-GENERAL-PURP-CODE` | 88-level | N/A | Condition | TRUE if code is a general-purpose (geographic) area code |
| 4 | `VALID-EASY-RECOG-AREA-CODE` | 88-level | N/A | Condition | TRUE if code is an easily recognizable service code (211, 311, 411, etc.) |

### US State Code Validation

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 5 | `US-STATE-CODE-TO-EDIT` | `X(2)` | 2 | Alphanumeric | State code to validate |
| 6 | `VALID-US-STATE-CODE` | 88-level | N/A | Condition | TRUE if code is a valid US state or territory code |

### State-ZIP Code Combination Validation

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 7 | `US-STATE-AND-FIRST-ZIP2` | `X(4)` | 4 | Alphanumeric | State code (2) + first 2 ZIP digits to validate |
| 8 | `VALID-US-STATE-ZIP-CD2-COMBO` | 88-level | N/A | Condition | TRUE if the state-ZIP combination is valid |

## Field Notes

1. **88-level conditions:** In COBOL, 88-level items define named conditions that are TRUE when the parent field contains one of the listed values. This is equivalent to a `Set.Contains()` check or an enum/flags validation in .NET. The program moves a value into the parent field, then tests the 88-level condition name.

2. **NANPA area codes:** The `VALID-PHONE-AREA-CODE` condition contains hundreds of valid North American area codes. These codes change over time as new area codes are assigned. In the .NET system, this should be a configurable/updatable data set, not hardcoded.

3. **Service codes:** The `VALID-EASY-RECOG-AREA-CODE` condition identifies N11 service codes (211=community info, 311=government, 411=directory, 511=traffic, 611=phone company, 711=TDD relay, 811=call before you dig, 911=emergency). These are not valid phone number area codes but are recognized patterns.

4. **US-centric validation:** All three validation sets are US-specific (NANPA codes, US states, US ZIP codes). For NordKredit AB (Swedish bank), these must be replaced or supplemented with:
   - Swedish phone number validation (country code +46, area codes)
   - Swedish county/region codes (lan)
   - Swedish postal code validation (5 digits, format NNN NN)

5. **State-ZIP cross-validation:** The `VALID-US-STATE-ZIP-CD2-COMBO` condition provides a rudimentary cross-validation between state code and ZIP code range. For example, Alabama ZIP codes start with 35 or 36. This prevents data entry errors where a valid ZIP code is entered for the wrong state.

6. **Validation usage pattern:** Programs use this copybook as follows:
   ```cobol
   MOVE input-area-code TO WS-US-PHONE-AREA-CODE-TO-EDIT
   IF VALID-PHONE-AREA-CODE
       CONTINUE
   ELSE
       MOVE 'Invalid area code' TO error-message
   END-IF
   ```

## Target Architecture Mapping

| Aspect | COBOL (Current) | .NET (Target) |
|--------|-----------------|---------------|
| **Validation mechanism** | 88-level condition names (compile-time) | FluentValidation rules or Data Annotation attributes |
| **Lookup data source** | Hardcoded in copybook | Configurable: database table, JSON file, or external service |
| **Phone validation** | US NANPA area codes only | International phone validation library (e.g., `libphonenumber-csharp`) with Swedish focus |
| **Address validation** | US states + ZIP codes | Swedish postal code and region validation + international support |
| **Update mechanism** | Requires COBOL recompilation | Runtime-configurable via database or configuration update |

### .NET Validation Service (Conceptual)

```csharp
public interface IAddressValidationService
{
    bool IsValidPostalCode(string countryCode, string postalCode);
    bool IsValidRegionCode(string countryCode, string regionCode);
    bool IsValidPostalCodeForRegion(string countryCode, string regionCode, string postalCode);
}

public interface IPhoneValidationService
{
    bool IsValidPhoneNumber(string phoneNumber, string countryCode);
}

// Swedish-specific implementation
public class SwedishAddressValidator : IAddressValidationService
{
    // Swedish postal codes: 5 digits, format "NNN NN" (e.g., "114 55")
    private static readonly Regex SwedishPostalCode = new(@"^\d{3}\s?\d{2}$");

    // Swedish county codes (lan)
    private static readonly HashSet<string> SwedishRegions = new()
    {
        "AB", "AC", "BD", "C", "D", "E", "F", "G", "H", "I",
        "K", "M", "N", "O", "S", "T", "U", "W", "X", "Y", "Z"
    };

    public bool IsValidPostalCode(string countryCode, string postalCode)
    {
        if (countryCode == "SE")
            return SwedishPostalCode.IsMatch(postalCode);
        // Delegate to international validation for other countries
        throw new NotSupportedException($"Country {countryCode} not supported");
    }

    // ... additional methods
}
```

## Migration Notes

1. **Replace US-centric validation with Swedish:** The entire content of this copybook is US-specific and must be replaced for NordKredit's Swedish operations. Swedish phone numbers, postal codes, and region codes follow different formats and rules. Consider supporting both Swedish and international formats for customers with foreign addresses.

2. **Externalize validation data:** Hardcoded validation tables (phone area codes, postal codes) should be stored in a database table or configuration file that can be updated without redeployment. Phone area codes and postal code ranges change periodically.

3. **Use established libraries:** Rather than maintaining custom phone/address validation lists, use established libraries:
   - Phone validation: `libphonenumber-csharp` (Google's phone number library for .NET)
   - Address validation: Swedish postal code databases from PostNord or SCB (Statistics Sweden)

4. **Retain cross-validation pattern:** The state-ZIP cross-validation pattern (ensuring postal code matches the declared region) is valuable and should be carried forward with Swedish postal code-region mappings.

5. **International support:** While the primary user base is Swedish, NordKredit may have international customers. The validation service should support international phone numbers and addresses, with Swedish validation as the default.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **GDPR** Art. 5(1)(d) | Accuracy -- personal data must be accurate and kept up to date | Address and phone validation rules help ensure data accuracy at the point of entry. The .NET validation service must validate Swedish addresses and phone numbers to prevent inaccurate data from entering the system. |
| **AML/KYC** (FFFS 2017:11) | Customer identification -- verify customer address | Address validation (postal code + region cross-check) supports KYC address verification requirements. Invalid address data should trigger a review workflow. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- data quality | Input validation is a key internal control. The migration must ensure that validation coverage is at least equivalent to the COBOL system, adapted for Swedish data formats. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
