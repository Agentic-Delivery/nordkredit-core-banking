---
id: "card-br-009"
title: "Card record data structure and field definitions"
domain: "card-management"
cobol_source: "CVACT02Y.cpy:1-14"
requirement_id: "CARD-BR-009"
regulations:
  - "GDPR Art. 5(1)(c)"
  - "GDPR Art. 5(1)(d)"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# CARD-BR-009: Card record data structure and field definitions

## Summary

The CVACT02Y copybook defines the primary card record layout used across all card management programs. Each card record is a fixed-length 150-byte VSAM record keyed by the 16-digit card number. The record contains the card number (primary key), account ID (11-digit foreign key to account master), CVV security code (3-digit numeric), embossed cardholder name (50 characters), expiration date (10-character formatted as YYYY-MM-DD), active status flag (single character 'Y'/'N'), and a 59-byte filler for future expansion. This structure is the single source of truth for card data and is shared by the list (COCRDLIC), detail (COCRDSLC), and update (COCRDUPC) programs.

## Business Logic

### Data Structure

```
CARD-RECORD (150 bytes total):
    CARD-NUM              PIC X(16)   -- Primary key, 16-digit card number
    CARD-ACCT-ID          PIC 9(11)   -- Foreign key to account, 11-digit numeric
    CARD-CVV-CD           PIC 9(03)   -- Card verification value, 3-digit numeric
    CARD-EMBOSSED-NAME    PIC X(50)   -- Cardholder name as embossed on card
    CARD-EXPIRAION-DATE   PIC X(10)   -- Expiry date formatted YYYY-MM-DD
    CARD-ACTIVE-STATUS    PIC X(01)   -- 'Y' = active, 'N' = inactive
    FILLER                PIC X(59)   -- Reserved for future use
```

### Field Specifications

| Field | Type | Length | Format | Constraints |
|-------|------|--------|--------|-------------|
| CARD-NUM | Alphanumeric | 16 | Numeric digits only | Must be 16 digits, not all zeros, primary key |
| CARD-ACCT-ID | Numeric | 11 | Zoned decimal | Must be 11 digits, not all zeros, FK to account |
| CARD-CVV-CD | Numeric | 3 | Zoned decimal | 3-digit security code, not user-editable |
| CARD-EMBOSSED-NAME | Alphanumeric | 50 | Alphabetics and spaces | Stored uppercase, max 50 chars |
| CARD-EXPIRAION-DATE | Alphanumeric | 10 | YYYY-MM-DD | Composed of year (1950-2099), month (01-12), day |
| CARD-ACTIVE-STATUS | Alphanumeric | 1 | 'Y' or 'N' | Binary active/inactive flag |
| FILLER | Alphanumeric | 59 | N/A | Reserved, initialized to spaces |

### VSAM File Organization

| File | Key | Key Length | Access Method | Purpose |
|------|-----|-----------|---------------|---------|
| CARDDAT | CARD-NUM | 16 bytes | Primary key (KSDS) | Direct card lookup by card number |
| CARDAIX | CARD-ACCT-ID | 11 bytes | Alternate index (AIX) | Card lookup by account ID |

## Source COBOL Reference

**Copybook:** `CVACT02Y.cpy`
**Lines:** 1-14

```cobol
      *****************************************************************
      *    Data-structure for card entity (RECLN 150)
      *****************************************************************
       01  CARD-RECORD.
           05  CARD-NUM                          PIC X(16).
           05  CARD-ACCT-ID                      PIC 9(11).
           05  CARD-CVV-CD                       PIC 9(03).
           05  CARD-EMBOSSED-NAME                PIC X(50).
           05  CARD-EXPIRAION-DATE               PIC X(10).
           05  CARD-ACTIVE-STATUS                PIC X(01).
           05  FILLER                            PIC X(59).
```

**Usage in programs:**

- `COCRDLIC.cbl` (line 290): `COPY CVACT02Y.` — used for reading card records during list display
- `COCRDSLC.cbl` (line 234): `COPY CVACT02Y.` — used for reading card records during detail view
- `COCRDUPC.cbl` (line 353): `COPY CVACT02Y.` — used for reading and rewriting card records during update

## Acceptance Criteria

### Scenario 1: Card record round-trip integrity

```gherkin
GIVEN a card record is written with:
  | Field              | Value              |
  | Card Number        | 4000123456789012   |
  | Account ID         | 12345678901        |
  | CVV Code           | 123                |
  | Embossed Name      | JOHN DOE           |
  | Expiration Date    | 2027-12-31         |
  | Active Status      | Y                  |
WHEN the record is read back from the CARDDAT file
THEN all field values match exactly as written
  AND the record length is 150 bytes
```

### Scenario 2: Expiration date format preservation

```gherkin
GIVEN a card record with expiration date "2028-06-15"
WHEN the record is read and displayed
THEN the year component is "2028" (positions 1-4)
  AND the month component is "06" (positions 6-7)
  AND the day component is "15" (positions 9-10)
  AND the separators are "-" (positions 5 and 8)
```

### Scenario 3: Account ID alternate index lookup

```gherkin
GIVEN multiple cards exist for account "12345678901"
WHEN a lookup is performed via the CARDAIX alternate index
THEN the first card (by primary key order) for that account is returned
```

### Scenario 4: Active status values

```gherkin
GIVEN a card record exists in the system
WHEN the CARD-ACTIVE-STATUS field is read
THEN its value is either 'Y' (active) or 'N' (inactive)
  AND no other values are valid for this field
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5(1)(c) | Data minimization — personal data must be adequate, relevant, and limited to what is necessary | The card record contains only fields necessary for card operations: identification (card number, account), security (CVV), display (name, expiry), and status. The 59-byte filler demonstrates forward planning without collecting unnecessary data |
| GDPR | Art. 5(1)(d) | Accuracy — personal data must be accurate and kept up to date | The embossed name and expiry date represent current card state; the update workflow (CARD-BR-006, CARD-BR-007) ensures these fields are validated before persistence |
| PSD2 | Art. 97 | Protection of payment instrument data | The CVV code is stored as part of the card record; the migrated system must ensure CVV data is protected at rest (encryption) and never displayed in full to users |

## Edge Cases

1. **EBCDIC to Unicode conversion**: COBOL PIC 9 fields (CARD-ACCT-ID, CARD-CVV-CD) are stored in EBCDIC zoned decimal format. During migration, these must be converted to their numeric string equivalents. PIC X fields must be converted from EBCDIC to Unicode (UTF-8), with special attention to character set differences.

2. **Filler field contents**: The 59-byte FILLER is initialized to spaces in new records but may contain arbitrary data in records created by older program versions. The migrated system should treat the filler as opaque padding and not rely on its contents.

3. **Card number as primary key**: The card number serves as both a primary key and a sensitive payment instrument identifier. The migrated system should consider whether to introduce a surrogate key for internal references while maintaining the card number for business operations.

4. **Expiration date as string**: The expiry date is stored as a formatted string "YYYY-MM-DD" rather than as separate numeric components. This means date arithmetic requires parsing. The migrated system should use a proper date type (e.g., `DateOnly` in .NET) while maintaining the formatted representation for display compatibility.

5. **CVV storage compliance**: PCI DSS prohibits storage of CVV after authorization. The presence of CVV in the card record suggests this system predates PCI DSS compliance requirements or is used for card issuance rather than transaction processing. The migrated system must evaluate whether CVV storage is still appropriate under current PCI DSS standards.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The CARD-RECORD layout must be mapped to an Azure SQL schema. Key decisions: (1) whether to maintain the 150-byte fixed-length record format or normalize into proper relational tables, (2) whether the 59-byte filler contains any meaningful data in production that needs preservation, (3) whether the CVV should be stored at all in the migrated system given PCI DSS requirements.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
