---
id: "acct-br-001"
title: "Account record data structure and field definitions"
domain: "account-management"
cobol_source: "CVACT01Y.cpy:1-20"
requirement_id: "ACCT-BR-001"
regulations:
  - "GDPR Art. 5(1)(c)"
  - "FSA FFFS 2014:5 Ch. 4"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-001: Account record data structure and field definitions

## Summary

The CVACT01Y copybook defines the canonical account record layout used across all account management programs in the AWS CardDemo application. Each account record is a fixed-length 300-byte VSAM KSDS (Key-Sequenced Data Set) record keyed by an 11-digit numeric account ID. The record contains core account attributes including balance fields (current balance, credit limit, cash credit limit), lifecycle dates (open date, expiry date, reissue date), current cycle accumulators (credit total, debit total), and classification fields (active status, ZIP code, group ID). The copybook is included via COPY statement in all programs that interact with the ACCTDAT file, making it the single source of truth for the account data contract. Any migration must preserve exact field lengths, decimal precision, signage, and the 300-byte record boundary.

## Business Logic

### Data Structure Definition

```
ACCOUNT-RECORD (300 bytes total):
    ACCT-ID                PIC 9(11)       -- Primary key, 11-digit account number
    ACCT-ACTIVE-STATUS     PIC X(01)       -- Account status: 'Y' = active, 'N' = inactive
    ACCT-CURR-BAL          PIC S9(10)V99   -- Current balance, signed, 2 decimal places
    ACCT-CREDIT-LIMIT      PIC S9(10)V99   -- Credit limit, signed, 2 decimal places
    ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99   -- Cash advance credit limit, signed, 2 decimal places
    ACCT-OPEN-DATE         PIC X(10)       -- Account open date, YYYY-MM-DD format
    ACCT-EXPIRAION-DATE    PIC X(10)       -- Account expiry date (misspelling preserved from COBOL)
    ACCT-REISSUE-DATE      PIC X(10)       -- Card reissue date, YYYY-MM-DD format
    ACCT-CURR-CYC-CREDIT   PIC S9(10)V99   -- Current cycle credit total, signed, 2 decimal places
    ACCT-CURR-CYC-DEBIT    PIC S9(10)V99   -- Current cycle debit total, signed, 2 decimal places
    ACCT-ADDR-ZIP          PIC X(10)       -- Account holder ZIP/postal code
    ACCT-GROUP-ID          PIC X(10)       -- Group identifier (links to disclosure/interest rate groups)
    FILLER                 PIC X(178)      -- Reserved for future expansion
```

### Pseudocode

```
DEFINE ACCOUNT-RECORD:
    Total record length = 300 bytes
    Key = ACCT-ID (11 bytes, numeric, position 1-11)

    Field layout (sequential byte positions):
        Bytes   1-11  : ACCT-ID               (11 bytes, numeric display)
        Byte    12    : ACCT-ACTIVE-STATUS     (1 byte, alphanumeric)
        Bytes  13-24  : ACCT-CURR-BAL          (12 bytes, signed packed/display)
        Bytes  25-36  : ACCT-CREDIT-LIMIT      (12 bytes, signed packed/display)
        Bytes  37-48  : ACCT-CASH-CREDIT-LIMIT (12 bytes, signed packed/display)
        Bytes  49-58  : ACCT-OPEN-DATE         (10 bytes, alphanumeric)
        Bytes  59-68  : ACCT-EXPIRAION-DATE    (10 bytes, alphanumeric)
        Bytes  69-78  : ACCT-REISSUE-DATE      (10 bytes, alphanumeric)
        Bytes  79-90  : ACCT-CURR-CYC-CREDIT   (12 bytes, signed packed/display)
        Bytes  91-102 : ACCT-CURR-CYC-DEBIT    (12 bytes, signed packed/display)
        Bytes 103-112 : ACCT-ADDR-ZIP          (10 bytes, alphanumeric)
        Bytes 113-122 : ACCT-GROUP-ID          (10 bytes, alphanumeric)
        Bytes 123-300 : FILLER                 (178 bytes, reserved)

VSAM FILE DEFINITION:
    File name  : ACCTDAT
    Organization: KSDS (Key-Sequenced Data Set)
    Record length: 300 bytes (fixed)
    Key field  : ACCT-ID
    Key length : 11 bytes
    Key offset : 0 (first field)
    Access mode: Primary key lookup, sequential browse
```

### Data Type Mapping (COBOL to .NET)

| COBOL Field | COBOL PIC | .NET Type | Notes |
|-------------|-----------|-----------|-------|
| ACCT-ID | PIC 9(11) | `string` (11 chars) | Preserve leading zeros; do not use `long` |
| ACCT-ACTIVE-STATUS | PIC X(01) | `char` or `enum` | 'Y' / 'N' only; consider `bool` with mapping |
| ACCT-CURR-BAL | PIC S9(10)V99 | `decimal` | Max: +/-9,999,999,999.99; use `decimal` not `double` |
| ACCT-CREDIT-LIMIT | PIC S9(10)V99 | `decimal` | Same as above |
| ACCT-CASH-CREDIT-LIMIT | PIC S9(10)V99 | `decimal` | Same as above |
| ACCT-OPEN-DATE | PIC X(10) | `DateOnly` | Parse YYYY-MM-DD; validate on read |
| ACCT-EXPIRAION-DATE | PIC X(10) | `DateOnly` | Note: property name should fix the typo |
| ACCT-REISSUE-DATE | PIC X(10) | `DateOnly` | Parse YYYY-MM-DD; validate on read |
| ACCT-CURR-CYC-CREDIT | PIC S9(10)V99 | `decimal` | Cycle accumulator, reset on cycle close |
| ACCT-CURR-CYC-DEBIT | PIC S9(10)V99 | `decimal` | Cycle accumulator, reset on cycle close |
| ACCT-ADDR-ZIP | PIC X(10) | `string` (10 chars) | May contain alphanumeric postal codes |
| ACCT-GROUP-ID | PIC X(10) | `string` (10 chars) | Foreign key to group/disclosure tables |

## Source COBOL Reference

**Copybook:** `CVACT01Y.cpy`
**Lines:** 1-20

```cobol
      *****************************************************************
      *    Data-structure for  account entity (RECLN 300)
      *****************************************************************
       01  ACCOUNT-RECORD.
           05  ACCT-ID                           PIC 9(11).
           05  ACCT-ACTIVE-STATUS                PIC X(01).
           05  ACCT-CURR-BAL                     PIC S9(10)V99.
           05  ACCT-CREDIT-LIMIT                 PIC S9(10)V99.
           05  ACCT-CASH-CREDIT-LIMIT            PIC S9(10)V99.
           05  ACCT-OPEN-DATE                    PIC X(10).
           05  ACCT-EXPIRAION-DATE               PIC X(10).
           05  ACCT-REISSUE-DATE                 PIC X(10).
           05  ACCT-CURR-CYC-CREDIT              PIC S9(10)V99.
           05  ACCT-CURR-CYC-DEBIT               PIC S9(10)V99.
           05  ACCT-ADDR-ZIP                     PIC X(10).
           05  ACCT-GROUP-ID                     PIC X(10).
           05  FILLER                            PIC X(178).
```

**Referenced by (COPY CVACT01Y):**

| Program | Line | Context |
|---------|------|---------|
| COACTUPC.cbl | 640 | Account update program -- reads and writes account records |
| COACTVWC.cbl | 244 | Account view program -- reads account records for display |
| CBACT01C.cbl | 89 | Batch account processing -- sequential account file processing |
| CBACT04C.cbl | 112 | Batch interest calculation -- reads account records for interest computation |

**VSAM file definition (JCL/CICS resource):**

```
File: ACCTDAT
Key: ACCT-ID (11 bytes, offset 0)
Access: KSDS (Key-Sequenced Data Set)
Record length: 300 bytes (fixed)
```

## Acceptance Criteria

### Scenario 1: Account record contains all required fields

```gherkin
GIVEN the account entity is defined in the migrated system
WHEN the entity class is inspected
THEN it contains all 12 data fields matching the CVACT01Y copybook
  AND the ACCT-ID field is a string of exactly 11 characters
  AND all currency fields (ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT) use decimal type
  AND all date fields (ACCT-OPEN-DATE, ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE) use DateOnly type
```

### Scenario 2: Currency fields preserve two-decimal precision

```gherkin
GIVEN an account record with ACCT-CURR-BAL = 1234567890.12
WHEN the balance is stored and retrieved
THEN the value is exactly 1234567890.12
  AND no floating-point rounding errors occur
  AND the value fits within the PIC S9(10)V99 range (-9999999999.99 to +9999999999.99)
```

### Scenario 3: Account ID preserves leading zeros

```gherkin
GIVEN an account with ACCT-ID = "00012345678"
WHEN the account is stored and retrieved
THEN the ACCT-ID is exactly "00012345678" (11 characters with leading zeros)
  AND leading zeros are not stripped
```

### Scenario 4: Active status allows only valid values

```gherkin
GIVEN an account record is being created or updated
WHEN the ACCT-ACTIVE-STATUS field is set
THEN only 'Y' (active) or 'N' (inactive) values are accepted
  AND any other value is rejected with a validation error
```

### Scenario 5: Date fields parse YYYY-MM-DD format

```gherkin
GIVEN an account record with ACCT-OPEN-DATE = "2024-01-15"
WHEN the date is parsed in the migrated system
THEN it is correctly interpreted as January 15, 2024
  AND invalid date strings (e.g., "2024-13-45") are rejected during validation
```

### Scenario 6: Record round-trip preserves all data

```gherkin
GIVEN an account record is read from the mainframe ACCTDAT file
WHEN it is migrated to the Azure SQL database and read back
THEN all 12 field values are identical to the original COBOL record
  AND no data is lost, truncated, or misaligned
  AND the FILLER bytes are not stored (reserved space is not migrated)
```

### Scenario 7: Negative balance values are preserved

```gherkin
GIVEN an account record with ACCT-CURR-BAL = -500.25 (overdrawn account)
WHEN the balance is stored and retrieved
THEN the value is exactly -500.25
  AND the sign is preserved through the EBCDIC-to-Unicode conversion
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5(1)(c) | Data minimisation -- personal data must be adequate, relevant, and limited to what is necessary | The account record defines a fixed set of fields with explicit lengths; the 178-byte FILLER ensures no undocumented fields are appended. The migrated system must not add PII fields beyond what the COBOL structure defines without a GDPR impact assessment |
| FSA FFFS 2014:5 | Ch. 4 | Credit institutions must maintain reliable records and adequate systems for account management | The account record structure is the foundation for all account operations; its faithful migration ensures continuity of account management capabilities and preserves audit-ready data integrity |
| GDPR | Art. 5(1)(d) | Accuracy -- personal data must be accurate and kept up to date | The ACCT-ADDR-ZIP field contains personal address data; the migrated system must ensure ZIP code data is accurately converted from EBCDIC and validated against Swedish postal code formats |

## Edge Cases

1. **EBCDIC signed numeric conversion**: The PIC S9(10)V99 fields use EBCDIC signed numeric representation where the sign is encoded in the last byte (zone portion). For example, a positive value has the last byte in the range 0xF0-0xF9, while a negative value uses 0xD0-0xD9. The EBCDIC-to-Unicode conversion must correctly interpret these signs. A naive ASCII conversion will produce corrupt balance data. The migrated system must use COBOL-aware data conversion that understands zoned decimal sign encoding.

2. **FILLER bytes and record alignment**: The 178-byte FILLER ensures the total record length is exactly 300 bytes. If future COBOL modifications added fields within the FILLER space, those fields would not be captured by the current copybook definition. The migration team must verify that no undocumented fields exist within the FILLER region by sampling actual VSAM file data and checking for non-space/non-zero patterns in bytes 123-300.

3. **ACCT-EXPIRAION-DATE misspelling**: The field name contains a misspelling ("EXPIRAION" instead of "EXPIRATION") that has been preserved across the codebase for decades. The migrated system should fix this typo in the .NET property name (use `ExpirationDate`) but must maintain a mapping annotation or comment that traces back to the original COBOL field name for traceability. Any data migration scripts referencing this field must use the original COBOL name.

4. **Implied decimal point**: The PIC S9(10)V99 fields have an implied decimal point (the `V` in the PIC clause). The COBOL runtime does not store an actual decimal point character; it stores 12 digits and tracks the decimal position at compile time. The migrated system must correctly interpret the last 2 digits as the fractional part. For example, the stored value `001234567890` represents `12345678.90`, not `1234567890`.

5. **Date fields as PIC X (not validated at storage level)**: The date fields are defined as PIC X(10), meaning COBOL treats them as plain alphanumeric strings with no intrinsic date validation at the storage level. Invalid dates (e.g., "0000-00-00", "9999-99-99", spaces) can exist in production data. The migrated system must handle invalid date values gracefully during data migration, either by cleaning them with a documented transformation rule or flagging them for manual review.

6. **Concurrent VSAM access**: Multiple CICS transactions can read the same ACCTDAT record simultaneously (read-only), but updates require exclusive control via CICS ENQUEUE or file-level locking. The migrated Azure SQL system uses different concurrency mechanisms (row-level locking, optimistic concurrency). The parallel-run period must verify that concurrent access patterns produce identical outcomes on both systems.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions for the retiring COBOL developers: (1) Are there any undocumented fields hidden within the 178-byte FILLER region? (2) Is the ACCT-GROUP-ID a foreign key to a specific group/disclosure table, and if so, what is the table name and structure? (3) Do any batch programs rely on the exact 300-byte record length for block-size calculations? (4) Are the date fields always populated in YYYY-MM-DD format, or do older records use different date formats?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
