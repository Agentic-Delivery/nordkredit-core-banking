---
id: "acct-br-001"
title: "Account record data structure and field definitions"
domain: "account-management"
cobol_source: "CVACT01Y.cpy:1-25"
requirement_id: "ACCT-BR-001"
regulations:
  - "GDPR Art. 5"
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-001: Account record data structure and field definitions

## Summary

The account record (ACCOUNT-RECORD, defined in CVACT01Y.cpy) is the primary data structure for all account management operations. It is a 300-byte VSAM KSDS record keyed by the 11-digit account identifier (ACCT-ID). The structure contains account identification, financial balances (current balance, credit limit, cash credit limit), cycle tracking (open balance, cycle credits/debits), group/status information, and date fields. This copybook is referenced by transaction processing programs (CBTRN01C, CBTRN02C), card management programs (COCRDSLC, COCRDUPC), and the dedicated account management programs (COACTVWC, COACTUPC). The record defines the authoritative schema for all account-related operations across the NordKredit mainframe system.

## Field Specifications

### ACCOUNT-RECORD (CVACT01Y.cpy) — Account Master Record

| Field | COBOL Name | PIC | Offset | Length | Description |
|---|---|---|---|---|---|
| Account ID | ACCT-ID | 9(11) | 1 | 11 | Unique account identifier (primary key) |
| Active Status | ACCT-ACTIVE-STATUS | X(01) | 12 | 1 | Account status: 'Y' = active, 'N' = inactive |
| Current Balance | ACCT-CURR-BAL | S9(10)V99 | 13 | 12 | Current account balance (signed, 2 decimals) |
| Credit Limit | ACCT-CREDIT-LIMIT | S9(10)V99 | 25 | 12 | Maximum credit allowed on account |
| Cash Credit Limit | ACCT-CASH-CREDIT-LIMIT | S9(10)V99 | 37 | 12 | Maximum cash advance credit allowed |
| Open Balance | ACCT-OPEN-BAL | S9(10)V99 | 49 | 12 | Balance at start of current cycle |
| Expiration Date | ACCT-EXPIRAION-DATE | X(10) | 61 | 10 | Account expiration date (YYYY-MM-DD) |
| Reissue Date | ACCT-REISSUE-DATE | X(10) | 71 | 10 | Date account was last reissued/renewed |
| Current Cycle Credit | ACCT-CURR-CYC-CREDIT | S9(10)V99 | 81 | 12 | Total credits in current billing cycle |
| Current Cycle Debit | ACCT-CURR-CYC-DEBIT | S9(10)V99 | 93 | 12 | Total debits in current billing cycle |
| Account Group ID | ACCT-GROUP-ID | X(10) | 105 | 10 | Disclosure/interest rate group assignment |
| Statement Cycle | ACCT-STMT-CYC | X(02) | 115 | 2 | Statement generation cycle code |
| Pending Credit | ACCT-PEND-CREDIT | S9(10)V99 | 117 | 12 | Pending (unposted) credits |
| Pending Debit | ACCT-PEND-DEBIT | S9(10)V99 | 129 | 12 | Pending (unposted) debits |
| Filler | FILLER | X(159) | 141 | 159 | Reserved/unused |
| **Total** | | | | **300** | |

### Financial Field Precision

| Field | PIC | SQL Mapping | Range |
|---|---|---|---|
| ACCT-CURR-BAL | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |
| ACCT-CREDIT-LIMIT | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |
| ACCT-CASH-CREDIT-LIMIT | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |
| ACCT-OPEN-BAL | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |
| ACCT-PEND-CREDIT | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |
| ACCT-PEND-DEBIT | S9(10)V99 | decimal(12,2) | -9,999,999,999.99 to +9,999,999,999.99 |

## Business Logic

### Pseudocode

```
ACCOUNT-RECORD STRUCTURE:
    Key: ACCT-ID (11-digit numeric, KSDS primary key)
    Record length: 300 bytes

    DEFINE financial-fields:
        All monetary fields use S9(10)V99 (signed, implied decimal, 12 bytes)
        Balances can be positive or negative
        Credit limits are always positive or zero

    DEFINE status-fields:
        ACCT-ACTIVE-STATUS: 'Y' (active) or 'N' (inactive)
        Inactive accounts cannot process new transactions
        Status changes require explicit update via COACTUPC

    DEFINE date-fields:
        ACCT-EXPIRAION-DATE: YYYY-MM-DD format, X(10)
        ACCT-REISSUE-DATE: YYYY-MM-DD format, X(10)
        Dates stored as character strings (not packed decimal)

    DEFINE group-fields:
        ACCT-GROUP-ID: Links to DIS-GROUP-RECORD (CVTRA02Y.cpy)
        Determines interest rate and disclosure group
        ACCT-STMT-CYC: Two-character cycle code for statement generation

    VSAM FILE DEFINITION:
        File: ACCTDAT (ACCOUNT file)
        Organization: KSDS (Key Sequenced Data Set)
        Primary key: ACCT-ID (11 bytes)
        Record length: 300 bytes
        Access: READ/WRITE/REWRITE (online), sequential READ (batch)
```

### Decision Table

| Field Category | Fields | Used By | Migration Priority |
|---|---|---|---|
| Identification | ACCT-ID | All programs | Critical — primary key |
| Status | ACCT-ACTIVE-STATUS | COACTVWC, COACTUPC, CBTRN02C | Critical — gates all operations |
| Current Balances | ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT | COACTVWC, COACTUPC, CBTRN02C | Critical — financial integrity |
| Cycle Tracking | ACCT-OPEN-BAL, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT | CBTRN02C, CBACT03C | High — billing accuracy |
| Pending | ACCT-PEND-CREDIT, ACCT-PEND-DEBIT | CBTRN02C | High — pre-posting totals |
| Group/Cycle | ACCT-GROUP-ID, ACCT-STMT-CYC | CBACT03C, statement generation | Medium — batch processing |
| Dates | ACCT-EXPIRAION-DATE, ACCT-REISSUE-DATE | COACTUPC, CBTRN02C | High — lifecycle management |
| Filler | FILLER (159 bytes) | None | Not migrated |

## Source COBOL Reference

**Copybook:** `CVACT01Y.cpy`
**Lines:** 1-25

```cobol
      *****************************************************************
      *    Data-structure for account entity (RECLN = 300)
      *****************************************************************
       01  ACCOUNT-RECORD.
           05  ACCT-ID                            PIC 9(11).
           05  ACCT-ACTIVE-STATUS                 PIC X(01).
           05  ACCT-CURR-BAL                      PIC S9(10)V99.
           05  ACCT-CREDIT-LIMIT                  PIC S9(10)V99.
           05  ACCT-CASH-CREDIT-LIMIT             PIC S9(10)V99.
           05  ACCT-OPEN-BAL                      PIC S9(10)V99.
           05  ACCT-EXPIRAION-DATE                PIC X(10).
           05  ACCT-REISSUE-DATE                  PIC X(10).
           05  ACCT-CURR-CYC-CREDIT              PIC S9(10)V99.
           05  ACCT-CURR-CYC-DEBIT               PIC S9(10)V99.
           05  ACCT-GROUP-ID                      PIC X(10).
           05  ACCT-STMT-CYC                      PIC X(02).
           05  ACCT-PEND-CREDIT                   PIC S9(10)V99.
           05  ACCT-PEND-DEBIT                    PIC S9(10)V99.
           05  FILLER                             PIC X(159).
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *
```

### Cross-Reference to Other Copybooks

```cobol
      * CVACT02Y.cpy — Card entity record (150 bytes)
      * Links card to account via CARD-ACCT-ID PIC 9(11)
       01  CARD-RECORD.
           05  CARD-NUM               PIC X(16).
           05  CARD-ACCT-ID           PIC 9(11).
           ...

      * CVACT03Y.cpy — Card cross-reference (50 bytes)
      * Links card -> customer -> account
       01  CARD-XREF-RECORD.
           05  XREF-CARD-NUM          PIC X(16).
           05  XREF-CUST-ID           PIC 9(09).
           05  XREF-ACCT-ID           PIC 9(11).
           ...
```

## Acceptance Criteria

### Scenario 1: Account record field mapping

```gherkin
GIVEN the ACCOUNT-RECORD copybook (CVACT01Y.cpy)
WHEN an account record is stored in the ACCTDAT VSAM file
THEN it occupies exactly 300 bytes
  AND each field is at the documented offset and length
  AND the primary key ACCT-ID is 11 numeric digits
```

### Scenario 2: Financial field precision

```gherkin
GIVEN all monetary fields use PIC S9(10)V99
WHEN a balance or limit value is stored
THEN it supports values from -9,999,999,999.99 to +9,999,999,999.99
  AND the implied decimal point provides exactly 2 decimal places
  AND no rounding occurs during storage
  AND the migrated SQL type is decimal(12,2)
```

### Scenario 3: Account status field values

```gherkin
GIVEN the ACCT-ACTIVE-STATUS field is PIC X(01)
WHEN an account record is read
THEN the status is either 'Y' (active) or 'N' (inactive)
  AND only active accounts ('Y') can process new transactions
  AND inactive accounts can still be viewed but not modified
```

### Scenario 4: Date field format

```gherkin
GIVEN the ACCT-EXPIRAION-DATE and ACCT-REISSUE-DATE fields are PIC X(10)
WHEN a date is stored
THEN the format is YYYY-MM-DD
  AND the migrated SQL type is date
  AND EBCDIC-to-UTF8 conversion preserves the date string
```

### Scenario 5: Account group linkage

```gherkin
GIVEN the ACCT-GROUP-ID field is PIC X(10)
WHEN an account is assigned to a disclosure group
THEN the group ID links to DIS-GROUP-RECORD (CVTRA02Y.cpy)
  AND the linked group determines the interest rate for the account
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| GDPR | Art. 5 (Data Minimization) | Store only necessary personal data | Account record contains only operationally necessary fields; 159-byte filler is unused and not migrated |
| FSA FFFS 2014:5 | Ch. 7 (Financial Systems) | Accurate recording of financial positions | Fixed-precision numeric fields (S9(10)V99) ensure consistent storage of all balance and limit values |
| PSD2 | Art. 97 (Account Information) | Accurate account balance reporting | Current balance, credit limits, and cycle tracking fields provide complete financial position for each account |
| FSA FFFS 2014:5 | Ch. 4 §3 (Operational Risk) | Data integrity controls | VSAM KSDS organization with unique 11-digit key prevents duplicate accounts |

## Edge Cases

1. **EBCDIC encoding**: All PIC X fields (status, dates, group ID) are stored in EBCDIC. Migration must convert to UTF-8. The ACCT-ID field (PIC 9) uses zoned decimal encoding which maps directly to ASCII/UTF-8 digits.

2. **Implied decimal point**: All S9(10)V99 fields use an implied (not stored) decimal point. The 12-byte storage holds a signed numeric value without an explicit decimal character. The migrated system must use `decimal(12,2)` and convert during data migration.

3. **Typo in field name**: The COBOL source contains `ACCT-EXPIRAION-DATE` (missing 'T' in EXPIRATION). This typo exists in the original CardDemo source and is preserved in documentation for traceability. The migrated system should use the corrected spelling `ExpirationDate`.

4. **Filler bytes**: The 159-byte FILLER constitutes over half the record (159 of 300 bytes). This is reserved space from the original system design. The migrated SQL schema does not need to preserve this padding.

5. **Active status semantics**: The Account.cs domain model in the transaction domain uses 'A' (active) and 'D' (dormant) for the mapped status field, which differs from the COBOL source's 'Y'/'N' values. This mapping discrepancy must be resolved with domain experts — the COBOL source is the authoritative reference.

6. **Credit limits as negative values**: Although credit limits are logically positive values, the signed field (S9) technically allows negative values. The COBOL programs do not validate against negative credit limits. The migrated system should add a CHECK constraint ensuring credit limits are >= 0.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- What is the full list of valid values for ACCT-ACTIVE-STATUS? The source shows 'Y'/'N' but Account.cs maps to 'A'/'D' — which is authoritative?
- Is the ACCT-GROUP-ID actively used for interest rate determination, or is it a legacy field?
- What is the meaning of the ACCT-STMT-CYC two-character code? Is it a month designation (e.g., '01'-'12'), a frequency code, or a named cycle?
- Should the 159-byte filler be reserved for any planned extensions, or is it truly unused?
- Is the ACCT-REISSUE-DATE populated for all accounts, or only for renewed/reissued accounts?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
