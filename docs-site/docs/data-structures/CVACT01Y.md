---
id: CVACT01Y
title: "Account Record"
copybook_name: "CVACT01Y.cpy"
domain: "account-management"
used_by_programs:
  - COACTUPC
  - COACTVWC
  - CBACT01C
  - CBACT04C
record_length: 300
status: "extracted"
target_schema: "dbo.Account"
---

# Account Record (CVACT01Y.cpy)

## Overview

The CVACT01Y copybook defines the `ACCOUNT-RECORD` structure, representing the core account entity in the CardDemo system. It is stored in the `ACCTDAT` VSAM KSDS (Key-Sequenced Data Set) file as fixed-block records of 300 bytes, keyed by the 11-digit `ACCT-ID`.

This is the primary account master record, accessed by both online CICS programs (account view and update transactions) and batch programs (account extract, interest calculation). Every account management operation depends on this structure. The record contains financial balance fields with signed packed decimal precision, lifecycle dates, current-cycle accumulators, and classification fields.

**Source COBOL:**

```cobol
01  ACCOUNT-RECORD.
    05  ACCT-ID                    PIC 9(11).
    05  ACCT-ACTIVE-STATUS         PIC X(01).
    05  ACCT-CURR-BAL              PIC S9(10)V99.
    05  ACCT-CREDIT-LIMIT          PIC S9(10)V99.
    05  ACCT-CASH-CREDIT-LIMIT     PIC S9(10)V99.
    05  ACCT-OPEN-DATE             PIC X(10).
    05  ACCT-EXPIRAION-DATE        PIC X(10).
    05  ACCT-REISSUE-DATE          PIC X(10).
    05  ACCT-CURR-CYC-CREDIT       PIC S9(10)V99.
    05  ACCT-CURR-CYC-DEBIT        PIC S9(10)V99.
    05  ACCT-ADDR-ZIP              PIC X(10).
    05  ACCT-GROUP-ID              PIC X(10).
    05  FILLER                     PIC X(178).
```

## Field Definitions

| Field Name | PIC Clause | Length (bytes) | Type | Description | Nullable | Target Column |
|------------|-----------|----------------|------|-------------|----------|---------------|
| ACCT-ID | 9(11) | 11 | Numeric (unsigned) | Primary key — 11-digit account number. | No | `account_id` |
| ACCT-ACTIVE-STATUS | X(01) | 1 | Alphanumeric | Account status flag. `Y` = active, `N` = inactive. | No | `is_active` |
| ACCT-CURR-BAL | S9(10)V99 | 12 | Signed decimal (2 places) | Current account balance. Signed to allow negative balances (overdraft). | No | `current_balance` |
| ACCT-CREDIT-LIMIT | S9(10)V99 | 12 | Signed decimal (2 places) | Credit limit assigned to the account. | No | `credit_limit` |
| ACCT-CASH-CREDIT-LIMIT | S9(10)V99 | 12 | Signed decimal (2 places) | Cash advance credit limit, separate from purchase credit limit. | No | `cash_credit_limit` |
| ACCT-OPEN-DATE | X(10) | 10 | Alphanumeric | Account opening date as string (`YYYY-MM-DD` format). | No | `open_date` |
| ACCT-EXPIRAION-DATE | X(10) | 10 | Alphanumeric | Account expiration date (`YYYY-MM-DD`). Note: field name contains original typo from source. | No | `expiration_date` |
| ACCT-REISSUE-DATE | X(10) | 10 | Alphanumeric | Date of last card/account reissue (`YYYY-MM-DD`). | Yes | `reissue_date` |
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | 12 | Signed decimal (2 places) | Total credits posted in the current billing cycle. | No | `current_cycle_credit` |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | 12 | Signed decimal (2 places) | Total debits posted in the current billing cycle. | No | `current_cycle_debit` |
| ACCT-ADDR-ZIP | X(10) | 10 | Alphanumeric | Cardholder ZIP/postal code. | Yes | `address_zip` |
| ACCT-GROUP-ID | X(10) | 10 | Alphanumeric | Disclosure group identifier, used for interest rate lookup in batch processing. | Yes | `group_id` |
| FILLER | X(178) | 178 | Filler | Unused padding to reach 300-byte record length. | N/A | N/A |

### Field Notes

- **ACCT-CURR-BAL, ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT**: All five monetary fields use `PIC S9(10)V99` — signed display numeric with an implied decimal point (V99 means 2 decimal places). In COBOL, this stores 12 digits as display characters with a trailing overpunch sign. The .NET target must use `decimal(12,2)` to preserve exact precision.
- **ACCT-EXPIRAION-DATE**: Contains a typo in the original COBOL source (`EXPIRAION` instead of `EXPIRATION`). The target column corrects this to `expiration_date`.
- **ACCT-GROUP-ID**: Used by the batch interest calculation program (CBACT04C) to look up the applicable interest rate from the disclosure group file (`DISCGRP`). If the group is not found, the program falls back to a `DEFAULT` group.
- **ACCT-ACTIVE-STATUS**: The account update program (COACTUPC) validates this field accepts only `Y` or `N`. The view program (COACTVWC) displays it as a read-only attribute.
- **FILLER (178 bytes)**: Over half the record is reserved filler. This suggests the structure was designed for future expansion. The target SQL schema should not carry forward unused filler.

## EBCDIC Encoding Notes

| Consideration | Detail |
|---------------|--------|
| Source encoding | EBCDIC (IBM Code Page 037 — US/Canada Latin-1) |
| Target encoding | UTF-8 |
| Special characters | `ACCT-ADDR-ZIP` may contain Swedish postal codes (5 digits, e.g., `114 55`). No special character issues expected for ZIP fields. |
| Packed decimal fields | None — all numeric fields use display format (`PIC 9` and `PIC S9`), not `COMP-3`. |
| Binary fields | None — no `COMP` or `COMP-4` fields in this copybook. |
| Sign handling | Five signed fields (`S9(10)V99`) use trailing overpunch sign encoding. The last byte encodes both the digit and the sign. Conversion must correctly extract sign and digit. |

### Encoding Validation

- Verify trailing overpunch sign bytes convert correctly for negative balances (e.g., `D` zone = negative in EBCDIC).
- Confirm date string fields parse correctly as `YYYY-MM-DD` after EBCDIC-to-UTF-8 conversion.

## Referential Integrity

| Relationship | Source Field | Target Table | Target Column | Constraint Type |
|-------------|-------------|-------------|---------------|-----------------|
| Account has cards | ACCT-ID | dbo.Card | account_id | FK (enforced) |
| Account linked to customer (via xref) | ACCT-ID | dbo.CustomerAccountCardXref | account_id | FK (enforced) |
| Account belongs to disclosure group | ACCT-GROUP-ID | dbo.DisclosureGroup | group_id | FK / Soft reference |

### Integrity Notes

- In the mainframe system, the relationship between Account, Card, and Customer is enforced by COBOL program logic in CICS transactions (COACTUPC reads card and customer via STARTBR on CARDAIX alternate index), not by database constraints.
- The cross-reference file (`CVACT03Y.cpy` / `CARDXREF`) links cards to accounts and customers. The three-file read chain (account → card xref → customer) is documented in [ACCT-BR-006](../business-rules/account-management/acct-br-006).
- The disclosure group relationship (`ACCT-GROUP-ID` → `DISCGRP` file) is used only by batch interest calculation ([ACCT-BR-008](../business-rules/account-management/acct-br-008)). The target schema should enforce this as a soft reference (nullable FK or lookup), since `DEFAULT` is used as a fallback.

## Sample Data

```
Record 1:
  ACCT-ID:              00000012345
  ACCT-ACTIVE-STATUS:   "Y"
  ACCT-CURR-BAL:        +0000025430.50
  ACCT-CREDIT-LIMIT:    +0000050000.00
  ACCT-CASH-CREDIT-LIMIT: +0000010000.00
  ACCT-OPEN-DATE:       "2019-03-15"
  ACCT-EXPIRAION-DATE:  "2027-03-31"
  ACCT-REISSUE-DATE:    "2024-03-15"
  ACCT-CURR-CYC-CREDIT: +0000002500.00
  ACCT-CURR-CYC-DEBIT:  +0000003150.75
  ACCT-ADDR-ZIP:        "11455     "
  ACCT-GROUP-ID:        "PRIME     "

Record 2:
  ACCT-ID:              00000067890
  ACCT-ACTIVE-STATUS:   "N"
  ACCT-CURR-BAL:        -0000001234.00
  ACCT-CREDIT-LIMIT:    +0000025000.00
  ACCT-CASH-CREDIT-LIMIT: +0000005000.00
  ACCT-OPEN-DATE:       "2015-08-22"
  ACCT-EXPIRAION-DATE:  "2024-08-31"
  ACCT-REISSUE-DATE:    "          "
  ACCT-CURR-CYC-CREDIT: +0000000000.00
  ACCT-CURR-CYC-DEBIT:  +0000000000.00
  ACCT-ADDR-ZIP:        "41258     "
  ACCT-GROUP-ID:        "DEFAULT   "
```

> **Note**: Sample data above is entirely synthetic. No real account numbers or PII are used.

### Data Characteristics

- Estimated record count: ~75,000 accounts (active + historical)
- Growth rate: ~300 new accounts/month
- Key distribution: `ACCT-ID` is a sequential 11-digit number. Active accounts are concentrated in recent ID ranges.

## Migration Notes

### Schema Mapping

- **Source**: VSAM file `AWS.M2.CARDDEMO.ACCTDATA.PS` (FB, LRECL=300)
- **Target**: `dbo.Account`
- **Migration approach**: Full extract followed by incremental sync during parallel-run period

### Target Table DDL

```sql
CREATE TABLE dbo.Account (
    account_id          BIGINT          NOT NULL,
    is_active           BIT             NOT NULL,
    current_balance     DECIMAL(12,2)   NOT NULL,
    credit_limit        DECIMAL(12,2)   NOT NULL,
    cash_credit_limit   DECIMAL(12,2)   NOT NULL,
    open_date           DATE            NOT NULL,
    expiration_date     DATE            NOT NULL,
    reissue_date        DATE            NULL,
    current_cycle_credit DECIMAL(12,2)  NOT NULL,
    current_cycle_debit DECIMAL(12,2)   NOT NULL,
    address_zip         NVARCHAR(10)    NULL,
    group_id            NVARCHAR(10)    NULL,
    CONSTRAINT PK_Account PRIMARY KEY (account_id)
);
```

### Data Quality

- Validate `ACCT-ACTIVE-STATUS` contains only `Y` or `N` — flag any other values.
- Verify `ACCT-OPEN-DATE` and `ACCT-EXPIRAION-DATE` are parseable dates and not placeholders (e.g., `0000-00-00` or spaces).
- Check for negative `ACCT-CREDIT-LIMIT` values (should always be positive).
- Verify `ACCT-CURR-BAL` is within credit limit bounds: `ACCT-CURR-BAL >= -(ACCT-CREDIT-LIMIT)`.
- Confirm `ACCT-GROUP-ID` values exist in the disclosure group reference file.

### Validation Strategy

- Record count reconciliation: VSAM file record count vs. `SELECT COUNT(*) FROM dbo.Account`.
- Field-level spot checks: Sample 1,000 random records and compare all field values.
- Monetary precision: Verify all five decimal fields maintain exact precision after conversion (no rounding artifacts).
- Referential integrity: `SELECT account_id FROM dbo.Account WHERE group_id NOT IN (SELECT group_id FROM dbo.DisclosureGroup)` should return zero rows (excluding NULL group_id).
- Business rule validation: All accounts with `expiration_date < GETDATE()` should have `is_active = 0` (or be flagged for review).

### Performance Considerations

- `account_id` is the primary lookup key — clustered index on PK is sufficient.
- Add non-clustered index on `group_id` for batch interest calculation join performance.
- Add non-clustered index on `is_active` for filtering active accounts in online queries.
- No partitioning needed at current volumes (~75K records).
- During parallel-run, use change tracking on `current_balance`, `current_cycle_credit`, and `current_cycle_debit` for incremental sync.
