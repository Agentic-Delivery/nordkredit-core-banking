---
id: CVACT03Y
title: "Card Cross-Reference Record"
copybook_name: "CVACT03Y.cpy"
domain: "account-management"
used_by_programs:
  - COACTUPC
  - COACTVWC
  - CBACT03C
record_length: 50
status: "extracted"
target_schema: "dbo.CustomerAccountCardXref"
---

# Card Cross-Reference Record (CVACT03Y.cpy)

## Overview

The CVACT03Y copybook defines the `CARD-XREF-RECORD` structure, representing the cross-reference linkage between cards, customers, and accounts in the CardDemo system. It is stored in the `CARDXREF` VSAM KSDS file as fixed-block records of 50 bytes, keyed by `XREF-CARD-NUM`.

This structure serves as the bridge table connecting the three primary entities in the system. Online CICS programs use the cross-reference to navigate from a card number to its associated customer and account records (the three-file read chain documented in [ACCT-BR-006](../business-rules/account-management/acct-br-006)). Batch programs use it for generating cross-reference reports ([ACCT-BR-009](../business-rules/account-management/acct-br-009)).

**Source COBOL:**

```cobol
01  CARD-XREF-RECORD.
    05  XREF-CARD-NUM              PIC X(16).
    05  XREF-CUST-ID               PIC 9(09).
    05  XREF-ACCT-ID               PIC 9(11).
    05  FILLER                     PIC X(14).
```

## Field Definitions

| Field Name | PIC Clause | Length (bytes) | Type | Description | Nullable | Target Column |
|------------|-----------|----------------|------|-------------|----------|---------------|
| XREF-CARD-NUM | X(16) | 16 | Alphanumeric | Card number (PAN) — primary key. Links to `CARD-NUM` in `CVACT02Y.cpy`. | No | `card_number` |
| XREF-CUST-ID | 9(09) | 9 | Numeric (unsigned) | Customer identifier. Links to the customer record in `CUSTDAT`. | No | `customer_id` |
| XREF-ACCT-ID | 9(11) | 11 | Numeric (unsigned) | Account identifier. Links to `ACCT-ID` in `CVACT01Y.cpy`. | No | `account_id` |
| FILLER | X(14) | 14 | Filler | Unused padding to reach 50-byte record length. | N/A | N/A |

### Field Notes

- **XREF-CARD-NUM**: The primary key, stored as alphanumeric to match the card number format in `CVACT02Y.cpy`. The CICS programs use this as the lookup key to navigate from card → customer → account in the three-file read chain.
- **XREF-CUST-ID**: 9-digit customer ID. Note this is shorter than `XREF-ACCT-ID` (11 digits). A single customer can have multiple accounts and cards. The customer record itself is defined in a separate copybook (`CUSTDAT` file, not part of the CVACT series).
- **XREF-ACCT-ID**: 11-digit account ID matching `ACCT-ID` in `CVACT01Y.cpy`. In the current system, there is a 1:1 relationship between cards and accounts (one card per account), but the cross-reference structure allows for future many-to-one expansion.
- **FILLER (14 bytes)**: Reserved padding. The target SQL schema should not carry forward unused filler.

## EBCDIC Encoding Notes

| Consideration | Detail |
|---------------|--------|
| Source encoding | EBCDIC (IBM Code Page 037 — US/Canada Latin-1) |
| Target encoding | UTF-8 |
| Special characters | No text fields containing free-form data. Card numbers and IDs are numeric/alphanumeric with no special characters. |
| Packed decimal fields | None — all numeric fields use display format (`PIC 9`), not `COMP-3`. |
| Binary fields | None — no `COMP` or `COMP-4` fields in this copybook. |
| Sign handling | No signed fields in this copybook. All numeric fields are unsigned. |

## Referential Integrity

| Relationship | Source Field | Target Table | Target Column | Constraint Type |
|-------------|-------------|-------------|---------------|-----------------|
| Xref references card | XREF-CARD-NUM | dbo.Card | card_number | FK (enforced) |
| Xref references account | XREF-ACCT-ID | dbo.Account | account_id | FK (enforced) |
| Xref references customer | XREF-CUST-ID | dbo.Customer | customer_id | FK (enforced) |

### Integrity Notes

- In the mainframe system, referential integrity between the cross-reference, card, account, and customer files is enforced entirely by COBOL program logic, not by database constraints. Each CICS online program that uses the cross-reference performs its own READ operations and handles `NOTFND` conditions programmatically.
- The three-file read chain pattern (documented in [ACCT-BR-006](../business-rules/account-management/acct-br-006)) is: card xref → account → customer. The account update program (COACTUPC) performs a `STARTBR` on the `CARDAIX` alternate index to find cards for a given account, then reads the cross-reference to find the customer.
- The target Azure SQL schema should enforce all three FK constraints at the database level. During migration, orphan cross-reference records (pointing to non-existent cards, accounts, or customers) should be flagged and remediated before enabling FK constraints.
- The `CARDXREF` file also has an alternate index (`CXACAIX`) keyed by `XREF-ACCT-ID`, enabling reverse lookups from account to card. The target SQL schema handles this via the FK index on `account_id`.

## Sample Data

```
Record 1:
  XREF-CARD-NUM:   "4532015112830366"
  XREF-CUST-ID:    000054321
  XREF-ACCT-ID:    00000012345

Record 2:
  XREF-CARD-NUM:   "4716826378940052"
  XREF-CUST-ID:    000098765
  XREF-ACCT-ID:    00000067890
```

> **Note**: Sample data above is entirely synthetic. No real card numbers, customer IDs, or account numbers are used.

### Data Characteristics

- Estimated record count: ~50,000 (one xref per card)
- Growth rate: ~500 new records/month (mirrors card issuance rate)
- Key distribution: `XREF-CARD-NUM` is unique and sequentially issued. `XREF-ACCT-ID` and `XREF-CUST-ID` may have multiple entries (multiple cards per account/customer).

## Migration Notes

### Schema Mapping

- **Source**: VSAM file `AWS.M2.CARDDEMO.CARDXREF.PS` (FB, LRECL=50) with alternate index `CXACAIX` on `XREF-ACCT-ID`
- **Target**: `dbo.CustomerAccountCardXref`
- **Migration approach**: Full extract followed by incremental sync during parallel-run period

### Target Table DDL

```sql
CREATE TABLE dbo.CustomerAccountCardXref (
    card_number     NVARCHAR(16)    NOT NULL,
    customer_id     BIGINT          NOT NULL,
    account_id      BIGINT          NOT NULL,
    CONSTRAINT PK_Xref PRIMARY KEY (card_number),
    CONSTRAINT FK_Xref_Card FOREIGN KEY (card_number)
        REFERENCES dbo.Card (card_number),
    CONSTRAINT FK_Xref_Account FOREIGN KEY (account_id)
        REFERENCES dbo.Account (account_id),
    CONSTRAINT FK_Xref_Customer FOREIGN KEY (customer_id)
        REFERENCES dbo.Customer (customer_id)
);

-- Replaces the CXACAIX alternate index for account-to-card lookups
CREATE NONCLUSTERED INDEX IX_Xref_AccountId
    ON dbo.CustomerAccountCardXref (account_id);

-- For customer-to-card lookups
CREATE NONCLUSTERED INDEX IX_Xref_CustomerId
    ON dbo.CustomerAccountCardXref (customer_id);
```

### Data Quality

- Verify no orphan `XREF-CARD-NUM` values (xref records pointing to non-existent cards in `CARDDATA`).
- Verify no orphan `XREF-ACCT-ID` values (xref records pointing to non-existent accounts in `ACCTDAT`).
- Verify no orphan `XREF-CUST-ID` values (xref records pointing to non-existent customers in `CUSTDAT`).
- Check for duplicate `XREF-CARD-NUM` values (should be unique as primary key).
- Validate all numeric ID fields contain only digits (no spaces or special characters).

### Validation Strategy

- Record count reconciliation: VSAM file record count vs. `SELECT COUNT(*) FROM dbo.CustomerAccountCardXref`.
- Referential integrity: All three FK columns should have matching records in their parent tables. Run orphan checks before enabling FK constraints.
- Cross-validation: For each xref record, verify the card's `CARD-ACCT-ID` (from `CVACT02Y`) matches the xref's `XREF-ACCT-ID`. Any mismatches indicate data corruption.
- Alternate index validation: Verify the non-clustered index on `account_id` produces the same result set as the `CXACAIX` alternate index for a sample of account IDs.

### Performance Considerations

- `card_number` is the primary lookup key — clustered index on PK is sufficient.
- Non-clustered indexes on `account_id` and `customer_id` replace the VSAM alternate indexes and support the reverse-lookup patterns used by CICS programs.
- No partitioning needed at current volumes (~50K records).
- This table is read-heavy (lookups during every account view/update operation). Consider adding to a memory-optimized filegroup if query latency is critical.
