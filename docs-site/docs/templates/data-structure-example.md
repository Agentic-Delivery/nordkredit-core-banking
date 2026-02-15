---
id: DS-CARD-001
title: "Card Record"
copybook_name: "CVACT02Y.cpy"
domain: "account-management"
used_by_programs:
  - COCRDLIC
  - COCRDSLC
  - COCRDUPC
  - CBTRN02C
record_length: 150
status: extracted
target_schema: "dbo.Card"
---

# Card Record (CVACT02Y.cpy)

## Overview

The CVACT02Y copybook defines the `CARD-RECORD` structure, representing a credit card entity in the CardDemo system. It is stored in the `CARDDATA` VSAM file as fixed-block records of 150 bytes.

This structure is the primary card master record and is accessed by both online CICS programs (card list, view, and update transactions) and batch programs (daily transaction validation). Each card is linked to exactly one account via `CARD-ACCT-ID`, forming a 1:1 relationship with the account record defined in `CVACT01Y.cpy`.

**Source COBOL:**

```cobol
01 CARD-RECORD.
   05 CARD-NUM             PIC X(16).
   05 CARD-ACCT-ID         PIC 9(11).
   05 CARD-CVV-CD          PIC 9(03).
   05 CARD-EMBOSSED-NAME   PIC X(50).
   05 CARD-EXPIRAION-DATE  PIC X(10).
   05 CARD-ACTIVE-STATUS   PIC X(01).
   05 FILLER               PIC X(59).
```

## Field Definitions

| Field Name | PIC Clause | Length (bytes) | Type | Description | Nullable | Target Column |
|------------|-----------|----------------|------|-------------|----------|---------------|
| CARD-NUM | X(16) | 16 | Alphanumeric | Primary card number (PAN). Left-padded with zeros for numbers shorter than 16 digits. | No | `card_number` |
| CARD-ACCT-ID | 9(11) | 11 | Numeric (unsigned) | Account identifier linking this card to its parent account in `CVACT01Y`. | No | `account_id` |
| CARD-CVV-CD | 9(03) | 3 | Numeric (unsigned) | Card verification value (3-digit security code). | No | `cvv_code` |
| CARD-EMBOSSED-NAME | X(50) | 50 | Alphanumeric | Cardholder name as embossed on the physical card. Right-padded with spaces. | No | `embossed_name` |
| CARD-EXPIRAION-DATE | X(10) | 10 | Alphanumeric | Card expiration date stored as a string (format: `YYYY-MM-DD`). Note: field name contains original typo from source. | No | `expiration_date` |
| CARD-ACTIVE-STATUS | X(01) | 1 | Alphanumeric | Card active/inactive indicator. `Y` = active, `N` = inactive. | No | `is_active` |
| FILLER | X(59) | 59 | Filler | Unused padding to reach 150-byte record length. | N/A | N/A |

### Field Notes

- **CARD-NUM**: Stored as alphanumeric (`PIC X`) rather than numeric, allowing leading zeros and non-numeric characters. The PAN is sensitive PII under GDPR and PCI-DSS — the target column must use encryption at rest and column-level access controls.
- **CARD-CVV-CD**: Stored as display numeric (`PIC 9`), not packed. PCI-DSS prohibits storing CVV after authorization — migration must evaluate whether this field should be carried over to the target system.
- **CARD-EXPIRAION-DATE**: The field name contains a typo (`EXPIRAION` instead of `EXPIRATION`) present in the original COBOL source. The target column corrects this to `expiration_date`. Stored as `X(10)` string in `YYYY-MM-DD` format — target maps to SQL `date` type.
- **CARD-ACTIVE-STATUS**: Simple flag field. Target maps `Y`/`N` to SQL `bit` type (`1`/`0`).

## EBCDIC Encoding Notes

| Consideration | Detail |
|---------------|--------|
| Source encoding | EBCDIC (IBM Code Page 037 — US/Canada Latin-1) |
| Target encoding | UTF-8 |
| Special characters | `CARD-EMBOSSED-NAME` may contain Swedish characters (å, ä, ö, Å, Ä, Ö) for Nordic cardholders. Verify code page mapping handles these correctly. |
| Packed decimal fields | None — all numeric fields use display format (`PIC 9`), not `COMP-3`. |
| Binary fields | None — no `COMP` or `COMP-4` fields in this copybook. |
| Sign handling | No signed fields in this copybook. `CARD-ACCT-ID` and `CARD-CVV-CD` are unsigned display numeric. |

### Encoding Validation

- Extract a sample of `CARD-EMBOSSED-NAME` values containing Nordic characters and verify round-trip conversion: EBCDIC → UTF-8 → display.
- Confirm that trailing spaces in `CARD-EMBOSSED-NAME` are preserved or trimmed consistently.

## Referential Integrity

| Relationship | Source Field | Target Table | Target Column | Constraint Type |
|-------------|-------------|-------------|---------------|-----------------|
| Card belongs to account | CARD-ACCT-ID | dbo.Account | account_id | FK (enforced) |
| Card linked to customer (via cross-reference) | CARD-NUM | dbo.CustomerAccountCardXref | card_number | FK (enforced) |

### Integrity Notes

- In the mainframe system, the 1:1:1 relationship between Account, Card, and Customer is enforced by COBOL program logic in the CICS transactions, not by database constraints.
- The cross-reference file (`CVACT03Y.cpy` / `CARDXREF`) links cards to accounts and customers. Orphan cards (cards without a matching cross-reference entry) should be flagged during migration validation.
- The target Azure SQL schema should enforce the Account → Card FK constraint at the database level, replacing the programmatic enforcement.

## Sample Data

```
Record 1:
  CARD-NUM:             "4532015112830366"
  CARD-ACCT-ID:         00000012345
  CARD-CVV-CD:          789
  CARD-EMBOSSED-NAME:   "ERIK JOHANSSON                                    "
  CARD-EXPIRAION-DATE:  "2027-08-31"
  CARD-ACTIVE-STATUS:   "Y"

Record 2:
  CARD-NUM:             "4716826378940052"
  CARD-ACCT-ID:         00000067890
  CARD-CVV-CD:          321
  CARD-EMBOSSED-NAME:   "ANNA BJÖRK LINDSTRÖM                              "
  CARD-EXPIRAION-DATE:  "2025-03-31"
  CARD-ACTIVE-STATUS:   "N"
```

> **Note**: Sample data above is entirely synthetic. No real card numbers or PII are used. The card numbers shown are generated values that pass Luhn check formatting but do not correspond to real cards.

### Data Characteristics

- Estimated record count: ~50,000 active cards + historical
- Growth rate: ~500 new cards/month
- Key distribution: `CARD-NUM` is unique, sequentially issued by card processor. `CARD-ACCT-ID` distribution mirrors account creation patterns.

## Migration Notes

### Schema Mapping

- **Source**: VSAM file `AWS.M2.CARDDEMO.CARDDATA.PS` (FB, LRECL=150)
- **Target**: `dbo.Card`
- **Migration approach**: Full extract followed by incremental sync during parallel-run period

### Target Table DDL

```sql
CREATE TABLE dbo.Card (
    card_number     NVARCHAR(16)    NOT NULL,
    account_id      BIGINT          NOT NULL,
    cvv_code        SMALLINT        NOT NULL,
    embossed_name   NVARCHAR(50)    NOT NULL,
    expiration_date DATE            NOT NULL,
    is_active       BIT             NOT NULL,
    CONSTRAINT PK_Card PRIMARY KEY (card_number),
    CONSTRAINT FK_Card_Account FOREIGN KEY (account_id)
        REFERENCES dbo.Account (account_id)
);
```

### Data Quality

- Verify no orphan `CARD-ACCT-ID` values (cards pointing to non-existent accounts).
- Validate `CARD-EXPIRAION-DATE` is a parseable date and not a placeholder (e.g., `0000-00-00`).
- Check for duplicate `CARD-NUM` values (should be unique but no mainframe constraint enforces this).
- Confirm `CARD-ACTIVE-STATUS` contains only `Y` or `N` — flag any other values.

### Validation Strategy

- Record count reconciliation: VSAM file record count vs. `SELECT COUNT(*) FROM dbo.Card`.
- Field-level spot checks: Sample 1,000 random records and compare all field values.
- Referential integrity: `SELECT card_number FROM dbo.Card WHERE account_id NOT IN (SELECT account_id FROM dbo.Account)` should return zero rows.
- Business rule validation: All cards with `expiration_date < GETDATE()` should have `is_active = 0` (or be flagged for review).

### Performance Considerations

- `card_number` is the primary lookup key — clustered index on PK is sufficient.
- Add non-clustered index on `account_id` for join performance with Account table.
- No partitioning needed at current volumes (~50K records).
- During parallel-run, use change tracking on `is_active` and `expiration_date` for incremental sync.

### PCI-DSS Considerations

- `card_number` (PAN) must be encrypted at rest using Azure SQL Always Encrypted or Transparent Data Encryption (TDE).
- `cvv_code` storage must be reviewed against PCI-DSS requirement 3.2 — CVV must not be stored after authorization. If this field is only used for card issuance records, document the justification.
- Column-level access controls should restrict `card_number` and `cvv_code` to authorized roles only.
