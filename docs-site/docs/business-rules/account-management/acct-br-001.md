---
id: "acct-br-001"
title: "Account record data structure and field definitions"
domain: "account-management"
cobol_source: "CVACT01Y.cpy:1-30 (referenced, not in repository)"
requirement_id: "ACCT-BR-001"
regulations:
  - "GDPR Art. 5(1)(c)"
  - "GDPR Art. 5(1)(d)"
  - "FSA FFFS 2014:5 Ch. 3"
  - "PSD2 Art. 64"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# ACCT-BR-001: Account record data structure and field definitions

## Summary

The CVACT01Y copybook defines the primary account master record layout used across the core banking system. Each account record is a fixed-length 300-byte VSAM KSDS record keyed by an 11-digit account identifier. The record contains financial fields (current balance, credit limit, cash credit limit, cycle credits, cycle debits), status fields (active status, expiration date), and customer linkage fields. This structure is the single source of truth for account data and is referenced by card management programs (COCRDLIC, COCRDSLC, COCRDUPC), transaction batch programs (CBTRN02C for balance updates), and billing processes. The copybook itself is not available in the current repository but its structure has been reconstructed from field references across multiple programs and the .NET domain model.

## Business Logic

### Data Structure

```
ACCOUNT-RECORD (300 bytes total, reconstructed from program references):
    ACCT-ID                   PIC 9(11)    -- Primary key, 11-digit account number
    ACCT-ACTIVE-STATUS        PIC X(01)    -- 'Y' = active, 'N' = inactive
    ACCT-CURR-BAL             PIC S9(10)V99 -- Current running balance (signed, 2 decimal)
    ACCT-CREDIT-LIMIT         PIC S9(10)V99 -- Authorized credit ceiling
    ACCT-CASH-CREDIT-LIMIT    PIC S9(10)V99 -- Cash advance credit limit
    ACCT-CURR-CYC-CREDIT      PIC S9(10)V99 -- Current billing cycle credits
    ACCT-CURR-CYC-DEBIT       PIC S9(10)V99 -- Current billing cycle debits
    ACCT-EXPIRAION-DATE       PIC X(10)    -- Account expiry (YYYY-MM-DD format, note COBOL typo)
    ACCT-GROUP-ID             PIC X(10)    -- Disclosure/interest group assignment
    FILLER                    PIC X(...)   -- Reserved (remainder of 300 bytes)
```

### Field Specifications

| Field | Type | Length | Format | Constraints |
|-------|------|--------|--------|-------------|
| ACCT-ID | Numeric | 11 | Zoned decimal | Must be 11 digits, not all zeros, primary key |
| ACCT-ACTIVE-STATUS | Alphanumeric | 1 | 'Y' or 'N' | Binary active/inactive flag |
| ACCT-CURR-BAL | Signed numeric | 12.2 | S9(10)V99 | Running balance, max ±9,999,999,999.99 |
| ACCT-CREDIT-LIMIT | Signed numeric | 12.2 | S9(10)V99 | Credit ceiling, used for overlimit checks |
| ACCT-CASH-CREDIT-LIMIT | Signed numeric | 12.2 | S9(10)V99 | Cash advance limit |
| ACCT-CURR-CYC-CREDIT | Signed numeric | 12.2 | S9(10)V99 | Cycle charges (positive accumulation) |
| ACCT-CURR-CYC-DEBIT | Signed numeric | 12.2 | S9(10)V99 | Cycle payments (negative accumulation) |
| ACCT-EXPIRAION-DATE | Alphanumeric | 10 | YYYY-MM-DD | String comparison for date checks |
| ACCT-GROUP-ID | Alphanumeric | 10 | Free-text | Links to disclosure group for interest rates |

### VSAM File Organization

| File | Key | Key Length | Access Method | Purpose |
|------|-----|-----------|---------------|---------|
| ACCTFILE | ACCT-ID | 11 bytes | Primary key (KSDS) | Direct account lookup by account ID |
| CARDAIX | CARD-ACCT-ID | 11 bytes | Alternate index on CARDDAT | Card lookup by account ID |

## Source COBOL Reference

**Copybook:** `CVACT01Y.cpy` (not available in repository — referenced from multiple programs)
**Reconstruction sources:**
- `COCRDSLC.cbl:230-231` — commented COPY CVACT01Y reference
- `COCRDUPC.cbl:349-350` — commented COPY CVACT01Y reference
- `CBTRN02C.cbl:~100` — COPY CVACT01Y for ACCTFILE I-O operations
- `src/NordKredit.Domain/Transactions/Account.cs` — .NET domain model with field mappings

```cobol
      *ACCOUNT RECORD LAYOUT
      *COPY CVACT01Y.
```
*(Lines 230-231, COCRDSLC.cbl — copybook reference commented out in card detail program)*

```cobol
      *ACCOUNT RECORD LAYOUT
      *COPY CVACT01Y.
```
*(Lines 349-350, COCRDUPC.cbl — copybook reference commented out in card update program)*

**Field references in CBTRN02C.cbl (transaction posting):**

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```
*(Lines 547-552 — demonstrates ACCT-CURR-BAL, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT fields)*

```cobol
000403               COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                   - ACCT-CURR-CYC-DEBIT
000405                                   + DALYTRAN-AMT
000407               IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
```
*(Lines 403-407 — demonstrates ACCT-CREDIT-LIMIT field)*

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
```
*(Line 414 — demonstrates ACCT-EXPIRAION-DATE field, note preserved COBOL typo)*

## Acceptance Criteria

### Scenario 1: Account record round-trip integrity

```gherkin
GIVEN an account record is written with:
  | Field              | Value              |
  | Account ID         | 12345678901        |
  | Active Status      | Y                  |
  | Current Balance    | 5000.00            |
  | Credit Limit       | 10000.00           |
  | Cash Credit Limit  | 2000.00            |
  | Cycle Credit       | 5000.00            |
  | Cycle Debit        | 0.00               |
  | Expiration Date    | 2028-12-31         |
WHEN the record is read back from the ACCTFILE
THEN all field values match exactly as written
  AND the record length is 300 bytes
```

### Scenario 2: Financial precision preservation

```gherkin
GIVEN an account record with ACCT-CURR-BAL = 9999999999.99
WHEN the balance is read and stored in the migrated system
THEN the value is exactly 9999999999.99 (no floating-point loss)
  AND the migrated type is decimal(12,2)
```

### Scenario 3: Account ID as primary key

```gherkin
GIVEN an account with ID "00000000042"
WHEN a lookup is performed by account ID
THEN the account record is returned via KSDS primary key access
  AND leading zeros are preserved in the 11-digit ID
```

### Scenario 4: Active status values

```gherkin
GIVEN an account record exists in the system
WHEN the ACCT-ACTIVE-STATUS field is read
THEN its value is either 'Y' (active) or 'N' (inactive)
  AND no other values are valid for this field
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5(1)(c) | Data minimization — personal data must be adequate, relevant, and limited to what is necessary | The account record contains only financial and status fields necessary for account operations. Customer PII is stored separately in customer records, not in the account master. |
| GDPR | Art. 5(1)(d) | Accuracy — personal data must be accurate and kept up to date | Financial balance fields are updated atomically via REWRITE in batch posting (CBTRN02C). The balance tracking mechanism maintains accuracy through cycle-based credit/debit classification. |
| FSA FFFS 2014:5 | Ch. 3 | Accurate accounting records | The account record maintains running balance, credit/debit cycle tracking, and credit limits — the minimum fields required for accurate financial record-keeping. |
| PSD2 | Art. 64 | Payment transaction data integrity | Balance fields updated during transaction posting reference this record structure; data integrity is maintained through VSAM KSDS keyed access. |

## Edge Cases

1. **EBCDIC to Unicode conversion**: COBOL PIC 9 fields (ACCT-ID) are stored in EBCDIC zoned decimal format. PIC S9(10)V99 fields use COBOL packed decimal. During migration, these must be converted to their numeric equivalents. The implied decimal point in V99 must be preserved exactly — any rounding or truncation would cause balance discrepancies.

2. **Copybook not in repository**: The CVACT01Y.cpy copybook is referenced but commented out in card programs. The full record layout must be obtained from the mainframe team. The 300-byte record length is derived from the .NET domain model comment. Additional fields beyond those reconstructed here may exist in the filler area.

3. **ACCT-EXPIRAION-DATE typo**: The COBOL field name contains a typo ("EXPIRAION" instead of "EXPIRATION"). This typo is preserved across all programs that reference the field. The migrated system should use the correct spelling but must document the mapping for traceability.

4. **Balance overflow risk**: With `S9(10)V99`, the maximum balance is ±9,999,999,999.99. COBOL truncates silently on overflow. The migrated system must use `decimal(12,2)` and add explicit overflow detection.

5. **Filler contents**: The 300-byte record likely contains additional fields not yet identified (customer reference, account opening date, last statement date, etc.). The filler area may contain data from older program versions. A full dump of the CVACT01Y copybook from the mainframe is needed to identify all fields.

6. **Signed numeric fields**: All financial fields use signed (`S`) PIC notation. Negative balances and negative cycle debits are valid and expected. The migrated system must support negative values in all balance columns.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The CVACT01Y.cpy copybook must be obtained from the mainframe team to validate the reconstructed record layout. Key questions: (1) What additional fields exist in the 300-byte record beyond those referenced in available programs? (2) Is there an account opening date field? (3) Is there a customer ID field in the account record or only via the cross-reference? (4) What are the exact field positions and offsets? (5) Are there any account type or product code fields? (6) Is the 300-byte size correct or could it be different?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
