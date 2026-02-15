---
id: "card-br-010"
title: "Card cross-reference structure linking cards to customers and accounts"
domain: "card-management"
cobol_source: "CVACT03Y.cpy:1-11"
requirement_id: "CARD-BR-010"
regulations:
  - "GDPR Art. 5(1)(c)"
  - "AML/KYC"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# CARD-BR-010: Card cross-reference structure linking cards to customers and accounts

## Summary

The CVACT03Y copybook defines the card cross-reference record, a 50-byte VSAM structure that establishes the three-way relationship between cards, customers, and accounts. Each cross-reference record maps a 16-digit card number to a 9-digit customer ID and an 11-digit account ID. This cross-reference enables the system to determine which customer owns a card and which account the card is linked to, supporting multi-card-per-account and multi-account-per-customer scenarios. The cross-reference file is referenced (but commented out) in the detail view (COCRDSLC) and update (COCRDUPC) programs, suggesting it was used in earlier versions or is planned for future integration.

## Business Logic

### Data Structure

```
CARD-XREF-RECORD (50 bytes total):
    XREF-CARD-NUM     PIC X(16)   -- Card number (links to CARD-RECORD.CARD-NUM)
    XREF-CUST-ID      PIC 9(09)   -- Customer ID (links to customer master)
    XREF-ACCT-ID      PIC 9(11)   -- Account ID (links to account master)
    FILLER             PIC X(14)   -- Reserved for future use
```

### Entity Relationships

```
CUSTOMER (1) ----< (N) CARD-XREF (N) >---- (1) ACCOUNT
                          |
                          | (1)
                          v
                        CARD (1)
```

| Relationship | Cardinality | Description |
|-------------|-------------|-------------|
| Customer → Cards | One-to-Many | A customer can have multiple cards |
| Account → Cards | One-to-Many | An account can have multiple cards linked |
| Card → Cross-ref | One-to-One | Each card has exactly one cross-reference entry |
| Customer → Accounts | Many-to-Many (via cards) | A customer may have cards on multiple accounts |

### Field Specifications

| Field | Type | Length | Format | Constraints |
|-------|------|--------|--------|-------------|
| XREF-CARD-NUM | Alphanumeric | 16 | Numeric digits | Foreign key to CARD-RECORD, primary key of xref |
| XREF-CUST-ID | Numeric | 9 | Zoned decimal | Foreign key to customer master |
| XREF-ACCT-ID | Numeric | 11 | Zoned decimal | Foreign key to account master |
| FILLER | Alphanumeric | 14 | N/A | Reserved for future use |

## Source COBOL Reference

**Copybook:** `CVACT03Y.cpy`
**Lines:** 1-11

```cobol
      *****************************************************************
      *    Data-structure for card xref (RECLN 50)
      *****************************************************************
       01 CARD-XREF-RECORD.
           05  XREF-CARD-NUM                     PIC X(16).
           05  XREF-CUST-ID                      PIC 9(09).
           05  XREF-ACCT-ID                      PIC 9(11).
           05  FILLER                            PIC X(14).
```

**References in programs:**

- `COCRDSLC.cbl` (line 237): `*COPY CVACT03Y.` — commented out, cross-reference not currently used in detail view
- `COCRDUPC.cbl` (line 356): `*COPY CVACT03Y.` — commented out, cross-reference not currently used in update

**Note:** The cross-reference copybook is commented out in both programs that reference it. The CARD-RECORD.CARD-ACCT-ID field (in CVACT02Y) provides a direct card-to-account link, making the cross-reference redundant for that relationship. The cross-reference adds the customer dimension (XREF-CUST-ID) which is not available in the card record itself.

## Acceptance Criteria

### Scenario 1: Card-to-customer lookup

```gherkin
GIVEN a card "4000123456789012" exists in the cross-reference file
  AND it is linked to customer "123456789" and account "12345678901"
WHEN the cross-reference record is read by card number
THEN the customer ID "123456789" is returned
  AND the account ID "12345678901" is returned
```

### Scenario 2: Multiple cards for same customer

```gherkin
GIVEN customer "123456789" has two cards:
  | Card Number        | Account ID    |
  | 4000123456789012   | 12345678901   |
  | 4000123456789028   | 12345678901   |
WHEN the cross-reference is queried for customer "123456789"
THEN both card records are returned
```

### Scenario 3: Cards across multiple accounts for one customer

```gherkin
GIVEN customer "123456789" has cards on different accounts:
  | Card Number        | Account ID    |
  | 4000123456789012   | 12345678901   |
  | 4000987654321098   | 98765432101   |
WHEN the cross-reference is queried for customer "123456789"
THEN cards from both accounts are returned
```

### Scenario 4: Cross-reference consistency with card record

```gherkin
GIVEN a card "4000123456789012" with account "12345678901" exists in CARDDAT
  AND a cross-reference for the same card exists in the xref file
WHEN the cross-reference account ID is compared to the card record account ID
THEN both XREF-ACCT-ID and CARD-ACCT-ID contain "12345678901"
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5(1)(c) | Data minimization — personal data processing must be adequate, relevant, and limited | The cross-reference contains only the minimum identifiers needed to establish relationships: card number, customer ID, and account ID — no additional personal data is duplicated |
| AML/KYC | General | Know Your Customer — financial institutions must maintain records linking payment instruments to their owners | The cross-reference provides the essential card-to-customer linkage required for AML/KYC compliance, enabling the institution to identify the beneficial owner of any card |

## Edge Cases

1. **Orphaned cross-reference records**: If a card is deleted from CARDDAT without removing its cross-reference entry, the xref becomes orphaned. The migrated system should enforce referential integrity through foreign key constraints or implement a cleanup process.

2. **Cross-reference not actively used**: The COPY statements for CVACT03Y are commented out in both COCRDSLC and COCRDUPC. The card programs currently use CARD-ACCT-ID from the card record directly rather than the cross-reference. The migrated system should determine whether the cross-reference file is maintained by other programs (e.g., card issuance batch) and whether the customer linkage is needed.

3. **Customer ID format (9 digits)**: The customer ID is PIC 9(09), which is shorter than the account ID (PIC 9(11)). The migrated system should verify that the 9-digit customer ID format aligns with the NordKredit customer numbering scheme.

4. **Migration of commented-out dependencies**: Even though the cross-reference is commented out in card management programs, it may be actively used by other programs outside the scope of this extraction (e.g., customer management, AML screening, statement generation). The migrated system must survey all programs that use CVACT03Y before deciding on the migration approach.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Is the cross-reference file actively maintained by card issuance or other batch processes? (2) Is the customer-to-card linkage used by AML screening or regulatory reporting processes? (3) Should the migrated system normalize this into a proper relational junction table or maintain it as a denormalized lookup? (4) Does the 14-byte filler contain meaningful data in production?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
