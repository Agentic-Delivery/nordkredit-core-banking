---
id: "bill-br-001"
title: "Interest rate assignment by disclosure group"
domain: "billing"
cobol_source: "CVTRA02Y.cpy:1-12"
requirement_id: "BILL-BR-001"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "PSD2 Art. 45"
  - "EU Consumer Credit Directive 2008/48/EC"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# BILL-BR-001: Interest rate assignment by disclosure group

## Summary

The billing system assigns interest rates to accounts through a disclosure group mechanism defined in the `CVTRA02Y.cpy` copybook. Each disclosure group record links an account group identifier to a specific transaction type and category, with an associated interest rate. This structure enables tiered interest rate assignment — different transaction types (e.g., purchases vs. cash advances) within the same account group can carry different interest rates. The disclosure group is the foundation of the billing system's interest computation, determining which rate applies to each category of balance. Extracted from `CVTRA02Y.cpy`.

## Business Logic

### Pseudocode

```
DISCLOSURE GROUP STRUCTURE:
    DIS-ACCT-GROUP-ID   = Account group identifier (10 chars)
    DIS-TRAN-TYPE-CD    = Transaction type code (2 chars)
    DIS-TRAN-CAT-CD     = Transaction category code (4 digits)
    DIS-INT-RATE        = Interest rate for this combination (signed, 4 integer + 2 decimal)

INTEREST RATE LOOKUP:
    INPUT: Account group ID, Transaction type, Transaction category

    Build composite key:
        KEY = DIS-ACCT-GROUP-ID + DIS-TRAN-TYPE-CD + DIS-TRAN-CAT-CD

    READ disclosure group file by KEY

    IF record found:
        RETURN DIS-INT-RATE for this combination
    ELSE:
        No rate defined for this combination
        (behavior depends on calling program — may use default rate or reject)
    END-IF

RATE APPLICATION:
    For each transaction category balance (from CVTRA01Y.cpy):
        Look up disclosure group by account's group ID + type + category
        Apply DIS-INT-RATE to TRAN-CAT-BAL
        Interest = TRAN-CAT-BAL * (DIS-INT-RATE / 100) / 12
            (monthly interest from annual rate — exact formula to be confirmed by domain expert)
```

### Decision Table

| Account Group | Transaction Type | Category | Rate Field | Outcome |
|---|---|---|---|---|
| Valid group ID | Valid type code | Valid category | DIS-INT-RATE populated | Rate applied to category balance |
| Valid group ID | Valid type code | Valid category | DIS-INT-RATE = 0 | No interest charged for this combination |
| Valid group ID | Type/category not in file | N/A | No record found | Default rate or error handling (domain expert to confirm) |
| Unknown group ID | Any | Any | No record found | Account not assigned to a disclosure group — requires investigation |

### Data Structure

| Field | COBOL Name | PIC | Length | Description |
|---|---|---|---|---|
| Account Group ID | DIS-ACCT-GROUP-ID | X(10) | 10 | Identifies the disclosure/rate group the account belongs to |
| Transaction Type | DIS-TRAN-TYPE-CD | X(02) | 2 | Transaction type code (e.g., purchase, cash advance) |
| Category Code | DIS-TRAN-CAT-CD | 9(04) | 4 | Transaction category within the type |
| Interest Rate | DIS-INT-RATE | S9(04)V99 | 6 | Signed interest rate with 2 decimal places |
| Filler | FILLER | X(28) | 28 | Reserved space |
| **Total** | | | **50** | |

### Financial Precision

| Field | COBOL PIC | Precision | Notes |
|---|---|---|---|
| DIS-INT-RATE | S9(04)V99 | Signed, 4 integer, 2 decimal | Supports rates from -9999.99 to +9999.99; typical values 0.00 to 29.99 (APR) |
| TRAN-CAT-BAL | S9(09)V99 | Signed, 9 integer, 2 decimal | Category balance the rate is applied to (from CVTRA01Y.cpy) |

**Critical**: Interest rate calculations in COBOL use fixed-point arithmetic. The rate field `S9(04)V99` provides 2 decimal places for the rate itself. The migrated system must use `decimal(6,2)` for rates and `decimal(11,2)` for balances. All interest calculations must use `decimal` types — floating-point types (float/double) are NOT acceptable for financial calculations.

## Source COBOL Reference

**Copybook:** `CVTRA02Y.cpy`
**Lines:** 1-12

```cobol
      *    Data-structure for DISClosure GRouP record (RECLN = 50)
       01  DIS-GROUP-RECORD.
           05  DIS-ACCT-GROUP-ID                      PIC X(10).
           05  DIS-TRAN-TYPE-CD                       PIC X(02).
           05  DIS-TRAN-CAT-CD                        PIC 9(04).
           05  DIS-INT-RATE                           PIC S9(04)V99.
           05  FILLER                                 PIC X(28).
```

### Related Structures

The disclosure group works in conjunction with:

- **CVTRA01Y.cpy** (Transaction Category Balance): `TRAN-CAT-BAL` holds the running balance per account/type/category that interest rates are applied to
- **CVTRA03Y.cpy** (Transaction Type): Maps type codes to descriptions
- **CVTRA04Y.cpy** (Transaction Category): Maps type+category codes to descriptions
- **CBTRN02C.cbl**: Posts transactions and updates category balances that disclosure group rates are applied to

## Acceptance Criteria

### Scenario 1: Interest rate lookup for a known disclosure group

```gherkin
GIVEN an account belongs to disclosure group "PREMIUM01"
  AND a disclosure record exists for group "PREMIUM01", type "01", category "0001"
  AND the DIS-INT-RATE is 18.99
WHEN the interest rate is looked up for this combination
THEN the rate 18.99 is returned
  AND the rate is stored with exactly 2 decimal places
```

### Scenario 2: Different rates for different transaction types within same group

```gherkin
GIVEN an account belongs to disclosure group "STANDARD1"
  AND a disclosure record for type "01" (purchases) has rate 19.99
  AND a disclosure record for type "02" (cash advances) has rate 24.99
WHEN interest is calculated for this account
THEN purchase balances are charged at 19.99
  AND cash advance balances are charged at 24.99
```

### Scenario 3: Zero interest rate

```gherkin
GIVEN a disclosure record exists with DIS-INT-RATE = 0.00
WHEN interest is calculated for transactions in this group/type/category
THEN no interest is charged
  AND the zero rate is preserved (not treated as missing)
```

### Scenario 4: No disclosure record found

```gherkin
GIVEN an account belongs to disclosure group "PROMO2024"
  AND no disclosure record exists for type "03", category "0005"
WHEN the interest rate is looked up
THEN the system handles the missing rate according to business rules
  AND an exception is logged for review
```

### Scenario 5: Financial precision in rate storage

```gherkin
GIVEN a disclosure rate is defined as 18.99 (APR)
WHEN the rate is stored in the system
THEN it is stored as decimal(6,2) with exactly 2 decimal places
  AND no floating-point rounding occurs
  AND the rate can be negative (for promotional credit rates)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 6 | Interest rate disclosure and transparency for credit products | The disclosure group structure maintains traceable rate assignments per account group and transaction type, enabling transparent rate disclosure |
| PSD2 | Art. 45 | Information on charges and interest rates must be provided to payment service users | The structured rate assignment enables the bank to report applicable rates per transaction type to customers |
| EU Consumer Credit Directive | 2008/48/EC Art. 10 | Credit agreements must specify the borrowing rate and conditions | The disclosure group provides the data structure for maintaining and applying agreed-upon interest rates |

## Edge Cases

1. **Negative interest rates**: The `S9(04)V99` field is signed, allowing negative rates. In a negative interest rate environment (as seen in Sweden 2015-2019), this field could theoretically hold negative rates for promotional or regulatory purposes. The migrated system must support signed decimal rates.

2. **Rate precision limitations**: With `S9(04)V99`, the maximum representable rate is 9999.99 and the minimum is -9999.99. Typical credit card APRs range from 0.00 to 29.99. The field is oversized for typical rates but the precision is limited to 2 decimal places. If the business requires higher precision (e.g., 18.995%), the migrated system must address this limitation.

3. **Account group assignment**: The COBOL source does not show how accounts are assigned to disclosure groups. This mapping likely exists in the account master record (a field not present in the available copybooks). The migration must identify and preserve this mapping.

4. **Rate changes over time**: The current structure has no effective date or version history. A rate change overwrites the previous rate. The migrated system should consider adding temporal rate tracking for regulatory audit requirements.

5. **Composite key ordering**: The VSAM file key is the concatenation of group ID (10) + type (2) + category (4) = 16 bytes. The migrated database should use a composite primary key or unique constraint on these three columns.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Confirm the interest calculation formula — is the rate an APR divided by 12 for monthly billing, or applied differently? (2) How are accounts assigned to disclosure groups — is there a field in the account master record? (3) What happens when no disclosure record exists for a given combination — is there a default rate? (4) Are rate changes applied prospectively only, or can they be retroactive? (5) Is the DIS-INT-RATE actively used in current production billing, or is it a legacy structure?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
