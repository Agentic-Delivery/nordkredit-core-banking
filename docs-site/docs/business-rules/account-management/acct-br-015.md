---
id: "acct-br-015"
title: "Account interest calculation and disclosure grouping"
domain: "account-management"
cobol_source: "CBTRN02C.cbl:91-97, 467-543 (TCATBAL structure); dedicated interest program not yet in repository"
requirement_id: "ACCT-BR-015"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "EU Consumer Credit Directive 2008/48/EC Art. 6"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
  - "PSD2 Art. 64"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# ACCT-BR-015: Account interest calculation and disclosure grouping

## Summary

Interest calculation on account balances is a critical financial function that uses the transaction category balance records (TCATBAL, documented in ACCT-BR-009) to compute interest charges or accruals by category. Different transaction categories (purchases, cash advances, balance transfers, promotional balances) typically carry different interest rates, and the TCATBAL file provides the category-level balance breakdown needed for these differentiated calculations. The dedicated interest calculation batch program is not yet available in the repository.

This rule is inferred from the TCATBAL file structure in CBTRN02C (ACCT-BR-009), the disclosure group reference in the TRANCATG-FILE used by CBTRN03C, the billing cycle tracking fields in the account master (ACCT-BR-004), and EU Consumer Credit Directive requirements for APR disclosure and periodic statement itemization.

Swedish and EU regulations require that interest be calculated using a specified method (typically average daily balance per category), disclosed to the customer with the applicable rate per category, and reported as the Annual Percentage Rate (APR) inclusive of all costs.

## Business Logic

### Pseudocode

```
PERFORM INTEREST-CALCULATION-BATCH:
    OPEN ACCOUNT-FILE (I-O)
    OPEN TCATBAL-FILE (INPUT, browse by account)
    OPEN INTEREST-RATE-TABLE (INPUT)

    PERFORM UNTIL END-OF-ACCOUNT-FILE
        READ next ACCOUNT-RECORD
        IF ACCT-ACTIVE-STATUS = 'Y'
            MOVE 0 TO WS-TOTAL-INTEREST
            PERFORM CALCULATE-INTEREST-BY-CATEGORY
            ADD WS-TOTAL-INTEREST TO ACCT-CURR-BAL
            REWRITE ACCOUNT-RECORD
        END-IF
    END-PERFORM

PERFORM CALCULATE-INTEREST-BY-CATEGORY:
    -- Browse TCATBAL records for this account
    START TCATBAL-FILE KEY >= account-prefix
    PERFORM UNTIL TCATBAL account changes or EOF
        READ NEXT TCATBAL-RECORD

        -- Step 1: Look up interest rate for this category
        MOVE TRANCAT-TYPE-CD TO rate-table-key
        MOVE TRANCAT-CD      TO rate-table-subkey
        READ INTEREST-RATE-TABLE
        SET daily-rate = annual-rate / 365

        -- Step 2: Calculate interest for category
        COMPUTE category-interest =
            TRAN-CAT-BAL * daily-rate * days-in-period

        -- Step 3: Accumulate
        ADD category-interest TO WS-TOTAL-INTEREST

        -- Step 4: Record for disclosure
        WRITE interest-detail-record:
            account-id, type-cd, cat-cd, balance, rate, interest-amount
    END-PERFORM

PERFORM DISCLOSURE-STATEMENT:
    -- Generate disclosure grouping for customer statement
    FOR EACH category with interest
        WRITE disclosure line:
            Category description | Balance | Rate | Interest | Period
    END-FOR
    WRITE total interest line
    WRITE APR calculation
```

### Interest Calculation Model

| Component | Calculation | Source |
|---|---|---|
| Category balance | TRAN-CAT-BAL from TCATBAL file | ACCT-BR-009 |
| Interest rate | Annual rate per TYPE-CD + CAT-CD | Rate table (not in repository) |
| Daily rate | Annual rate / 365 | Derived |
| Period | Days in billing cycle | Date parameter or cycle dates |
| Category interest | Balance x Daily rate x Days | Per-category calculation |
| Total interest | Sum of all category interests | Accumulated |
| APR | Effective annual rate including all costs | EU CCD formula |

### Disclosure Grouping Structure

| Group | Transaction Type | Typical Rate | Example |
|---|---|---|---|
| Purchases | Standard purchase transactions | Base rate | 18.9% APR |
| Cash advances | ATM withdrawals, cash equivalents | Higher rate | 24.9% APR |
| Balance transfers | Transferred balances from other issuers | Promotional rate | 0% for 12 months |
| Fees | Annual fees, late fees, overlimit fees | N/A (flat amount) | N/A |

### TCATBAL-to-Interest Mapping

| TCATBAL Field | Interest Calculation Use |
|---|---|
| TRANCAT-ACCT-ID (9(11)) | Account identifier — group all categories for this account |
| TRANCAT-TYPE-CD (X(02)) | Transaction type — maps to interest rate tier |
| TRANCAT-CD (9(04)) | Transaction category — maps to specific rate sub-tier |
| TRAN-CAT-BAL (S9(09)V99) | Balance subject to interest for this category |

## Source COBOL Reference

**Program:** Dedicated interest calculation batch program — not yet in repository.

The infrastructure supporting interest calculation exists in available programs:

**TCATBAL file and record (CBTRN02C.cbl lines 57-61, 91-97):**

```cobol
000057            SELECT TCATBAL-FILE ASSIGN TO TCATBALF
000058                   ORGANIZATION IS INDEXED
000059                   ACCESS MODE  IS RANDOM
000060                   RECORD KEY   IS FD-TRAN-CAT-KEY
000061                   FILE STATUS  IS TCATBALF-STATUS.

000091        FD  TCATBAL-FILE.
000092        01  FD-TRAN-CAT-BAL-RECORD.
000093            05 FD-TRAN-CAT-KEY.
000094               10 FD-TRANCAT-ACCT-ID             PIC 9(11).
000095               10 FD-TRANCAT-TYPE-CD             PIC X(02).
000096               10 FD-TRANCAT-CD                  PIC 9(04).
000097            05 FD-FD-TRAN-CAT-DATA               PIC X(33).
```
*(CBTRN02C.cbl lines 57-97 — the TCATBAL file provides the category-level balance breakdown needed for interest calculation. The 33-byte data area contains the category balance and potentially other fields.)*

**Category balance update logic (CBTRN02C.cbl lines 503-510):**

```cobol
000503        2700-A-CREATE-TCATBAL-REC.
000504            INITIALIZE TRAN-CAT-BAL-RECORD
000505            MOVE XREF-ACCT-ID TO TRANCAT-ACCT-ID
000506            MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD
000507            MOVE DALYTRAN-CAT-CD TO TRANCAT-CD
000508            ADD DALYTRAN-AMT TO TRAN-CAT-BAL
```
*(CBTRN02C.cbl lines 503-508 — each transaction contributes to its category balance, which becomes the basis for interest calculation.)*

**Transaction category reference file (CBTRN03C.cbl lines 45-49):**

```cobol
000045            SELECT TRANCATG-FILE ASSIGN TO TRANCATG
000046                   ORGANIZATION IS INDEXED
000047                   ACCESS MODE  IS RANDOM
000048                   RECORD KEY   IS FD-TRAN-CAT-KEY
000049                   FILE STATUS  IS TRANCATG-STATUS.
```
*(CBTRN03C.cbl lines 45-49 — the TRANCATG reference file contains category descriptions used for report disclosure. The interest calculation program likely uses a similar or related reference file for rate lookup.)*

**Billing cycle fields (from ACCT-BR-004, CBTRN02C.cbl lines 547-552):**

```cobol
000547            ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548            IF DALYTRAN-AMT >= 0
000549               ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550            ELSE
000551               ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552            END-IF
```
*(CBTRN02C.cbl lines 547-552 — the cycle credit/debit tracking provides the billing cycle boundary context for interest period calculation.)*

## Acceptance Criteria

### Scenario 1: Interest calculated per transaction category

```gherkin
GIVEN an account with TCATBAL records:
  | Type | Category | Balance |
  | SA | 5001 | 3000.00 |
  | CA | 5002 | 500.00 |
  AND interest rates:
  | Type | Category | Annual Rate |
  | SA | 5001 | 18.9% |
  | CA | 5002 | 24.9% |
  AND the billing period is 30 days
WHEN interest calculation runs
THEN category interest is calculated:
  | Category | Balance | Daily Rate | Days | Interest |
  | SA-5001 | 3000.00 | 0.0518% | 30 | 46.60 |
  | CA-5002 | 500.00 | 0.0682% | 30 | 10.23 |
  AND total interest of 56.83 is added to ACCT-CURR-BAL
```

### Scenario 2: Zero balance category generates no interest

```gherkin
GIVEN a TCATBAL record with TRAN-CAT-BAL = 0.00
WHEN interest calculation processes this category
THEN no interest is charged for this category
  AND the record is skipped
```

### Scenario 3: Negative balance (credit) category

```gherkin
GIVEN a TCATBAL record with TRAN-CAT-BAL = -200.00 (overpayment)
WHEN interest calculation processes this category
THEN interest calculation produces a negative result (credit interest)
  AND the credit is applied to the account balance
```

### Scenario 4: Disclosure statement itemizes by category

```gherkin
GIVEN interest has been calculated for multiple categories
WHEN the disclosure statement is generated
THEN each category is listed with:
  | Field | Content |
  | Category description | Resolved from TRANCATG reference |
  | Balance | Category balance from TCATBAL |
  | Rate | Annual percentage rate |
  | Interest amount | Calculated interest for the period |
  AND the APR is disclosed per EU Consumer Credit Directive
```

### Scenario 5: Inactive account excluded from interest calculation

```gherkin
GIVEN an account with ACCT-ACTIVE-STATUS = 'N'
WHEN the interest calculation batch runs
THEN the account is skipped
  AND no interest is calculated or posted
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accurate accounting of interest charges on financial products | Per-category interest calculation ensures precise accounting of charges based on actual balances and applicable rates |
| EU Consumer Credit Directive | Art. 6 (2008/48/EC) | Pre-contractual information including APR and total cost of credit | Disclosure grouping provides the category-level breakdown needed to calculate and disclose the effective APR |
| EU Consumer Credit Directive | Art. 10 (2008/48/EC) | Credit agreement must state interest rate and conditions, with itemized periodic statements | Category-level interest disclosure on periodic statements satisfies the itemization requirement |
| PSD2 | Art. 64 | Transaction data integrity — financial charges must be accurately computed and disclosed | Interest calculations are derived from verified category balances maintained by ACCT-BR-009 |

## Edge Cases

1. **Interest rate table not in repository**: The interest rate table (annual rates per transaction type/category) is not available in the COBOL source. It may be a DB2 table, a VSAM file, or hardcoded in the interest calculation program. The migrated system needs a configurable rate table with effective dates and version history.

2. **Average daily balance vs. cycle-end balance**: The calculation method (average daily balance over the cycle vs. balance at cycle end) significantly affects interest amounts. The available source does not indicate which method is used. The EU Consumer Credit Directive requires specific calculation methods for APR — the migrated system must use the correct method.

3. **Grace period**: Many credit card products offer a grace period (typically 21-25 days) where no interest is charged on purchases if the previous cycle balance was paid in full. The grace period logic is not visible in available source. The migrated system must implement grace period rules per the product terms.

4. **Minimum interest charge**: Some jurisdictions or product terms specify a minimum interest charge (e.g., minimum SEK 50). The migrated system should check if the calculated interest meets the minimum threshold.

5. **Cycle boundary and interest accrual**: Interest is typically calculated at billing cycle close, but the cycle close program is not available. The relationship between ACCT-CURR-CYC-CREDIT/DEBIT reset (ACCT-BR-004) and interest posting must be clarified.

6. **Rounding**: COBOL packed decimal arithmetic may produce different rounding results than .NET decimal. The migrated system must use consistent rounding rules (typically round half-up to 2 decimal places) and validate against mainframe parallel-run output.

7. **Promotional rates**: Some categories may carry promotional rates (e.g., 0% for 12 months on balance transfers). The rate lookup must support time-bound promotional rates with automatic reversion to standard rates.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated interest calculation batch program must be obtained from the mainframe team. Key questions: (1) What is the COBOL program name for interest calculation? (2) What calculation method is used — average daily balance or cycle-end balance? (3) Is there a grace period, and what are its terms? (4) Where is the interest rate table stored — VSAM file, DB2 table, or program constants? (5) How are promotional rates tracked and applied? (6) What is the relationship between interest calculation and billing cycle close — are they the same program or separate batch steps? (7) Does the CVTRA02Y.cpy copybook (referenced but not in repository) contain the disclosure group/interest rate structure?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
