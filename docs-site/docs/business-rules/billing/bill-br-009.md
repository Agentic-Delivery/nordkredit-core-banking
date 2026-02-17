---
id: "bill-br-009"
title: "Interest computation batch for billing accounts"
domain: "billing"
cobol_source: "Dedicated program not yet in repository (referenced via ACCT-GROUP-ID in CVACT01Y.cpy and CVTRA02Y.cpy)"
requirement_id: "BILL-BR-009"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 45"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# BILL-BR-009: Interest computation batch for billing accounts

## Summary

The interest computation batch is the core billing process that calculates interest charges on outstanding balances for credit accounts. The COBOL system uses the disclosure group mechanism (CVTRA02Y.cpy, extracted as BILL-BR-001) to look up the applicable interest rate for each account's transaction categories, then computes interest on the category-level balances (CVTRA01Y.cpy, extracted as BILL-BR-004). This batch runs nightly to accrue daily interest and posts the accumulated interest charge at the end of each billing cycle.

The dedicated interest computation batch program has not yet been obtained from the mainframe. However, the disclosure group structure (BILL-BR-001), category balance tracking (BILL-BR-004), and the billing cycle fields in the account master record provide sufficient structural evidence to extract this rule. The interest rate is looked up via `ACCT-GROUP-ID` + `TRAN-TYPE-CD` + `TRAN-CAT-CD` from the disclosure group file, and applied to the corresponding `TRAN-CAT-BAL` from the category balance file.

Under the EU Consumer Credit Directive Art. 10, credit agreements must clearly specify the borrowing rate, conditions for applying the rate, and any reference rates. FSA FFFS 2014:5 requires accurate interest calculation and recording for all credit products.

## Business Logic

### Pseudocode

```
PERFORM NIGHTLY-INTEREST-CALCULATION:
    OPEN ACCOUNT-FILE (INPUT)
    OPEN TCATBAL-FILE (INPUT)
    OPEN DISCLOSURE-GROUP-FILE (INPUT)
    OPEN INTEREST-ACCRUAL-FILE (I-O)

    PERFORM UNTIL end-of-accounts
        READ next ACCOUNT-RECORD

        -- Step 1: Skip inactive and non-credit accounts
        IF ACCT-ACTIVE-STATUS NOT = 'Y'
            CONTINUE
        END-IF
        IF ACCT-CREDIT-LIMIT = 0
            SKIP (not a credit/billing account)
        END-IF

        -- Step 2: For each transaction category with a balance
        FOR EACH TRAN-CAT-BAL-RECORD where TRANCAT-ACCT-ID = ACCT-ID
            IF TRAN-CAT-BAL > 0

                -- Step 3: Look up interest rate from disclosure group
                BUILD KEY: ACCT-GROUP-ID + TRANCAT-TYPE-CD + TRANCAT-CD
                READ DISCLOSURE-GROUP-FILE using composite key
                SET annual-rate = DIS-INT-RATE / 100

                -- Step 4: Calculate daily interest
                SET daily-rate = annual-rate / day-count-basis
                COMPUTE daily-interest = TRAN-CAT-BAL * daily-rate
                ROUND daily-interest to 4 decimal places (intermediate)

                -- Step 5: Accumulate to accrual record
                ADD daily-interest TO accrued-interest-for-category
                UPDATE INTEREST-ACCRUAL-FILE

            END-IF
        END-FOR

        -- Step 6: At billing cycle close, post total interest
        IF today = ACCT-CYCLE-CLOSE-DATE
            SET total-interest = SUM of all category accruals
            ROUND total-interest to 2 decimal places
            ADD total-interest TO ACCT-CURR-BAL
            ADD total-interest TO ACCT-CURR-CYC-DEBIT
            WRITE interest-charge-transaction
            RESET all category accrual balances to 0
            REWRITE ACCOUNT-RECORD
        END-IF
    END-PERFORM

INTEREST RATE APPLICATION PER CATEGORY:
    Category interest enables different rates for different transaction types:
        - Purchases (type "01"): Standard purchase APR (e.g., 19.99%)
        - Cash advances (type "02"): Higher cash advance APR (e.g., 24.99%)
        - Balance transfers (type "03"): Promotional transfer APR (e.g., 0.00%)
        - Fees (type "04"): May accrue interest at purchase rate
    Each category's balance accrues interest independently at its own rate.
```

### Decision Table

| Account Active | Credit Limit > 0 | Category Balance > 0 | Disclosure Group Found | Outcome |
|---------------|-------------------|---------------------|----------------------|---------|
| No | N/A | N/A | N/A | Skip — no interest calculated |
| Yes | No | N/A | N/A | Skip — not a credit account |
| Yes | Yes | No (= 0) | N/A | No interest on zero-balance category |
| Yes | Yes | Yes | No | Error — missing rate for category |
| Yes | Yes | Yes | Yes, rate = 0 | No interest (promotional rate) |
| Yes | Yes | Yes | Yes, rate > 0 | Interest accrued at category rate |

### Interest Calculation Formula

```
Daily Interest per Category = TRAN-CAT-BAL × (DIS-INT-RATE / 100) / Day-Count-Basis

Where:
    DIS-INT-RATE = Annual percentage rate from disclosure group (e.g., 19.99)
    Day-Count-Basis = 360 (typical for credit card billing) or 365
    TRAN-CAT-BAL = Outstanding balance for this transaction category

Total Monthly Interest = Σ (daily interest per category × days in billing cycle)

Billing Cycle Interest Charge = ROUND(Total Monthly Interest, 2)
```

### Financial Precision

| Calculation Step | Precision | Type |
|-----------------|-----------|------|
| DIS-INT-RATE lookup | S9(04)V99 — 2 decimal places | Annual rate |
| Daily rate | 8+ decimal places (intermediate) | Daily divisor |
| Daily interest per category | 4 decimal places (intermediate) | Accrual amount |
| Total cycle interest (posted) | 2 decimal places (final) | Charge to account |

## Source COBOL Reference

**Program:** Dedicated interest computation batch program not yet available in repository.
**Inferred from:** `CVTRA02Y.cpy` (disclosure group with interest rates), `CVTRA01Y.cpy` (category balance records), `CVACT01Y.cpy` (account master with group ID and cycle fields), `CBTRN02C.cbl` (batch processing pattern)

The disclosure group record defines interest rates per category:

```cobol
      *    Data-structure for DISClosure GRouP record (RECLN = 50)
       01  DIS-GROUP-RECORD.
           05  DIS-ACCT-GROUP-ID                      PIC X(10).
           05  DIS-TRAN-TYPE-CD                       PIC X(02).
           05  DIS-TRAN-CAT-CD                        PIC 9(04).
           05  DIS-INT-RATE                           PIC S9(04)V99.
           05  FILLER                                 PIC X(28).
```
*(CVTRA02Y.cpy — the disclosure group provides the interest rate for each account group + transaction type + category combination. This is the rate lookup structure for the interest computation batch.)*

The category balance file tracks balances that interest is applied to:

```cobol
       FD  TCATBAL-FILE.
       01  FD-TRAN-CAT-BAL-RECORD.
           05 FD-TRAN-CAT-KEY.
              10 FD-TRANCAT-ACCT-ID             PIC 9(11).
              10 FD-TRANCAT-TYPE-CD             PIC X(02).
              10 FD-TRANCAT-CD                  PIC 9(04).
           05 FD-FD-TRAN-CAT-DATA               PIC X(33).
```
*(Lines 91-97, CBTRN02C.cbl — the TCATBAL file keyed by account + type + category provides the balance that the disclosure group rate is applied to)*

## Acceptance Criteria

### Scenario 1: Daily interest accrual on purchase balance

```gherkin
GIVEN an active credit account with:
  | AccountGroupId | STANDARD1 |
  | CreditLimit    | 50000.00  |
  AND a category balance for type "01" (purchases), category "0001":
  | CategoryBalance | 25000.00 |
  AND disclosure group STANDARD1/01/0001 has rate 19.99%
  AND the day-count basis is 360
WHEN the nightly interest calculation runs
THEN daily interest = 25000.00 * (19.99 / 100) / 360 = 13.8819 (4 decimal intermediate)
  AND the accrued interest for this category is updated
```

### Scenario 2: Different rates for different categories

```gherkin
GIVEN an active credit account in group "STANDARD1" with:
  | Category | Type | Balance   | Rate   |
  | Purchases| 01   | 20000.00  | 19.99% |
  | Cash Adv | 02   | 5000.00   | 24.99% |
WHEN the nightly interest calculation runs
THEN purchase daily interest = 20000.00 * 0.1999 / 360 = 11.1056
  AND cash advance daily interest = 5000.00 * 0.2499 / 360 = 3.4708
  AND total daily accrual = 14.5764
```

### Scenario 3: Billing cycle interest posting

```gherkin
GIVEN an active credit account where today is the billing cycle close date
  AND accrued interest over the 30-day cycle totals 415.34 (4-decimal accumulated)
WHEN the cycle close processing runs
THEN 415.34 is rounded to 2 decimal places
  AND 415.34 is posted as an interest charge to the account balance
  AND ACCT-CURR-BAL increases by 415.34
  AND an interest charge transaction is written
  AND all category accrual balances are reset to 0.00
```

### Scenario 4: Promotional zero-rate category

```gherkin
GIVEN a balance transfer of 30000.00 in category type "03"
  AND disclosure group rate for this category is 0.00%
WHEN the nightly interest calculation runs
THEN daily interest = 30000.00 * 0.00 / 360 = 0.00
  AND no interest is accrued for this category
  AND the zero rate is preserved (not treated as an error)
```

### Scenario 5: Missing disclosure group rate

```gherkin
GIVEN a transaction category balance with no matching disclosure group record
WHEN the interest calculation attempts to look up the rate
THEN an error is logged for this account/category combination
  AND no interest is accrued for the missing category
  AND processing continues for other accounts
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — interest income must be accurately calculated for all credit exposures | The category-level interest calculation ensures accurate rate application per transaction type, matching the disclosure group rate schedule |
| FSA FFFS 2014:5 | Ch. 7 | Financial systems — interest charges must be accurately recorded in the accounting system | Each interest charge generates a transaction record with the rate applied, period, and amount for full audit trail |
| PSD2 | Art. 45 | Information on charges — applicable interest rates must be disclosed to users | The disclosure group mechanism provides transparent, per-category rate definitions that can be disclosed to cardholders |
| Consumer Credit Directive | Art. 10(2)(f) | Credit agreement must specify borrowing rate and conditions for application | The disclosure group defines rates per account group and transaction category, enabling precise rate disclosure in credit agreements |

## Edge Cases

1. **Compounding within billing cycle**: The nightly accrual adds to the accrued interest balance but does not increase the principal balance until cycle close. This means interest does not compound within a single billing cycle (no interest-on-interest until the cycle closes and unpaid interest is added to the balance). This simple interest convention must be confirmed with the mainframe team.

2. **Grace period for new purchases**: Many credit card products offer a grace period (typically 21-25 days) where no interest is charged on new purchases if the previous statement balance was paid in full. The interest computation must check whether the grace period applies before accruing interest on purchase categories.

3. **Minimum interest charge**: Some products impose a minimum interest charge per cycle (e.g., if calculated interest is less than 50 SEK, charge 50 SEK). The minimum charge threshold is likely in the fee schedule.

4. **Rate changes mid-cycle**: If the disclosure group rate changes during a billing cycle, the system must pro-rate: apply the old rate up to the change date and the new rate from the change date. The mechanism for mid-cycle rate changes is not visible in the available COBOL source.

5. **Cash advance interest from day one**: Unlike purchases (which may have a grace period), cash advances typically accrue interest from the transaction date with no grace period. The interest computation must distinguish between categories for grace period eligibility.

6. **Negative balance (credit balance)**: If the account has a negative balance (customer overpaid), no interest should be charged. In some jurisdictions, credit interest may be owed to the customer on credit balances held beyond a threshold period.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated interest computation batch program must be obtained from the mainframe team. Key questions: (1) What is the batch job name for interest computation? (2) What day-count convention is used — Actual/360, 30/360, or Actual/365? (3) Is there a grace period mechanism, and how is eligibility determined? (4) Is interest compounded at cycle close only, or is there daily compounding? (5) Is there a minimum interest charge per cycle? (6) How are promotional rates (0.00%) tracked and expired — by date or by balance payoff? (7) Does the batch process all accounts nightly or only accounts approaching cycle close?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
