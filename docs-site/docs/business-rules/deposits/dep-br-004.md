---
id: "dep-br-004"
title: "Interest calculation and accrual for deposit accounts"
domain: "deposits"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "DEP-BR-004"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Deposit Guarantee Directive 2014/49/EU"
  - "PSD2 Art. 57"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# DEP-BR-004: Interest calculation and accrual for deposit accounts

## Summary

Interest calculation is a core batch process for deposit accounts, running nightly to accrue interest on deposit balances. The interest rate for each account is determined by the disclosure group assignment (`ACCT-GROUP-ID`), which links the account to a rate schedule. The nightly batch computes daily interest based on the account's closing balance, the applicable rate, and the day-count convention. Accrued interest is tracked separately and posted to the account at defined intervals (monthly, quarterly, or at term maturity).

The dedicated COBOL program for deposit interest calculation is not yet available in the repository. This rule is inferred from the account data structure (CVACT01Y.cpy disclosure group field), the billing domain's interest rate structure (CVTRA02Y.cpy), and Swedish regulatory requirements for deposit interest. The interest rate lookup via `ACCT-GROUP-ID` is referenced in the lending domain extraction (LND-BR-004) and follows the same disclosure group mechanism.

## Business Logic

### Pseudocode

```
PERFORM NIGHTLY-INTEREST-CALCULATION:
    OPEN ACCOUNT-FILE (I-O)
    OPEN DISCLOSURE-GROUP-FILE (INPUT)
    OPEN INTEREST-ACCRUAL-FILE (I-O)

    PERFORM UNTIL END-OF-ACCOUNT-FILE
        READ next ACCOUNT-RECORD

        -- Step 1: Skip inactive accounts
        IF ACCT-ACTIVE-STATUS NOT = 'Y'
            CONTINUE
        END-IF

        -- Step 2: Skip non-deposit accounts (credit limit > 0)
        IF ACCT-CREDIT-LIMIT > 0
            SKIP (this is a lending account)
        END-IF

        -- Step 3: Look up interest rate from disclosure group
        READ DISCLOSURE-GROUP-FILE using ACCT-GROUP-ID
        SET daily-rate = annual-rate / day-count-basis

        -- Step 4: Compute daily interest
        COMPUTE daily-interest = ACCT-CURR-BAL * daily-rate
        ROUND daily-interest to 4 decimal places (intermediate)

        -- Step 5: Apply tiered rates if applicable
        IF product-has-tiered-rates
            COMPUTE daily-interest using tier schedule:
                Tier 1: balance up to tier-1-limit at tier-1-rate
                Tier 2: balance from tier-1-limit to tier-2-limit at tier-2-rate
                Tier 3: balance above tier-2-limit at tier-3-rate
        END-IF

        -- Step 6: Accumulate accrued interest
        ADD daily-interest TO accrued-interest-balance
        UPDATE INTEREST-ACCRUAL-FILE

        -- Step 7: Post interest if posting date reached
        IF today = interest-posting-date
            ROUND accrued-interest to 2 decimal places
            ADD accrued-interest TO ACCT-CURR-BAL
            ADD accrued-interest TO ACCT-CURR-CYC-CREDIT
            RESET accrued-interest-balance TO 0
            REWRITE ACCOUNT-RECORD
            WRITE interest-posting-transaction
        END-IF
    END-PERFORM
```

### Decision Table

| Account Status | Balance | Disclosure Group | Rate Found | Outcome |
|---------------|---------|-----------------|-----------|---------|
| Active | > 0 | Valid group ID | Yes | Interest accrued at group rate |
| Active | = 0 | Valid group ID | Yes | No interest (zero balance) |
| Active | < 0 | Valid group ID | Yes | No interest (negative balance — error condition) |
| Active | > 0 | Invalid/missing | No | Error — log and skip account |
| Inactive | Any | Any | Any | Skip — no interest on inactive accounts |

### Interest Calculation Formula

```
Daily Interest = Account Balance × (Annual Rate / Day Count Basis)

Where:
    Day Count Basis = 360 (ACT/360) or 365 (ACT/365)
    Annual Rate = from disclosure group rate schedule

For Tiered Rates:
    Daily Interest = Σ (Tier Balance × Tier Rate / Day Count Basis)
    where each tier covers a specific balance range
```

## Source COBOL Reference

**Program:** Dedicated interest calculation batch program — not yet in repository.

The interest rate linkage is inferred from the disclosure group field in the account record:

```cobol
       COPY CVACT01Y.
```
*(CBTRN02C.cbl line 121 — CVACT01Y copybook containing ACCT-GROUP-ID field that links to the disclosure group/interest rate schedule)*

The disclosure group record structure is referenced in the copybook:

```cobol
       COPY CVTRA02Y.
```
*(CBTRN02C.cbl line 126 — CVTRA02Y copybook for disclosure group records. This structure contains interest rate definitions by group. For deposit accounts, this defines the savings interest rate applied to balances.)*

## Acceptance Criteria

### Scenario 1: Daily interest accrual on positive balance

```gherkin
GIVEN a deposit account with:
  | CurrentBalance    | 100000.00 |
  | DisclosureGroupId | SAV-STD   |
  AND the SAV-STD group has annual rate 2.50%
  AND the day count convention is ACT/365
WHEN the nightly interest calculation runs
THEN daily interest = 100000.00 × (0.025 / 365) = 6.8493 (4 decimal intermediate)
  AND the accrued interest balance is updated
  AND the account balance is NOT updated (interest is accrued, not posted yet)
```

### Scenario 2: Monthly interest posting

```gherkin
GIVEN a deposit account with accrued interest of 205.48 over the past month
  AND today is the interest posting date (month-end)
WHEN the nightly interest calculation runs
THEN the accrued interest of 205.48 is posted to the account balance
  AND CurrentBalance increases by 205.48
  AND CycleCredit increases by 205.48
  AND the accrued interest balance is reset to 0.00
  AND an interest posting transaction is written to the transaction file
```

### Scenario 3: Tiered interest rate calculation

```gherkin
GIVEN a deposit account with CurrentBalance = 500000.00
  AND the product has tiered rates:
  | Tier | Balance Range          | Annual Rate |
  | 1    | 0 - 100,000           | 1.00%       |
  | 2    | 100,001 - 500,000     | 1.50%       |
  | 3    | 500,001+              | 2.00%       |
WHEN the nightly interest calculation runs
THEN interest is calculated as:
  | Tier 1 | 100000.00 × (0.01 / 365) = 2.7397 |
  | Tier 2 | 400000.00 × (0.015 / 365) = 16.4384 |
  | Total  | 19.1781 per day                      |
```

### Scenario 4: Zero balance account receives no interest

```gherkin
GIVEN a deposit account with CurrentBalance = 0.00
WHEN the nightly interest calculation runs
THEN daily interest = 0.00
  AND no accrual record is created or updated
```

### Scenario 5: Inactive account is skipped

```gherkin
GIVEN a deposit account with ActiveStatus = 'N'
  AND CurrentBalance = 50000.00
WHEN the nightly interest calculation runs
THEN the account is skipped
  AND no interest is accrued
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — interest accrual must be accurately tracked and recorded | Daily interest accrual with 4-decimal intermediate precision ensures accurate interest tracking across the accrual period |
| FSA FFFS 2014:5 | Ch. 6 | Risk management — interest rate risk must be managed through proper rate assignment and calculation | The disclosure group mechanism ensures each account is assigned appropriate rates based on product type and risk classification |
| Deposit Guarantee Directive | 2014/49/EU Art. 6 | Eligible deposits include accrued interest — the guarantee covers principal plus accrued interest | Separate tracking of accrued (unposted) interest ensures accurate guarantee calculations that include both principal and accrued amounts |
| PSD2 | Art. 57 | Information on transactions — customers must be informed of applicable interest rates | The disclosure group linkage provides a transparent, auditable mapping from account to applicable interest rate |

## Edge Cases

1. **Day-count convention**: Swedish banking may use ACT/360 or ACT/365 for deposit interest. The convention affects daily rate calculations (2.5% / 360 = 0.006944% vs 2.5% / 365 = 0.006849%). The mainframe team must confirm which convention applies.

2. **Leap year handling**: If ACT/365 is used, interest on Feb 29 in leap years may use 366 as the divisor (ACT/ACT convention). The mainframe team must clarify the leap year convention.

3. **Rounding**: Intermediate daily calculations should use 4 decimal places; final posted amounts use 2 decimal places. Cumulative rounding errors over a month/quarter must be within acceptable tolerance (typically ±0.01 SEK).

4. **Negative interest rates**: Sweden has historically had negative policy rates. If deposit rates can be negative (charging depositors), the calculation must handle negative daily interest — reducing the accrued balance. This is a policy decision that must be confirmed.

5. **Rate changes mid-period**: If the disclosure group rate changes during an accrual period, the system must apply the old rate up to the change date and the new rate from the change date. The rate effective date mechanism is not in the available COBOL source.

6. **Interest on interest (compound)**: Monthly posting of accrued interest creates a compounding effect. The daily calculation uses the posted balance (principal + previously posted interest), not the original principal. This is standard for Swedish deposit products but must be confirmed.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated interest calculation batch program must be obtained from the mainframe team. Key questions: (1) What day-count convention is used — ACT/360, ACT/365, or ACT/ACT? (2) Are interest rates tiered by balance, or is a single rate applied per disclosure group? (3) What is the interest posting frequency — monthly, quarterly, or at maturity? (4) How are rate changes handled mid-accrual period? (5) Can deposit rates be negative, and if so, how is negative interest posted? (6) Is there a separate accrual tracking file, or is accrued interest stored in the account record?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
