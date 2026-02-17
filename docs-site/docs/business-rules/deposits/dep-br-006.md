---
id: "dep-br-006"
title: "Savings product configuration and tiered rates"
domain: "deposits"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "DEP-BR-006"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Deposit Guarantee Directive 2014/49/EU"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# DEP-BR-006: Savings product configuration and tiered rates

## Summary

The bank offers multiple deposit product types (demand savings, term deposits, children's savings, etc.), each with distinct interest rate schedules, minimum balance requirements, and withdrawal restrictions. Product configuration is managed through the disclosure group mechanism: each account's `ACCT-GROUP-ID` field links to a product definition record containing the applicable interest rate(s), rate tier boundaries, and product rules. The disclosure group record (CVTRA02Y.cpy) stores rate information, and dedicated product configuration programs manage the product catalog.

This rule covers the product configuration data structure and the rate tier lookup logic that drives interest calculation (DEP-BR-004). The dedicated COBOL programs for product configuration are not yet available in the repository. The disclosure group mechanism is inferred from CVTRA02Y.cpy references and the ACCT-GROUP-ID field usage across programs.

## Business Logic

### Pseudocode

```
PRODUCT-CONFIGURATION:
    DISCLOSURE-GROUP-RECORD (from CVTRA02Y.cpy):
        GROUP-ID              PIC X(10)    -- Product identifier
        GROUP-DESCRIPTION     PIC X(50)    -- Product name
        ANNUAL-INTEREST-RATE  PIC S9(04)V99 -- Base annual rate (%)
        TIER-1-LIMIT          PIC S9(10)V99 -- Balance threshold for tier 1
        TIER-1-RATE           PIC S9(04)V99 -- Rate for tier 1
        TIER-2-LIMIT          PIC S9(10)V99 -- Balance threshold for tier 2
        TIER-2-RATE           PIC S9(04)V99 -- Rate for tier 2
        TIER-3-RATE           PIC S9(04)V99 -- Rate for tier 3 (above tier 2)
        MIN-BALANCE           PIC S9(10)V99 -- Minimum balance requirement
        MAX-WITHDRAWALS       PIC 9(02)     -- Monthly withdrawal limit (0=unlimited)
        PRODUCT-TYPE          PIC X(02)     -- DD=Demand, TD=Term, CS=Children's

PERFORM RATE-LOOKUP:
    READ DISCLOSURE-GROUP-FILE using ACCT-GROUP-ID
    IF product-has-tiered-rates (TIER-1-LIMIT > 0)
        EVALUATE TRUE
            WHEN ACCT-CURR-BAL <= TIER-1-LIMIT
                RETURN TIER-1-RATE
            WHEN ACCT-CURR-BAL <= TIER-2-LIMIT
                RETURN blended rate across tiers
            WHEN ACCT-CURR-BAL > TIER-2-LIMIT
                RETURN blended rate across all tiers
        END-EVALUATE
    ELSE
        RETURN ANNUAL-INTEREST-RATE
    END-IF
```

### Product Types (Inferred)

| Product Type | Code | Interest Model | Withdrawal Rules | Typical Group IDs |
|-------------|------|---------------|-----------------|-------------------|
| Demand Savings (sparkonto) | DD | Variable rate, tiered | Unlimited withdrawals | SAV-STD, SAV-PREM |
| Term Deposit (bundet konto) | TD | Fixed rate for term | No withdrawal until maturity | TD-3M, TD-6M, TD-12M |
| Children's Savings (barnsparkonto) | CS | Higher rate, tiered | Restricted until age 18 | SAV-CHILD |
| Business Deposit (företagskonto) | BD | Variable rate | Per agreement | BIZ-STD, BIZ-PREM |

## Source COBOL Reference

**Program:** Dedicated product configuration program — not yet in repository.

The disclosure group record is referenced in the batch processing copybook:

```cobol
       COPY CVTRA02Y.
```
*(CBTRN02C.cbl line 126 — CVTRA02Y copybook defining the disclosure group record structure. This record contains the interest rate and product configuration data for each disclosure group.)*

The account-to-product linkage:

```cobol
    ACCT-GROUP-ID             PIC X(10)
```
*(CVACT01Y.cpy — disclosure group ID field in the account master record, linking each deposit account to its product configuration and interest rate schedule.)*

## Acceptance Criteria

### Scenario 1: Single-rate product lookup

```gherkin
GIVEN a deposit account with DisclosureGroupId = "SAV-STD"
  AND the SAV-STD product has:
  | AnnualRate | 1.75% |
  | TieredRates | No |
WHEN the interest rate is looked up for this account
THEN the rate returned is 1.75%
```

### Scenario 2: Tiered-rate product lookup

```gherkin
GIVEN a deposit account with DisclosureGroupId = "SAV-PREM"
  AND the SAV-PREM product has tiered rates:
  | Tier | Balance Range          | Annual Rate |
  | 1    | 0 - 100,000           | 1.50%       |
  | 2    | 100,001 - 500,000     | 2.00%       |
  | 3    | 500,001+              | 2.50%       |
WHEN the interest rate is looked up for a balance of 250,000.00
THEN the blended rate is computed across applicable tiers
```

### Scenario 3: Minimum balance enforcement

```gherkin
GIVEN a deposit product with MinBalance = 10000.00
  AND a deposit account with CurrentBalance = 8000.00
WHEN the balance falls below the minimum
THEN the system flags the account for minimum balance notification
  AND the interest rate may be reduced to a penalty rate
```

### Scenario 4: Withdrawal limit enforcement

```gherkin
GIVEN a savings product with MaxWithdrawals = 6 per month
  AND a deposit account has already made 6 withdrawals this month
WHEN a 7th withdrawal is attempted
THEN the withdrawal is rejected
  AND the customer is notified of the withdrawal limit
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — product terms and applicable rates must be accurately maintained | The disclosure group record provides a single source of truth for product configuration, ensuring all accounts of the same type receive correct, consistent rates |
| FSA FFFS 2014:5 | Ch. 6 | Risk management — interest rate risk from deposit products must be tracked and managed | The product configuration with explicit rate definitions enables interest rate risk reporting and scenario analysis across the deposit portfolio |
| Deposit Guarantee Directive | 2014/49/EU Art. 6 | Eligible deposits — product type determines guarantee eligibility | The product type code enables identification of eligible deposits (consumer savings, term deposits) vs. non-eligible products (if any) for deposit guarantee calculations |

## Edge Cases

1. **Rate effective dates**: Product rates change over time. The disclosure group record must support effective dating so that rate changes apply from a specific date forward without retroactively affecting previously accrued interest.

2. **Discontinued products**: When a savings product is discontinued, existing accounts must continue under their current terms. New account openings are blocked for the product, but the rate schedule must remain available for existing accounts.

3. **Negative rates**: If Sweden returns to negative interest rate policy, certain deposit products may have negative rates. The product configuration must support negative ANNUAL-INTEREST-RATE values.

4. **Promotional rates**: Temporary promotional rates (e.g., introductory rate for first 6 months) may override the standard product rate. The mechanism for promotional rate handling is not in the available COBOL source.

5. **Children's savings restrictions**: Children's savings accounts have legal restrictions on withdrawals. The MAX-WITHDRAWALS field may be insufficient — age-based restrictions require integration with customer data.

6. **Business deposit agreements**: Business deposit rates may be individually negotiated rather than following the standard product schedule. The system must support account-level rate overrides.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated product configuration programs and the full CVTRA02Y.cpy copybook must be obtained from the mainframe team. Key questions: (1) What is the complete structure of the disclosure group record — how many rate tiers are supported? (2) How are rate changes managed — effective dating or separate versions of the group record? (3) Are there product-specific withdrawal restrictions beyond a simple count limit? (4) How are promotional/introductory rates represented? (5) Can individual accounts have rate overrides that differ from their disclosure group? (6) What is the complete list of deposit product types offered by NordKredit?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
