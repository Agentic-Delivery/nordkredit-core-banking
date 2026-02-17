---
id: "dep-br-010"
title: "Deposit guarantee scheme compliance and reporting"
domain: "deposits"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "DEP-BR-010"
regulations:
  - "EU Deposit Guarantee Directive 2014/49/EU"
  - "FSA FFFS 2014:5"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# DEP-BR-010: Deposit guarantee scheme compliance and reporting

## Summary

The Swedish deposit guarantee scheme (insättningsgarantin), administered by Riksgälden (Swedish National Debt Office), protects depositors up to SEK 1,050,000 per depositor per institution. NordKredit AB must maintain systems capable of calculating each depositor's total covered balance across all accounts, generating the Single Customer View (SCV) file required by Riksgälden, and reporting aggregate guarantee fund exposure. This is a critical compliance function — Riksgälden requires institutions to produce the SCV file within 24 hours of a guarantee event (bank failure).

The dedicated COBOL program for deposit guarantee reporting is not yet available in the repository. This rule is inferred from the account data structure (CVACT01Y.cpy), the customer cross-reference (CVACT03Y.cpy), and EU/Swedish regulatory requirements for deposit guarantee schemes.

## Business Logic

### Pseudocode

```
PERFORM GENERATE-DEPOSIT-GUARANTEE-REPORT:
    OPEN ACCOUNT-FILE (INPUT)
    OPEN CUSTOMER-FILE (INPUT)
    OPEN XREF-FILE (INPUT)
    OPEN SCV-OUTPUT-FILE (OUTPUT)

    -- Step 1: Aggregate deposits per depositor
    FOR EACH unique customer (XREF-CUST-ID)
        INITIALIZE customer-total = 0
        INITIALIZE eligible-accounts = []

        -- Find all accounts for this customer
        FOR EACH account linked to customer via XREF
            READ ACCOUNT-RECORD
            IF ACCT-ACTIVE-STATUS = 'Y'
              AND account is eligible deposit (not excluded type)
                ADD ACCT-CURR-BAL TO customer-total
                ADD accrued-interest TO customer-total
                APPEND to eligible-accounts
            END-IF
        END-FOR

        -- Step 2: Calculate coverage
        IF customer-total <= 1050000.00
            SET covered-amount = customer-total
            SET excess-amount = 0
        ELSE
            SET covered-amount = 1050000.00
            SET excess-amount = customer-total - 1050000.00
        END-IF

        -- Step 3: Write SCV record
        WRITE SCV-RECORD:
            customer-id
            customer-name
            personnummer
            total-eligible-deposits
            covered-amount
            excess-amount
            number-of-accounts
            account-details[]
    END-FOR

    -- Step 4: Generate summary statistics
    COMPUTE total-covered-deposits = SUM(all covered amounts)
    COMPUTE total-excess-deposits = SUM(all excess amounts)
    COMPUTE total-depositors = COUNT(unique customers)
    COMPUTE depositors-at-limit = COUNT(customers with excess > 0)
    WRITE SUMMARY-RECORD
```

### Single Customer View (SCV) Structure

```
SCV-RECORD:
    CUST-ID               PIC 9(09)      -- Customer identifier
    CUST-PERSONNUMMER     PIC X(12)      -- Swedish national ID
    CUST-NAME             PIC X(50)      -- Full name
    CUST-ADDRESS          PIC X(100)     -- Registered address
    TOTAL-ELIGIBLE-BAL    PIC S9(12)V99  -- Sum of all eligible deposits
    COVERED-AMOUNT        PIC S9(12)V99  -- Guaranteed amount (max 1,050,000)
    EXCESS-AMOUNT         PIC S9(12)V99  -- Amount above guarantee limit
    NUM-ACCOUNTS          PIC 9(03)      -- Number of eligible accounts
    ACCT-DETAIL occurs 1-99:
        ACCT-ID           PIC 9(11)
        ACCT-BALANCE      PIC S9(10)V99
        ACCT-ACCRUED-INT  PIC S9(10)V99
        ACCT-PRODUCT-TYPE PIC X(02)
```

### Coverage Calculation

```
Guarantee Limit (2024): SEK 1,050,000 per depositor per institution

Eligible Deposits:
    + Demand deposit balances (sparkonto)
    + Term deposit principal + accrued interest (bundet konto)
    + Children's savings balances (barnsparkonto)
    + Business deposits (if sole proprietor)

Excluded from Guarantee:
    - Institutional deposits (large corporate, financial institutions)
    - Deposits from public authorities (limited exceptions)
    - Deposits secured by other means
```

## Source COBOL Reference

**Program:** Dedicated deposit guarantee reporting program — not yet in repository.

The customer-to-account linkage is available through the cross-reference:

```cobol
       01 CARD-XREF-RECORD.
           05  XREF-CARD-NUM                     PIC X(16).
           05  XREF-CUST-ID                      PIC 9(09).
           05  XREF-ACCT-ID                      PIC 9(11).
           05  FILLER                            PIC X(14).
```
*(CVACT03Y.cpy — cross-reference record linking customers (XREF-CUST-ID) to accounts (XREF-ACCT-ID). This structure enables aggregation of all deposit accounts per customer for guarantee calculations.)*

The account balance field used for coverage calculation:

```cobol
    ACCT-CURR-BAL             PIC S9(10)V99
```
*(CVACT01Y.cpy — current account balance. For deposit guarantee, this balance plus any accrued but unposted interest determines the covered amount.)*

## Acceptance Criteria

### Scenario 1: Single depositor within guarantee limit

```gherkin
GIVEN a customer with personnummer "199001011234"
  AND the customer has deposit accounts:
  | Account     | Balance    | Product |
  | 12345678901 | 500000.00  | Demand  |
  | 12345678902 | 200000.00  | Term    |
  AND total accrued interest is 3500.00
WHEN the deposit guarantee report is generated
THEN the SCV record shows:
  | TotalEligible | 703500.00 |
  | CoveredAmount | 703500.00 |
  | ExcessAmount  | 0.00      |
```

### Scenario 2: Single depositor exceeding guarantee limit

```gherkin
GIVEN a customer with personnummer "196505152345"
  AND the customer has deposit accounts:
  | Account     | Balance      | Product |
  | 23456789012 | 800000.00    | Demand  |
  | 23456789013 | 300000.00    | Term    |
  AND total accrued interest is 5000.00
WHEN the deposit guarantee report is generated
THEN the SCV record shows:
  | TotalEligible | 1105000.00 |
  | CoveredAmount | 1050000.00 |
  | ExcessAmount  | 55000.00   |
```

### Scenario 3: SCV file generation within 24-hour SLA

```gherkin
GIVEN the bank has 500,000 deposit customers
  AND a guarantee event is triggered by Riksgälden
WHEN the SCV file generation is initiated
THEN the complete SCV file is produced within 24 hours
  AND all eligible depositors are included
  AND the file format meets Riksgälden specifications
```

### Scenario 4: Joint account guarantee allocation

```gherkin
GIVEN a joint account with balance 2,000,000.00
  AND the account has two owners: Customer A and Customer B
WHEN the deposit guarantee report is generated
THEN each customer's SCV includes 1,000,000.00 from this account
  AND Customer A's guarantee calculation includes their share
  AND Customer B's guarantee calculation includes their share
```

### Scenario 5: Inactive account included in guarantee

```gherkin
GIVEN a dormant deposit account (DormancyStatus = 'DORMANT')
  AND CurrentBalance = 50000.00
  AND ActiveStatus = 'Y' (still active, just dormant)
WHEN the deposit guarantee report is generated
THEN the account IS included in the SCV
  AND the balance contributes to the depositor's total eligible deposits
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| Deposit Guarantee Directive | 2014/49/EU Art. 6 | Coverage level — deposits covered up to EUR 100,000 (SEK 1,050,000 equivalent) per depositor per institution | The SCV calculation caps coverage at SEK 1,050,000 per depositor, aggregating across all eligible deposit accounts |
| Deposit Guarantee Directive | 2014/49/EU Art. 8 | Repayment — guarantee scheme must be able to repay within 7 working days | The 24-hour SCV generation SLA ensures Riksgälden has depositor data in time to process repayments |
| Deposit Guarantee Directive | 2014/49/EU Art. 16 | Stress testing — institutions must regularly test SCV generation capability | The report generation process can be run as a stress test to verify the system meets the 24-hour SLA |
| FSA FFFS 2014:5 | General | Institutions must maintain systems supporting regulatory reporting requirements | The deposit guarantee reporting system provides regulatory data on demand |
| DORA | Art. 11 | ICT risk management — critical data processes must be reliable and tested | SCV generation is a critical ICT process that must be resilient and tested regularly |

## Edge Cases

1. **Guarantee limit changes**: The SEK 1,050,000 limit is periodically reviewed. The system must be configurable to update the guarantee limit without code changes. Historical limits must be retained for audit.

2. **Multi-currency deposits**: If a customer holds deposits in SEK and foreign currencies (EUR, USD), the foreign currency balances must be converted to SEK at the ECB reference rate on the reporting date for guarantee calculations.

3. **Beneficial ownership**: For accounts held by legal entities, the beneficial owners may be entitled to separate guarantee coverage. The system must support beneficial ownership linkage for corporate deposit accounts.

4. **Temporary high balances**: The Deposit Guarantee Directive provides temporary higher coverage (up to SEK 5,000,000) for specific life events (property sale, insurance payout, inheritance). The system must flag accounts with recent large inflows for potential temporary high balance treatment.

5. **Cross-border depositors**: EU depositors at NordKredit's branch in their home country may be covered by the Swedish scheme. The system must correctly assign depositors to the Swedish guarantee based on the servicing branch.

6. **Accrued interest inclusion**: The guarantee covers principal PLUS accrued but unposted interest. The SCV must include accrued interest from the interest accrual file (DEP-BR-004), not just the posted balance.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated deposit guarantee reporting programs and the Riksgälden SCV file specification must be obtained. Key questions: (1) What is the current SCV file format — is it the EU standard or a Sweden-specific variant? (2) How is the 24-hour SLA currently met — batch or on-demand generation? (3) How are joint accounts allocated between depositors — equal split or specified shares? (4) Does the system currently handle temporary high balance identification? (5) How are beneficial owners of corporate accounts identified in the customer data? (6) What is the frequency of regular (non-emergency) guarantee reporting to Riksgälden?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
