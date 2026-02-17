---
id: "lnd-br-004"
title: "Interest calculation and amortization schedule"
domain: "lending"
cobol_source: "Dedicated program not yet in repository (referenced via ACCT-GROUP-ID in CVACT01Y.cpy)"
requirement_id: "LND-BR-004"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
  - "EU Consumer Credit Directive 2008/48/EC Art. 19"
  - "GDPR Art. 15"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# LND-BR-004: Interest calculation and amortization schedule

## Summary

Interest calculation is a core lending function that determines the cost of borrowing for each account. The COBOL system links accounts to interest rate schedules via the ACCT-GROUP-ID (disclosure group) field in the account master record. While the dedicated interest calculation batch program has not yet been obtained from the mainframe, the data structure and regulatory requirements allow extraction of the business rule framework. The interest calculation batch job must run nightly (per the system's batch SLA of completion by 06:00) and produce interest accruals for each active account based on its outstanding balance and applicable rate.

Under the EU Consumer Credit Directive (Art. 19), the Annual Percentage Rate of charge (APR) must be calculated using a standardized formula. For Swedish krona (SEK) lending, interest is typically calculated on an Actual/360 or 30/360 day-count convention. The amortization schedule for term loans defines the repayment plan, splitting each payment into principal and interest components.

## Business Logic

### Pseudocode

```
PERFORM NIGHTLY-INTEREST-CALCULATION:
    OPEN ACCTFILE for INPUT
    OPEN INTEREST-ACCRUAL-FILE for OUTPUT
    OPEN DISCLOSURE-GROUP-FILE for INPUT

    PERFORM UNTIL end-of-accounts
        READ next ACCOUNT-RECORD from ACCTFILE
        IF ACCT-ACTIVE-STATUS = 'Y'
           AND ACCT-CURR-BAL > 0

            -- Look up interest rate from disclosure group
            READ DISCLOSURE-GROUP-FILE using ACCT-GROUP-ID
            SET annual-rate = DISCLOSURE-ANNUAL-RATE
            SET day-count-method = DISCLOSURE-DAY-COUNT

            -- Calculate daily interest
            IF day-count-method = 'ACT/360'
                SET daily-rate = annual-rate / 360
                SET days-in-period = actual-days-elapsed
            ELSE IF day-count-method = '30/360'
                SET daily-rate = annual-rate / 360
                SET days-in-period = 30 (for monthly)
            END-IF

            COMPUTE interest-amount = ACCT-CURR-BAL
                                    × daily-rate
                                    × days-in-period

            -- Round to 2 decimal places (banker's rounding)
            COMPUTE interest-amount ROUNDED = interest-amount

            -- Write interest accrual record
            WRITE INTEREST-ACCRUAL-RECORD
                (ACCT-ID, interest-amount, annual-rate, period-start, period-end)

        END-IF
    END-PERFORM

PERFORM AMORTIZATION-SCHEDULE:
    -- For term loans (not revolving credit)
    SET principal = loan-amount
    SET monthly-rate = annual-rate / 12
    SET num-payments = loan-term-months

    -- Fixed monthly payment (annuity formula)
    COMPUTE monthly-payment = principal
        × (monthly-rate × (1 + monthly-rate) ^ num-payments)
        / ((1 + monthly-rate) ^ num-payments - 1)

    FOR each payment period:
        COMPUTE interest-portion = remaining-principal × monthly-rate
        COMPUTE principal-portion = monthly-payment - interest-portion
        COMPUTE remaining-principal = remaining-principal - principal-portion
        WRITE schedule-record
            (period, payment, principal-portion, interest-portion, remaining-principal)
    END-FOR
```

### Decision Table

| Account Active | Balance > 0 | Disclosure Group Found | Outcome |
|---------------|-------------|----------------------|---------|
| No | N/A | N/A | Skip — no interest calculated |
| Yes | No (= 0) | N/A | Skip — no interest on zero balance |
| Yes | Yes | No | Error — missing rate configuration |
| Yes | Yes | Yes | Interest calculated and accrued |

## Source COBOL Reference

**Program:** Dedicated interest calculation batch program not yet available in repository.
**Inferred from:** `CVACT01Y.cpy` (ACCT-GROUP-ID field), `CBTRN02C.cbl` (batch processing pattern)

The disclosure group linkage is defined in the account master record:

```cobol
      * ACCT-GROUP-ID             PIC X(10)
      * Links account to disclosure/interest rate group
```
*(Reconstructed from CVACT01Y.cpy — ACCT-GROUP-ID field provides the foreign key to the interest rate schedule)*

The batch processing pattern from CBTRN02C shows how lending batch jobs are structured:

```cobol
000193       PROCEDURE DIVISION.
000194           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C'.
000195           PERFORM 0000-DALYTRAN-OPEN.
             ...
000202           PERFORM UNTIL END-OF-FILE = 'Y'
000203               IF  END-OF-FILE = 'N'
000204                   PERFORM 1000-DALYTRAN-GET-NEXT
                     ...
000219           END-PERFORM.
             ...
000232           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C'.
000234           GOBACK.
```
*(Lines 193-234, CBTRN02C.cbl — standard batch processing pattern: open files, process in loop, close files, GOBACK. The interest calculation batch would follow this same pattern.)*

## Acceptance Criteria

### Scenario 1: Daily interest accrual on active account

```gherkin
GIVEN an active loan account with:
  | Current Balance  | 100000.00 |
  | Disclosure Group | GRP-STD   |
  AND disclosure group GRP-STD has annual rate 8.50%
  AND the day-count method is Actual/360
WHEN the nightly interest calculation batch runs
THEN the daily interest is calculated as: 100000.00 × (0.085 / 360) × 1 = 23.61
  AND an interest accrual record is written for the account
  AND the accrual amount is rounded to 2 decimal places
```

### Scenario 2: No interest on zero balance

```gherkin
GIVEN an active loan account with:
  | Current Balance  | 0.00    |
  | Disclosure Group | GRP-STD |
WHEN the nightly interest calculation batch runs
THEN no interest accrual record is generated for this account
```

### Scenario 3: No interest on inactive account

```gherkin
GIVEN an account with:
  | Active Status    | N        |
  | Current Balance  | 50000.00 |
WHEN the nightly interest calculation batch runs
THEN no interest accrual record is generated for this account
```

### Scenario 4: Term loan amortization schedule

```gherkin
GIVEN a term loan with:
  | Principal    | 500000.00 |
  | Annual Rate  | 6.00%     |
  | Term         | 360 months |
WHEN the amortization schedule is generated
THEN the monthly payment is approximately 2997.75 (annuity formula)
  AND the first payment splits as: interest = 2500.00, principal = 497.75
  AND each subsequent payment has decreasing interest and increasing principal
  AND the final payment reduces remaining principal to 0.00
```

### Scenario 5: APR calculation per Consumer Credit Directive

```gherkin
GIVEN a loan with:
  | Nominal Rate | 8.50%   |
  | Fees         | 500.00  |
  | Term         | 12 months |
  | Amount       | 100000.00 |
WHEN the APR is calculated per EU Consumer Credit Directive Art. 19
THEN the APR includes the nominal rate plus all mandatory fees
  AND the APR is expressed as an annual percentage to 1 decimal place
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — accurate measurement of credit exposure including accrued interest | Nightly interest calculation ensures the outstanding balance reflects accrued interest charges accurately |
| Consumer Credit Directive | Art. 10(2)(f) | Credit agreement must specify borrowing rate, conditions governing application of the rate, and any index or reference rate | The disclosure group mechanism provides the rate structure; the calculation method (day count, compounding) is defined per group |
| Consumer Credit Directive | Art. 19 | Annual Percentage Rate of charge — calculation using standardized EU formula | APR calculation must include all costs (interest + fees) and be presented per the Directive's annex formula |
| GDPR | Art. 15 | Right of access — data subjects can request information about how their data is processed | Interest calculation details (rate applied, method, period) must be available for customer inquiry and regulatory audit |

## Edge Cases

1. **Rounding accumulation**: Daily interest calculation with rounding to 2 decimal places can accumulate rounding errors over a billing cycle. The COBOL system may use a "true-up" mechanism at cycle close. The migrated system should use banker's rounding (round half to even) to minimize bias.

2. **Variable rate changes**: If the disclosure group rate changes mid-period, the interest calculation must pro-rate: apply the old rate up to the change date and the new rate from the change date. The mechanism for rate changes is not yet visible in the available COBOL source.

3. **Negative balance interest**: If ACCT-CURR-BAL is negative (customer overpaid), the system may need to calculate credit interest payable to the customer. Whether the COBOL system handles this or treats it as zero-interest is unknown.

4. **Leap year handling**: With Actual/360 day count, a leap year has 366 days divided by 360, resulting in slightly higher annual interest than the stated rate. The system must handle February 29 correctly.

5. **Batch SLA compliance**: The nightly interest calculation must complete by 06:00 (per batch SLAs). For ~2 million accounts, the migrated Azure Functions implementation must be designed for parallel processing to meet this deadline.

6. **Currency precision**: Swedish krona (SEK) uses 2 decimal places for display but may use higher precision (e.g., 4 decimal places) for intermediate calculations. The COBOL PIC S9(10)V99 suggests 2-decimal precision throughout. The migrated system should use DECIMAL(12,4) for intermediate calculations.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The interest calculation batch program must be obtained from the mainframe team. Key questions: (1) What is the program name for interest calculation? (2) What day-count convention is used (Actual/360, 30/360, Actual/365)? (3) Is interest compounded daily, monthly, or at statement cycle close? (4) What is the disclosure group master file structure — does it contain tiered rates based on balance ranges? (5) How are rate changes applied — immediately, at next cycle, or at next statement? (6) Is there a separate program for APR calculation and disclosure?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
