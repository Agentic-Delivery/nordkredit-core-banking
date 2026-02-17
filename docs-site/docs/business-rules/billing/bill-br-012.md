---
id: "bill-br-012"
title: "Minimum payment calculation"
domain: "billing"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "BILL-BR-012"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
  - "EU Consumer Credit Directive 2008/48/EC Art. 16"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-012: Minimum payment calculation

## Summary

Minimum payment calculation determines the smallest amount a cardholder must pay by the payment due date to keep the account in good standing. The minimum payment is calculated at billing cycle close as part of statement generation (BILL-BR-010) and is printed on the billing statement. The calculation typically includes the greater of a fixed minimum amount or a percentage of the outstanding balance, plus any past-due amounts, overlimit amounts, and current cycle interest and fees.

The dedicated COBOL minimum payment calculation program has not yet been obtained from the mainframe. This rule is inferred from the account master fields (CVACT01Y.cpy balance and cycle fields), the fee and interest structures, and EU Consumer Credit Directive requirements. The minimum payment determination is critical for late payment detection (BILL-BR-011) — the system compares actual payments received against this calculated minimum.

Under the EU Consumer Credit Directive Art. 10, credit agreements must specify the minimum payment and the conditions for its calculation. Art. 16 provides for early repayment rights. FSA FFFS 2014:5 Ch. 6 requires that minimum payment terms be sufficient to ensure eventual repayment of the credit, preventing perpetual debt cycles.

## Business Logic

### Pseudocode

```
PERFORM CALCULATE-MINIMUM-PAYMENT:
    INPUT: new-balance, interest-charged, fees-charged,
           past-due-amount, overlimit-amount

    -- Step 1: If balance is zero or credit, no payment due
    IF new-balance <= 0
        SET minimum-payment = 0.00
        RETURN
    END-IF

    -- Step 2: If balance is below absolute minimum threshold
    IF new-balance <= absolute-minimum-threshold (e.g., 200 SEK)
        SET minimum-payment = new-balance
        RETURN
    END-IF

    -- Step 3: Calculate percentage-based component
    SET pct-component = new-balance * minimum-payment-percentage / 100
        (typical: 2% to 3% of outstanding balance)

    -- Step 4: Apply floor minimum
    SET floor-minimum = MAX(pct-component, fixed-minimum-amount)
        (typical fixed minimum: 200-300 SEK)

    -- Step 5: Add mandatory components
    SET minimum-payment = floor-minimum
                        + past-due-amount
                        + overlimit-amount

    -- Step 6: Include current interest and fees (if not already in pct)
    -- Some methods: interest + fees + percentage of principal
    -- Other methods: percentage of total balance (includes interest/fees)
    -- Method depends on product configuration (domain expert to confirm)

    -- Step 7: Cap at total balance
    IF minimum-payment > new-balance
        SET minimum-payment = new-balance
    END-IF

    -- Step 8: Round to nearest whole krona (or 2 decimal places)
    ROUND minimum-payment to 2 decimal places

    RETURN minimum-payment

MINIMUM PAYMENT FORMULA (typical Swedish credit card):
    MinPayment = MAX(
        FixedMinimum,
        Balance × MinPaymentPct%
    ) + PastDueAmount + OverlimitAmount

    Where:
        FixedMinimum = Product-specific floor (e.g., 200 SEK)
        MinPaymentPct = Product-specific percentage (e.g., 2-3%)
        PastDueAmount = Any minimum payments not received from prior cycles
        OverlimitAmount = MAX(0, Balance - CreditLimit)
```

### Decision Table

| Balance | Past Due | Overlimit | Calculation | Outcome |
|---------|----------|-----------|-------------|---------|
| <= 0 | N/A | N/A | No payment due | MinPayment = 0.00 |
| > 0, <= threshold | No | No | Full balance | MinPayment = Balance |
| > threshold | No | No | MAX(Fixed, Pct × Balance) | Standard minimum |
| > threshold | Yes | No | Standard + PastDue | Includes past due |
| > threshold | No | Yes | Standard + Overlimit | Includes overlimit |
| > threshold | Yes | Yes | Standard + PastDue + Overlimit | Includes both |
| Any | Any | Any | If calc > Balance | MinPayment = Balance |

### Minimum Payment Components

| Component | Source | Description |
|-----------|--------|-------------|
| Base Percentage | Fee schedule / product config | 2-3% of outstanding balance |
| Fixed Minimum | Fee schedule / product config | Floor amount (e.g., 200 SEK) |
| Past Due Amount | Prior cycle unpaid minimums | Cumulative missed minimum payments |
| Overlimit Amount | Balance - Credit Limit | Amount by which balance exceeds limit |
| Interest and Fees | Current cycle charges | May be included in base or added separately |

## Source COBOL Reference

**Program:** Dedicated minimum payment calculation program not yet available in repository.
**Inferred from:** `CVACT01Y.cpy` (account balance, cycle accumulators, credit limit), `CBTRN02C.cbl` (credit limit enforcement and balance tracking)

The credit limit check in CBTRN02C provides the overlimit calculation:

```cobol
000403                COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                    - ACCT-CURR-CYC-DEBIT
000405                                    + DALYTRAN-AMT
000406
000407                IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408                  CONTINUE
000409                ELSE
000410                  MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411                  MOVE 'OVERLIMIT TRANSACTION'
000412                    TO WS-VALIDATION-FAIL-REASON-DESC
000413                END-IF
```
*(Lines 403-413, CBTRN02C.cbl — the overlimit detection logic computes balance vs. credit limit. The minimum payment calculation uses the same comparison to determine the overlimit component: `MAX(0, ACCT-CURR-BAL - ACCT-CREDIT-LIMIT)`.)*

The balance tracking provides the input for minimum payment calculation:

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
```
*(Line 547, CBTRN02C.cbl — ACCT-CURR-BAL is the current balance that the minimum payment percentage is applied to)*

## Acceptance Criteria

### Scenario 1: Standard minimum payment calculation

```gherkin
GIVEN an active credit account with:
  | NewBalance      | 25000.00 |
  | CreditLimit     | 50000.00 |
  | PastDueAmount   | 0.00     |
  AND the product minimum payment percentage = 2%
  AND the fixed minimum = 200 SEK
WHEN the minimum payment is calculated
THEN percentage component = 25000.00 * 2% = 500.00
  AND floor minimum = MAX(500.00, 200.00) = 500.00
  AND minimum payment = 500.00 + 0.00 (past due) + 0.00 (overlimit) = 500.00
```

### Scenario 2: Fixed minimum exceeds percentage

```gherkin
GIVEN an account with:
  | NewBalance      | 5000.00 |
  AND the product minimum payment percentage = 2%
  AND the fixed minimum = 200 SEK
WHEN the minimum payment is calculated
THEN percentage component = 5000.00 * 2% = 100.00
  AND floor minimum = MAX(100.00, 200.00) = 200.00
  AND minimum payment = 200.00
```

### Scenario 3: Minimum payment with past-due amount

```gherkin
GIVEN an account with:
  | NewBalance      | 25000.00 |
  | PastDueAmount   | 500.00   |
  | OverlimitAmount | 0.00     |
WHEN the minimum payment is calculated
THEN base minimum = 500.00 (2% of 25000)
  AND minimum payment = 500.00 + 500.00 (past due) = 1000.00
```

### Scenario 4: Minimum payment with overlimit amount

```gherkin
GIVEN an account with:
  | NewBalance      | 55000.00 |
  | CreditLimit     | 50000.00 |
  | PastDueAmount   | 0.00     |
WHEN the minimum payment is calculated
THEN base minimum = 1100.00 (2% of 55000)
  AND overlimit amount = 55000.00 - 50000.00 = 5000.00
  AND minimum payment = 1100.00 + 5000.00 = 6100.00
```

### Scenario 5: Small balance — full balance due

```gherkin
GIVEN an account with:
  | NewBalance | 150.00 |
  AND the absolute minimum threshold = 200 SEK
WHEN the minimum payment is calculated
THEN minimum payment = 150.00 (full balance, since below threshold)
```

### Scenario 6: Zero or credit balance

```gherkin
GIVEN an account with:
  | NewBalance | -500.00 |
WHEN the minimum payment is calculated
THEN minimum payment = 0.00
  AND no payment is due
```

### Scenario 7: Minimum payment capped at balance

```gherkin
GIVEN an account with:
  | NewBalance      | 300.00   |
  | PastDueAmount   | 500.00   |
WHEN the minimum payment is calculated
THEN the raw calculation would be 200.00 + 500.00 = 700.00
  AND since 700.00 > 300.00 (balance), minimum payment is capped at 300.00
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — minimum payment terms must ensure eventual repayment of credit | The percentage-based minimum (2-3% of balance) ensures the balance decreases over time. Past-due and overlimit components accelerate repayment for higher-risk accounts |
| Consumer Credit Directive | Art. 10(2)(i) | Credit agreement must specify the amount, number, and frequency of payments | The minimum payment formula is defined per product and disclosed in the credit agreement. Each statement shows the calculated minimum |
| Consumer Credit Directive | Art. 16 | Right of early repayment — consumer may repay early without penalty | The minimum payment is a floor, not a cap. Cardholders can pay any amount up to the full balance at any time without penalty |

## Edge Cases

1. **Extremely high balance**: For very high balances, the percentage-based minimum could be a substantial amount. There may be a maximum minimum payment cap to prevent unreasonable payment demands. The mainframe team must confirm whether such a cap exists.

2. **Multiple past-due cycles**: If a cardholder has missed minimum payments for multiple cycles, the past-due amount accumulates. The minimum payment for the current cycle includes all prior unpaid minimums, which can create a snowball effect.

3. **Balance transfer accounts**: Accounts with promotional balance transfer rates (0%) may have different minimum payment terms — for example, a lower percentage or fixed amount for the transferred balance.

4. **Hardship modification**: Accounts in a hardship program may have a temporarily reduced minimum payment. The calculation must check for active hardship flags and apply modified terms.

5. **Currency precision**: The minimum payment should be rounded to 2 decimal places (SEK precision). For very small percentages of large balances, rounding to whole kronor may be more customer-friendly.

6. **Minimum payment warning**: EU Consumer Credit Directive requires that statements include a warning about the cost of making only minimum payments (e.g., "If you pay only the minimum, it will take X years to repay and cost Y in interest"). This calculation depends on the minimum payment formula and current interest rate.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The minimum payment calculation program must be obtained from the mainframe team. Key questions: (1) What is the minimum payment percentage per product type (2%, 2.5%, 3%)? (2) What is the fixed minimum amount in SEK? (3) Are interest and fees included in the base percentage or added separately? (4) Is there a maximum minimum payment cap? (5) How is the past-due amount tracked — cumulative across all missed cycles or just the most recent? (6) Is the minimum payment rounded to whole kronor or 2 decimal places? (7) Is the minimum payment warning (time-to-repay calculation) generated by the same program?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
