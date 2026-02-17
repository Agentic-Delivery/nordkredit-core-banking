---
id: "bill-br-010"
title: "Statement generation and billing cycle close"
domain: "billing"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "BILL-BR-010"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 57"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
  - "GDPR Art. 15"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# BILL-BR-010: Statement generation and billing cycle close

## Summary

Statement generation is the billing cycle close process that produces a periodic billing statement for each credit account. The statement summarizes all transactions, fees, and interest charges during the billing cycle, calculates the new balance, determines the minimum payment due, and sets the payment due date. This batch process runs after the interest computation (BILL-BR-009) and fee assessment (BILL-BR-008) have completed for the cycle.

The dedicated COBOL statement generation program has not yet been obtained from the mainframe. This rule is inferred from the account master record fields (CVACT01Y.cpy) which include cycle-level accumulators (`ACCT-CURR-CYC-CREDIT`, `ACCT-CURR-CYC-DEBIT`), the transaction posting logic in CBTRN02C.cbl that maintains these accumulators, and the daily transaction report structure (CBTRN03C.cbl/CVTRA07Y.cpy) which provides a template for statement formatting.

Under PSD2 Art. 57, payment service providers must provide periodic statements with detailed transaction information. The EU Consumer Credit Directive Art. 10 requires that credit card statements include specific information about outstanding balances, minimum payments, and the cost of credit. GDPR Art. 15 gives customers the right to access their transaction data.

## Business Logic

### Pseudocode

```
PERFORM STATEMENT-GENERATION (monthly batch — after interest and fee posting):
    OPEN ACCOUNT-FILE (I-O)
    OPEN TRANSACT-FILE (INPUT)
    OPEN STATEMENT-FILE (OUTPUT)

    PERFORM UNTIL end-of-accounts
        READ next ACCOUNT-RECORD

        -- Step 1: Check if cycle close date is today
        IF today NOT = ACCT-CYCLE-CLOSE-DATE
            CONTINUE
        END-IF
        IF ACCT-ACTIVE-STATUS NOT = 'Y'
            CONTINUE
        END-IF

        -- Step 2: Gather cycle transactions
        READ all transactions for ACCT-ID
            WHERE TRAN-PROC-TS between cycle-start and cycle-end

        -- Step 3: Calculate statement balances
        SET previous-balance = ACCT-PREV-STMT-BAL
        SET payments-credits = ACCT-CURR-CYC-CREDIT
        SET purchases-debits = ACCT-CURR-CYC-DEBIT
        SET interest-charged = cycle-interest-total (from BILL-BR-009)
        SET fees-charged = cycle-fees-total (from BILL-BR-008)

        SET new-balance = previous-balance
                        - payments-credits
                        + purchases-debits
                        + interest-charged
                        + fees-charged

        -- Step 4: Calculate minimum payment (see BILL-BR-012)
        PERFORM CALCULATE-MINIMUM-PAYMENT
            USING new-balance, interest-charged, fees-charged
            RETURNING minimum-payment-due

        -- Step 5: Set payment due date
        SET payment-due-date = cycle-close-date + payment-grace-days
            (typically 21-25 days after statement date)

        -- Step 6: Generate statement record
        WRITE STATEMENT-RECORD:
            Statement Date        = today
            Account Number        = ACCT-ID
            Previous Balance      = previous-balance
            Payments/Credits      = payments-credits
            Purchases/Debits      = purchases-debits
            Interest Charged      = interest-charged
            Fees Charged          = fees-charged
            New Balance           = new-balance
            Minimum Payment Due   = minimum-payment-due
            Payment Due Date      = payment-due-date
            Credit Limit          = ACCT-CREDIT-LIMIT
            Available Credit      = ACCT-CREDIT-LIMIT - new-balance
            Transaction Details   = [list of cycle transactions]

        -- Step 7: Roll cycle accumulators
        MOVE new-balance TO ACCT-PREV-STMT-BAL
        MOVE 0 TO ACCT-CURR-CYC-CREDIT
        MOVE 0 TO ACCT-CURR-CYC-DEBIT
        ADVANCE cycle-close-date to next month
        REWRITE ACCOUNT-RECORD
    END-PERFORM
```

### Statement Content Requirements

| Section | Content | Regulation |
|---------|---------|------------|
| Account Summary | Previous balance, payments, new charges, new balance | PSD2 Art. 57 |
| Transaction Details | Date, description, amount for each transaction | PSD2 Art. 57(1) |
| Interest Summary | Rate applied per category, interest amount per category, total interest | Consumer Credit Directive Art. 10 |
| Fee Summary | Each fee type and amount charged during cycle | PSD2 Art. 45 |
| Payment Information | Minimum payment due, payment due date, payment instructions | Consumer Credit Directive Art. 10 |
| Credit Information | Credit limit, available credit, overlimit status | Consumer Credit Directive Art. 10 |
| Regulatory Notices | Late payment warning, minimum payment warning | Consumer Credit Directive Art. 10 |

### Cycle Accumulator Fields (from CVACT01Y.cpy)

| Field | COBOL Name | Purpose |
|-------|-----------|---------|
| Current Balance | ACCT-CURR-BAL | Running total of all debits and credits |
| Cycle Credits | ACCT-CURR-CYC-CREDIT | Sum of payments and credits this cycle |
| Cycle Debits | ACCT-CURR-CYC-DEBIT | Sum of purchases, fees, and interest this cycle |
| Previous Statement Balance | ACCT-PREV-STMT-BAL | Balance at last cycle close (inferred field) |

## Source COBOL Reference

**Program:** Dedicated statement generation program not yet available in repository.
**Inferred from:** `CVACT01Y.cpy` (cycle accumulators), `CBTRN02C.cbl` (balance tracking), `CBTRN03C.cbl` (report generation pattern), `CVTRA07Y.cpy` (report layout structures)

The cycle accumulators are maintained by the transaction posting logic:

```cobol
000545       2800-UPDATE-ACCOUNT-REC.
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```
*(Lines 545-552, CBTRN02C.cbl — the cycle credit/debit accumulators are the foundation of the statement balance calculation. At cycle close, these are summarized on the statement and then reset to zero.)*

The report generation pattern from CBTRN03C provides the template for statement formatting:

```cobol
       SELECT REPORT-FILE ASSIGN TO TRANREPT
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS  IS TRANREPT-STATUS.
```
*(Line 51, CBTRN03C.cbl — sequential report file output. The statement generation would follow a similar pattern but write per-account statement records rather than a consolidated report.)*

## Acceptance Criteria

### Scenario 1: Standard billing cycle close

```gherkin
GIVEN an active account with cycle close date of 2026-03-15 and:
  | PreviousStatementBalance | 10000.00 |
  | CycleCredits (payments)  | 5000.00  |
  | CycleDebits (purchases)  | 3000.00  |
  | InterestCharged          | 125.50   |
  | FeesCharged              | 0.00     |
WHEN the statement generation batch runs on 2026-03-15
THEN new balance = 10000.00 - 5000.00 + 3000.00 + 125.50 + 0.00 = 8125.50
  AND a statement is generated with new balance 8125.50
  AND payment due date is set to 2026-04-08 (24 days after statement)
  AND the cycle accumulators are reset to 0.00
  AND ACCT-PREV-STMT-BAL is updated to 8125.50
```

### Scenario 2: Statement with zero balance

```gherkin
GIVEN an active account at cycle close with:
  | PreviousStatementBalance | 5000.00 |
  | CycleCredits (payments)  | 5000.00 |
  | CycleDebits              | 0.00    |
  | InterestCharged          | 0.00    |
WHEN the statement generation batch runs
THEN new balance = 0.00
  AND a statement is generated showing zero balance
  AND minimum payment due = 0.00
  AND no payment due date is set (or marked as "N/A")
```

### Scenario 3: Statement includes all transaction details

```gherkin
GIVEN an account with 15 transactions during the billing cycle
  AND the transactions include purchases, payments, fees, and interest
WHEN the statement is generated
THEN all 15 transactions appear in the transaction detail section
  AND each transaction shows: date, description, amount
  AND transactions are sorted by date
  AND the total matches the cycle debit/credit accumulators
```

### Scenario 4: Cycle accumulator rollover

```gherkin
GIVEN an account at cycle close with:
  | ACCT-CURR-CYC-CREDIT | 5000.00  |
  | ACCT-CURR-CYC-DEBIT  | 3125.50  |
WHEN the statement is generated and the cycle is closed
THEN ACCT-PREV-STMT-BAL is set to the new balance
  AND ACCT-CURR-CYC-CREDIT is reset to 0.00
  AND ACCT-CURR-CYC-DEBIT is reset to 0.00
  AND the next cycle close date is advanced by one month
```

### Scenario 5: Credit balance on statement

```gherkin
GIVEN an account where payments exceed the previous balance:
  | PreviousStatementBalance | 3000.00 |
  | CycleCredits (payments)  | 5000.00 |
  | CycleDebits              | 0.00    |
WHEN the statement is generated
THEN new balance = -2000.00 (credit balance)
  AND the statement indicates a credit balance of 2000.00
  AND minimum payment due = 0.00
  AND no interest is charged on a credit balance
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 7 | Financial reporting — periodic account statements must be accurate and complete | The statement summarizes all cycle activity with verified accumulators matching posted transactions |
| PSD2 | Art. 57 | Information on individual payment transactions — statement must include transaction reference, amount, date, and payee | Transaction detail section includes all required PSD2 fields for each transaction in the cycle |
| Consumer Credit Directive | Art. 10(2) | Credit agreement information — statements must show outstanding balance, interest, minimum payment, and repayment warnings | Statement includes all required elements: balance, interest breakdown, minimum payment, and regulatory warnings |
| GDPR | Art. 15 | Right of access — data subjects can request information about their personal data processing | Statement data is structured and retrievable, enabling customer access requests. Statement archive supports right of access |

## Edge Cases

1. **Cycle close on weekend/holiday**: If the cycle close date falls on a non-business day, the statement may be generated on the next business day. Transactions posted on the weekend may fall into the current or next cycle depending on the cutoff logic.

2. **No transactions in cycle**: An account with no activity during the cycle still generates a statement if there is an outstanding balance (interest continues to accrue). Zero-balance, zero-activity accounts may skip statement generation.

3. **Disputed transactions**: Transactions under dispute may need special handling on the statement — shown as "in dispute" and potentially excluded from the interest calculation and minimum payment.

4. **Statement delivery method**: The COBOL system generates a flat-file statement record. The migrated system must support multiple delivery methods: physical mail (print file), electronic statement (PDF), and digital banking portal. GDPR requires secure delivery of statements containing personal financial data.

5. **Multi-currency accounts**: If an account has transactions in multiple currencies, the statement must show both the original currency amount and the SEK equivalent. Currency conversion rates and dates must be included per PSD2 requirements.

6. **Retroactive adjustments**: If a transaction is reversed or adjusted after the statement is generated, a subsequent statement must include the adjustment. The COBOL system may issue a credit memo or simply include the reversal in the next cycle.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated statement generation program must be obtained from the mainframe team. Key questions: (1) What is the batch job name and processing sequence (does it run after interest and fee posting)? (2) What is the payment grace period — 21 or 25 days after statement date? (3) How are cycle dates determined — fixed day of month per account, or rolling? (4) What regulatory warning text is included on credit card statements? (5) Is there a separate statement archive/history file? (6) How are paper vs. electronic statement preferences tracked?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
