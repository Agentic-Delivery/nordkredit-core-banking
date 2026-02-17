---
id: "dep-br-005"
title: "Term deposit maturity and renewal processing"
domain: "deposits"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "DEP-BR-005"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "EU Deposit Guarantee Directive 2014/49/EU"
  - "PSD2 Art. 57"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# DEP-BR-005: Term deposit maturity and renewal processing

## Summary

Term deposits (bundna insättningar) have a fixed maturity date after which the deposited funds become available for withdrawal or are automatically renewed. The maturity processing batch runs nightly to identify term deposits reaching their maturity date. Based on the account's renewal instructions (automatic renewal, payout to linked account, or manual decision), the system takes the appropriate action. The ACCT-EXPIRAION-DATE field in the account master (CVACT01Y.cpy) stores the maturity date, and CBTRN02C.cbl already enforces transaction restrictions on accounts past their expiration date.

The dedicated COBOL program for term deposit maturity and renewal is not yet available in the repository. This rule is inferred from the account expiration date field usage in CBTRN02C.cbl and Swedish regulatory requirements for term deposits.

## Business Logic

### Pseudocode

```
PERFORM NIGHTLY-MATURITY-PROCESSING:
    OPEN ACCOUNT-FILE (I-O)
    OPEN TRANSACT-FILE (OUTPUT)

    PERFORM UNTIL END-OF-ACCOUNT-FILE
        READ next ACCOUNT-RECORD

        -- Step 1: Identify term deposits at maturity
        IF ACCT-ACTIVE-STATUS = 'Y'
          AND ACCT-EXPIRAION-DATE <= TODAY
          AND ACCT-EXPIRAION-DATE NOT = SPACES
          AND account-type = TERM-DEPOSIT
            PERFORM PROCESS-MATURITY
        END-IF
    END-PERFORM

PERFORM PROCESS-MATURITY:
    -- Step 2: Calculate final interest up to maturity date
    PERFORM CALCULATE-FINAL-INTEREST
    ADD final-accrued-interest TO ACCT-CURR-BAL

    -- Step 3: Apply renewal instructions
    EVALUATE RENEWAL-INSTRUCTION
        WHEN 'AUTO-RENEW'
            -- Renew at current rate for same term
            SET new-maturity = maturity-date + original-term
            MOVE new-maturity TO ACCT-EXPIRAION-DATE
            LOOKUP new disclosure group rate
            MOVE new-group-id TO ACCT-GROUP-ID (if rate changed)
            REWRITE ACCOUNT-RECORD
            LOG renewal confirmation

        WHEN 'PAYOUT'
            -- Transfer funds to linked demand deposit account
            PERFORM TRANSFER-TO-LINKED-ACCOUNT
            SET ACCT-ACTIVE-STATUS = 'N'
            SET ACCT-CURR-BAL = 0
            REWRITE ACCOUNT-RECORD
            LOG payout confirmation

        WHEN 'HOLD' OR SPACES
            -- Hold funds, notify customer for manual decision
            FLAG account for customer notification
            -- Account remains active but no new term starts
            LOG maturity notification
    END-EVALUATE
```

### Decision Table

| At Maturity | Renewal Instruction | Outcome |
|-------------|-------------------|---------|
| Matured | Auto-renew | New term starts, maturity date extended, rate may update |
| Matured | Payout | Funds transferred to linked account, term deposit closed |
| Matured | Hold/Manual | Funds held, customer notified, no new term |
| Not matured | N/A | Skip — term not yet reached |
| No maturity date | N/A | Skip — demand deposit, not term |

## Source COBOL Reference

**Program:** Dedicated maturity processing batch program — not yet in repository.

The account expiration date enforcement is visible in the existing batch processing:

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                 CONTINUE
000416               ELSE
000417                 MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                 MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                    TO WS-VALIDATION-FAIL-REASON-DESC
000420                END-IF
```
*(Lines 414-420, CBTRN02C.cbl — account expiration check during transaction validation. For term deposits, this prevents deposits and withdrawals after the maturity date until the renewal/payout decision is processed.)*

## Acceptance Criteria

### Scenario 1: Term deposit matures and auto-renews

```gherkin
GIVEN a 12-month term deposit with:
  | CurrentBalance  | 200000.00 |
  | ExpirationDate  | 2026-02-17 |
  | RenewalInstr    | AUTO-RENEW |
  | AccruedInterest | 3500.00    |
WHEN the nightly maturity processing runs on 2026-02-17
THEN accrued interest of 3500.00 is posted to the balance
  AND CurrentBalance becomes 203500.00
  AND ExpirationDate is extended to 2027-02-17
  AND the disclosure group rate is updated to the current 12-month rate
  AND a renewal confirmation record is logged
```

### Scenario 2: Term deposit matures and pays out

```gherkin
GIVEN a 6-month term deposit with:
  | CurrentBalance  | 100000.00 |
  | ExpirationDate  | 2026-02-17 |
  | RenewalInstr    | PAYOUT |
  | LinkedAccount   | 12345678901 |
  | AccruedInterest | 1250.00 |
WHEN the nightly maturity processing runs on 2026-02-17
THEN accrued interest of 1250.00 is posted to the balance
  AND 101250.00 is transferred to linked account 12345678901
  AND the term deposit account balance becomes 0.00
  AND ActiveStatus is set to 'N'
  AND a payout transaction is recorded
```

### Scenario 3: Term deposit matures with no renewal instruction

```gherkin
GIVEN a term deposit with:
  | CurrentBalance | 50000.00 |
  | ExpirationDate | 2026-02-17 |
  | RenewalInstr   | HOLD |
WHEN the nightly maturity processing runs on 2026-02-17
THEN the funds remain in the account
  AND the customer is notified of maturity
  AND no new term is started
  AND the account remains active for manual decision
```

### Scenario 4: Transactions blocked on matured term deposit

```gherkin
GIVEN a term deposit that matured on 2026-02-15
  AND the renewal instruction is HOLD
WHEN a new transaction with date 2026-02-17 is submitted
THEN the transaction is rejected with code 103
  AND the rejection reason is "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — maturity events must be accurately recorded and funds properly accounted for | The maturity processing ensures proper handling of principal and accrued interest at term end, with full audit trail of renewal or payout decisions |
| Deposit Guarantee Directive | 2014/49/EU Art. 6 | Eligible deposits — term deposit balances including accrued interest must be accurately tracked for guarantee purposes | Final interest calculation at maturity ensures the guaranteed amount reflects the complete deposit including all earned interest |
| PSD2 | Art. 57 | Information on transactions — customers must receive information about account changes | Customer notifications for maturity events (renewal, payout, hold) ensure transparency of account status changes |

## Edge Cases

1. **Weekend/holiday maturity**: If the maturity date falls on a weekend or Swedish bank holiday (e.g., Midsommarafton), the system must decide whether to process on the prior business day or the next. The mainframe team must confirm the convention.

2. **Rate change at renewal**: Auto-renewed term deposits may receive a different interest rate than the original term. The system must look up the current rate for the product/term at renewal time, not carry forward the old rate.

3. **Partial withdrawal at maturity**: Some term deposit products may allow partial renewal (e.g., renew 80%, payout 20%). The COBOL system's approach to partial maturity processing must be clarified.

4. **Early withdrawal penalty**: If a customer requests early withdrawal before maturity, the system must apply the contractually specified penalty (typically reduced interest rate or flat fee). The penalty calculation program is not available.

5. **Linked account validation**: For payout instructions, the linked demand deposit account must be valid and active. If the linked account is closed or dormant, the payout fails and the funds must be held.

6. **Maturity date calculation for month-end**: A 6-month term starting Jan 31 — does it mature on Jul 31 or Jul 30? End-of-month convention must be clarified.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated term deposit maturity program must be obtained from the mainframe team. Key questions: (1) Where are renewal instructions stored — in the account record or a separate file? (2) What term lengths are supported (3, 6, 12, 24 months)? (3) Is there an early withdrawal penalty, and how is it calculated? (4) How are weekend/holiday maturities handled? (5) Can term deposits be partially renewed/partially paid out? (6) Is there a grace period after maturity before the auto-renewal takes effect?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
