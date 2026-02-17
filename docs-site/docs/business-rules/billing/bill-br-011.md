---
id: "bill-br-011"
title: "Late payment detection and penalty processing"
domain: "billing"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "BILL-BR-011"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "FSA FFFS 2014:5 Ch. 7"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
  - "PSD2 Art. 83"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-011: Late payment detection and penalty processing

## Summary

Late payment handling detects when a cardholder fails to make the minimum payment by the payment due date and applies appropriate penalties. The process involves checking whether the minimum payment (BILL-BR-012) has been received by the due date, assessing late payment fees per the fee schedule (BILL-BR-008), and potentially increasing the applicable interest rate to a penalty rate. Persistent late payments trigger delinquency staging and may result in account suspension or collections referral.

The dedicated COBOL late payment processing program has not yet been obtained from the mainframe. This rule is inferred from the account master fields (payment due date, cycle accumulators), the fee schedule structure, and Swedish/EU regulatory requirements. The account expiration and rejection logic in CBTRN02C.cbl (lines 414-420, extracted as BILL-BR-005) demonstrates the pattern for account-level enforcement that the late payment program would follow.

Under the EU Consumer Credit Directive Art. 10, credit agreements must specify the charges payable in the event of late payments. PSD2 Art. 83 requires timely execution of payment transactions. FSA FFFS 2014:5 Ch. 6 requires credit institutions to manage credit risk through effective delinquency monitoring.

## Business Logic

### Pseudocode

```
PERFORM LATE-PAYMENT-DETECTION (daily batch — runs after payment posting):
    OPEN ACCOUNT-FILE (I-O)
    OPEN FEE-SCHEDULE-FILE (INPUT)
    OPEN DELINQUENCY-FILE (I-O)

    PERFORM UNTIL end-of-accounts
        READ next ACCOUNT-RECORD

        -- Step 1: Skip accounts not past due
        IF ACCT-ACTIVE-STATUS NOT = 'Y'
            CONTINUE
        END-IF
        IF today < payment-due-date
            CONTINUE
        END-IF
        IF ACCT-CURR-BAL <= 0
            CONTINUE (credit balance — nothing owed)
        END-IF

        -- Step 2: Check if minimum payment was received
        SET payments-since-stmt = total payments received since last statement
        SET minimum-payment-due = last-statement-minimum-payment

        IF payments-since-stmt >= minimum-payment-due
            -- Payment received — account is current
            IF account was previously late
                RESET late-payment-count if brought current
                REVERT interest rate from penalty to standard (if applicable)
            END-IF
            CONTINUE
        END-IF

        -- Step 3: Account is past due — assess late fee
        SET days-past-due = today - payment-due-date

        IF days-past-due = 1 (first day past due)
            -- Assess late payment fee (once per cycle)
            READ FEE-SCHEDULE-FILE using ACCT-GROUP-ID
            SET late-fee = fee-schedule.LATE-PAYMENT-FEE-AMT

            -- Cap late fee per regulation (cannot exceed minimum payment)
            IF late-fee > minimum-payment-due
                SET late-fee = minimum-payment-due
            END-IF

            ADD late-fee TO ACCT-CURR-BAL
            WRITE fee-transaction (type='LP', amount=late-fee)
            INCREMENT late-payment-count
        END-IF

        -- Step 4: Apply penalty interest rate (if applicable)
        IF late-payment-count >= penalty-rate-threshold
            SET account-rate-override = penalty-APR
            -- Penalty rate applies to entire balance until brought current
        END-IF

        -- Step 5: Update delinquency status
        EVALUATE TRUE
            WHEN days-past-due >= 1 AND < 30
                SET delinquency-stage = 'LATE'
                -- Send payment reminder notification
            WHEN days-past-due >= 30 AND < 60
                SET delinquency-stage = 'DELINQUENT-30'
                -- Send formal notice
            WHEN days-past-due >= 60 AND < 90
                SET delinquency-stage = 'DELINQUENT-60'
                -- Restrict new transactions
            WHEN days-past-due >= 90
                SET delinquency-stage = 'DELINQUENT-90'
                -- Flag for collections (see lending BILL-BR parallel)
                -- Report to credit bureaus
        END-EVALUATE

        WRITE/UPDATE DELINQUENCY-RECORD
        REWRITE ACCOUNT-RECORD
    END-PERFORM
```

### Decision Table

| Payment Status | Days Past Due | Action | Fee | Rate Impact |
|---------------|---------------|--------|-----|-------------|
| Minimum payment received | N/A | Account current — no action | None | Standard rate |
| Partial payment (< minimum) | 0 | Grace — check again tomorrow | None | Standard rate |
| No payment | 1 | Assess late fee, send reminder | Late fee | Standard rate |
| No payment | 30+ | First delinquency notice | None (already assessed) | May trigger penalty rate |
| No payment | 60+ | Restrict new transactions | None | Penalty rate |
| No payment | 90+ | Collections referral, credit bureau report | None | Penalty rate |

### Late Fee Caps

| Condition | Maximum Late Fee |
|-----------|-----------------|
| First late payment | Lesser of fee-schedule amount or minimum payment due |
| Subsequent late payment (same cycle) | No additional late fee within same cycle |
| Balance under threshold (e.g., < 100 SEK) | No late fee if balance is minimal |
| Regulatory cap | Per Swedish consumer credit regulations |

## Source COBOL Reference

**Program:** Dedicated late payment processing program not yet available in repository.
**Inferred from:** `CVACT01Y.cpy` (account balance and cycle fields), `CBTRN02C.cbl` (transaction validation and rejection pattern), fee schedule structure

The account expiration enforcement pattern demonstrates the validation approach:

```cobol
000414                IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                  CONTINUE
000416                ELSE
000417                  MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                  MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                    TO WS-VALIDATION-FAIL-REASON-DESC
000420                END-IF
```
*(Lines 414-420, CBTRN02C.cbl — the late payment system would use a similar date comparison pattern: if payment-due-date has passed and minimum payment not received, trigger late payment processing)*

The fee posting follows the transaction posting pattern:

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```
*(Lines 547-552, CBTRN02C.cbl — late payment fees are posted as debit transactions, increasing the account balance)*

## Acceptance Criteria

### Scenario 1: Late fee assessment on first day past due

```gherkin
GIVEN an active credit account with:
  | PaymentDueDate    | 2026-03-10 |
  | MinimumPaymentDue | 500.00     |
  | PaymentsReceived  | 0.00       |
  AND the fee schedule late payment fee = 350.00 SEK
WHEN the late payment batch runs on 2026-03-11
THEN a late fee of 350.00 is posted to the account
  AND the account balance increases by 350.00
  AND a late fee transaction record is written with type 'LP'
  AND a payment reminder notification is generated
```

### Scenario 2: Late fee capped at minimum payment

```gherkin
GIVEN an account with:
  | MinimumPaymentDue  | 200.00 |
  | LateFeeSchedule    | 350.00 |
WHEN the late fee is calculated
THEN the late fee is capped at 200.00 (cannot exceed minimum payment)
  AND 200.00 is posted to the account
```

### Scenario 3: Payment received — account brought current

```gherkin
GIVEN an account flagged as 'LATE' with 15 days past due
  AND a payment of 500.00 is received (meeting the minimum payment)
WHEN the late payment batch runs after the payment posts
THEN the account status is updated to 'CURRENT'
  AND the delinquency record is cleared
  AND if penalty rate was applied, rate reverts to standard
```

### Scenario 4: Delinquency escalation at 30 days

```gherkin
GIVEN an account with no payment received
  AND the payment due date was 31 days ago
WHEN the late payment batch runs
THEN the delinquency stage is updated to 'DELINQUENT-30'
  AND a formal delinquency notice is generated
  AND no additional late fee is charged (already assessed at day 1)
```

### Scenario 5: Account restriction at 60 days

```gherkin
GIVEN an account with no payment received for 60+ days
WHEN the late payment batch runs
THEN the delinquency stage is updated to 'DELINQUENT-60'
  AND new transactions are restricted (account blocked for new purchases)
  AND existing recurring transactions may continue per cardholder agreement
```

### Scenario 6: No late fee on minimal balance

```gherkin
GIVEN an account with:
  | CurrentBalance    | 50.00  |
  | MinimumPaymentDue | 50.00  |
  AND the fee schedule late fee = 350.00
WHEN the late fee is calculated
THEN no late fee is assessed (balance below threshold)
  AND a payment reminder is still sent
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — institutions must monitor and manage delinquent exposures | The delinquency staging system classifies accounts by days past due and triggers escalating management actions |
| FSA FFFS 2014:5 | Ch. 7 | Financial systems — penalty charges must be accurately recorded | Each late fee generates a separate transaction record with type code and amount for audit trail |
| Consumer Credit Directive | Art. 10(2)(l) | Credit agreement must specify charges payable in event of late payment | Late payment fees are defined in the fee schedule per product group and disclosed in the credit agreement |
| PSD2 | Art. 83 | Execution time — obligations regarding timely payment processing | The daily late payment batch ensures timely detection of missed payments, enabling prompt customer notification |

## Edge Cases

1. **Payment received on due date**: A payment posted on the due date (before end-of-day cutoff) should be treated as on-time. The cutoff time for same-day payment must be defined (typically end of business day).

2. **Weekend/holiday due dates**: If the payment due date falls on a non-business day, payment received on the next business day should be treated as on-time per standard Swedish banking practice.

3. **Partial payment**: If the cardholder pays more than zero but less than the minimum payment, the system may still assess a late fee but should reduce the delinquency severity. Some products treat partial payment (>50% of minimum) as mitigating the penalty.

4. **Multiple late payments**: A cardholder who is late on multiple consecutive cycles has an escalating delinquency. The late fee is assessed once per cycle, but the penalty rate and delinquency stage compound across cycles.

5. **Hardship programs**: The bank may offer hardship programs that temporarily waive late fees and reduce interest rates. The late payment system must check for active hardship flags before assessing penalties.

6. **Dispute-related late payment**: If a cardholder disputes a transaction and withholds payment pending resolution, the late payment should be suspended for the disputed amount per consumer protection regulations.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated late payment processing program must be obtained from the mainframe team. Key questions: (1) What is the batch job name and run sequence (before or after daily transaction posting)? (2) What is the current late payment fee per product type? (3) Is there a penalty APR, and how many late payments trigger it? (4) What is the delinquency escalation timeline (30/60/90 or different)? (5) Is there an automated account blocking mechanism at 60 days? (6) How are payment due dates adjusted for weekends/holidays? (7) Is there a minimum balance threshold below which no late fee is charged?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
