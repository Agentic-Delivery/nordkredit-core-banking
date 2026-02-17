---
id: "lnd-br-005"
title: "Repayment processing and balance update"
domain: "lending"
cobol_source: "CBTRN02C.cbl:545-560"
requirement_id: "LND-BR-005"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "PSD2 Art. 64"
  - "PSD2 Art. 89"
  - "EU Consumer Credit Directive 2008/48/EC Art. 16"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# LND-BR-005: Repayment processing and balance update

## Summary

Repayment processing updates the loan account balance when a payment (negative transaction) is received. In the COBOL system, repayments flow through the same daily batch transaction posting process (CBTRN02C) as all other transactions. A repayment is identified by a negative transaction amount (`DALYTRAN-AMT < 0`). The program adds the negative amount to the current balance (effectively reducing it) and accumulates the payment in the cycle debit field (`ACCT-CURR-CYC-DEBIT`). The account record is then rewritten to the ACCTFILE with the updated balances.

This rule implements the payment application logic required by PSD2 Art. 89 (value dating) and the Consumer Credit Directive Art. 16 (early repayment). All repayments must be applied to the account balance accurately and atomically, with full audit trail through the posted transaction file.

## Business Logic

### Pseudocode

```
PERFORM REPAYMENT-PROCESSING (within 2000-POST-TRANSACTION):
    -- Transaction has already passed validation (1500-VALIDATE-TRAN)
    -- For repayments: DALYTRAN-AMT is negative

    -- Step 1: Update running balance
    ADD DALYTRAN-AMT TO ACCT-CURR-BAL
    -- (Adding a negative amount reduces the balance)

    -- Step 2: Classify into cycle tracking
    IF DALYTRAN-AMT >= 0
        -- This is a charge/purchase (positive)
        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
    ELSE
        -- This is a payment/repayment (negative)
        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
    END-IF

    -- Step 3: Persist updated account
    REWRITE ACCOUNT-RECORD to ACCTFILE
    IF rewrite fails (INVALID KEY)
        SET validation-fail-reason = 109
        SET fail-description = 'ACCOUNT RECORD NOT FOUND'
    END-IF

    -- Step 4: Update transaction category balance
    PERFORM 2700-UPDATE-TCATBAL

    -- Step 5: Write transaction to posted transaction file
    PERFORM 2900-WRITE-TRANSACTION-FILE
```

### Decision Table

| Transaction Amount | Classification | Balance Effect | Cycle Field Updated |
|-------------------|---------------|---------------|-------------------|
| Positive (>= 0) | Charge/Purchase | Balance increases | ACCT-CURR-CYC-CREDIT |
| Negative (< 0) | Payment/Repayment | Balance decreases | ACCT-CURR-CYC-DEBIT |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 545-560 (balance update), 424-444 (post transaction orchestration)

```cobol
000545 2800-UPDATE-ACCOUNT-REC.
000546      * Update the balances in account record to reflect posted trans.
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
000553
000554           REWRITE FD-ACCTFILE-REC FROM  ACCOUNT-RECORD
000555              INVALID KEY
000556                MOVE 109 TO WS-VALIDATION-FAIL-REASON
000557                MOVE 'ACCOUNT RECORD NOT FOUND'
000558                  TO WS-VALIDATION-FAIL-REASON-DESC
000559           END-REWRITE.
000560           EXIT.
```
*(Lines 545-560 — the core repayment balance update: adds negative transaction amount to running balance and cycle debit field, then rewrites the account record atomically)*

```cobol
000424 2000-POST-TRANSACTION.
000425           MOVE  DALYTRAN-ID            TO    TRAN-ID
000426           MOVE  DALYTRAN-TYPE-CD       TO    TRAN-TYPE-CD
000427           MOVE  DALYTRAN-CAT-CD        TO    TRAN-CAT-CD
000428           MOVE  DALYTRAN-SOURCE        TO    TRAN-SOURCE
000429           MOVE  DALYTRAN-DESC          TO    TRAN-DESC
000430           MOVE  DALYTRAN-AMT           TO    TRAN-AMT
             ...
000440           PERFORM 2700-UPDATE-TCATBAL
000441           PERFORM 2800-UPDATE-ACCOUNT-REC
000442           PERFORM 2900-WRITE-TRANSACTION-FILE
000444           EXIT.
```
*(Lines 424-444 — post transaction flow: maps daily transaction fields, updates category balance, updates account, writes to transaction file)*

```cobol
000467 2700-UPDATE-TCATBAL.
000468      * Update the balances in transaction balance file.
000469           MOVE XREF-ACCT-ID TO FD-TRANCAT-ACCT-ID
000470           MOVE DALYTRAN-TYPE-CD TO FD-TRANCAT-TYPE-CD
000471           MOVE DALYTRAN-CAT-CD TO FD-TRANCAT-CD
```
*(Lines 467-471 — transaction category balance update: tracks balances by account + transaction type + category code)*

## Acceptance Criteria

### Scenario 1: Standard repayment reduces balance

```gherkin
GIVEN a loan account with:
  | Current Balance     | 25000.00 |
  | Current Cycle Credit | 5000.00  |
  | Current Cycle Debit  | 0.00     |
WHEN a repayment of -3000.00 is processed
THEN the current balance is updated to 22000.00 (25000.00 + (-3000.00))
  AND the cycle debit is updated to -3000.00 (0.00 + (-3000.00))
  AND the cycle credit remains 5000.00 (unchanged)
  AND the account record is rewritten to the ACCTFILE
  AND the repayment transaction is written to the transaction file
```

### Scenario 2: Overpayment creates negative balance

```gherkin
GIVEN a loan account with:
  | Current Balance     | 1000.00 |
  | Current Cycle Credit | 1000.00 |
  | Current Cycle Debit  | 0.00    |
WHEN a repayment of -1500.00 is processed
THEN the current balance is updated to -500.00 (overpayment)
  AND the cycle debit is updated to -1500.00
  AND the negative balance represents a credit to the customer
```

### Scenario 3: Repayment passes credit limit check

```gherkin
GIVEN a loan account with:
  | Credit Limit        | 10000.00 |
  | Current Cycle Credit | 10000.00 |
  | Current Cycle Debit  | 0.00     |
WHEN a repayment of -5000.00 is submitted
THEN the projected balance is: 10000.00 - 0.00 + (-5000.00) = 5000.00
  AND 10000.00 >= 5000.00 is TRUE
  AND the repayment is not rejected by the overlimit check
  AND the balance is updated to reflect the payment
```

### Scenario 4: Multiple repayments accumulate in cycle debit

```gherkin
GIVEN a loan account with:
  | Current Balance     | 50000.00  |
  | Current Cycle Debit  | -2000.00  |
WHEN a repayment of -3000.00 is processed
THEN the current balance is updated to 47000.00
  AND the cycle debit is updated to -5000.00 (-2000.00 + (-3000.00))
```

### Scenario 5: Transaction category balance updated for repayment

```gherkin
GIVEN a loan account with account ID "12345678901"
WHEN a repayment with type code "PA" and category code "0001" is processed
THEN the transaction category balance file is updated for key:
  | Account ID | 12345678901 |
  | Type Code  | PA          |
  | Category   | 0001        |
  AND the category balance reflects the repayment amount
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accurate accounting records — institutions must maintain correct and up-to-date records of all financial transactions | The balance update atomically modifies the account record, and the transaction is written to the posted file for audit trail |
| PSD2 | Art. 64 | Transaction data integrity — payment service providers must ensure the integrity of payment transactions | The REWRITE operation is atomic; the transaction is classified (credit vs. debit) and tracked in both account and category balance files |
| PSD2 | Art. 89 | Value dating — the credit value date must be no later than the business day on which the amount is received | Repayments are processed in the daily batch, ensuring the balance is updated on the same business day the payment is received (within batch processing window) |
| Consumer Credit Directive | Art. 16 | Early repayment — the consumer is entitled to early repayment at any time, with a reduction in the total cost of credit | The system accepts repayments of any amount, including overpayments that exceed the balance, enabling partial and full early repayment |

## Edge Cases

1. **REWRITE failure (code 109)**: If the REWRITE fails with INVALID KEY, the validation fail reason is set to 109 but the program does NOT abend — it continues processing. This means a failed balance update could leave the transaction posted but the account balance unchanged. The migrated system must treat this as a critical error requiring rollback.

2. **Non-atomic multi-file update**: The post transaction performs three sequential file updates (category balance, account balance, transaction file) without a transaction coordinator. A failure between updates could leave files inconsistent. The migrated system should use a database transaction to ensure atomicity.

3. **Cycle debit sign convention**: Cycle debits are stored as negative values (the negative transaction amount is added to the field). This means ACCT-CURR-CYC-DEBIT accumulates negative values. The projected balance formula in the overlimit check subtracts cycle debits, which double-negates back to positive. The migrated system should clarify the sign convention.

4. **Repayment allocation order**: The COBOL system does not implement payment allocation (principal vs. interest vs. fees). All repayments reduce the current balance uniformly. Dedicated repayment allocation logic (e.g., interest first, then principal, per regulation) may exist in programs not yet obtained.

5. **Same-day balance conflict**: If multiple batch transactions update the same account (e.g., a charge and a repayment), the COBOL sequential processing ensures they are applied in order. However, the READ-COMPUTE-REWRITE pattern means each transaction reads the latest balance. The migrated system must handle concurrent access carefully.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Is there a separate repayment allocation program that splits payments into principal, interest, and fees? (2) Does the system support designated repayments (customer specifying which charges to pay)? (3) What happens when a repayment creates a negative balance — is a refund triggered automatically? (4) Is there a minimum payment enforcement mechanism (e.g., minimum monthly payment)? (5) How are returned/bounced payments handled — is there a reversal program?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
