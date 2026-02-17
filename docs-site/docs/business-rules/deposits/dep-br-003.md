---
id: "dep-br-003"
title: "Deposit posting and balance update"
domain: "deposits"
cobol_source: "CBTRN02C.cbl:545-560"
requirement_id: "DEP-BR-003"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "PSD2 Art. 64"
  - "PSD2 Art. 89"
  - "GDPR Art. 5(1)(d)"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# DEP-BR-003: Deposit posting and balance update

## Summary

The deposit posting rule governs how deposit transactions (credits) and withdrawals (debits) are applied to the account balance during daily batch processing. In CBTRN02C.cbl, after a transaction passes validation, the 2800-UPDATE-ACCOUNT-REC paragraph updates the account master record: the transaction amount is added to ACCT-CURR-BAL, and then classified as either a cycle credit (positive amount = deposit inflow) or cycle debit (negative amount = withdrawal outflow). The updated account record is then rewritten to the VSAM account master file.

For deposit accounts, this rule is the core mechanism for maintaining accurate balances. Unlike lending accounts where positive transactions represent charges, for deposit accounts positive transactions represent deposits (money in) and negative transactions represent withdrawals (money out). The same COBOL code handles both account types through the signed amount convention.

## Business Logic

### Pseudocode

```
PERFORM UPDATE-DEPOSIT-ACCOUNT (2800-UPDATE-ACCOUNT-REC):
    -- Step 1: Update running balance
    ADD transaction-amount TO ACCT-CURR-BAL

    -- Step 2: Classify into cycle credit or debit
    IF transaction-amount >= 0
        -- Deposit (inflow): add to cycle credit
        ADD transaction-amount TO ACCT-CURR-CYC-CREDIT
    ELSE
        -- Withdrawal (outflow): add to cycle debit
        ADD transaction-amount TO ACCT-CURR-CYC-DEBIT
    END-IF

    -- Step 3: Persist updated record
    REWRITE ACCOUNT-RECORD to ACCOUNT-FILE
    IF rewrite fails (INVALID KEY)
        SET validation-fail-reason = 109
        SET fail-description = 'ACCOUNT RECORD NOT FOUND'
    END-IF
```

### Decision Table

| Transaction Amount | Balance Update | Cycle Credit | Cycle Debit | Outcome |
|-------------------|---------------|-------------|-------------|---------|
| Positive (deposit) | BAL + amount | CYC_CR + amount | No change | Deposit posted, balance increases |
| Zero | BAL + 0 | CYC_CR + 0 | No change | No-op (classified as credit) |
| Negative (withdrawal) | BAL + amount (decreases) | No change | CYC_DB + amount | Withdrawal posted, balance decreases |

### Data Flow

```
DALYTRAN-FILE (input)
    |
    v
1500-VALIDATE-TRAN (card/account lookup, expiration check)
    |
    v [passes validation]
2000-POST-TRANSACTION
    |---> 2700-UPDATE-TCATBAL (category balance tracking)
    |---> 2800-UPDATE-ACCOUNT-REC (deposit balance update) <-- THIS RULE
    |---> 2900-WRITE-TRANSACTION-FILE (posted transaction record)
```

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 545-560 (account balance update), 424-444 (post-transaction orchestration)

```cobol
000545       2800-UPDATE-ACCOUNT-REC.
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
*(Lines 545-560 — the complete deposit/withdrawal balance update logic. Transaction amount is added to current balance, then classified as cycle credit or cycle debit. The REWRITE persists the updated record back to the indexed VSAM file.)*

```cobol
000424       2000-POST-TRANSACTION.
000425           MOVE  DALYTRAN-ID            TO    TRAN-ID
000426           MOVE  DALYTRAN-TYPE-CD       TO    TRAN-TYPE-CD
000427           MOVE  DALYTRAN-CAT-CD        TO    TRAN-CAT-CD
000440           PERFORM 2700-UPDATE-TCATBAL
000441           PERFORM 2800-UPDATE-ACCOUNT-REC
000442           PERFORM 2900-WRITE-TRANSACTION-FILE
```
*(Lines 424-442 — post-transaction orchestration showing the three-step sequence: update category balance, update account balance, write transaction record. For deposit accounts, all three steps execute atomically within the batch run.)*

## Acceptance Criteria

### Scenario 1: Deposit (positive amount) increases balance

```gherkin
GIVEN a deposit account with:
  | CurrentBalance | 50000.00 |
  | CycleCredit    | 10000.00 |
  | CycleDebit     | 2000.00  |
WHEN a deposit transaction of 5000.00 is posted
THEN CurrentBalance becomes 55000.00
  AND CycleCredit becomes 15000.00
  AND CycleDebit remains 2000.00
```

### Scenario 2: Withdrawal (negative amount) decreases balance

```gherkin
GIVEN a deposit account with:
  | CurrentBalance | 50000.00 |
  | CycleCredit    | 10000.00 |
  | CycleDebit     | 2000.00  |
WHEN a withdrawal transaction of -3000.00 is posted
THEN CurrentBalance becomes 47000.00
  AND CycleCredit remains 10000.00
  AND CycleDebit becomes 5000.00
```

### Scenario 3: Zero amount transaction is classified as credit

```gherkin
GIVEN a deposit account with:
  | CurrentBalance | 50000.00 |
  | CycleCredit    | 10000.00 |
  | CycleDebit     | 2000.00  |
WHEN a transaction of 0.00 is posted
THEN CurrentBalance remains 50000.00
  AND CycleCredit remains 10000.00 (0 added)
  AND CycleDebit remains 2000.00
```

### Scenario 4: Account rewrite failure during balance update

```gherkin
GIVEN a deposit transaction has passed validation
WHEN the account record REWRITE fails with INVALID KEY
THEN error code 109 is set
  AND the error description is "ACCOUNT RECORD NOT FOUND"
  AND the batch processing continues (no ABEND)
```

### Scenario 5: Large deposit maintains precision

```gherkin
GIVEN a deposit account with CurrentBalance = 9999999990.00
WHEN a deposit of 9.99 is posted
THEN CurrentBalance becomes exactly 9999999999.99
  AND no precision loss occurs (DECIMAL(12,2) handles maximum value)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — institutions must maintain accurate and up-to-date records of all account balances | The balance update atomically applies each transaction to the running balance, maintaining an accurate real-time record of the deposit position |
| PSD2 | Art. 64 | Transaction data integrity — payment service providers must ensure the integrity and authentication of payment transactions | Each deposit/withdrawal updates both the running balance and the cycle tracking fields, providing dual-entry verification of transaction integrity |
| PSD2 | Art. 89 | Value dating — the credit value date shall be no later than the business day on which the amount is received | Deposit transactions are posted during the daily batch with the transaction's origination timestamp, ensuring value dating alignment |
| GDPR | Art. 5(1)(d) | Accuracy — personal data must be kept accurate and up to date | Balance fields are updated atomically for each transaction, and the cycle credit/debit classification provides an audit trail for accuracy verification |

## Edge Cases

1. **Batch-only balance updates**: The COBOL system updates deposit balances only during daily batch processing (CBTRN02C). Deposits made through online channels are recorded in the daily transaction file but not reflected in the account balance until the batch runs. The migrated system should consider real-time balance updates for deposits.

2. **REWRITE failure handling**: If the REWRITE fails (INVALID KEY on line 554), error code 109 is set but there is no explicit rollback of the category balance update (2700) that already occurred. This means the TCATBAL file could be out of sync with the ACCOUNT file. The migrated system must use database transactions to ensure atomicity.

3. **Concurrent access**: The COBOL batch opens the ACCOUNT-FILE in I-O mode (line 311). If an online CICS program attempts to read the account during batch processing, there may be contention. The migrated system must use optimistic concurrency control.

4. **Withdrawal exceeding balance**: The COBOL code does not check whether a withdrawal (negative amount) would bring the deposit balance below zero. For lending accounts, the credit limit check occurs separately. For deposit accounts, this means the batch could post a withdrawal that results in a negative balance. The migrated system should enforce minimum balance constraints for deposit accounts.

5. **Cycle reset timing**: The cycle credit and cycle debit fields accumulate throughout a billing/statement cycle. The reset mechanism (zeroing cycle fields at cycle end) is not in CBTRN02C — it likely occurs in a separate batch program. The mainframe team must clarify the cycle reset logic.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) For deposit accounts, is there a separate minimum balance check before posting withdrawals? (2) Are there withdrawal frequency limits (e.g., Regulation D equivalent in Sweden for savings accounts)? (3) When are cycle credit/debit fields reset — at statement generation or on a fixed calendar cycle? (4) Is there real-time balance checking in online CICS programs for deposit transactions, or is it strictly batch? (5) How are interest postings recorded — as regular transactions through the daily file or through a separate batch process?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
