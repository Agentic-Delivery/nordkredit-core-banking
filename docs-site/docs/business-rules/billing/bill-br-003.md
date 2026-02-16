---
id: "bill-br-003"
title: "Billing cycle balance tracking and credit/debit classification"
domain: "billing"
cobol_source: "CBTRN02C.cbl:545-560"
requirement_id: "BILL-BR-003"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 64"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# BILL-BR-003: Billing cycle balance tracking and credit/debit classification

## Summary

The billing system maintains running account balances through a cycle-based tracking mechanism implemented in `CBTRN02C.cbl`. When a transaction is posted, three balance fields are updated: the overall current balance (`ACCT-CURR-BAL`), and either the current cycle credit (`ACCT-CURR-CYC-CREDIT`) or current cycle debit (`ACCT-CURR-CYC-DEBIT`) depending on the sign of the transaction amount. This credit/debit classification is fundamental to the billing cycle — it determines how charges and payments are tracked within each billing period, directly impacting statement generation, minimum payment calculation, and interest computation. Extracted from `CBTRN02C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM 2800-UPDATE-ACCOUNT-REC:
    Step 1: Update overall balance
        ADD DALYTRAN-AMT TO ACCT-CURR-BAL
            (always, regardless of sign — accumulates net position)

    Step 2: Classify and update cycle balance
        IF DALYTRAN-AMT >= 0     (credit/charge transaction)
            ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
        ELSE                     (debit/payment transaction)
            ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
        END-IF

    Step 3: Persist updated account record
        REWRITE account record to ACCTFILE

BILLING CYCLE CONTEXT:
    - ACCT-CURR-BAL tracks the cumulative account balance
    - ACCT-CURR-CYC-CREDIT tracks total charges in the current billing cycle
    - ACCT-CURR-CYC-DEBIT tracks total payments/credits in the current billing cycle
    - At cycle close: these values are used for statement generation and interest calculation
    - Cycle reset: CYC-CREDIT and CYC-DEBIT are reset to zero at the start of each new billing cycle
        (cycle reset logic not present in available COBOL — to be confirmed by domain expert)
```

### Decision Table

| Transaction Amount | Sign | ACCT-CURR-BAL | ACCT-CURR-CYC-CREDIT | ACCT-CURR-CYC-DEBIT |
|---|---|---|---|---|
| +500.00 (purchase) | Positive | += 500.00 | += 500.00 | unchanged |
| +0.00 (zero amount) | Non-negative | += 0.00 | += 0.00 | unchanged |
| -200.00 (payment) | Negative | += -200.00 | unchanged | += -200.00 |
| -1000.00 (large payment) | Negative | += -1000.00 | unchanged | += -1000.00 |
| +0.01 (minimum charge) | Positive | += 0.01 | += 0.01 | unchanged |

### Financial Precision

| Field | COBOL PIC | Migrated Type | Notes |
|---|---|---|---|
| DALYTRAN-AMT | S9(09)V99 | decimal(11,2) | Signed transaction amount |
| ACCT-CURR-BAL | S9(10)V99 | decimal(12,2) | Running account balance (10 integer digits) |
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | decimal(12,2) | Current cycle credits |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | decimal(12,2) | Current cycle debits |

**Critical**: The `ADD` statement in COBOL performs fixed-point addition. When adding `S9(09)V99` to `S9(10)V99`, the result is stored in the larger field without overflow risk for normal transaction volumes. However, if `ACCT-CURR-BAL` approaches 9,999,999,999.99 (10 integer digits), an overflow condition occurs silently in COBOL. The migrated system should use `decimal(12,2)` for all account balance fields and add explicit overflow detection.

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 545-560 (account balance update)

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```

### Account Record Context

The account balance fields are part of the account master record (ACCTFILE), read and updated in I-O mode by `CBTRN02C.cbl`. The full account record structure is defined in `CVACT01Y.cpy` (not available in current source — referenced from CBTRN02C.cbl line ~100). Key fields include:

| Field | COBOL Name | PIC | Description |
|---|---|---|---|
| Account ID | ACCT-ID | 9(11) | Primary key |
| Current Balance | ACCT-CURR-BAL | S9(10)V99 | Running total balance |
| Credit Limit | ACCT-CREDIT-LIMIT | S9(10)V99 | Authorized credit ceiling |
| Cycle Credits | ACCT-CURR-CYC-CREDIT | S9(10)V99 | Total charges this billing cycle |
| Cycle Debits | ACCT-CURR-CYC-DEBIT | S9(10)V99 | Total payments this billing cycle |
| Expiration Date | ACCT-EXPIRAION-DATE | X(10) | Account expiry (YYYY-MM-DD) |

## Acceptance Criteria

### Scenario 1: Purchase transaction updates balance and cycle credit

```gherkin
GIVEN an account with ACCT-CURR-BAL = 1000.00
  AND ACCT-CURR-CYC-CREDIT = 1000.00
  AND ACCT-CURR-CYC-DEBIT = 0.00
WHEN a purchase transaction of +250.00 is posted
THEN ACCT-CURR-BAL = 1250.00
  AND ACCT-CURR-CYC-CREDIT = 1250.00
  AND ACCT-CURR-CYC-DEBIT = 0.00 (unchanged)
```

### Scenario 2: Payment transaction updates balance and cycle debit

```gherkin
GIVEN an account with ACCT-CURR-BAL = 1000.00
  AND ACCT-CURR-CYC-CREDIT = 1200.00
  AND ACCT-CURR-CYC-DEBIT = -200.00
WHEN a payment of -300.00 is posted
THEN ACCT-CURR-BAL = 700.00
  AND ACCT-CURR-CYC-CREDIT = 1200.00 (unchanged)
  AND ACCT-CURR-CYC-DEBIT = -500.00
```

### Scenario 3: Zero amount transaction classified as credit

```gherkin
GIVEN any account state
WHEN a transaction of +0.00 is posted
THEN 0.00 is added to ACCT-CURR-BAL (no change)
  AND 0.00 is added to ACCT-CURR-CYC-CREDIT (no change)
  AND ACCT-CURR-CYC-DEBIT is NOT updated
  AND the zero amount is classified as credit (>= 0 check)
```

### Scenario 4: Multiple transactions in same batch

```gherkin
GIVEN an account with ACCT-CURR-BAL = 0.00
  AND ACCT-CURR-CYC-CREDIT = 0.00
  AND ACCT-CURR-CYC-DEBIT = 0.00
WHEN the batch processes: +100.00, +200.00, -50.00
THEN after all three transactions:
  ACCT-CURR-BAL = 250.00
  ACCT-CURR-CYC-CREDIT = 300.00
  ACCT-CURR-CYC-DEBIT = -50.00
  AND the relationship: CURR-BAL = CYC-CREDIT + CYC-DEBIT holds (250 = 300 + (-50))
```

### Scenario 5: Account record is persisted after update

```gherkin
GIVEN a transaction has been validated and posted
WHEN the account balance is updated
THEN the account record is written back to the ACCTFILE via REWRITE
  AND the updated balances are available for subsequent transactions in the same batch
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 7 | Financial institutions must maintain accurate and complete financial records | The three-field balance tracking (total, cycle credit, cycle debit) provides a complete picture of account activity within each billing cycle |
| PSD2 | Art. 64 | Payment transaction data integrity | Each transaction updates the account balance atomically, maintaining data integrity through the REWRITE operation |
| DORA | Art. 11 | ICT risk management — data integrity requirements | The balance update sequence (overall balance + cycle classification) ensures consistent state transitions; any I/O error triggers an ABEND preventing partial updates |

## Edge Cases

1. **Balance overflow**: With `S9(10)V99`, the maximum balance is 9,999,999,999.99. If the ADD operation would exceed this, COBOL truncates silently (no exception). For high-value corporate accounts, this is a real risk. The migrated system must detect and handle overflow.

2. **Credit/debit classification of zero**: A zero-amount transaction (`DALYTRAN-AMT = 0.00`) is classified as a credit because the condition is `>= 0`. This is consistent but should be documented — zero-amount transactions (e.g., authorization-only) increment the cycle credit count without changing the balance.

3. **Cycle debit accumulates negative values**: Since negative amounts are added to `ACCT-CURR-CYC-DEBIT`, this field becomes increasingly negative. The relationship `ACCT-CURR-BAL ≈ ACCT-CURR-CYC-CREDIT + ACCT-CURR-CYC-DEBIT` holds (approximately, subject to cycle boundary effects). The migrated system should enforce this invariant.

4. **No transaction isolation within batch**: Each transaction reads and updates the account record sequentially within the batch. There is no locking between transactions — the COBOL batch has exclusive access to the file. The migrated system must use database transactions with appropriate isolation levels to prevent concurrent update anomalies.

5. **Cycle reset timing**: The available COBOL source does not show when or how cycle credits and debits are reset to zero for a new billing cycle. This is likely handled by a separate batch job (statement generation). The migrated system must identify and preserve the cycle reset process.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Confirm the invariant: does `ACCT-CURR-BAL = ACCT-CURR-CYC-CREDIT + ACCT-CURR-CYC-DEBIT` always hold, or can the overall balance include carried-over amounts from previous cycles? (2) When are cycle credits and debits reset — at statement generation, at cycle close, or another event? (3) Is there a separate batch job for cycle closing and statement generation? (4) Should the migrated system use database transactions to ensure atomicity of the balance update, or is eventual consistency acceptable?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
