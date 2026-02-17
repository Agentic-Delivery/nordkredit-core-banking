---
id: "acct-br-004"
title: "Account balance management and cycle tracking"
domain: "account-management"
cobol_source: "CBTRN02C.cbl:545-560"
requirement_id: "ACCT-BR-004"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 64"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# ACCT-BR-004: Account balance management and cycle tracking

## Summary

Account balance management is the most financially critical business rule in the system. The daily batch posting program (CBTRN02C) maintains three balance fields per account: the running current balance (`ACCT-CURR-BAL`), the current billing cycle credit total (`ACCT-CURR-CYC-CREDIT`), and the current billing cycle debit total (`ACCT-CURR-CYC-DEBIT`). Every posted transaction updates the current balance and classifies the amount into either cycle credit or cycle debit based on the sign of the transaction amount. This three-field tracking mechanism enables statement generation, interest calculation, and minimum payment computation. The balance update is performed via REWRITE to the ACCTFILE opened in I-O mode.

## Business Logic

### Pseudocode

```
PERFORM 2800-UPDATE-ACCOUNT-REC:
    -- Step 1: Update running balance (always, regardless of sign)
    ADD DALYTRAN-AMT TO ACCT-CURR-BAL

    -- Step 2: Classify into billing cycle bucket
    IF DALYTRAN-AMT >= 0    (purchase/charge — positive amount)
        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
    ELSE                     (payment/credit — negative amount)
        ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
    END-IF

    -- Step 3: Persist updated record
    REWRITE ACCOUNT-RECORD TO ACCTFILE

BALANCE INVARIANT:
    ACCT-CURR-BAL ≈ ACCT-CURR-CYC-CREDIT + ACCT-CURR-CYC-DEBIT
    (approximately equal — may diverge at billing cycle boundaries
     if carry-forward amounts exist from previous cycles)
```

### Decision Table

| Transaction Amount | Sign | ACCT-CURR-BAL Update | ACCT-CURR-CYC-CREDIT Update | ACCT-CURR-CYC-DEBIT Update |
|---|---|---|---|---|
| +500.00 (purchase) | Positive | += 500.00 | += 500.00 | unchanged |
| +0.00 (zero amount) | Non-negative | += 0.00 | += 0.00 | unchanged |
| -200.00 (payment) | Negative | += -200.00 | unchanged | += -200.00 |
| -1000.00 (large payment) | Negative | += -1000.00 | unchanged | += -1000.00 |
| +0.01 (minimum charge) | Positive | += 0.01 | += 0.01 | unchanged |

### Financial Precision

| Field | COBOL PIC | Migrated Type | Range | Notes |
|---|---|---|---|---|
| DALYTRAN-AMT | S9(09)V99 | decimal(11,2) | ±999,999,999.99 | Input transaction amount |
| ACCT-CURR-BAL | S9(10)V99 | decimal(12,2) | ±9,999,999,999.99 | Running account balance |
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | decimal(12,2) | ±9,999,999,999.99 | Current cycle charges |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | decimal(12,2) | ±9,999,999,999.99 | Current cycle payments |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 545-560 (account balance update paragraph)

```cobol
000545 2800-UPDATE-ACCOUNT-REC.
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```

**Context — balance fields read from ACCTFILE (I-O mode):**

The ACCTFILE is opened in I-O mode by CBTRN02C.cbl, allowing both READ and REWRITE operations. The account record is read during validation (paragraph 1500-B-LOOKUP-ACCT), and the balance fields are updated and written back during posting (paragraph 2800-UPDATE-ACCOUNT-REC).

**Credit limit pre-check (lines 403-407):**

```cobol
000403               COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                   - ACCT-CURR-CYC-DEBIT
000405                                   + DALYTRAN-AMT
000407               IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
```

## Acceptance Criteria

### Scenario 1: Purchase transaction updates balance and cycle credit

```gherkin
GIVEN an account with:
  | Field | Value |
  | ACCT-CURR-BAL | 1000.00 |
  | ACCT-CURR-CYC-CREDIT | 1000.00 |
  | ACCT-CURR-CYC-DEBIT | 0.00 |
WHEN a purchase transaction of +250.00 is posted
THEN ACCT-CURR-BAL = 1250.00
  AND ACCT-CURR-CYC-CREDIT = 1250.00
  AND ACCT-CURR-CYC-DEBIT = 0.00 (unchanged)
  AND the account record is written back via REWRITE
```

### Scenario 2: Payment transaction updates balance and cycle debit

```gherkin
GIVEN an account with:
  | Field | Value |
  | ACCT-CURR-BAL | 1000.00 |
  | ACCT-CURR-CYC-CREDIT | 1200.00 |
  | ACCT-CURR-CYC-DEBIT | -200.00 |
WHEN a payment of -300.00 is posted
THEN ACCT-CURR-BAL = 700.00
  AND ACCT-CURR-CYC-CREDIT = 1200.00 (unchanged)
  AND ACCT-CURR-CYC-DEBIT = -500.00
```

### Scenario 3: Zero amount classified as credit

```gherkin
GIVEN any account state
WHEN a transaction of +0.00 is posted
THEN ACCT-CURR-BAL is unchanged
  AND ACCT-CURR-CYC-CREDIT is unchanged (0.00 added)
  AND ACCT-CURR-CYC-DEBIT is NOT updated
  AND the zero amount follows the >= 0 path (classified as credit)
```

### Scenario 4: Multiple transactions in same batch preserve order

```gherkin
GIVEN an account with all balances at 0.00
WHEN the batch sequentially processes: +100.00, +200.00, -50.00
THEN after all three transactions:
  | Field | Value |
  | ACCT-CURR-BAL | 250.00 |
  | ACCT-CURR-CYC-CREDIT | 300.00 |
  | ACCT-CURR-CYC-DEBIT | -50.00 |
  AND the invariant CURR-BAL = CYC-CREDIT + CYC-DEBIT holds (250 = 300 + (-50))
```

### Scenario 5: Account record persisted after each transaction

```gherkin
GIVEN a transaction has been validated and posted
WHEN the account balance update completes
THEN the account record is written back to ACCTFILE via REWRITE
  AND the updated balances are available for the next transaction in the batch
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Financial institutions must maintain accurate accounting records | Three-field balance tracking provides a complete picture of account activity: net position (CURR-BAL), charges (CYC-CREDIT), and payments (CYC-DEBIT) |
| FSA FFFS 2014:5 | Ch. 7 | Financial system accuracy and completeness | Fixed-point COBOL arithmetic ensures no floating-point errors in balance calculations; the migrated system must use decimal types |
| PSD2 | Art. 64 | Payment transaction data integrity — amounts must be processed accurately | Each transaction updates the balance atomically via REWRITE, maintaining data integrity |
| DORA | Art. 11 | ICT risk management — data integrity and system resilience | Any I/O error during REWRITE triggers an ABEND, preventing partial balance updates that could leave the account in an inconsistent state |

## Edge Cases

1. **Balance overflow**: With `S9(10)V99`, the maximum balance is ±9,999,999,999.99. COBOL truncates silently on overflow without raising an exception. For high-value corporate accounts approaching this limit, transactions could silently corrupt the balance. The migrated system must use `decimal(12,2)` and add explicit overflow detection.

2. **Credit/debit classification of zero**: A zero-amount transaction (`DALYTRAN-AMT = 0.00`) is classified as credit because the condition is `>= 0`. This is consistent but should be documented — zero-amount transactions (e.g., authorization-only) follow the credit path without changing any balance.

3. **Negative cycle debit accumulation**: Since negative amounts are added to `ACCT-CURR-CYC-DEBIT`, this field becomes increasingly negative over the billing cycle. The relationship `ACCT-CURR-BAL ≈ CYC-CREDIT + CYC-DEBIT` holds. The migrated system should enforce this invariant.

4. **No transaction isolation within batch**: Each transaction reads and updates the account record sequentially within the COBOL batch. There is no locking between transactions because the batch has exclusive file access. The migrated system must use database transactions with appropriate isolation levels (e.g., SERIALIZABLE or row-level locking) to prevent concurrent update anomalies.

5. **Cycle reset timing unknown**: The available COBOL source does not show when cycle credits and debits are reset for a new billing cycle. This is likely handled by a separate statement generation batch job not in the repository. The migrated system must identify and preserve the cycle reset mechanism.

6. **Working storage precision mismatch**: `WS-TEMP-BAL` uses `S9(09)V99` (9 integer digits) while account fields use `S9(10)V99` (10 integer digits). This means the credit limit check could truncate for accounts with very large balances. The migrated system should use consistent precision for all intermediate calculations.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Does the invariant `ACCT-CURR-BAL = CYC-CREDIT + CYC-DEBIT` always hold, or can the balance include carried-over amounts from previous cycles? (2) When and how are cycle credits and debits reset to zero — at statement generation, at cycle close, or another event? (3) Is there a separate batch job for billing cycle closing? If so, what is the program name? (4) Should the migrated system use database transactions to ensure atomicity of the balance update, or is the current sequential-in-batch approach sufficient? (5) Can multiple batch runs process the same account concurrently (e.g., intraday vs nightly)?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
