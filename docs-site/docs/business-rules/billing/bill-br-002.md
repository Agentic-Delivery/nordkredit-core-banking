---
id: "bill-br-002"
title: "Credit limit enforcement and overlimit handling"
domain: "billing"
cobol_source: "CBTRN02C.cbl:370-420"
requirement_id: "BILL-BR-002"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "PSD2 Art. 64"
  - "EU Consumer Credit Directive 2008/48/EC"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# BILL-BR-002: Credit limit enforcement and overlimit handling

## Summary

The billing system enforces credit limits during the daily batch transaction posting process in `CBTRN02C.cbl`. Before posting a transaction, the system calculates a projected balance by combining the current cycle credits, current cycle debits, and the incoming transaction amount. If the projected balance exceeds the account's credit limit, the transaction is rejected with reason code 102 ("OVERLIMIT TRANSACTION"). This is a critical billing control that prevents accounts from exceeding their authorized credit, directly impacting the bank's credit risk exposure. Extracted from `CBTRN02C.cbl`.

## Business Logic

### Pseudocode

```
CREDIT LIMIT CHECK (performed in 1500-B-LOOKUP-ACCT):
    INPUT:
        ACCT-CURR-CYC-CREDIT  (current billing cycle credits)
        ACCT-CURR-CYC-DEBIT   (current billing cycle debits)
        DALYTRAN-AMT           (incoming transaction amount)
        ACCT-CREDIT-LIMIT     (authorized credit limit)

    Step 1: Calculate projected balance
        WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
                    - ACCT-CURR-CYC-DEBIT
                    + DALYTRAN-AMT

    Step 2: Compare against credit limit
        IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
            Transaction passes credit limit check
        ELSE
            SET fail reason = 102
            SET fail description = "OVERLIMIT TRANSACTION"
            Transaction is REJECTED
        END-IF

NOTES:
    - Projected balance represents net exposure (credits minus debits plus new amount)
    - Negative transaction amounts (debits/payments) REDUCE the projected balance
    - Check is performed BEFORE the transaction is posted
    - Check is sequential: if the overlimit check fails but the expired check (103) also fails,
      the expired reason overwrites the overlimit reason
```

### Decision Table

| Cycle Credit | Cycle Debit | Transaction Amount | Projected Balance | Credit Limit | Result |
|---|---|---|---|---|---|
| 1000.00 | 200.00 | 500.00 | 1300.00 | 5000.00 | PASS (1300 <= 5000) |
| 4500.00 | 200.00 | 800.00 | 5100.00 | 5000.00 | REJECT code 102 (5100 > 5000) |
| 3000.00 | 1000.00 | 2500.00 | 4500.00 | 5000.00 | PASS (4500 <= 5000) |
| 5000.00 | 0.00 | 0.01 | 5000.01 | 5000.00 | REJECT code 102 (5000.01 > 5000) |
| 5000.00 | 0.00 | -500.00 | 4500.00 | 5000.00 | PASS (payment reduces balance) |
| 0.00 | 0.00 | 100.00 | 100.00 | 0.00 | REJECT code 102 (any amount exceeds zero limit) |

### Financial Precision

| Field | COBOL PIC | Migrated Type | Notes |
|---|---|---|---|
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | decimal(12,2) | Current cycle credit total |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | decimal(12,2) | Current cycle debit total |
| DALYTRAN-AMT | S9(09)V99 | decimal(11,2) | Incoming transaction amount |
| WS-TEMP-BAL | S9(09)V99 | decimal(11,2) | **WARNING**: Smaller than account fields |
| ACCT-CREDIT-LIMIT | S9(10)V99 | decimal(12,2) | Authorized credit limit |

**Critical**: The working storage variable `WS-TEMP-BAL` uses `S9(09)V99` (9 integer digits) while account balance fields use `S9(10)V99` (10 integer digits). This creates a truncation risk: if `CYC-CREDIT - CYC-DEBIT` exceeds 999,999,999.99, the projected balance calculation will truncate. The migrated system should use `decimal(12,2)` for all intermediate calculations to avoid this limitation.

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 370-420 (validation), specifically 398-420 (credit limit and expiration checks)

```cobol
000403               COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                   - ACCT-CURR-CYC-DEBIT
000405                                   + DALYTRAN-AMT
000407               IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408                 CONTINUE
000409               ELSE
000410                 MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411                 MOVE 'OVERLIMIT TRANSACTION'
000412                   TO WS-VALIDATION-FAIL-REASON-DESC
000413               END-IF
```

### Reject Record Format

When a transaction is rejected for overlimit:

| Field | Position | Length | Value |
|---|---|---|---|
| Original transaction | 1-350 | 350 | Complete DALYTRAN-RECORD |
| Fail reason code | 351-354 | 4 | 102 |
| Fail reason description | 355-430 | 76 | "OVERLIMIT TRANSACTION" |

## Acceptance Criteria

### Scenario 1: Transaction within credit limit

```gherkin
GIVEN an account with credit limit 5000.00
  AND current cycle credit of 1000.00
  AND current cycle debit of 200.00
WHEN a transaction of 500.00 is processed
THEN the projected balance is 1300.00
  AND 1300.00 <= 5000.00
  AND the transaction passes the credit limit check
```

### Scenario 2: Transaction exceeding credit limit

```gherkin
GIVEN an account with credit limit 5000.00
  AND current cycle credit of 4500.00
  AND current cycle debit of 200.00
WHEN a transaction of 800.00 is processed
THEN the projected balance is 5100.00
  AND 5100.00 > 5000.00
  AND the transaction is rejected with code 102
  AND the rejection description is "OVERLIMIT TRANSACTION"
```

### Scenario 3: Payment (negative amount) reduces projected balance

```gherkin
GIVEN an account with credit limit 5000.00
  AND current cycle credit of 5000.00
  AND current cycle debit of 0.00
WHEN a payment of -500.00 is processed
THEN the projected balance is 4500.00
  AND 4500.00 <= 5000.00
  AND the transaction passes the credit limit check
```

### Scenario 4: Transaction exactly at credit limit boundary

```gherkin
GIVEN an account with credit limit 5000.00
  AND current cycle credit of 4999.99
  AND current cycle debit of 0.00
WHEN a transaction of 0.01 is processed
THEN the projected balance is 5000.00
  AND 5000.00 <= 5000.00 (equal is allowed)
  AND the transaction passes the credit limit check
```

### Scenario 5: Transaction one cent over credit limit

```gherkin
GIVEN an account with credit limit 5000.00
  AND current cycle credit of 5000.00
  AND current cycle debit of 0.00
WHEN a transaction of 0.01 is processed
THEN the projected balance is 5000.01
  AND 5000.01 > 5000.00
  AND the transaction is rejected with code 102
```

### Scenario 6: Overlimit combined with expired account

```gherkin
GIVEN an account that is both over the credit limit AND expired
WHEN a transaction is validated
THEN the fail reason is 103 (expired) because the expiration check runs AFTER the overlimit check
  AND the overlimit reason (102) is overwritten
  AND only one rejection reason is recorded per transaction
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 6 | Credit institutions must manage credit risk exposure | Credit limit enforcement prevents accounts from exceeding authorized credit, controlling the bank's aggregate credit risk |
| PSD2 | Art. 64 | Payment transaction integrity and accuracy | Pre-posting validation ensures only authorized transactions within agreed limits are processed |
| EU Consumer Credit Directive | 2008/48/EC Art. 10(2)(e) | Credit agreements must specify the credit limit | The ACCT-CREDIT-LIMIT field enforces the contractually agreed credit limit |

## Edge Cases

1. **Truncation risk in projected balance**: The `WS-TEMP-BAL` field (`S9(09)V99`) is smaller than the account balance fields (`S9(10)V99`). For accounts with cycle credits exceeding 999,999,999.99, the COMPUTE statement will silently truncate, potentially causing incorrect overlimit decisions. The migrated system must use consistent precision for all fields in this calculation.

2. **Sequential validation overwrites**: The overlimit check (code 102) runs before the expiration check (code 103). If both conditions are true, only code 103 is recorded. The migrated system should capture all validation failures, not just the last one.

3. **Zero credit limit**: An account with ACCT-CREDIT-LIMIT = 0 will reject any positive transaction. This may be intentional (frozen account) or an error. The migrated system should distinguish between intentionally zero-limited accounts and data errors.

4. **Negative projected balance**: When large debits (payments) exceed credits, the projected balance becomes negative. The system correctly handles this — a negative balance is always less than the credit limit, so payments are never rejected for overlimit.

5. **Batch processing order**: The order in which transactions are processed within a daily batch affects which transactions are rejected. If two transactions together exceed the limit but each individually would pass, only the second will be rejected. The processing order is determined by the sequential file order, which may not match chronological order.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Is the formula `CYC-CREDIT - CYC-DEBIT + AMT` the correct projected balance calculation, or should it be `CURR-BAL + AMT`? (2) Should the migrated system capture all validation failures instead of only the last one? (3) What is the business process for handling the WS-TEMP-BAL truncation risk? (4) Are there any overlimit tolerance thresholds (e.g., allow 5% over limit)? (5) How should transaction ordering within a daily batch be determined in the migrated system?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
