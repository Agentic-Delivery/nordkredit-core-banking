---
id: "acct-br-007"
title: "Account credit limit enforcement"
domain: "account-management"
cobol_source: "CBTRN02C.cbl:403-413"
requirement_id: "ACCT-BR-007"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "PSD2 Art. 64"
  - "EU Consumer Credit Directive 2008/48/EC"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# ACCT-BR-007: Account credit limit enforcement

## Summary

Credit limit enforcement is performed during daily batch transaction posting (CBTRN02C.cbl) as part of the multi-step validation pipeline. Before a transaction is posted, the system calculates a projected balance by adding the transaction amount to the difference between current cycle credits and cycle debits. If this projected balance exceeds the account's credit limit, the transaction is rejected with code 102 ("OVERLIMIT TRANSACTION"). The credit limit is stored in the account master record (`ACCT-CREDIT-LIMIT` field, PIC S9(10)V99) and a separate cash credit limit (`ACCT-CASH-CREDIT-LIMIT`) exists but is not used in the available validation logic. This enforcement is the primary mechanism preventing accounts from exceeding their authorized credit ceiling.

## Business Logic

### Pseudocode

```
PERFORM CREDIT-LIMIT-CHECK (within 1500-VALIDATE-TRAN):
    -- Step 1: Calculate projected balance
    COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
                        - ACCT-CURR-CYC-DEBIT
                        + DALYTRAN-AMT

    -- Step 2: Compare against credit limit
    IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
        -- Within limit, transaction proceeds
        CONTINUE
    ELSE
        -- Over limit, reject transaction
        MOVE 102 TO WS-VALIDATION-FAIL-REASON
        MOVE 'OVERLIMIT TRANSACTION'
            TO WS-VALIDATION-FAIL-REASON-DESC
    END-IF
```

### Projected Balance Formula

```
Projected Balance = Cycle Credits - Cycle Debits + Transaction Amount

Where:
  Cycle Credits  = sum of all positive transactions this billing cycle
  Cycle Debits   = sum of all negative transactions this billing cycle (negative value)
  Transaction Amount = current transaction being validated (positive or negative)

Decision:
  IF Credit Limit >= Projected Balance → APPROVED
  IF Credit Limit <  Projected Balance → REJECTED (code 102)
```

### Decision Table

| CYC-CREDIT | CYC-DEBIT | DALYTRAN-AMT | Projected Balance | Credit Limit | Result |
|---|---|---|---|---|---|
| 5000.00 | -1000.00 | +500.00 | 6500.00 | 10000.00 | Approved (6500 <= 10000) |
| 9000.00 | -500.00 | +2000.00 | 11500.00 | 10000.00 | REJECTED (11500 > 10000) |
| 9000.00 | -500.00 | -200.00 | 9300.00 | 10000.00 | Approved (9300 <= 10000) |
| 10000.00 | 0.00 | +0.01 | 10000.01 | 10000.00 | REJECTED (10000.01 > 10000) |
| 10000.00 | 0.00 | +0.00 | 10000.00 | 10000.00 | Approved (10000 <= 10000) |
| 0.00 | 0.00 | +10000.00 | 10000.00 | 10000.00 | Approved (exactly at limit) |
| 5000.00 | -3000.00 | -500.00 | 2500.00 | 10000.00 | Approved (payment reduces projected) |

### Financial Precision

| Field | COBOL PIC | Migrated Type | Range |
|---|---|---|---|
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | decimal(12,2) | ±9,999,999,999.99 |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | decimal(12,2) | ±9,999,999,999.99 |
| DALYTRAN-AMT | S9(09)V99 | decimal(11,2) | ±999,999,999.99 |
| WS-TEMP-BAL | S9(09)V99 | decimal(11,2) | ±999,999,999.99 |
| ACCT-CREDIT-LIMIT | S9(10)V99 | decimal(12,2) | ±9,999,999,999.99 |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 403-413 (credit limit validation within 1500-B-LOOKUP-ACCT)

```cobol
000403               COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                   - ACCT-CURR-CYC-DEBIT
000405                                   + DALYTRAN-AMT
000406
000407               IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408                 CONTINUE
000409               ELSE
000410                 MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411                 MOVE 'OVERLIMIT TRANSACTION'
000412                   TO WS-VALIDATION-FAIL-REASON-DESC
000413               END-IF
```

**Context — validation order (lines 370-422):**

The credit limit check occurs after the account lookup succeeds. The validation sequence is:
1. Card number lookup in XREF file (fail code 100)
2. Account lookup by account ID (fail code 101)
3. **Credit limit check** (fail code 102)
4. Account expiration check (fail code 103)

If multiple validations fail, only the last one's code is retained (sequential overwrite).

## Acceptance Criteria

### Scenario 1: Transaction within credit limit

```gherkin
GIVEN an account with:
  | Field | Value |
  | ACCT-CREDIT-LIMIT | 10000.00 |
  | ACCT-CURR-CYC-CREDIT | 5000.00 |
  | ACCT-CURR-CYC-DEBIT | -1000.00 |
  AND a transaction amount of +500.00
WHEN the credit limit is checked
THEN projected balance = 5000.00 - (-1000.00) + 500.00 = 6500.00
  AND 10000.00 >= 6500.00 is TRUE
  AND the transaction proceeds
```

### Scenario 2: Transaction exceeds credit limit

```gherkin
GIVEN an account with:
  | Field | Value |
  | ACCT-CREDIT-LIMIT | 10000.00 |
  | ACCT-CURR-CYC-CREDIT | 9000.00 |
  | ACCT-CURR-CYC-DEBIT | -500.00 |
  AND a transaction amount of +2000.00
WHEN the credit limit is checked
THEN projected balance = 9000.00 - (-500.00) + 2000.00 = 11500.00
  AND 10000.00 >= 11500.00 is FALSE
  AND the transaction is rejected with code 102
  AND reason "OVERLIMIT TRANSACTION"
```

### Scenario 3: Transaction exactly at credit limit

```gherkin
GIVEN an account with:
  | Field | Value |
  | ACCT-CREDIT-LIMIT | 10000.00 |
  | ACCT-CURR-CYC-CREDIT | 0.00 |
  | ACCT-CURR-CYC-DEBIT | 0.00 |
  AND a transaction amount of +10000.00
WHEN the credit limit is checked
THEN projected balance = 0.00 - 0.00 + 10000.00 = 10000.00
  AND 10000.00 >= 10000.00 is TRUE
  AND the transaction proceeds (boundary: exactly at limit is approved)
```

### Scenario 4: Payment reduces projected balance

```gherkin
GIVEN an account near its credit limit
  | Field | Value |
  | ACCT-CREDIT-LIMIT | 10000.00 |
  | ACCT-CURR-CYC-CREDIT | 9800.00 |
  | ACCT-CURR-CYC-DEBIT | 0.00 |
  AND a payment (negative transaction) of -500.00
WHEN the credit limit is checked
THEN projected balance = 9800.00 - 0.00 + (-500.00) = 9300.00
  AND 10000.00 >= 9300.00 is TRUE
  AND the payment proceeds (negative amounts free up credit)
```

### Scenario 5: Sequential transactions within same batch

```gherkin
GIVEN an account with credit limit 10000.00 and initial cycle credit 0.00, cycle debit 0.00
WHEN the batch processes transactions: +8000.00, +1500.00, +1000.00
THEN +8000.00 projected = 8000.00 → approved (within limit)
  AND +1500.00 projected = 9500.00 → approved (within limit)
  AND +1000.00 projected = 10500.00 → REJECTED code 102 (exceeds limit)
  AND the rejection occurs because cycle credits were updated by prior approved transactions
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — institutions must manage credit exposure within authorized limits | The credit limit check prevents account balances from exceeding the authorized credit ceiling, maintaining credit risk within approved parameters |
| PSD2 | Art. 64 | Payment transaction validation — payment service providers must validate transactions | The overlimit check is part of the multi-step validation pipeline that ensures only valid transactions are posted |
| EU Consumer Credit Directive | 2008/48/EC Art. 10 | Credit agreements must specify credit limit and consequences of exceeding it | The ACCT-CREDIT-LIMIT field captures the agreed credit ceiling; the overlimit rejection (code 102) is the enforcement mechanism |

## Edge Cases

1. **WS-TEMP-BAL precision mismatch**: The projected balance is stored in `WS-TEMP-BAL` which is `S9(09)V99` (9 integer digits), while account fields are `S9(10)V99` (10 integer digits) and the credit limit is also `S9(10)V99`. If `CYC-CREDIT - CYC-DEBIT` exceeds 999,999,999.99, the result is truncated in WS-TEMP-BAL, potentially allowing overlimit transactions. The migrated system must use consistent precision.

2. **Negative amounts reduce projected balance**: The formula `CYC-CREDIT - CYC-DEBIT + AMT` means payment transactions (negative amounts) reduce the projected balance, freeing up credit. This is correct business logic and must be preserved.

3. **Sequential validation overwrite**: If both overlimit (102) and expired (103) conditions are true, code 103 overwrites 102 because the checks are sequential. The migrated system should capture ALL validation failures, not just the last one.

4. **Credit limit of zero**: An account with `ACCT-CREDIT-LIMIT = 0` would reject any positive transaction (projected balance > 0). This could be used for debit-only accounts or as a soft freeze mechanism.

5. **Cash credit limit unused**: The `ACCT-CASH-CREDIT-LIMIT` field exists in the account record (referenced in the .NET domain model) but is not used in the available validation logic. This may be enforced in a separate cash advance processing program not in the repository.

6. **Batch ordering affects approval**: Because cycle credits and debits are updated between transactions within the same batch, the order of transactions affects which ones are approved. A large purchase followed by a payment may exceed the limit, while the same payment followed by the purchase may not.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Is `CYC-CREDIT - CYC-DEBIT + AMT` the correct projected balance formula, or should it use ACCT-CURR-BAL + AMT? (2) Should overlimit transactions be rejected outright, or should there be a tolerance/grace amount? (3) Is the cash credit limit (ACCT-CASH-CREDIT-LIMIT) enforced in a separate program? (4) Are there credit limit increase/decrease procedures in the COBOL system? (5) Should the migrated system enforce real-time credit limit checks (not just batch), and if so, what concurrency model should be used? (6) Are there different credit limit enforcement rules for different transaction types?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
