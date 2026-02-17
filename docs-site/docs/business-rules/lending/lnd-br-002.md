---
id: "lnd-br-002"
title: "Credit limit enforcement and overlimit detection"
domain: "lending"
cobol_source: "CBTRN02C.cbl:393-421"
requirement_id: "LND-BR-002"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
  - "PSD2 Art. 64"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# LND-BR-002: Credit limit enforcement and overlimit detection

## Summary

The credit limit enforcement rule is the primary lending risk control in the system. During daily batch transaction posting (CBTRN02C), each transaction is validated against the account's authorized credit limit before posting. The system computes a projected balance by taking the current cycle credits, subtracting current cycle debits, and adding the pending transaction amount. If this projected balance exceeds the account's credit limit, the transaction is rejected with validation failure code 102 ("OVERLIMIT TRANSACTION") and written to the daily rejects file. This is a hard limit — there is no tolerance margin or soft overlimit allowance in the current COBOL implementation.

This rule implements the core credit risk management requirement under FSA FFFS 2014:5 Chapter 6, ensuring that credit exposures do not exceed authorized limits. The validation occurs during batch processing, meaning overlimit detection is not real-time but operates on a daily cycle.

## Business Logic

### Pseudocode

```
PERFORM CREDIT-LIMIT-CHECK:
    -- Step 1: Look up card cross-reference to find account
    READ XREF-FILE using DALYTRAN-CARD-NUM
    IF card not found
        SET validation-fail-reason = 100
        SET fail-description = 'INVALID CARD NUMBER FOUND'
        EXIT (reject transaction)
    END-IF

    -- Step 2: Look up account record
    READ ACCOUNT-FILE using XREF-ACCT-ID
    IF account not found
        SET validation-fail-reason = 101
        SET fail-description = 'ACCOUNT RECORD NOT FOUND'
        EXIT (reject transaction)
    END-IF

    -- Step 3: Compute projected balance after transaction
    COMPUTE projected-balance = current-cycle-credit
                              - current-cycle-debit
                              + transaction-amount

    -- Step 4: Compare against credit limit
    IF credit-limit >= projected-balance
        CONTINUE (transaction passes credit check)
    ELSE
        SET validation-fail-reason = 102
        SET fail-description = 'OVERLIMIT TRANSACTION'
    END-IF

    -- Step 5: Check account expiration
    IF account-expiration-date >= transaction-date
        CONTINUE (account is not expired)
    ELSE
        SET validation-fail-reason = 103
        SET fail-description = 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
    END-IF
```

### Decision Table

| Condition | Card Found | Account Found | Within Credit Limit | Account Not Expired | Outcome |
|-----------|-----------|---------------|--------------------|--------------------|---------|
| All valid | Yes | Yes | Yes | Yes | Transaction posted |
| Invalid card | No | N/A | N/A | N/A | Rejected (code 100) |
| No account | Yes | No | N/A | N/A | Rejected (code 101) |
| Overlimit | Yes | Yes | No | Yes | Rejected (code 102) |
| Expired | Yes | Yes | Yes | No | Rejected (code 103) |
| Overlimit + Expired | Yes | Yes | No | No | Rejected (code 102, first failure wins) |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 370-421 (validation), 193-234 (main processing loop)

```cobol
001500 1500-VALIDATE-TRAN.
000371           PERFORM 1500-A-LOOKUP-XREF.
000372           IF WS-VALIDATION-FAIL-REASON = 0
000373              PERFORM 1500-B-LOOKUP-ACCT
000374           ELSE
000375              CONTINUE
000376           END-IF
000377      * ADD MORE VALIDATIONS HERE
000378           EXIT.
```
*(Lines 370-378 — validation dispatcher: first validates card cross-reference, then validates account if card is valid)*

```cobol
000380 1500-A-LOOKUP-XREF.
000382           MOVE DALYTRAN-CARD-NUM TO FD-XREF-CARD-NUM
000383           READ XREF-FILE INTO CARD-XREF-RECORD
000384              INVALID KEY
000385                MOVE 100 TO WS-VALIDATION-FAIL-REASON
000386                MOVE 'INVALID CARD NUMBER FOUND'
000387                  TO WS-VALIDATION-FAIL-REASON-DESC
000388              NOT INVALID KEY
000390                  CONTINUE
000391           END-READ
000392           EXIT.
```
*(Lines 380-392 — card cross-reference validation: checks that the card number exists in the XREF file)*

```cobol
000393 1500-B-LOOKUP-ACCT.
000394           MOVE XREF-ACCT-ID TO FD-ACCT-ID
000395           READ ACCOUNT-FILE INTO ACCOUNT-RECORD
000396              INVALID KEY
000397                MOVE 101 TO WS-VALIDATION-FAIL-REASON
000398                MOVE 'ACCOUNT RECORD NOT FOUND'
000399                  TO WS-VALIDATION-FAIL-REASON-DESC
000400              NOT INVALID KEY
000403                COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                    - ACCT-CURR-CYC-DEBIT
000405                                    + DALYTRAN-AMT
000407                IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408                  CONTINUE
000409                ELSE
000410                  MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411                  MOVE 'OVERLIMIT TRANSACTION'
000412                    TO WS-VALIDATION-FAIL-REASON-DESC
000413                END-IF
000414                IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                  CONTINUE
000416                ELSE
000417                  MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                  MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                    TO WS-VALIDATION-FAIL-REASON-DESC
000420                END-IF
000421           END-READ
```
*(Lines 393-421 — the core credit limit enforcement: computes projected balance and compares against ACCT-CREDIT-LIMIT)*

```cobol
000202           PERFORM UNTIL END-OF-FILE = 'Y'
000208                     MOVE 0 TO WS-VALIDATION-FAIL-REASON
000209                     MOVE SPACES TO WS-VALIDATION-FAIL-REASON-DESC
000210                     PERFORM 1500-VALIDATE-TRAN
000211                     IF WS-VALIDATION-FAIL-REASON = 0
000212                       PERFORM 2000-POST-TRANSACTION
000213                     ELSE
000214                       ADD 1 TO WS-REJECT-COUNT
000215                       PERFORM 2500-WRITE-REJECT-REC
000216                     END-IF
```
*(Lines 202-216 — main loop: validates each daily transaction and either posts or rejects it)*

## Acceptance Criteria

### Scenario 1: Transaction within credit limit is posted

```gherkin
GIVEN a loan account with:
  | Credit Limit        | 10000.00 |
  | Current Cycle Credit | 3000.00  |
  | Current Cycle Debit  | 1000.00  |
WHEN a transaction of 5000.00 is submitted
THEN the projected balance is calculated as 3000.00 - 1000.00 + 5000.00 = 7000.00
  AND 10000.00 >= 7000.00 is TRUE
  AND the transaction is posted successfully
```

### Scenario 2: Transaction exceeding credit limit is rejected

```gherkin
GIVEN a loan account with:
  | Credit Limit        | 10000.00 |
  | Current Cycle Credit | 8000.00  |
  | Current Cycle Debit  | 500.00   |
WHEN a transaction of 3000.00 is submitted
THEN the projected balance is calculated as 8000.00 - 500.00 + 3000.00 = 10500.00
  AND 10000.00 >= 10500.00 is FALSE
  AND the transaction is rejected with code 102
  AND the rejection reason is "OVERLIMIT TRANSACTION"
  AND the transaction is written to the daily rejects file
```

### Scenario 3: Transaction exactly at credit limit is posted

```gherkin
GIVEN a loan account with:
  | Credit Limit        | 10000.00 |
  | Current Cycle Credit | 5000.00  |
  | Current Cycle Debit  | 0.00     |
WHEN a transaction of 5000.00 is submitted
THEN the projected balance is calculated as 5000.00 - 0.00 + 5000.00 = 10000.00
  AND 10000.00 >= 10000.00 is TRUE (boundary: equal is allowed)
  AND the transaction is posted successfully
```

### Scenario 4: Negative transaction (payment) always passes credit check

```gherkin
GIVEN a loan account with:
  | Credit Limit        | 10000.00  |
  | Current Cycle Credit | 10000.00  |
  | Current Cycle Debit  | 0.00      |
WHEN a payment (negative transaction) of -2000.00 is submitted
THEN the projected balance is calculated as 10000.00 - 0.00 + (-2000.00) = 8000.00
  AND 10000.00 >= 8000.00 is TRUE
  AND the payment is posted successfully
```

### Scenario 5: Overlimit check uses cycle balances, not current balance

```gherkin
GIVEN a loan account with:
  | Current Balance     | 15000.00  |
  | Credit Limit        | 10000.00  |
  | Current Cycle Credit | 2000.00   |
  | Current Cycle Debit  | 0.00      |
WHEN a transaction of 7000.00 is submitted
THEN the projected balance uses CYCLE values: 2000.00 - 0.00 + 7000.00 = 9000.00
  AND 10000.00 >= 9000.00 is TRUE
  AND the transaction is posted (even though current balance exceeds credit limit)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — institutions must have policies and procedures for managing credit risk, including setting and enforcing credit limits | The overlimit detection prevents credit exposure from exceeding the authorized limit by rejecting transactions that would breach the ceiling |
| Consumer Credit Directive | Art. 10(2)(d) | Credit agreement must specify total amount of credit and conditions governing drawdown | The credit limit field defines the total authorized credit; the enforcement logic ensures drawdowns (transactions) do not exceed this amount |
| PSD2 | Art. 64 | Transaction data integrity — payment service providers must ensure integrity of payment transactions | The validation ensures only authorized transactions (within credit limits) are posted to the transaction file; rejected transactions are written to a separate rejects file with audit trail |

## Edge Cases

1. **Batch-only enforcement**: The overlimit check runs only during daily batch posting (CBTRN02C). There is no real-time overlimit check in the online CICS programs. A customer could theoretically exceed their limit through multiple same-day transactions that are individually within limits but collectively exceed it. The migrated system should consider real-time credit limit enforcement.

2. **Validation order dependency**: The COBOL code checks credit limit BEFORE checking account expiration (lines 407-420). If both conditions fail, only code 102 (overlimit) is recorded because code 103 overwrites it. The last validation failure wins. The migrated system should collect all failures, not just the last one.

3. **Cycle balance vs. current balance**: The projected balance formula uses `ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + transaction`, NOT `ACCT-CURR-BAL + transaction`. This means the overlimit check operates on the current billing cycle's net utilization, not the running account balance. If there is a carried-over balance from a previous cycle, it is not included in the overlimit calculation.

4. **Zero credit limit**: If ACCT-CREDIT-LIMIT is zero, any positive transaction will be rejected as overlimit. The COBOL code does not have a special case for zero limits. The migrated system must decide whether a zero limit means "credit frozen" or is an error condition.

5. **Cash advance limit not checked**: The ACCT-CASH-CREDIT-LIMIT field exists in the account record but is NOT checked in CBTRN02C. The available COBOL source does not differentiate between purchase transactions and cash advances for limit enforcement. Dedicated cash advance validation logic may exist in programs not yet obtained from the mainframe.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Is the cycle-based overlimit check intentional, or should the running balance (ACCT-CURR-BAL) be used instead? (2) Is there a separate real-time overlimit check in CICS online programs not yet obtained? (3) Are there soft overlimit allowances or grace amounts for certain account types? (4) How is the cash advance limit (ACCT-CASH-CREDIT-LIMIT) enforced — is there a separate validation program? (5) What happens when an overlimit transaction is rejected — is the customer notified in real-time or only through the daily reject report?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
