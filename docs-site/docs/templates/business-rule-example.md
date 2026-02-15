---
id: "ACCT-BR-001"
title: "Account withdrawal requires sufficient balance and active status"
domain: "account-management"
cobol_source: "CBACT01C.cbl:120-145"
requirement_id: "ACCT-BR-001"
regulations:
  - "PSD2 Art. 97"
  - "FFFS 2014:5 Ch. 4 §3"
status: "validated"
validated_by: "Erik Lindström"
validated_date: "2026-02-10"
priority: "critical"
---

# ACCT-BR-001: Account withdrawal requires sufficient balance and active status

## Summary

A withdrawal from a customer account may only be processed if the account status is active and the available balance is sufficient to cover the withdrawal amount. This rule prevents overdrafts on standard accounts and ensures that frozen, closed, or suspended accounts cannot be debited. It is a core safety check executed on every withdrawal transaction in the online banking and teller systems.

## Business Logic

### Pseudocode

```
PERFORM VALIDATE-WITHDRAWAL:
    READ account record from DB2 using account-id
    IF account-status NOT = 'ACTIVE' THEN
        SET error-code = 'ACCT-INACTIVE'
        SET error-message = 'Account is not in active status'
        PERFORM ERROR-HANDLER
        EXIT
    END-IF

    COMPUTE available-balance = current-balance - hold-amount

    IF available-balance < withdrawal-amount THEN
        SET error-code = 'INSUFF-FUNDS'
        SET error-message = 'Insufficient available balance'
        PERFORM ERROR-HANDLER
        EXIT
    END-IF

    COMPUTE new-balance = current-balance - withdrawal-amount
    UPDATE account SET balance = new-balance
    WRITE transaction-log (account-id, withdrawal-amount, timestamp, teller-id)
    COMMIT
```

### Decision Table

| Account Status | Available Balance >= Amount | Outcome |
|---------------|---------------------------|---------|
| ACTIVE        | Yes                       | Withdrawal processed, balance updated |
| ACTIVE        | No                        | Rejected: insufficient funds (INSUFF-FUNDS) |
| FROZEN        | Yes                       | Rejected: account inactive (ACCT-INACTIVE) |
| FROZEN        | No                        | Rejected: account inactive (ACCT-INACTIVE) |
| CLOSED        | Yes                       | Rejected: account inactive (ACCT-INACTIVE) |
| CLOSED        | No                        | Rejected: account inactive (ACCT-INACTIVE) |
| SUSPENDED     | Yes                       | Rejected: account inactive (ACCT-INACTIVE) |
| SUSPENDED     | No                        | Rejected: account inactive (ACCT-INACTIVE) |

## Source COBOL Reference

**Program:** `CBACT01C.cbl`
**Lines:** 120-145

```cobol
000120 2000-VALIDATE-WITHDRAWAL.
000121     READ ACCOUNT-MASTER INTO WS-ACCOUNT-REC
000122         KEY IS WS-ACCT-ID
000123         INVALID KEY
000124             MOVE 'ACCT-NOT-FOUND' TO WS-ERROR-CODE
000125             PERFORM 9000-ERROR-HANDLER
000126             GO TO 2000-EXIT
000127     END-READ.
000128
000129     IF WS-ACCT-STATUS NOT = 'A'
000130         MOVE 'ACCT-INACTIVE' TO WS-ERROR-CODE
000131         MOVE 'Account is not in active status'
000132             TO WS-ERROR-MSG
000133         PERFORM 9000-ERROR-HANDLER
000134         GO TO 2000-EXIT
000135     END-IF.
000136
000137     COMPUTE WS-AVAIL-BAL =
000138         WS-CURRENT-BAL - WS-HOLD-AMT.
000139
000140     IF WS-AVAIL-BAL < WS-WITHDRAWAL-AMT
000141         MOVE 'INSUFF-FUNDS' TO WS-ERROR-CODE
000142         MOVE 'Insufficient available balance'
000143             TO WS-ERROR-MSG
000144         PERFORM 9000-ERROR-HANDLER
000145         GO TO 2000-EXIT
000146     END-IF.
000147
000148     COMPUTE WS-NEW-BAL =
000149         WS-CURRENT-BAL - WS-WITHDRAWAL-AMT.
000150     PERFORM 3000-UPDATE-BALANCE.
000151     PERFORM 4000-WRITE-TRANSACTION-LOG.
000152
000153 2000-EXIT.
000154     EXIT.
```

## Acceptance Criteria

### Scenario 1: Successful withdrawal from active account with sufficient balance

```gherkin
GIVEN an account with status "ACTIVE"
  AND a current balance of 10000.00 SEK
  AND a hold amount of 0.00 SEK
WHEN a withdrawal of 5000.00 SEK is requested
THEN the withdrawal is processed successfully
  AND the account balance is updated to 5000.00 SEK
  AND a transaction log entry is created with the withdrawal details
```

### Scenario 2: Withdrawal rejected due to insufficient funds

```gherkin
GIVEN an account with status "ACTIVE"
  AND a current balance of 3000.00 SEK
  AND a hold amount of 1000.00 SEK
WHEN a withdrawal of 2500.00 SEK is requested
THEN the withdrawal is rejected with error code "INSUFF-FUNDS"
  AND the account balance remains unchanged at 3000.00 SEK
  AND no transaction log entry is created
```

### Scenario 3: Withdrawal rejected due to inactive account status

```gherkin
GIVEN an account with status "FROZEN"
  AND a current balance of 50000.00 SEK
WHEN a withdrawal of 1000.00 SEK is requested
THEN the withdrawal is rejected with error code "ACCT-INACTIVE"
  AND the account balance remains unchanged
  AND no transaction log entry is created
```

### Scenario 4: Withdrawal for exact available balance (boundary)

```gherkin
GIVEN an account with status "ACTIVE"
  AND a current balance of 5000.00 SEK
  AND a hold amount of 2000.00 SEK
WHEN a withdrawal of 3000.00 SEK is requested
THEN the withdrawal is processed successfully
  AND the account balance is updated to 2000.00 SEK
```

### Scenario 5: Account not found

```gherkin
GIVEN an account ID that does not exist in the system
WHEN a withdrawal is requested for that account
THEN the withdrawal is rejected with error code "ACCT-NOT-FOUND"
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for electronic payment transactions | The withdrawal validation is a prerequisite check before SCA is invoked; ensures only valid accounts proceed to authentication |
| FFFS 2014:5 | Ch. 4 §3 | Credit institutions must maintain adequate systems for managing operational risk | Balance validation prevents unauthorized overdrafts and ensures account state consistency |
| GDPR | Art. 6(1)(b) | Processing necessary for performance of a contract | Account balance check is necessary to fulfill the contractual obligation of maintaining accurate account records |

## Edge Cases

1. **Hold amount exceeds current balance**: If administrative holds placed on the account exceed the current balance (e.g., balance = 1000, hold = 1500), the available balance becomes negative. The COBOL code handles this correctly — any withdrawal amount will be greater than the negative available balance, resulting in rejection. The migrated code must preserve this behavior.

2. **Concurrent withdrawals**: The original COBOL/IMS system uses record-level locking (PROCOPT=E) to prevent race conditions on balance updates. Two simultaneous withdrawals for the same account are serialized at the database level. The migrated implementation must use equivalent pessimistic locking or optimistic concurrency with retry.

3. **Zero-amount withdrawal**: The COBOL code does not explicitly check for zero or negative withdrawal amounts — this is validated earlier in the IMS transaction input processing (program CBTRN00C). The migrated system should validate this at the API boundary.

4. **Maximum transaction amount**: The COBOL copybook defines `WS-WITHDRAWAL-AMT PIC S9(11)V99`, allowing amounts up to 99,999,999,999.99. In practice, daily transaction limits are enforced by a separate rule (ACCT-BR-015). The migrated code should use `decimal` type to match COBOL precision.

## Domain Expert Notes

- **Erik Lindström** (2026-02-10): The `WS-HOLD-AMT` field was added in 1998 when Finansinspektionen required banks to support administrative freezes without changing account status. Before that, the only way to block withdrawals was to change the status to 'F' (frozen). Some old batch jobs still set status to 'F' instead of using holds — both paths must be supported.

- **Erik Lindström** (2026-02-10): The error handler at paragraph 9000 writes to both the IMS message queue and the Db2 audit log. In the migrated system, make sure both the user-facing error response and the audit trail are maintained. FSA auditors specifically check the audit log for rejected transaction attempts.

- **Margareta Johansson** (2026-02-08): The available balance calculation (`current-balance - hold-amount`) does not account for pending clearinghouse transactions from Bankgirot. Those are tracked in a separate file (VSAM dataset PEND.TRANS) and reconciled during the nightly batch run. The migrated system should consider whether to include pending transactions in real-time available balance.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
