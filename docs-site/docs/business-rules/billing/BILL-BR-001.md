---
id: "BILL-BR-001"
title: "Online bill payment pays full current balance and creates transaction record"
domain: "billing"
cobol_source: "COBIL00C.cbl:98-244"
requirement_id: "BILL-BR-001"
regulations:
  - "PSD2 Art. 64 — Receipt of Payment Orders"
  - "PSD2 Art. 78 — Amounts Transferred and Received"
  - "FFFS 2014:5 Ch. 6 — Payment Services"
  - "GDPR Art. 5(1)(e) — Storage Limitation"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-001: Online bill payment pays full current balance and creates transaction record

## Summary

The online bill payment function (COBIL00C) allows a user to pay the full current balance of a credit card account. The user enters an account ID, the system looks up the current balance, and upon confirmation creates a payment transaction record for the full balance amount. The account balance is then reduced by the payment amount (set to zero). Partial payments are not supported — the system always pays the entire current balance.

## Business Logic

### Pseudocode

```
PROCESS-ENTER-KEY:
    IF account-id IS EMPTY OR SPACES THEN
        SET error = 'Acct ID can NOT be empty...'
        EXIT with error
    END-IF

    EVALUATE confirmation-field:
        WHEN 'Y' or 'y':
            SET confirmed = TRUE
            PERFORM READ-ACCTDAT-FILE
        WHEN 'N' or 'n':
            CLEAR screen
            SET error flag
        WHEN SPACES or LOW-VALUES:
            PERFORM READ-ACCTDAT-FILE (initial lookup, no confirmation yet)
        WHEN OTHER:
            SET error = 'Invalid value. Valid values are (Y/N)...'
            EXIT with error
    END-EVALUATE

    DISPLAY current-balance on screen

    IF account-current-balance <= ZERO THEN
        SET error = 'You have nothing to pay...'
        EXIT with error
    END-IF

    IF confirmed = TRUE THEN
        READ cross-reference file (CXACAIX) to get card number
        BROWSE transaction file from HIGH-VALUES (end)
        READ PREVIOUS to get last transaction ID
        NEW-TRAN-ID = last-transaction-id + 1

        INITIALIZE transaction-record:
            TRAN-ID       = new sequential ID
            TRAN-TYPE-CD  = '02'
            TRAN-CAT-CD   = 2
            TRAN-SOURCE   = 'POS TERM'
            TRAN-DESC     = 'BILL PAYMENT - ONLINE'
            TRAN-AMT      = account-current-balance (full amount)
            TRAN-CARD-NUM = card number from xref
            TRAN-MERCHANT-ID   = 999999999
            TRAN-MERCHANT-NAME = 'BILL PAYMENT'
            TRAN-MERCHANT-CITY = 'N/A'
            TRAN-MERCHANT-ZIP  = 'N/A'
            TRAN-ORIG-TS  = current timestamp
            TRAN-PROC-TS  = current timestamp

        WRITE transaction-record to TRANSACT file
        COMPUTE account-current-balance = account-current-balance - payment-amount
        REWRITE account record (balance now zero)
        DISPLAY 'Payment successful. Your Transaction ID is <id>.'
    ELSE
        DISPLAY 'Confirm to make a bill payment...'
    END-IF
```

### Decision Table

| Acct ID Provided | Confirmation | Balance > 0 | Account Found | Xref Found | Outcome |
|------------------|-------------|-------------|---------------|------------|---------|
| No (empty) | — | — | — | — | Error: 'Acct ID can NOT be empty...' |
| Yes | Invalid value | — | — | — | Error: 'Invalid value. Valid values are (Y/N)...' |
| Yes | N/n | — | — | — | Screen cleared, error flag set |
| Yes | Blank (initial) | — | No | — | Error: 'Account ID NOT found...' |
| Yes | Blank (initial) | <= 0 | Yes | — | Error: 'You have nothing to pay...' |
| Yes | Blank (initial) | > 0 | Yes | — | Display balance, prompt for confirmation |
| Yes | Y/y | > 0 | Yes | No | Error: 'Account ID NOT found...' (xref lookup) |
| Yes | Y/y | > 0 | Yes | Yes | Payment processed, balance set to zero |

## Source COBOL Reference

**Program:** `COBIL00C.cbl`
**Lines:** 98-244 (MAIN-PARA through PROCESS-ENTER-KEY)

```cobol
       PROCESS-ENTER-KEY.

           SET CONF-PAY-NO TO TRUE

           EVALUATE TRUE
               WHEN ACTIDINI OF COBIL0AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Acct ID can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO ACTIDINL OF COBIL0AI
                   PERFORM SEND-BILLPAY-SCREEN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
```

```cobol
           IF NOT ERR-FLG-ON
               IF ACCT-CURR-BAL <= ZEROS AND
                  ACTIDINI OF COBIL0AI NOT = SPACES AND LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'You have nothing to pay...' TO
                                   WS-MESSAGE
                   MOVE -1       TO ACTIDINL OF COBIL0AI
                   PERFORM SEND-BILLPAY-SCREEN
               END-IF
           END-IF
```

```cobol
               IF CONF-PAY-YES
                   PERFORM READ-CXACAIX-FILE
                   MOVE HIGH-VALUES TO TRAN-ID
                   PERFORM STARTBR-TRANSACT-FILE
                   PERFORM READPREV-TRANSACT-FILE
                   PERFORM ENDBR-TRANSACT-FILE
                   MOVE TRAN-ID     TO WS-TRAN-ID-NUM
                   ADD 1 TO WS-TRAN-ID-NUM
                   INITIALIZE TRAN-RECORD
                   MOVE WS-TRAN-ID-NUM       TO TRAN-ID
                   MOVE '02'                 TO TRAN-TYPE-CD
                   MOVE 2                    TO TRAN-CAT-CD
                   MOVE 'POS TERM'           TO TRAN-SOURCE
                   MOVE 'BILL PAYMENT - ONLINE' TO TRAN-DESC
                   MOVE ACCT-CURR-BAL        TO TRAN-AMT
                   MOVE XREF-CARD-NUM        TO TRAN-CARD-NUM
                   MOVE 999999999            TO TRAN-MERCHANT-ID
                   MOVE 'BILL PAYMENT'       TO TRAN-MERCHANT-NAME
                   MOVE 'N/A'                TO TRAN-MERCHANT-CITY
                   MOVE 'N/A'                TO TRAN-MERCHANT-ZIP
                   PERFORM GET-CURRENT-TIMESTAMP
                   MOVE WS-TIMESTAMP         TO TRAN-ORIG-TS
                                                TRAN-PROC-TS
                   PERFORM WRITE-TRANSACT-FILE
                   COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT
                   PERFORM UPDATE-ACCTDAT-FILE
```

**Copybooks:** `CVACT01Y.cpy` (Account Record), `CVACT03Y.cpy` (Card Cross-Reference), `CVTRA05Y.cpy` (Transaction Record)

## Acceptance Criteria

### Scenario 1: Successful bill payment with confirmed full balance

```gherkin
GIVEN account "00000012345" has a current balance of $1,500.00
  AND the cross-reference file links this account to card "1234567890123456"
  AND the last transaction ID in the TRANSACT file is "0000000000000042"
WHEN the user enters account ID "00000012345" and confirms with "Y"
THEN a new transaction record is created with:
  | Field | Value |
  | TRAN-ID | 0000000000000043 |
  | TRAN-TYPE-CD | 02 |
  | TRAN-CAT-CD | 0002 |
  | TRAN-SOURCE | POS TERM |
  | TRAN-DESC | BILL PAYMENT - ONLINE |
  | TRAN-AMT | 1500.00 |
  | TRAN-CARD-NUM | 1234567890123456 |
  | TRAN-MERCHANT-ID | 999999999 |
  | TRAN-MERCHANT-NAME | BILL PAYMENT |
  AND the account balance is updated to $0.00
  AND the message "Payment successful. Your Transaction ID is 43." is displayed
```

### Scenario 2: Bill payment rejected — empty account ID

```gherkin
GIVEN the bill payment screen is displayed
WHEN the user presses Enter with an empty account ID field
THEN the error message "Acct ID can NOT be empty..." is displayed
  AND the cursor is positioned on the account ID field
  AND no file lookups are performed
```

### Scenario 3: Bill payment rejected — zero balance

```gherkin
GIVEN account "00000012345" has a current balance of $0.00
WHEN the user enters account ID "00000012345"
THEN the error message "You have nothing to pay..." is displayed
  AND no transaction record is created
```

### Scenario 4: Bill payment rejected — negative balance (credit)

```gherkin
GIVEN account "00000012345" has a current balance of -$50.00 (credit)
WHEN the user enters account ID "00000012345"
THEN the error message "You have nothing to pay..." is displayed
  AND no transaction record is created
```

### Scenario 5: Bill payment rejected — invalid confirmation value

```gherkin
GIVEN account "00000012345" has a current balance of $500.00
  AND the user has entered the account ID and the balance is displayed
WHEN the user enters "X" in the confirmation field
THEN the error message "Invalid value. Valid values are (Y/N)..." is displayed
  AND no transaction is processed
```

### Scenario 6: Bill payment cancelled by user

```gherkin
GIVEN account "00000012345" has a current balance of $500.00
  AND the user has entered the account ID and the balance is displayed
WHEN the user enters "N" in the confirmation field
THEN the screen is cleared
  AND no transaction is processed
  AND no balance change occurs
```

### Scenario 7: Account not found

```gherkin
GIVEN no account record exists for account ID "99999999999"
WHEN the user enters account ID "99999999999"
THEN the error message "Account ID NOT found..." is displayed
  AND no transaction is processed
```

### Scenario 8: Duplicate transaction ID

```gherkin
GIVEN a valid payment is being processed
  AND the generated transaction ID already exists in the TRANSACT file
WHEN the system attempts to write the transaction record
THEN the error message "Tran ID already exist..." is displayed
  AND the account balance is NOT updated
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 64 | Payment orders must be received and acknowledged | The system requires explicit confirmation (Y/N) before processing the payment, and returns a transaction ID as acknowledgment. |
| PSD2 | Art. 78 | The full amount of the payment transaction shall be transferred | The system pays the full current balance — no partial deductions or fees are applied to the payment amount. |
| FFFS 2014:5 | Ch. 6 §1-3 | Payment services must follow orderly procedures | The two-step process (display balance, confirm, then process) ensures the user sees the amount before committing. |
| GDPR | Art. 5(1)(e) | Personal data stored only as long as necessary | Transaction records include timestamps but no unnecessary PII. Merchant fields use generic values ('BILL PAYMENT', 'N/A'). |

## Edge Cases

1. **Full balance payment only**: The system does not support partial payments. The `TRAN-AMT` is always set to `ACCT-CURR-BAL` (line 224). The migration must decide whether to add partial payment support or preserve the full-balance-only behavior.

2. **Transaction ID generation**: New transaction IDs are generated by reading the last (highest) ID via `READPREV` from `HIGH-VALUES` and adding 1 (lines 212-217). This is sequential and not thread-safe in a concurrent environment. The migration should use a database sequence or GUID.

3. **UPDATE lock on READ**: The account record is read with `UPDATE` option (line 351 of READ-ACCTDAT-FILE), which locks the record for exclusive access during the CICS transaction. This prevents concurrent modifications but could cause contention. The migration needs equivalent optimistic or pessimistic concurrency control.

4. **Balance can go negative from other transactions**: While bill payment checks `ACCT-CURR-BAL <= ZEROS`, other transactions could modify the balance between the read and the payment. The CICS UPDATE lock mitigates this in the mainframe. The migration must handle race conditions.

5. **Confirmation is case-insensitive**: Both 'Y'/'y' and 'N'/'n' are accepted (lines 174-180). The migration should preserve this behavior.

6. **Hardcoded merchant values**: The merchant ID is `999999999`, name is `'BILL PAYMENT'`, city and zip are `'N/A'` (lines 226-229). These are sentinel values indicating a system-generated transaction rather than a merchant purchase.

7. **PF3 navigation**: PF3 returns to the calling program or defaults to `COMEN01C` (main menu). PF4 clears the screen. Only Enter processes the payment.

## Domain Expert Notes

- **Pending validation**: This rule requires review by a domain expert familiar with the bill payment flow and transaction recording.
- The transaction type code `'02'` with category `2` indicates an online bill payment. The migration must maintain this classification scheme for downstream reporting and interest calculation.
- The `TRAN-ORIG-TS` and `TRAN-PROC-TS` are both set to the current timestamp (lines 231-232), meaning processing is immediate with no deferred settlement. The migration should preserve this behavior unless a new settlement model is designed.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
