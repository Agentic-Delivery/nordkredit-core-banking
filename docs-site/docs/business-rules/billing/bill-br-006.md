---
id: "bill-br-006"
title: "Transaction rejection and billing exception handling"
domain: "billing"
cobol_source: "CBTRN02C.cbl:193-234,370-422,562-579"
requirement_id: "BILL-BR-006"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "AML 2017:11"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-006: Transaction rejection and billing exception handling

## Summary

The billing system implements a structured rejection mechanism for transactions that fail validation during the daily batch posting process in `CBTRN02C.cbl`. Failed transactions are written to a dedicated rejects file (DALYREJS) with the original transaction data plus a coded reason and description. The batch program tracks rejection counts and sets a warning return code when any rejections occur. This rejection handling provides the billing department with an actionable exception report for manual review and resolution — rejected transactions represent potential billing disputes, fraud indicators, or data quality issues. Extracted from `CBTRN02C.cbl`.

## Business Logic

### Pseudocode

```
MAIN PROCESSING LOOP:
    FOR EACH transaction in DALYTRAN file:
        ADD 1 TO WS-TRANSACTION-COUNT

        Initialize WS-VALIDATION-FAIL-REASON = 0
        PERFORM 1500-VALIDATE-TRAN

        IF WS-VALIDATION-FAIL-REASON = 0
            PERFORM 2000-POST-TRANSACTION
        ELSE
            ADD 1 TO WS-REJECT-COUNT
            PERFORM 2500-WRITE-REJECT-REC
        END-IF
    END-FOR

    Close all files
    DISPLAY "TRANSACTIONS PROCESSED: " WS-TRANSACTION-COUNT
    DISPLAY "TRANSACTIONS REJECTED : " WS-REJECT-COUNT

    IF WS-REJECT-COUNT > 0
        SET RETURN-CODE = 4   (warning — some rejections)
    END-IF
    GOBACK

VALIDATION REASONS:
    Code 100: "INVALID CARD NUMBER FOUND" (card not in XREF)
    Code 101: "ACCOUNT RECORD NOT FOUND" (account lookup failed)
    Code 102: "OVERLIMIT TRANSACTION" (exceeds credit limit)
    Code 103: "TRANSACTION RECEIVED AFTER ACCT EXPIRATION" (expired account)

REJECT RECORD STRUCTURE:
    Positions 1-350:   Original DALYTRAN-RECORD (complete)
    Positions 351-354: WS-VALIDATION-FAIL-REASON (numeric code)
    Positions 355-430: WS-VALIDATION-FAIL-REASON-DESC (text description)
    Total: 430 bytes

BATCH RETURN CODES:
    Return code 0: All transactions posted successfully
    Return code 4: Warning — one or more rejections occurred
    ABEND 999:     Fatal I/O error during processing
```

### Decision Table

| Validation Result | Action | Reject File | Return Code |
|---|---|---|---|
| All validations pass (reason = 0) | Post transaction | Not written | 0 (if all pass) |
| Card not found (reason = 100) | Reject | Written with code 100 | 4 |
| Account not found (reason = 101) | Reject | Written with code 101 | 4 |
| Over credit limit (reason = 102) | Reject | Written with code 102 | 4 |
| Expired account (reason = 103) | Reject | Written with code 103 | 4 |
| I/O error during processing | ABEND | N/A | 999 (abnormal) |

### Rejection Code Summary

| Code | Description | Billing Impact | Typical Resolution |
|---|---|---|---|
| 100 | Invalid card number | Transaction cannot be billed — card not recognized | Investigate card number, check for data entry error or fraud |
| 101 | Account not found | Transaction cannot be billed — no account to charge | Investigate XREF mapping, check for closed/migrated account |
| 102 | Overlimit transaction | Transaction exceeds billing limit | Customer contact for limit increase or payment |
| 103 | Expired account | Billing not permitted after expiration | Account renewal or card reissuance |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 193-234 (main processing loop with reject counting)

```cobol
000193     PERFORM UNTIL END-OF-FILE = 'Y'
000194         READ DALYTRAN-FILE INTO DALYTRAN-RECORD
000195             AT END
000196                 SET END-OF-FILE TO TRUE
000197             NOT AT END
000198                 ADD 1 TO WS-TRANSACTION-COUNT
000199
000200                 MOVE 0 TO WS-VALIDATION-FAIL-REASON
000201                 PERFORM 1500-VALIDATE-TRAN
000202
000203                 IF WS-VALIDATION-FAIL-REASON = 0
000204                     PERFORM 2000-POST-TRANSACTION
000205                 ELSE
000206                     ADD 1 TO WS-REJECT-COUNT
000207                     PERFORM 2500-WRITE-REJECT-REC
000208                 END-IF
000209         END-READ
000210     END-PERFORM
```

**Lines:** 562-579 (reject record write and batch completion)

```cobol
000562 2500-WRITE-REJECT-REC.
000563           MOVE DALYTRAN-RECORD
000564             TO REJECT-RECORD(1:350)
000565           MOVE WS-VALIDATION-FAIL-REASON
000566             TO REJECT-RECORD(351:4)
000567           MOVE WS-VALIDATION-FAIL-REASON-DESC
000568             TO REJECT-RECORD(355:76)
000569           WRITE REJECT-RECORD
000570           IF FD-DALYREJS-STATUS NOT = '00'
000571               PERFORM 9999-ABEND-PROGRAM
000572           END-IF
000573           .
```

```cobol
000575     DISPLAY 'TRANSACTIONS PROCESSED: ' WS-TRANSACTION-COUNT
000576     DISPLAY 'TRANSACTIONS REJECTED : ' WS-REJECT-COUNT
000577
000578     IF WS-REJECT-COUNT > 0
000579         MOVE 4 TO RETURN-CODE
000580     END-IF
```

### Reject File Organization

| Attribute | Value |
|---|---|
| File | DALYREJS |
| Record Length | 430 bytes |
| Access Method | Sequential (append) |
| Open Mode | OUTPUT |
| Contents | Original transaction (350) + reason code (4) + description (76) |

## Acceptance Criteria

### Scenario 1: Transaction rejected for invalid card number

```gherkin
GIVEN a daily transaction with card number "9999888877776666"
  AND this card number does not exist in the cross-reference file
WHEN the batch validates the transaction
THEN the transaction is written to the rejects file
  AND the reject record contains the full 350-byte transaction
  AND positions 351-354 contain "0100"
  AND positions 355-430 contain "INVALID CARD NUMBER FOUND"
  AND WS-REJECT-COUNT is incremented by 1
```

### Scenario 2: Batch completes with rejections

```gherkin
GIVEN a daily batch of 100 transactions
  AND 3 transactions fail validation
WHEN the batch completes processing
THEN WS-TRANSACTION-COUNT = 100
  AND WS-REJECT-COUNT = 3
  AND RETURN-CODE = 4 (warning)
  AND 97 transactions are posted to TRANSACT
  AND 3 transactions are written to DALYREJS
```

### Scenario 3: Batch completes with zero rejections

```gherkin
GIVEN a daily batch of 50 transactions
  AND all transactions pass validation
WHEN the batch completes processing
THEN WS-TRANSACTION-COUNT = 50
  AND WS-REJECT-COUNT = 0
  AND RETURN-CODE = 0 (success)
  AND DALYREJS file contains no records
```

### Scenario 4: Fatal I/O error during reject write

```gherkin
GIVEN a transaction has failed validation
WHEN the reject record is being written to DALYREJS
  AND the WRITE operation returns a non-zero file status
THEN the program ABENDs with code 999
  AND the entire batch is halted
  AND no further transactions are processed
```

### Scenario 5: Reject record preserves original transaction data

```gherkin
GIVEN a transaction with ID "0000000001234567" and amount 500.00
WHEN the transaction is rejected for overlimit (code 102)
THEN positions 1-350 of the reject record contain the EXACT original DALYTRAN-RECORD
  AND the original data is not modified
  AND the reject reason is appended (not overlaid on transaction data)
```

### Scenario 6: Multiple rejection reasons — only last is recorded

```gherkin
GIVEN a transaction that fails both overlimit (102) and expired (103) checks
WHEN the transaction is validated
THEN the reject record contains code 103 (not 102)
  AND this is because the expiration check overwrites the overlimit code
  AND the original overlimit condition is lost
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 7 | Financial institutions must maintain audit trails for financial operations | The reject file provides a complete audit trail of declined transactions with coded reasons, enabling post-batch review and regulatory reporting |
| AML 2017:11 | Para. 3 | Suspicious transaction monitoring and reporting | Rejected transactions (especially code 100 — invalid card) may indicate fraudulent activity; the reject file enables AML screening of declined transactions |
| DORA | Art. 11 | ICT risk management — operational resilience monitoring | The return code mechanism (0 for success, 4 for warnings, 999 for fatal) enables JCL-level monitoring of batch health, supporting operational resilience |

## Edge Cases

1. **Sequential rejection overwrites**: Because validation checks run sequentially and share a single fail-reason variable, only the last failing check's reason is recorded. If an account is both overlimit (102) and expired (103), only 103 is captured. The migrated system should capture all validation failures.

2. **Reject file write failure**: If writing to the reject file itself fails, the program ABENDs. This means a full reject file (disk space) can halt the entire batch. The migrated system should handle reject storage failures gracefully (e.g., log to secondary storage).

3. **Return code semantics**: Return code 4 (warning) in JCL typically means the step completed but with conditions. Downstream JCL steps may use COND parameters to decide whether to proceed. The migrated system must map these return codes to equivalent Azure Functions exit codes or monitoring alerts.

4. **Reject record size mismatch**: The reject record (430 bytes) is larger than the input record (350 bytes). The additional 80 bytes hold the coded reason. If the reject file's DCB (data control block) specifies a different LRECL, truncation or padding may occur. The migrated system should use a structured format (e.g., JSON or database table) rather than fixed-width records.

5. **Transaction count includes rejects**: `WS-TRANSACTION-COUNT` counts all transactions read (both posted and rejected). The relationship `WS-TRANSACTION-COUNT = posted count + WS-REJECT-COUNT` should be validated as a reconciliation check.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) What is the current process for reviewing and resolving rejected transactions — is there a daily review workflow? (2) Should the migrated system capture all validation failures per transaction instead of just the last one? (3) How is the reject file used by downstream processes — is it input to another batch or a manual review queue? (4) What monitoring/alerting exists for high rejection rates? (5) Is return code 4 sufficient for all rejection scenarios, or should different codes indicate different severity levels?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
