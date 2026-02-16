---
id: "TRN-BR-005"
title: "Daily transaction file posting with card verification"
domain: "transactions"
cobol_source: "CBTRN01C.cbl:154-250"
requirement_id: "TRN-BR-005"
regulations:
  - "PSD2 Art. 97 — Transaction authorization verification"
  - "FFFS 2014:5 Ch. 4 §3 — Operational risk management"
  - "AML/KYC — Transaction source verification"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-005: Daily transaction file posting with card verification

## Summary

The CBTRN01C batch program reads the daily transaction file (DALYTRAN) sequentially and performs card-to-account verification for each transaction. For each daily transaction record, the card number is looked up in the cross-reference file (XREF) to retrieve the associated account ID. If the card is not found, the transaction is skipped with a display message. If found, the account record is read to confirm the account exists. This is the first step in the daily batch processing pipeline — verification only, without balance updates.

## Business Logic

### Pseudocode

```
PERFORM MAIN-PROCESSING:
    OPEN files: DALYTRAN (input), CUSTOMER, XREF, CARD, ACCOUNT, TRANSACT
    PERFORM UNTIL end of daily transaction file:
        READ next daily transaction record
        IF read successful:
            DISPLAY transaction record
            MOVE card number from daily tran to XREF lookup key
            PERFORM LOOKUP-XREF
            IF XREF lookup successful (status = 0):
                MOVE account ID from XREF to account lookup key
                PERFORM READ-ACCOUNT
                IF account NOT found:
                    DISPLAY "ACCOUNT {id} NOT FOUND"
                END-IF
            ELSE:
                DISPLAY "CARD NUMBER {num} COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-{id}"
            END-IF
        END-IF
    END-PERFORM
    CLOSE all files

LOOKUP-XREF:
    READ XREF file by card number
    IF INVALID KEY:
        DISPLAY "INVALID CARD NUMBER FOR XREF"
        SET xref-read-status = 4
    ELSE:
        DISPLAY card number, account ID, customer ID
        SET xref-read-status = 0
    END-IF

READ-ACCOUNT:
    READ ACCOUNT file by account ID
    IF INVALID KEY:
        DISPLAY "INVALID ACCOUNT NUMBER FOUND"
        SET acct-read-status = 4
    END-IF
```

### Decision Table

| Card in XREF | Account Found | Outcome |
|-------------|---------------|---------|
| Yes | Yes | Transaction verified — proceed to next step |
| Yes | No | Display account not found warning; transaction not posted |
| No | N/A | Display card verification failure; skip transaction |

## Source COBOL Reference

**Program:** `CBTRN01C.cbl`
**Lines:** 154-250

```cobol
000154 PROCEDURE DIVISION.
000155 MAIN-PARA.
000156     DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN01C'.
000157     PERFORM 0000-DALYTRAN-OPEN.
...
000164     PERFORM UNTIL END-OF-DAILY-TRANS-FILE = 'Y'
000165         IF  END-OF-DAILY-TRANS-FILE = 'N'
000166             PERFORM 1000-DALYTRAN-GET-NEXT
000167             IF  END-OF-DAILY-TRANS-FILE = 'N'
000168                 DISPLAY DALYTRAN-RECORD
000169             END-IF
000170             MOVE 0                 TO WS-XREF-READ-STATUS
000171             MOVE DALYTRAN-CARD-NUM TO XREF-CARD-NUM
000172             PERFORM 2000-LOOKUP-XREF
000173             IF WS-XREF-READ-STATUS = 0
000174               MOVE 0            TO WS-ACCT-READ-STATUS
000175               MOVE XREF-ACCT-ID TO ACCT-ID
000176               PERFORM 3000-READ-ACCOUNT
000177               IF WS-ACCT-READ-STATUS NOT = 0
000178                   DISPLAY 'ACCOUNT ' ACCT-ID ' NOT FOUND'
000179               END-IF
000180             ELSE
000181               DISPLAY 'CARD NUMBER ' DALYTRAN-CARD-NUM
000182               ' COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-'
000183               DALYTRAN-ID
000184             END-IF
000185         END-IF
000186     END-PERFORM.
...
000227 2000-LOOKUP-XREF.
000228     MOVE XREF-CARD-NUM TO FD-XREF-CARD-NUM
000229     READ XREF-FILE RECORD INTO CARD-XREF-RECORD
000230     KEY IS FD-XREF-CARD-NUM
000231          INVALID KEY
000232            DISPLAY 'INVALID CARD NUMBER FOR XREF'
000233            MOVE 4 TO WS-XREF-READ-STATUS
000234          NOT INVALID KEY
000235            DISPLAY 'SUCCESSFUL READ OF XREF'
000241 3000-READ-ACCOUNT.
000242     MOVE ACCT-ID TO FD-ACCT-ID
000243     READ ACCOUNT-FILE RECORD INTO ACCOUNT-RECORD
000244     KEY IS FD-ACCT-ID
000245          INVALID KEY
000246            DISPLAY 'INVALID ACCOUNT NUMBER FOUND'
000247            MOVE 4 TO WS-ACCT-READ-STATUS
```

## Acceptance Criteria

### Scenario 1: Valid card with existing account

```gherkin
GIVEN a daily transaction with card number "4000000000000001"
  AND the card exists in the XREF file linked to account "00000000001"
  AND account "00000000001" exists in the ACCOUNT file
WHEN the batch program processes this transaction
THEN the card-to-account verification succeeds
  AND the program displays the card number, account ID, and customer ID
```

### Scenario 2: Card number not in cross-reference

```gherkin
GIVEN a daily transaction with card number "9999999999999999"
  AND this card number does not exist in the XREF file
WHEN the batch program processes this transaction
THEN the message "CARD NUMBER 9999999999999999 COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-{id}" is displayed
  AND the transaction is skipped
```

### Scenario 3: Account not found for valid card

```gherkin
GIVEN a daily transaction with a valid card number in the XREF file
  AND the linked account ID does not exist in the ACCOUNT file
WHEN the batch program processes this transaction
THEN the message "ACCOUNT {id} NOT FOUND" is displayed
```

### Scenario 4: File open error causes abend

```gherkin
GIVEN the DALYTRAN file cannot be opened
WHEN the batch program starts
THEN an error message is displayed
  AND the program abends with code 999
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Transaction authorization verification | Card-to-account cross-reference validates the payment instrument |
| FFFS 2014:5 | Ch. 4 §3 | Operational controls for risk management | Pre-posting verification prevents invalid transactions from being posted |
| AML/KYC | General | Transaction source verification | Card number verification traces transactions to known accounts and customers |

## Edge Cases

1. **Orphaned cross-reference records**: If the XREF file contains a card-to-account mapping but the account record is deleted, the transaction passes XREF verification but fails account lookup. The program logs the error but continues processing — the transaction is effectively dropped silently.

2. **File read errors**: Any file status other than '00' (success) or '10' (EOF) on DALYTRAN causes the program to ABEND with code 999 via CEE3ABD. The migrated system should implement more graceful error handling with retry logic.

3. **No balance updates**: This program (CBTRN01C) only verifies cards and accounts — it does NOT update account balances or post transactions to the TRANSACT file. That is done by CBTRN02C. This is a verification-only step.

4. **Display-only logging**: All verification results are written to the job log via DISPLAY statements. There is no structured error file or database logging. The migrated system should use structured logging (Application Insights).

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Is this program run independently or always as part of a JCL job chain with CBTRN02C?
- What happens to transactions where the account is not found — are they reprocessed later?
- Should failed verifications be written to a reject file (like CBTRN02C does)?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
