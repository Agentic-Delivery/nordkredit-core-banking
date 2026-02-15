---
id: "trn-br-004"
title: "Daily transaction file ingestion and card verification"
domain: "transactions"
cobol_source: "CBTRN01C.cbl:154-251"
requirement_id: "TRN-BR-004"
regulations:
  - "PSD2 Art. 64"
  - "FSA FFFS 2014:5"
  - "AML 2017:11"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-004: Daily transaction file ingestion and card verification

## Summary

CBTRN01C is a batch program that processes the daily transaction file (DALYTRAN). For each transaction in the sequential input file, it verifies the card number exists in the cross-reference file (XREF) and then confirms the linked account exists in the account master file. Transactions with invalid card numbers or missing accounts are logged to the console and skipped. This is the first stage of the daily batch transaction processing pipeline. Extracted from `CBTRN01C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM MAIN-PARA:
    Open all files:
        DALYTRAN (input, sequential) - daily transaction feed
        CUSTFILE (input, indexed by CUST-ID) - customer master
        XREFFILE (input, indexed by XREF-CARD-NUM) - card cross-reference
        CARDFILE (input, indexed by CARD-NUM) - card master
        ACCTFILE (input, indexed by ACCT-ID) - account master
        TRANFILE (input, indexed by TRANS-ID) - transaction master

    PERFORM UNTIL end-of-daily-trans-file = 'Y'
        READ next record from DALYTRAN into DALYTRAN-RECORD

        IF read successful
            DISPLAY transaction record (audit log)

            Step 1: Card verification
            MOVE DALYTRAN-CARD-NUM TO XREF-CARD-NUM
            PERFORM 2000-LOOKUP-XREF (read XREF by card number)

            IF card found in XREF (status = 0)
                Step 2: Account verification
                MOVE XREF-ACCT-ID TO ACCT-ID
                PERFORM 3000-READ-ACCOUNT (read ACCT by account ID)
                IF account NOT found
                    DISPLAY "ACCOUNT {id} NOT FOUND"
                END-IF
            ELSE
                DISPLAY "CARD NUMBER {num} COULD NOT BE VERIFIED.
                         SKIPPING TRANSACTION ID-{id}"
            END-IF
        END-IF

        IF end-of-file reached: SET end flag = 'Y'
        IF read error: DISPLAY error, ABEND program
    END-PERFORM

    Close all files
    DISPLAY "END OF EXECUTION"
    GOBACK
```

### Processing Flow

```
DALYTRAN (sequential) ──► Read record
                              │
                              ▼
                    Card # in XREF? ──► No ──► Log & Skip
                              │
                              ▼ Yes
                    Account in ACCT? ──► No ──► Log "NOT FOUND"
                              │
                              ▼ Yes
                    Display success info
                    (Card #, Account ID, Customer ID)
```

### File Dependencies

| File | DD Name | Organization | Access | Key | Purpose |
|---|---|---|---|---|---|
| DALYTRAN | DALYTRAN | Sequential | Sequential | N/A | Daily transaction input feed |
| CUSTFILE | CUSTFILE | Indexed | Random | FD-CUST-ID (9(09)) | Customer master |
| XREFFILE | XREFFILE | Indexed | Random | FD-XREF-CARD-NUM (X(16)) | Card-to-account cross-reference |
| CARDFILE | CARDFILE | Indexed | Random | FD-CARD-NUM (X(16)) | Card master |
| ACCTFILE | ACCTFILE | Indexed | Random | FD-ACCT-ID (9(11)) | Account master |
| TRANFILE | TRANFILE | Indexed | Random | FD-TRANS-ID (X(16)) | Transaction master |

## Source COBOL Reference

**Program:** `CBTRN01C.cbl`
**Lines:** 154-197 (main processing loop), 202-225 (read daily trans), 227-239 (XREF lookup), 241-250 (account read), 252-467 (file open/close), 469-489 (abend/display IO)

```cobol
000164           PERFORM UNTIL END-OF-DAILY-TRANS-FILE = 'Y'
000165               IF  END-OF-DAILY-TRANS-FILE = 'N'
000166                   PERFORM 1000-DALYTRAN-GET-NEXT
000167                   IF  END-OF-DAILY-TRANS-FILE = 'N'
000168                       DISPLAY DALYTRAN-RECORD
000169                   END-IF
000170                   MOVE 0                 TO WS-XREF-READ-STATUS
000171                   MOVE DALYTRAN-CARD-NUM TO XREF-CARD-NUM
000172                   PERFORM 2000-LOOKUP-XREF
000173                   IF WS-XREF-READ-STATUS = 0
000174                     MOVE 0            TO WS-ACCT-READ-STATUS
000175                     MOVE XREF-ACCT-ID TO ACCT-ID
000176                     PERFORM 3000-READ-ACCOUNT
000177                     IF WS-ACCT-READ-STATUS NOT = 0
000178                         DISPLAY 'ACCOUNT ' ACCT-ID ' NOT FOUND'
000179                     END-IF
000180                   ELSE
000181                     DISPLAY 'CARD NUMBER ' DALYTRAN-CARD-NUM
000182                     ' COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-'
000183                     DALYTRAN-ID
000184                   END-IF
000185               END-IF
000186           END-PERFORM.
```

```cobol
000227       2000-LOOKUP-XREF.
000228           MOVE XREF-CARD-NUM TO FD-XREF-CARD-NUM
000229           READ XREF-FILE  RECORD INTO CARD-XREF-RECORD
000230           KEY IS FD-XREF-CARD-NUM
000231                INVALID KEY
000232                  DISPLAY 'INVALID CARD NUMBER FOR XREF'
000233                  MOVE 4 TO WS-XREF-READ-STATUS
000234                NOT INVALID KEY
000235                  DISPLAY 'SUCCESSFUL READ OF XREF'
000236                  DISPLAY 'CARD NUMBER: ' XREF-CARD-NUM
000237                  DISPLAY 'ACCOUNT ID : ' XREF-ACCT-ID
000238                  DISPLAY 'CUSTOMER ID: ' XREF-CUST-ID
000239           END-READ.
```

## Acceptance Criteria

### Scenario 1: Successful card and account verification

GIVEN a daily transaction record with a valid card number
  AND the card number exists in the XREF file
  AND the linked account ID exists in the account master
WHEN the batch program processes the record
THEN the card number, account ID, and customer ID are logged
  AND processing continues to the next record

### Scenario 2: Invalid card number

GIVEN a daily transaction record with a card number not in the XREF file
WHEN the batch program looks up the cross-reference
THEN the message "CARD NUMBER \{num\} COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-\{id\}" is logged
  AND the transaction is skipped

### Scenario 3: Account not found for valid card

GIVEN a daily transaction with a valid card number in XREF
  AND the linked account ID does not exist in the account master
WHEN the batch program reads the account file
THEN the message "ACCOUNT \{id\} NOT FOUND" is logged

### Scenario 4: File open error

GIVEN any required file cannot be opened
WHEN the batch program starts
THEN an error message is displayed with the file status
  AND the program ABENDs with code 999

### Scenario 5: Read error on daily transaction file

GIVEN a read error occurs on the DALYTRAN file (status not '00' or '10')
WHEN the batch program reads the next record
THEN the error is displayed with the IO status
  AND the program ABENDs with code 999

### Scenario 6: All records displayed for audit

GIVEN a daily transaction record is read successfully
WHEN it is processed
THEN the entire DALYTRAN-RECORD is displayed to the console (SYSOUT)
  AND this provides an audit trail for the batch run

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 64 | Integrity of payment data | Card number verification ensures transaction references a valid payment instrument |
| FSA FFFS 2014:5 | Ch. 7 | Batch processing controls | Console logging of each record provides audit trail for the daily transaction batch |
| AML 2017:11 | Para. 3 | Transaction monitoring | Card and account verification is a prerequisite for downstream AML screening |

## Edge Cases

1. **No error handling for account-not-found**: When the account is not found (WS-ACCT-READ-STATUS not 0), the transaction is NOT explicitly skipped — processing continues to the next record. The migrated system should decide whether to reject these transactions.

2. **ABEND on non-EOF read errors**: Any read error that is not EOF triggers an ABEND (code 999). The migrated system should implement more granular error handling with retry logic for transient errors.

3. **Customer and Card files opened but not actively used**: The CUSTFILE and CARDFILE are opened but the main loop only uses XREFFILE and ACCTFILE. These files may be used by called subroutines or reserved for future use. The migrated system should validate which lookups are actually needed.

4. **DISPLAY-based audit trail**: The program uses DISPLAY statements for all logging, which goes to SYSOUT. The migrated system must capture equivalent audit information through structured logging.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Should transactions with valid card but missing account be rejected or processed? The current program logs but continues. (2) Are the CUSTFILE and CARDFILE actually needed in this processing step or are they vestigial? (3) This appears to be a verification-only step — the actual posting happens in CBTRN02C. Confirm this is the intended pipeline sequence.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
