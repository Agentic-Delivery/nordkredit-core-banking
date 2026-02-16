---
id: "TRN-BR-004"
title: "Transaction ID auto-generation and record persistence"
domain: "transactions"
cobol_source: "COTRN02C.cbl:442-749"
requirement_id: "TRN-BR-004"
regulations:
  - "FFFS 2014:5 Ch. 8 — Operational requirements for information systems"
  - "PSD2 Art. 94 — Transaction record retention"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# TRN-BR-004: Transaction ID auto-generation and record persistence

## Summary

When a new transaction is confirmed for addition via the online screen (COTRN02C), the system auto-generates a unique Transaction ID by reading the highest existing ID from the TRANSACT file and incrementing by 1. The transaction record is then written to the TRANSACT VSAM file with all validated fields. The system also supports a "copy last transaction" function (PF5) that pre-populates the screen with the most recent transaction's data fields for rapid entry of similar transactions.

## Business Logic

### Pseudocode

```
PERFORM ADD-TRANSACTION:
    MOVE HIGH-VALUES to TRAN-ID
    STARTBR TRANSACT file (positions at end)
    READPREV (reads the last record)
    ENDBR
    MOVE last TRAN-ID to numeric field WS-TRAN-ID-N
    ADD 1 to WS-TRAN-ID-N (new ID = last ID + 1)

    INITIALIZE TRAN-RECORD
    MOVE WS-TRAN-ID-N    TO TRAN-ID
    MOVE screen fields    TO transaction record fields:
        Type Code, Category Code, Source, Description,
        Amount (via NUMVAL-C conversion), Card Number,
        Merchant ID, Merchant Name, Merchant City, Merchant Zip,
        Origination Date, Processing Date

    PERFORM WRITE-TRANSACT-FILE
    IF write successful
        Clear screen fields
        Display success: "Transaction added successfully. Your Tran ID is NNNN."
    ELSE IF DUPKEY or DUPREC
        Error: "Tran ID already exist..."
    ELSE
        Error: "Unable to Add Transaction..."
    END-IF

PERFORM COPY-LAST-TRAN-DATA:
    Validate key fields (Account/Card must be verified)
    MOVE HIGH-VALUES to TRAN-ID
    STARTBR TRANSACT (positions at end)
    READPREV (reads last record)
    ENDBR
    Populate screen fields from last transaction:
        Type Code, Category Code, Source, Amount,
        Description, Dates, Merchant details
    Invoke PROCESS-ENTER-KEY (allows user to modify and confirm)
```

### ID Generation Logic

| Step | Action | Value |
|------|--------|-------|
| 1 | Set TRAN-ID to HIGH-VALUES (X'FF' x 16) | Positions STARTBR at end of file |
| 2 | READPREV to get last record | Retrieves highest key |
| 3 | Move TRAN-ID to PIC 9(16) numeric field | Converts to number |
| 4 | ADD 1 | Next sequential ID |
| 5 | Move back to TRAN-ID PIC X(16) | New transaction ID |

## Source COBOL Reference

**Program:** `COTRN02C.cbl`
**Lines:** 442-749

```cobol
000442 ADD-TRANSACTION.
000444     MOVE HIGH-VALUES TO TRAN-ID
000445     PERFORM STARTBR-TRANSACT-FILE
000446     PERFORM READPREV-TRANSACT-FILE
000447     PERFORM ENDBR-TRANSACT-FILE
000448     MOVE TRAN-ID     TO WS-TRAN-ID-N
000449     ADD 1 TO WS-TRAN-ID-N
000450     INITIALIZE TRAN-RECORD
000451     MOVE WS-TRAN-ID-N         TO TRAN-ID
000452     MOVE TTYPCDI  OF COTRN2AI TO TRAN-TYPE-CD
000453     MOVE TCATCDI  OF COTRN2AI TO TRAN-CAT-CD
000454     MOVE TRNSRCI  OF COTRN2AI TO TRAN-SOURCE
000455     MOVE TDESCI   OF COTRN2AI TO TRAN-DESC
000456     COMPUTE WS-TRAN-AMT-N = FUNCTION NUMVAL-C(TRNAMTI OF COTRN2AI)
000457     MOVE WS-TRAN-AMT-N TO TRAN-AMT
000458     MOVE CARDNINI OF COTRN2AI TO TRAN-CARD-NUM
...
000711 WRITE-TRANSACT-FILE.
000713     EXEC CICS WRITE
000714          DATASET   (WS-TRANSACT-FILE)
000715          FROM      (TRAN-RECORD)
000716          LENGTH    (LENGTH OF TRAN-RECORD)
000717          RIDFLD    (TRAN-ID)
000718          KEYLENGTH (LENGTH OF TRAN-ID)
000719          RESP      (WS-RESP-CD)
000720          RESP2     (WS-REAS-CD)
000721     END-EXEC
000723     EVALUATE WS-RESP-CD
000724         WHEN DFHRESP(NORMAL)
000725             PERFORM INITIALIZE-ALL-FIELDS
000726             MOVE SPACES TO WS-MESSAGE
000727             MOVE DFHGREEN TO ERRMSGC OF COTRN2AO
000728             STRING 'Transaction added successfully. '
000729                    ' Your Tran ID is ' TRAN-ID '.'
000730               INTO WS-MESSAGE
000735         WHEN DFHRESP(DUPKEY)
000736         WHEN DFHRESP(DUPREC)
000737             MOVE 'Y' TO WS-ERR-FLG
000738             MOVE 'Tran ID already exist...' TO WS-MESSAGE
```

## Acceptance Criteria

### Scenario 1: Successful transaction creation

```gherkin
GIVEN all input fields are valid and confirmed
  AND the highest existing Transaction ID is "0000000000000099"
WHEN the transaction is submitted
THEN a new record is written with Transaction ID "0000000000000100"
  AND the success message includes the new Transaction ID
  AND all screen fields are cleared for the next entry
```

### Scenario 2: First transaction in empty file

```gherkin
GIVEN the TRANSACT file is empty
  AND READPREV returns ENDFILE
WHEN a transaction is submitted
THEN TRAN-ID defaults to zeros
  AND the new Transaction ID is "0000000000000001"
```

### Scenario 3: Duplicate key handling

```gherkin
GIVEN a Transaction ID collision occurs (DUPKEY/DUPREC)
WHEN the write is attempted
THEN the error message "Tran ID already exist..." is displayed
  AND the transaction is not written
```

### Scenario 4: Copy last transaction data

```gherkin
GIVEN the user has entered a valid Account ID or Card Number
WHEN the user presses PF5
THEN the screen is populated with the last transaction's data fields
  AND the user can modify fields before confirming
```

### Scenario 5: Amount conversion precision

```gherkin
GIVEN the user enters amount "+00000100.50"
WHEN the transaction is created
THEN the TRAN-AMT field stores the value as S9(9)V99 = +000000100.50
  AND the value is preserved with exact 2-decimal precision
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 | Information systems must maintain accurate records | Sequential ID generation ensures unique, auditable transaction identifiers |
| PSD2 | Art. 94 | Transaction records must be retained and accessible | VSAM write creates the persistent record with full transaction details |

## Edge Cases

1. **Concurrent ID generation**: The STARTBR/READPREV/ENDBR + WRITE sequence is not atomic. Two concurrent users could generate the same ID. The DUPKEY/DUPREC handling (lines 735-738) is the safety net, but the user would need to retry. The migrated system should use database sequences or GUID generation for thread safety.

2. **Maximum Transaction ID**: The WS-TRAN-ID-N field is PIC 9(16), supporting up to 9,999,999,999,999,999 transactions. Overflow would cause a COBOL size error. The migrated system should use a sufficiently large type (long/GUID).

3. **Amount conversion via NUMVAL-C**: The FUNCTION NUMVAL-C converts formatted amounts (e.g., "+00000100.50") to numeric PIC S9(9)V99. If the input contains invalid characters despite passing format validation, NUMVAL-C will trigger a runtime error. The migrated system should use TryParse for safety.

4. **READPREV on empty file**: When the TRANSACT file is empty, READPREV returns ENDFILE status, and TRAN-ID is set to ZEROS (line 689). Adding 1 produces "0000000000000001" as the first ID.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Is there a maximum transaction amount that should be enforced at the input level?
- Should the migrated system use GUIDs instead of sequential IDs for distributed operation?
- Are there concurrent access patterns where the STARTBR/READPREV ID generation could fail?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
