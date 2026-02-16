---
id: "TRN-BR-002"
title: "Transaction detail view retrieval by Transaction ID"
domain: "transactions"
cobol_source: "COTRN01C.cbl:85-296"
requirement_id: "TRN-BR-002"
regulations:
  - "FFFS 2014:5 Ch. 8 — Operational requirements for information systems"
  - "PSD2 Art. 94 — Access to transaction history"
  - "GDPR Art. 15 — Right of access by the data subject"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# TRN-BR-002: Transaction detail view retrieval by Transaction ID

## Summary

The transaction view screen (CICS transaction CT01) displays the full detail of a single transaction retrieved from the TRANSACT VSAM file by its Transaction ID. It is invoked either from the transaction list screen (COTRN00C) with a pre-selected ID, or directly by entering a Transaction ID. All fields are displayed read-only including card number, transaction type, category, source, amount, description, origination and processing timestamps, and merchant details.

## Business Logic

### Pseudocode

```
PERFORM MAIN-FLOW:
    IF no COMMAREA → return to sign-on screen
    IF first entry into program
        SET reenter flag
        IF Transaction ID pre-selected from list screen
            MOVE selected ID to search field
            PERFORM lookup
        END-IF
        SEND view screen
    ELSE
        RECEIVE screen input
        EVALUATE key pressed:
            WHEN ENTER → validate and lookup transaction
            WHEN PF3   → return to previous screen
            WHEN PF4   → clear all fields
            WHEN PF5   → return to transaction list (COTRN00C)
            WHEN OTHER → display "Invalid key" error
        END-EVALUATE
    END-IF

PROCESS-ENTER-KEY:
    IF Transaction ID is empty
        SET error "Tran ID can NOT be empty..."
        EXIT
    END-IF
    CLEAR all display fields
    READ TRANSACT file by Transaction ID (with UPDATE lock)
    IF NORMAL response
        Populate all screen fields from TRAN-RECORD
    ELSE IF NOTFND
        SET error "Transaction ID NOT found..."
    ELSE
        SET error "Unable to lookup Transaction..."
    END-IF
```

### Decision Table

| Transaction ID Input | File Read Result | Outcome |
|---------------------|-----------------|---------|
| Empty/spaces | N/A | Error: "Tran ID can NOT be empty..." |
| Valid ID | Record found | Display all transaction fields |
| Valid ID | Record not found | Error: "Transaction ID NOT found..." |
| Valid ID | File error | Error: "Unable to lookup Transaction..." |

## Source COBOL Reference

**Program:** `COTRN01C.cbl`
**Lines:** 144-296

```cobol
000144 PROCESS-ENTER-KEY.
000146     EVALUATE TRUE
000147         WHEN TRNIDINI OF COTRN1AI = SPACES OR LOW-VALUES
000148             MOVE 'Y'     TO WS-ERR-FLG
000149             MOVE 'Tran ID can NOT be empty...' TO WS-MESSAGE
000150             MOVE -1       TO TRNIDINL OF COTRN1AI
000151             PERFORM SEND-TRNVIEW-SCREEN
000152         WHEN OTHER
000153             MOVE -1       TO TRNIDINL OF COTRN1AI
000154             CONTINUE
000155     END-EVALUATE
...
000267 READ-TRANSACT-FILE.
000269     EXEC CICS READ
000270          DATASET   (WS-TRANSACT-FILE)
000271          INTO      (TRAN-RECORD)
000272          LENGTH    (LENGTH OF TRAN-RECORD)
000273          RIDFLD    (TRAN-ID)
000274          KEYLENGTH (LENGTH OF TRAN-ID)
000275          UPDATE
000276          RESP      (WS-RESP-CD)
000277          RESP2     (WS-REAS-CD)
000278     END-EXEC.
000280     EVALUATE WS-RESP-CD
000281         WHEN DFHRESP(NORMAL)
000282             CONTINUE
000283         WHEN DFHRESP(NOTFND)
000284             MOVE 'Y'     TO WS-ERR-FLG
000285             MOVE 'Transaction ID NOT found...' TO WS-MESSAGE
```

## Acceptance Criteria

### Scenario 1: View transaction from list selection

```gherkin
GIVEN the user selected transaction "0000000000000042" from the list screen
WHEN the transaction view screen loads
THEN all fields are populated with the transaction details
  AND the Transaction ID field shows "0000000000000042"
  AND the amount is formatted as +99999999.99
```

### Scenario 2: View transaction by direct ID entry

```gherkin
GIVEN the user is on the transaction view screen
  AND enters Transaction ID "0000000000000100"
WHEN the user presses ENTER
THEN the transaction details for ID "0000000000000100" are displayed
```

### Scenario 3: Transaction ID not found

```gherkin
GIVEN the user enters Transaction ID "9999999999999999"
  AND no transaction with that ID exists
WHEN the user presses ENTER
THEN the error message "Transaction ID NOT found..." is displayed
  AND all detail fields remain blank
```

### Scenario 4: Empty Transaction ID

```gherkin
GIVEN the user leaves the Transaction ID field empty
WHEN the user presses ENTER
THEN the error message "Tran ID can NOT be empty..." is displayed
```

### Scenario 5: Clear screen

```gherkin
GIVEN the user is viewing a transaction
WHEN the user presses PF4
THEN all fields are cleared to spaces
  AND the cursor returns to the Transaction ID input field
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 | Information systems must support operational needs | Detail view enables investigation of individual transactions |
| PSD2 | Art. 94 | Payment service providers must retain and provide access to transaction data | Full transaction detail accessible by ID |
| GDPR | Art. 15 | Data subjects have the right to access their personal data | Transaction detail including merchant info may be subject to access requests |

## Edge Cases

1. **UPDATE lock on READ**: The CICS READ uses the UPDATE option (line 275), which acquires a record-level lock. This is unusual for a view-only screen and may be a legacy pattern. The migrated system should use a read-only query unless record locking is required for consistency.

2. **Transaction ID format**: The ID is a 16-character field (PIC X(16)). The COBOL code does not validate numeric format for the lookup — it directly uses the entered value as the VSAM key. The migrated system should validate format before querying.

3. **Displayed fields**: 14 fields are displayed: Tran ID, Card Number, Type Code, Category Code, Source, Amount, Description, Origination Date, Processing Date, Merchant ID, Merchant Name, Merchant City, Merchant Zip. The card number (PIC X(16)) is displayed unmasked — GDPR/PCI-DSS may require masking in the migrated system.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Should the UPDATE lock on READ be preserved, or is a read-only access pattern sufficient?
- Is the unmasked card number display intentional, or should PCI-DSS masking be applied?
- Are there audit logging requirements for viewing transaction details?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
