---
id: "acct-br-014"
title: "Customer-account relationship management"
domain: "account-management"
cobol_source: "CBTRN01C.cbl:164-250, CVACT03Y.cpy:1-8"
requirement_id: "ACCT-BR-014"
regulations:
  - "GDPR Art. 5(1)(d)"
  - "AML 2017:11 Art. 11"
  - "PSD2 Art. 97"
  - "FSA FFFS 2014:5 Ch. 3"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-014: Customer-account relationship management

## Summary

The system maintains a three-way relationship between cards, customers, and accounts through the XREF (cross-reference) file. Every financial transaction must traverse this relationship chain — from card number to XREF record to account master and optionally to customer record — before being processed. The daily transaction verification batch (CBTRN01C) enforces this relationship by performing dual validation: first looking up the card in the XREF file to obtain the account ID and customer ID, then verifying the account exists in the ACCTFILE. Transactions that fail either lookup are rejected.

This relationship model is the foundation of the account holder management system. While ACCT-BR-003 documents the XREF lookup for online card programs, this rule documents the comprehensive relationship validation performed during batch transaction processing across all five entity files: daily transactions, customers, XREF, cards, and accounts.

## Business Logic

### Pseudocode

```
PERFORM DAILY-TRANSACTION-VERIFICATION (CBTRN01C):
    OPEN DALYTRAN-FILE (INPUT, sequential)
    OPEN CUSTOMER-FILE (INPUT, indexed random)
    OPEN XREF-FILE (INPUT, indexed random)
    OPEN CARD-FILE (INPUT, indexed random)
    OPEN ACCOUNT-FILE (INPUT, indexed random)
    OPEN TRANSACT-FILE (INPUT, indexed random)

    PERFORM UNTIL END-OF-DAILY-TRANS-FILE
        READ next DALYTRAN-RECORD

        -- Step 1: XREF lookup (card → account + customer)
        MOVE DALYTRAN-CARD-NUM TO XREF-CARD-NUM
        READ XREF-FILE by XREF-CARD-NUM
        IF INVALID KEY (card not found in XREF)
            DISPLAY 'CARD NUMBER COULD NOT BE VERIFIED'
            SKIP this transaction
            CONTINUE to next
        END-IF
        -- XREF provides: XREF-ACCT-ID, XREF-CUST-ID

        -- Step 2: Account verification
        MOVE XREF-ACCT-ID TO ACCT-ID
        READ ACCOUNT-FILE by ACCT-ID
        IF INVALID KEY (account not found)
            DISPLAY 'ACCOUNT NOT FOUND'
            SKIP this transaction
            CONTINUE to next
        END-IF
        -- Account exists: transaction is verified
    END-PERFORM
```

### Entity Relationship Model

```
CUSTOMER (CUST-ID 9(09))
    |
    | 1:N (one customer can have many cards)
    |
CARD-XREF (XREF-CARD-NUM X(16) → XREF-CUST-ID, XREF-ACCT-ID)
    |                                    |
    | N:1                                | N:1
    |                                    |
CARD (CARD-NUM X(16))           ACCOUNT (ACCT-ID 9(11))
```

### File Access Patterns

| File | Organization | Access Mode | Key | Purpose in CBTRN01C |
|---|---|---|---|---|
| DALYTRAN-FILE | Sequential | Sequential | FD-TRAN-ID X(16) | Source daily transactions |
| CUSTOMER-FILE | Indexed KSDS | Random | FD-CUST-ID 9(09) | Customer identity lookup |
| XREF-FILE | Indexed KSDS | Random | FD-XREF-CARD-NUM X(16) | Card-to-account-customer mapping |
| CARD-FILE | Indexed KSDS | Random | FD-CARD-NUM X(16) | Card record validation |
| ACCOUNT-FILE | Indexed KSDS | Random | FD-ACCT-ID 9(11) | Account existence verification |
| TRANSACT-FILE | Indexed KSDS | Random | FD-TRANS-ID X(16) | Posted transaction log |

### XREF Record Structure (CVACT03Y.cpy)

| Field | PIC | Migrated Type | Purpose |
|---|---|---|---|
| XREF-CARD-NUM | X(16) | char(16) | Card number (primary key) |
| XREF-CUST-ID | 9(09) | int / char(9) | Customer identifier (foreign key to CUSTOMER-FILE) |
| XREF-ACCT-ID | 9(11) | bigint / char(11) | Account identifier (foreign key to ACCOUNT-FILE) |
| FILLER | X(14) | — | Reserved/unused space |

## Source COBOL Reference

**Program:** `CBTRN01C.cbl`
**Lines:** 28-62 (file definitions), 104-127 (copybook includes), 164-186 (main verification loop), 227-250 (lookup paragraphs)

**File definitions establishing entity relationships (lines 28-62):**

```cobol
000029            SELECT DALYTRAN-FILE ASSIGN TO DALYTRAN
000034            SELECT CUSTOMER-FILE ASSIGN TO   CUSTFILE
000035                   ORGANIZATION IS INDEXED
000036                   ACCESS MODE  IS RANDOM
000037                   RECORD KEY   IS FD-CUST-ID
000040            SELECT XREF-FILE ASSIGN TO   XREFFILE
000041                   ORGANIZATION IS INDEXED
000042                   ACCESS MODE  IS RANDOM
000043                   RECORD KEY   IS FD-XREF-CARD-NUM
000046            SELECT CARD-FILE ASSIGN TO   CARDFILE
000047                   ORGANIZATION IS INDEXED
000048                   ACCESS MODE  IS RANDOM
000049                   RECORD KEY   IS FD-CARD-NUM
000052            SELECT ACCOUNT-FILE ASSIGN TO   ACCTFILE
000053                   ORGANIZATION IS INDEXED
000054                   ACCESS MODE  IS RANDOM
000055                   RECORD KEY   IS FD-ACCT-ID
```
*(Lines 28-56 — all five entity files are opened for random access verification.)*

**Main verification loop (lines 164-186):**

```cobol
000164            PERFORM UNTIL END-OF-DAILY-TRANS-FILE = 'Y'
000170                MOVE 0                 TO WS-XREF-READ-STATUS
000171                MOVE DALYTRAN-CARD-NUM TO XREF-CARD-NUM
000172                PERFORM 2000-LOOKUP-XREF
000173                IF WS-XREF-READ-STATUS = 0
000174                  MOVE 0            TO WS-ACCT-READ-STATUS
000175                  MOVE XREF-ACCT-ID TO ACCT-ID
000176                  PERFORM 3000-READ-ACCOUNT
000177                  IF WS-ACCT-READ-STATUS NOT = 0
000178                      DISPLAY 'ACCOUNT ' ACCT-ID ' NOT FOUND'
000179                  END-IF
000180                ELSE
000181                  DISPLAY 'CARD NUMBER ' DALYTRAN-CARD-NUM
000182                  ' COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-'
000183                  DALYTRAN-ID
000184                END-IF
```
*(Lines 164-184 — two-stage validation: XREF lookup, then account verification.)*

**XREF lookup paragraph (lines 227-239):**

```cobol
000227        2000-LOOKUP-XREF.
000228            MOVE XREF-CARD-NUM TO FD-XREF-CARD-NUM
000229            READ XREF-FILE  RECORD INTO CARD-XREF-RECORD
000230            KEY IS FD-XREF-CARD-NUM
000231                 INVALID KEY
000232                   DISPLAY 'INVALID CARD NUMBER FOR XREF'
000233                   MOVE 4 TO WS-XREF-READ-STATUS
000234                 NOT INVALID KEY
000235                   DISPLAY 'SUCCESSFUL READ OF XREF'
000236                   DISPLAY 'CARD NUMBER: ' XREF-CARD-NUM
000237                   DISPLAY 'ACCOUNT ID : ' XREF-ACCT-ID
000238                   DISPLAY 'CUSTOMER ID: ' XREF-CUST-ID
```
*(Lines 227-238 — XREF read returns all three identifiers: card, account, and customer.)*

**Account verification paragraph (lines 241-250):**

```cobol
000241        3000-READ-ACCOUNT.
000242            MOVE ACCT-ID TO FD-ACCT-ID
000243            READ ACCOUNT-FILE RECORD INTO ACCOUNT-RECORD
000244            KEY IS FD-ACCT-ID
000245                 INVALID KEY
000246                   DISPLAY 'INVALID ACCOUNT NUMBER FOUND'
000247                   MOVE 4 TO WS-ACCT-READ-STATUS
000248                 NOT INVALID KEY
000249                   DISPLAY 'SUCCESSFUL READ OF ACCOUNT FILE'
```
*(Lines 241-250 — account must exist for the XREF-derived account ID.)*

**CVACT03Y.cpy (XREF copybook, lines 4-8):**

```cobol
       01 CARD-XREF-RECORD.
          05  XREF-CARD-NUM              PIC X(16).
          05  XREF-CUST-ID              PIC 9(09).
          05  XREF-ACCT-ID              PIC 9(11).
          05  FILLER                     PIC X(14).
```
*(CVACT03Y.cpy lines 4-8 — the XREF record establishing the three-way relationship.)*

## Acceptance Criteria

### Scenario 1: Valid transaction traverses full relationship chain

```gherkin
GIVEN a daily transaction for card 4000000000000001
  AND the XREF file contains a record:
    | XREF-CARD-NUM | 4000000000000001 |
    | XREF-CUST-ID | 123456789 |
    | XREF-ACCT-ID | 41000000001 |
  AND account 41000000001 exists in ACCTFILE
WHEN the transaction is verified
THEN the XREF lookup succeeds (WS-XREF-READ-STATUS = 0)
  AND the account lookup succeeds (WS-ACCT-READ-STATUS = 0)
  AND the transaction is accepted for processing
```

### Scenario 2: Transaction rejected — card not in XREF

```gherkin
GIVEN a daily transaction for card 9999999999999999
  AND no XREF record exists for this card number
WHEN the XREF lookup is performed
THEN WS-XREF-READ-STATUS = 4 (INVALID KEY)
  AND the message 'CARD NUMBER COULD NOT BE VERIFIED' is displayed
  AND the transaction is skipped
  AND the account lookup is NOT attempted
```

### Scenario 3: Transaction rejected — account not found

```gherkin
GIVEN a daily transaction for card 4000000000000001
  AND the XREF record links to account 41000000099
  AND account 41000000099 does NOT exist in ACCTFILE
WHEN the account lookup is performed
THEN WS-ACCT-READ-STATUS = 4 (INVALID KEY)
  AND the message 'ACCOUNT NOT FOUND' is displayed
  AND the transaction is skipped
```

### Scenario 4: Multiple cards linked to same account

```gherkin
GIVEN two XREF records:
  | Card | Account | Customer |
  | 4000000000000001 | 41000000001 | 123456789 |
  | 4000000000000002 | 41000000001 | 123456789 |
WHEN transactions for both cards are verified
THEN both resolve to the same account 41000000001
  AND both are accepted for processing against the same account
```

### Scenario 5: All five entity files opened for verification

```gherkin
GIVEN the CBTRN01C batch program starts
WHEN initialization completes
THEN all five files are opened:
  | File | Mode | Purpose |
  | DALYTRAN-FILE | INPUT | Source transactions |
  | CUSTOMER-FILE | INPUT | Customer lookup |
  | XREF-FILE | INPUT | Card-account mapping |
  | CARD-FILE | INPUT | Card record access |
  | ACCOUNT-FILE | INPUT | Account verification |
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| GDPR | Art. 5(1)(d) | Accuracy — personal data must be accurate and kept up to date | The XREF lookup validates that card-account-customer relationships are consistent before processing transactions |
| AML 2017:11 | Art. 11 | Customer identification — institutions must verify the identity of the customer for each transaction | The three-way relationship chain (card → XREF → account → customer) ensures every transaction is attributable to a verified customer |
| PSD2 | Art. 97 | Strong customer authentication — payment transactions must be linked to an authenticated customer | The XREF linkage from card to customer provides the authentication context for each transaction |
| FSA FFFS 2014:5 | Ch. 3 | Accounting records must maintain accurate entity relationships | The five-file verification ensures referential integrity across the entity model before any financial posting |

## Edge Cases

1. **Orphaned XREF records**: If an account is deleted but the XREF record remains, the XREF lookup succeeds but the account lookup fails. The system correctly handles this with the two-stage validation, rejecting the transaction.

2. **Orphaned accounts**: If an account exists but no XREF record points to it, the account is effectively unreachable for transaction processing. This could occur if the XREF write failed during account opening (ACCT-BR-011). The migrated system should include referential integrity checks.

3. **Card reissuance**: When a card is reissued with a new number, the old XREF record must be updated or replaced. During the transition period, both old and new card numbers may need to resolve to the same account.

4. **Customer ID mismatch**: The XREF provides both customer ID and account ID, but CBTRN01C does not verify that the customer record exists (it opens CUSTOMER-FILE but doesn't explicitly read it in the verification loop). The migrated system should validate the full chain including customer.

5. **Batch processing order**: CBTRN01C processes daily transactions sequentially. If a transaction references a card that was created earlier in the same day's batch but not yet in the XREF file, it would be rejected. The migrated system should ensure XREF updates are committed before transaction verification runs.

6. **FILLER in XREF record**: The 14-byte FILLER in CVACT03Y.cpy may contain additional relationship metadata (e.g., relationship type, authorization level, effective date) not visible in the copybook. The mainframe team should clarify the FILLER contents.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Is CBTRN01C a standalone verification step, or is it the first stage of the CBTRN02C posting pipeline? (2) What is in the 14-byte FILLER of the XREF record — relationship type, dates, or unused space? (3) Can a customer have multiple accounts (1:N customer-to-account), and if so, how are they distinguished in the XREF? (4) Why does CBTRN01C open the CUSTOMER-FILE and CARD-FILE but not appear to read them in the main loop — are there additional verification steps not visible in the available source? (5) Is there a separate program for managing XREF records (add, update, delete)?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
