---
id: "acct-br-005"
title: "Optimistic concurrency control for account updates"
domain: "account-management"
cobol_source: "COACTUPC.cbl:3888-4195"
requirement_id: "ACCT-BR-005"
regulations:
  - "PSD2 Art. 97"
  - "FSA FFFS 2014:5 Ch. 4 S3"
  - "DORA Art. 9"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-005: Optimistic concurrency control for account updates

## Summary

The account update program (COACTUPC) implements a dual-layer concurrency control strategy to ensure data integrity during multi-user account updates. Layer 1 uses pessimistic locking via CICS READ UPDATE to obtain exclusive locks on both the account (ACCTDAT) and customer (CUSTDAT) records before any write. Layer 2 performs an optimistic concurrency check after locking by comparing every field in the currently locked records against the snapshot taken when the user first loaded the screen. If any field has changed since the snapshot was taken, the write is abandoned, the user is informed that another user modified the record, and the screen is refreshed with the latest data. Write operations follow a strict order (account first, customer second) with SYNCPOINT ROLLBACK if the second write fails, ensuring transactional atomicity across the two-file update. A pre-validation change detection step (before locking) compares old and new user-entered values using case-insensitive trimmed comparison to avoid unnecessary lock acquisition when no actual changes were made.

## Business Logic

### Pseudocode

```
PERFORM 3900-WRITE-PROCESSING:

    -- Step 0: Pre-validation change detection (lines 1681-1779)
    -- Already performed before reaching write processing
    -- Compares FUNCTION UPPER-CASE(FUNCTION TRIM(new-value))
    --    with FUNCTION UPPER-CASE(FUNCTION TRIM(old-value))
    -- If no changes detected: message + abort (no lock acquired)

    -- Step 1: Pessimistic locking - Lock account record
    EXEC CICS READ UPDATE
        FILE('ACCTDAT')
        INTO(WS-ACCT-RECORD)
        RIDFLD(ACCT-ID)
        RESP(WS-RESP-CD)
    END-EXEC

    IF WS-RESP-CD NOT = DFHRESP(NORMAL)
        SET COULD-NOT-LOCK-ACCT-FOR-UPDATE = TRUE
        SET ACUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE  -- state 'L'
        EXIT PARAGRAPH
    END-IF

    -- Step 2: Pessimistic locking - Lock customer record
    EXEC CICS READ UPDATE
        FILE('CUSTDAT')
        INTO(WS-CUST-RECORD)
        RIDFLD(CUST-ID)
        RESP(WS-RESP-CD)
    END-EXEC

    IF WS-RESP-CD NOT = DFHRESP(NORMAL)
        -- Must unlock the already-locked account record
        EXEC CICS UNLOCK FILE('ACCTDAT') END-EXEC
        SET COULD-NOT-LOCK-CUST-FOR-UPDATE = TRUE
        SET ACUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE  -- state 'L'
        EXIT PARAGRAPH
    END-IF

    -- Step 3: Optimistic concurrency check
    PERFORM 4100-CHECK-DATA-CHANGED

    IF DATA-WAS-CHANGED-BEFORE-UPDATE
        -- Another user changed data since we loaded
        EXEC CICS UNLOCK FILE('ACCTDAT') END-EXEC
        EXEC CICS UNLOCK FILE('CUSTDAT') END-EXEC
        PERFORM 9000-READ-ACCT  -- Re-read fresh data
        SET ACUP-SHOW-DETAILS TO TRUE  -- Back to state 'S'
        MOVE "Record changed by some one else. Please review"
            TO WS-MESSAGE
        EXIT PARAGRAPH
    END-IF

    -- Step 4: Apply updates to records
    MOVE ACUP-NEW-values TO WS-ACCT-RECORD fields
    MOVE ACUP-NEW-values TO WS-CUST-RECORD fields

    -- Step 5: Write account record (REWRITE)
    EXEC CICS REWRITE
        FILE('ACCTDAT')
        FROM(WS-ACCT-RECORD)
        RESP(WS-RESP-CD)
    END-EXEC

    IF WS-RESP-CD NOT = DFHRESP(NORMAL)
        SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE  -- state 'F'
        EXIT PARAGRAPH
    END-IF

    -- Step 6: Write customer record (REWRITE)
    EXEC CICS REWRITE
        FILE('CUSTDAT')
        FROM(WS-CUST-RECORD)
        RESP(WS-RESP-CD)
    END-EXEC

    IF WS-RESP-CD NOT = DFHRESP(NORMAL)
        -- Account was already written; must rollback
        EXEC CICS SYNCPOINT ROLLBACK END-EXEC
        SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE  -- state 'F'
        EXIT PARAGRAPH
    END-IF

    -- Step 7: Success
    SET ACUP-CHANGES-OKAYED-AND-DONE TO TRUE  -- state 'C'


4100-CHECK-DATA-CHANGED:
    SET DATA-NOT-CHANGED TO TRUE

    -- Account field comparisons
    IF WS-ACCT-ACTIVE-STATUS NOT = ACUP-OLD-ACTIVE-STATUS
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF WS-ACCT-CURR-BAL NOT = ACUP-OLD-CURR-BAL
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF WS-ACCT-CREDIT-LIMIT NOT = ACUP-OLD-CREDIT-LIMIT
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF WS-ACCT-CASH-CREDIT-LIMIT NOT = ACUP-OLD-CASH-CREDIT-LIMIT
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF WS-ACCT-CURR-CYC-CREDIT NOT = ACUP-OLD-CURR-CYC-CREDIT
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF WS-ACCT-CURR-CYC-DEBIT NOT = ACUP-OLD-CURR-CYC-DEBIT
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    -- Date comparisons (year, month, day separately)
    IF WS-ACCT-OPEN-DATE NOT = ACUP-OLD-OPEN-DATE
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF WS-ACCT-EXPIRATION-DATE NOT = ACUP-OLD-EXPIRATION-DATE
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF WS-ACCT-REISSUE-DATE NOT = ACUP-OLD-REISSUE-DATE
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    -- Group ID compared case-insensitively
    IF FUNCTION LOWER-CASE(WS-ACCT-GROUP-ID)
        NOT = FUNCTION LOWER-CASE(ACUP-OLD-GROUP-ID)
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF

    -- Customer field comparisons (UPPER-CASE)
    IF FUNCTION UPPER-CASE(WS-CUST-FIRST-NAME)
        NOT = FUNCTION UPPER-CASE(ACUP-OLD-FIRST-NAME)
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF FUNCTION UPPER-CASE(WS-CUST-MIDDLE-NAME)
        NOT = FUNCTION UPPER-CASE(ACUP-OLD-MIDDLE-NAME)
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    IF FUNCTION UPPER-CASE(WS-CUST-LAST-NAME)
        NOT = FUNCTION UPPER-CASE(ACUP-OLD-LAST-NAME)
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    -- Address fields (exact match)
    IF WS-CUST-ADDR-LINE-1 NOT = ACUP-OLD-ADDR-LINE-1
        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
    END-IF
    -- ... remaining address, phone, SSN, DOB, FICO, EFT, PRI-CARD fields
```

### Decision Table

| Lock Account | Lock Customer | Data Changed by Other User | Account Write | Customer Write | Outcome | State |
|-------------|-------------|--------------------------|--------------|---------------|---------|-------|
| Fail | N/A | N/A | N/A | N/A | Lock error | 'L' |
| Success | Fail | N/A | N/A | N/A | Lock error (unlock account) | 'L' |
| Success | Success | Yes | N/A | N/A | Concurrent change detected (unlock both, re-read) | 'S' |
| Success | Success | No | Fail | N/A | Write error | 'F' |
| Success | Success | No | Success | Fail | Rollback account write, write error | 'F' |
| Success | Success | No | Success | Success | Update complete | 'C' |

### Fields Compared in Optimistic Concurrency Check

| Category | Field | Comparison Method |
|----------|-------|------------------|
| Account | ACCT-ACTIVE-STATUS | Exact match |
| Account | ACCT-CURR-BAL | Exact match |
| Account | ACCT-CREDIT-LIMIT | Exact match |
| Account | ACCT-CASH-CREDIT-LIMIT | Exact match |
| Account | ACCT-CURR-CYC-CREDIT | Exact match |
| Account | ACCT-CURR-CYC-DEBIT | Exact match |
| Account | ACCT-OPEN-DATE (year, month, day) | Exact match (each component) |
| Account | ACCT-EXPIRAION-DATE (year, month, day) | Exact match (each component) |
| Account | ACCT-REISSUE-DATE (year, month, day) | Exact match (each component) |
| Account | ACCT-GROUP-ID | Case-insensitive (LOWER-CASE) |
| Customer | CUST-FIRST-NAME | Case-insensitive (UPPER-CASE) |
| Customer | CUST-MIDDLE-NAME | Case-insensitive (UPPER-CASE) |
| Customer | CUST-LAST-NAME | Case-insensitive (UPPER-CASE) |
| Customer | CUST-ADDR-LINE-1 | Exact match |
| Customer | CUST-ADDR-LINE-2 | Exact match |
| Customer | CUST-ADDR-LINE-3 (city) | Exact match |
| Customer | CUST-ADDR-STATE-CD | Exact match |
| Customer | CUST-ADDR-ZIP | Exact match |
| Customer | CUST-ADDR-COUNTRY-CD | Exact match |
| Customer | CUST-PHONE-NUM-1 | Exact match |
| Customer | CUST-PHONE-NUM-2 | Exact match |
| Customer | CUST-SSN | Exact match |
| Customer | CUST-DOB | Exact match |
| Customer | CUST-FICO-CREDIT-SCORE | Exact match |
| Customer | CUST-EFT-ACCOUNT-ID | Exact match |
| Customer | CUST-PRI-CARD-HOLDER-IND | Exact match |

## Source COBOL Reference

**Program:** `COACTUPC.cbl`
**Lines:** 3888-4195 (write processing and concurrency check), 1681-1779 (client-side change detection)

### Pessimistic locking - Account record (lines 3892-3915)

```cobol
003892 3900-WRITE-PROCESSING.
003893     EXEC CICS READ
003894         UPDATE
003895         FILE('ACCTDAT')
003896         INTO(WS-ACCT-RECORD)
003897         RIDFLD(WS-ACCT-ID)
003898         KEYLENGTH(LENGTH OF WS-ACCT-ID)
003899         RESP(WS-RESP-CD)
003900         RESP2(WS-REAS-CD)
003901     END-EXEC.
003902
003903     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
003904         SET COULD-NOT-LOCK-ACCT-FOR-UPDATE TO TRUE
003905         MOVE 'Unable to lock Account record for update'
003906             TO WS-MESSAGE
003907         SET ACUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE
003908         GO TO 3900-WRITE-PROCESSING-EXIT
003915     END-IF.
```

### Pessimistic locking - Customer record (lines 3917-3942)

```cobol
003917     EXEC CICS READ
003918         UPDATE
003919         FILE('CUSTDAT')
003920         INTO(WS-CUST-RECORD)
003921         RIDFLD(WS-CUST-ID)
003922         KEYLENGTH(LENGTH OF WS-CUST-ID)
003923         RESP(WS-RESP-CD)
003924         RESP2(WS-REAS-CD)
003925     END-EXEC.
003926
003927     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
003928         EXEC CICS UNLOCK FILE('ACCTDAT') END-EXEC
003929         SET COULD-NOT-LOCK-CUST-FOR-UPDATE TO TRUE
003930         MOVE 'Unable to lock Customer record for update'
003931             TO WS-MESSAGE
003932         SET ACUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE
003933         GO TO 3900-WRITE-PROCESSING-EXIT
003942     END-IF.
```

### Account REWRITE (lines 4065-4081)

```cobol
004065     EXEC CICS REWRITE
004066         FILE('ACCTDAT')
004067         FROM(WS-ACCT-RECORD)
004068         RESP(WS-RESP-CD)
004069         RESP2(WS-REAS-CD)
004070     END-EXEC.
004071
004072     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
004073         MOVE 'Unable to update Account record'
004074             TO WS-MESSAGE
004075         SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
004076         GO TO 3900-WRITE-PROCESSING-EXIT
004081     END-IF.
```

### Customer REWRITE with SYNCPOINT ROLLBACK (lines 4085-4103)

```cobol
004085     EXEC CICS REWRITE
004086         FILE('CUSTDAT')
004087         FROM(WS-CUST-RECORD)
004088         RESP(WS-RESP-CD)
004089         RESP2(WS-REAS-CD)
004090     END-EXEC.
004091
004092     IF WS-RESP-CD NOT = DFHRESP(NORMAL)
004093         MOVE 'Unable to update Customer record'
004094             TO WS-MESSAGE
004095         SET ACUP-CHANGES-OKAYED-BUT-FAILED TO TRUE
004096         EXEC CICS SYNCPOINT ROLLBACK END-EXEC
004100         GO TO 3900-WRITE-PROCESSING-EXIT
004103     END-IF.
```

### Optimistic concurrency check - Account fields (lines 4112-4145)

```cobol
004109 4100-CHECK-DATA-CHANGED.
004110     SET DATA-NOT-CHANGED TO TRUE.
004112     IF ACCT-ACTIVE-STATUS OF WS-ACCT-RECORD
004113         NOT = ACUP-OLD-ACTIVE-STATUS
004114         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004115     END-IF.
004116     IF ACCT-CURR-BAL OF WS-ACCT-RECORD
004117         NOT = ACUP-OLD-CURR-BAL
004118         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004119     END-IF.
004120     IF ACCT-CREDIT-LIMIT OF WS-ACCT-RECORD
004121         NOT = ACUP-OLD-CREDIT-LIMIT
004122         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004123     END-IF.
004130     IF FUNCTION LOWER-CASE(ACCT-GROUP-ID OF WS-ACCT-RECORD)
004131         NOT = FUNCTION LOWER-CASE(ACUP-OLD-GROUP-ID)
004132         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004133     END-IF.
```

### Optimistic concurrency check - Customer fields (lines 4147-4191)

```cobol
004147     IF FUNCTION UPPER-CASE(CUST-FIRST-NAME OF WS-CUST-RECORD)
004148         NOT = FUNCTION UPPER-CASE(ACUP-OLD-FIRST-NAME)
004149         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004150     END-IF.
004151     IF FUNCTION UPPER-CASE(CUST-MIDDLE-NAME OF WS-CUST-RECORD)
004152         NOT = FUNCTION UPPER-CASE(ACUP-OLD-MIDDLE-NAME)
004153         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004154     END-IF.
004155     IF FUNCTION UPPER-CASE(CUST-LAST-NAME OF WS-CUST-RECORD)
004156         NOT = FUNCTION UPPER-CASE(ACUP-OLD-LAST-NAME)
004157         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004158     END-IF.
004170     IF CUST-SSN OF WS-CUST-RECORD
004171         NOT = ACUP-OLD-SSN
004172         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004173     END-IF.
004180     IF CUST-FICO-CREDIT-SCORE OF WS-CUST-RECORD
004181         NOT = ACUP-OLD-FICO-CREDIT-SCORE
004182         SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
004183     END-IF.
004191 4100-EXIT.
004192     EXIT.
```

### Concurrent change handling (after concurrency check)

```cobol
003950     IF DATA-WAS-CHANGED-BEFORE-UPDATE
003951         EXEC CICS UNLOCK FILE('ACCTDAT') END-EXEC
003952         EXEC CICS UNLOCK FILE('CUSTDAT') END-EXEC
003953         PERFORM 9000-READ-ACCT
003954         SET ACUP-SHOW-DETAILS TO TRUE
003955         MOVE 'Record changed by some one else. '
003956           'Please review'
003957             TO WS-MESSAGE
003958         GO TO 3900-WRITE-PROCESSING-EXIT
003960     END-IF.
```

## Acceptance Criteria

### Scenario 1: Successful update with no concurrent changes

```gherkin
GIVEN the user has loaded account "12345678901" and its data snapshot is stored
  AND the user has modified the credit limit from 5000.00 to 7500.00
  AND the changes have been validated and confirmed (state = 'N')
WHEN the system acquires locks on both the account and customer records
  AND the concurrency check confirms no other user has modified the data
THEN the account record is rewritten with the new credit limit
  AND the customer record is rewritten
  AND the state transitions to SUCCESS ('C')
  AND the message "Update successful" is displayed
```

### Scenario 2: Concurrent modification detected

```gherkin
GIVEN User A has loaded account "12345678901" with credit limit 5000.00
  AND User B has since updated the same account's credit limit to 6000.00
  AND User A has modified the credit limit to 7500.00 and confirmed
WHEN the system acquires locks and performs the concurrency check
THEN the concurrency check detects that ACCT-CREDIT-LIMIT has changed (5000.00 vs 6000.00)
  AND both locks are released
  AND the data is re-read from the files
  AND the state transitions to SHOW DETAILS ('S')
  AND the message "Record changed by some one else. Please review" is displayed
  AND the screen now shows the credit limit as 6000.00 (User B's value)
```

### Scenario 3: Account record lock failure

```gherkin
GIVEN the user has confirmed changes for account "12345678901"
  AND another transaction currently holds a lock on the account record
WHEN the system attempts to acquire a lock on the account record via READ UPDATE
THEN the lock attempt fails
  AND the state transitions to LOCK ERROR ('L')
  AND the message "Unable to lock Account record for update" is displayed
  AND no write is performed
```

### Scenario 4: Customer record lock failure after account lock

```gherkin
GIVEN the user has confirmed changes for account "12345678901"
  AND the system has successfully locked the account record
  AND another transaction currently holds a lock on the customer record
WHEN the system attempts to acquire a lock on the customer record
THEN the lock attempt fails
  AND the previously acquired account lock is released (UNLOCK)
  AND the state transitions to LOCK ERROR ('L')
  AND the message "Unable to lock Customer record for update" is displayed
```

### Scenario 5: Customer write failure triggers SYNCPOINT ROLLBACK

```gherkin
GIVEN the system has locked both account and customer records
  AND the concurrency check has passed
  AND the account REWRITE has completed successfully
WHEN the customer REWRITE fails
THEN CICS SYNCPOINT ROLLBACK is executed
  AND the successful account write is rolled back
  AND the state transitions to WRITE ERROR ('F')
  AND the message "Unable to update Customer record" is displayed
  AND no partial update remains in either file
```

### Scenario 6: No changes detected skips locking entirely

```gherkin
GIVEN the user has loaded account "12345678901"
  AND the user has not modified any field values (or only changed case/whitespace)
WHEN the user presses ENTER
THEN the pre-validation change detection compares trimmed uppercase values
  AND determines no meaningful changes exist
  AND the message "No change detected with respect to values fetched." is displayed
  AND no lock acquisition or write processing occurs
```

### Scenario 7: Case-insensitive group ID comparison

```gherkin
GIVEN the user has loaded account "12345678901" with group ID "GroupA"
  AND another user has changed the group ID to "GROUPA" (same value, different case)
WHEN the system performs the optimistic concurrency check
THEN the FUNCTION LOWER-CASE comparison of "groupa" equals "groupa"
  AND the concurrency check does NOT flag this as a change
  AND the write proceeds normally
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication and secure communication for payment account modifications | The dual-layer concurrency control ensures that account modifications are atomic and consistent; the optimistic check prevents silent overwrites of concurrent changes, maintaining data integrity for payment accounts |
| FSA FFFS 2014:5 | Ch. 4 S3 | Operational risk management for IT systems processing financial data | The pessimistic locking prevents simultaneous conflicting writes; the SYNCPOINT ROLLBACK ensures atomicity across the two-file update, preventing partial updates that could leave account and customer data inconsistent |
| DORA | Art. 9 | ICT risk management framework requires data integrity protection | The field-by-field concurrency check (26 fields across account and customer records) provides granular change detection; the rollback mechanism protects against partial failures; the migrated system must implement equivalent transactional guarantees using database transactions |

## Edge Cases

1. **Lock timeout vs. immediate failure**: The COBOL program uses RESP checking without TIMEOUT, meaning CICS uses the default DTIMOUT for the transaction. If the lock is held by a long-running transaction, the requesting transaction may wait until timeout before returning NOTFND. The migrated system should implement configurable lock timeouts and distinguish between "record locked by another user" and "record does not exist."

2. **SYNCPOINT ROLLBACK scope**: CICS SYNCPOINT ROLLBACK undoes all recoverable resource changes since the last syncpoint (or start of task). If other files or queues were updated earlier in the same transaction (unlikely in this program but possible in future extensions), the rollback would undo those changes too. The migrated system should use database savepoints to limit rollback scope to the current operation.

3. **Partial field comparison inconsistency**: Account group ID uses LOWER-CASE comparison while customer names use UPPER-CASE comparison. Address fields use exact match. This inconsistency means that a case change in an address field would be flagged as a concurrent modification, while a case change in a name field would not. The migrated system should document and preserve these comparison semantics or normalize to a consistent approach with domain expert approval.

4. **Snapshot field misspelling in COBOL**: The COBOL source uses "ACCT-EXPIRAION-DATE" (misspelling of "EXPIRATION"). The migrated system must map this correctly to the target schema field name while documenting the original source field name for traceability.

5. **Race condition window**: Between the optimistic check (Step 3) and the REWRITE (Steps 5-6), another transaction could theoretically modify the data. However, since READ UPDATE holds the lock, no other transaction can acquire a write lock during this window. The migrated system must ensure that the optimistic check and write occur within the same database transaction to maintain this guarantee.

6. **Numeric field precision in comparison**: Financial fields like ACCT-CURR-BAL (PIC S9(10)V99) are compared as packed decimal. The migrated system must ensure that decimal precision is preserved exactly during comparison to avoid false positives or missed concurrent changes due to floating-point rounding.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Critical questions for validation: (1) Is the SYNCPOINT ROLLBACK sufficient for production atomicity, or should the migrated system use distributed transactions? (2) Should the case-insensitive comparison rules for group ID and customer names be preserved exactly or normalized? (3) Are there any additional fields added to the concurrency check in maintenance patches beyond the source listing? (4) What is the expected lock contention frequency in production, and should the migrated system implement retry logic for lock failures?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
