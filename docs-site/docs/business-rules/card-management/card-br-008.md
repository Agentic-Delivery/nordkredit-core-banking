---
id: "card-br-008"
title: "Optimistic concurrency control for card updates"
domain: "card-management"
cobol_source: "COCRDUPC.cbl:1420-1523"
requirement_id: "CARD-BR-008"
regulations:
  - "FFFS 2014:5 Ch. 8 §4"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# CARD-BR-008: Optimistic concurrency control for card updates

## Summary

The card update program (COCRDUPC) implements optimistic concurrency control to prevent lost updates when multiple users modify the same card record simultaneously. Before writing changes, the program re-reads the record with an UPDATE lock and compares all mutable fields (CVV code, embossed name, expiration date components, and active status) against the values originally loaded. If any field has changed since the initial read, the update is aborted, the user is shown the refreshed data, and they must review and resubmit their changes. This pattern ensures data integrity without holding long-duration locks.

## Business Logic

### Pseudocode

```
PERFORM 9200-WRITE-PROCESSING:
    READ card-record WITH UPDATE LOCK
        RIDFLD(card-number)
        RESP(response-code)

    IF response-code NOT = NORMAL
        SET COULD-NOT-LOCK-FOR-UPDATE = TRUE
        EXIT
    END-IF

    PERFORM 9300-CHECK-CHANGE-IN-REC:
        CONVERT embossed-name TO UPPERCASE (for comparison)

        IF  current-CVV       = stored-CVV
        AND current-name      = stored-name
        AND current-exp-year  = stored-exp-year
        AND current-exp-month = stored-exp-month
        AND current-exp-day   = stored-exp-day
        AND current-status    = stored-status
            CONTINUE (no conflict)
        ELSE
            SET DATA-WAS-CHANGED-BEFORE-UPDATE = TRUE
            REFRESH stored values from current record
            EXIT (abort update)
        END-IF

    IF data-was-changed
        EXIT (return to show refreshed data)
    END-IF

    PREPARE update record:
        MOVE new card number, account ID, CVV, embossed name
        FORMAT expiry date as "YYYY-MM-DD"
        MOVE active status

    REWRITE card-record
        RESP(response-code)

    IF response-code NOT = NORMAL
        SET LOCKED-BUT-UPDATE-FAILED = TRUE
    END-IF
```

### Decision Table

| Lock Acquired | Data Changed by Other | Rewrite Succeeds | Outcome |
|--------------|----------------------|------------------|---------|
| No | N/A | N/A | Error: "Could not lock record for update" |
| Yes | Yes | N/A | Abort: "Record changed by some one else. Please review" |
| Yes | No | Yes | Success: "Changes committed to database" |
| Yes | No | No | Error: "Update of record failed" |

## Source COBOL Reference

**Program:** `COCRDUPC.cbl`
**Lines:** 1420-1496 (9200-WRITE-PROCESSING), 1498-1523 (9300-CHECK-CHANGE-IN-REC)

Lock acquisition with UPDATE keyword:
```cobol
001427     EXEC CICS READ
001428          FILE      (LIT-CARDFILENAME)
001429          UPDATE
001430          RIDFLD    (WS-CARD-RID-CARDNUM)
001431          KEYLENGTH (LENGTH OF WS-CARD-RID-CARDNUM)
001432          INTO      (CARD-RECORD)
001433          LENGTH    (LENGTH OF CARD-RECORD)
001434          RESP      (WS-RESP-CD)
001435          RESP2     (WS-REAS-CD)
001436     END-EXEC
```

Lock failure handling:
```cobol
001441     IF WS-RESP-CD EQUAL TO DFHRESP(NORMAL)
001442        CONTINUE
001443     ELSE
001444        SET INPUT-ERROR                    TO TRUE
001445        IF  WS-RETURN-MSG-OFF
001446            SET COULD-NOT-LOCK-FOR-UPDATE  TO TRUE
001447        END-IF
001448        GO TO 9200-WRITE-PROCESSING-EXIT
001449     END-IF
```

Concurrent modification detection:
```cobol
001498 9300-CHECK-CHANGE-IN-REC.
001499     INSPECT CARD-EMBOSSED-NAME
001500     CONVERTING LIT-LOWER
001501             TO LIT-UPPER
001502
001503     IF  CARD-CVV-CD              EQUAL  TO CCUP-OLD-CVV-CD
001504     AND CARD-EMBOSSED-NAME       EQUAL  TO CCUP-OLD-CRDNAME
001505     AND CARD-EXPIRAION-DATE(1:4) EQUAL  TO CCUP-OLD-EXPYEAR
001506     AND CARD-EXPIRAION-DATE(6:2) EQUAL  TO CCUP-OLD-EXPMON
001507     AND CARD-EXPIRAION-DATE(9:2) EQUAL  TO CCUP-OLD-EXPDAY
001508     AND CARD-ACTIVE-STATUS       EQUAL  TO CCUP-OLD-CRDSTCD
001509         CONTINUE
001510     ELSE
001511        SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
001512        MOVE CARD-CVV-CD                 TO CCUP-OLD-CVV-CD
001513        MOVE CARD-EMBOSSED-NAME          TO CCUP-OLD-CRDNAME
001514        MOVE CARD-EXPIRAION-DATE(1:4)    TO CCUP-OLD-EXPYEAR
001515        MOVE CARD-EXPIRAION-DATE(6:2)    TO CCUP-OLD-EXPMON
001516        MOVE CARD-EXPIRAION-DATE(9:2)    TO CCUP-OLD-EXPDAY
001517        MOVE CARD-ACTIVE-STATUS          TO CCUP-OLD-CRDSTCD
001518        GO TO 9200-WRITE-PROCESSING-EXIT
001519     END-IF EXIT
```

Record preparation and rewrite:
```cobol
001461     INITIALIZE CARD-UPDATE-RECORD
001462     MOVE CCUP-NEW-CARDID             TO CARD-UPDATE-NUM
001463     MOVE CC-ACCT-ID-N                TO CARD-UPDATE-ACCT-ID
001464     MOVE CCUP-NEW-CVV-CD             TO CARD-CVV-CD-X
001465     MOVE CARD-CVV-CD-N               TO CARD-UPDATE-CVV-CD
001466     MOVE CCUP-NEW-CRDNAME            TO CARD-UPDATE-EMBOSSED-NAME
001467     STRING  CCUP-NEW-EXPYEAR
001468             '-'
001469             CCUP-NEW-EXPMON
001470             '-'
001471             CCUP-NEW-EXPDAY
001472             DELIMITED BY SIZE
001473        INTO CARD-UPDATE-EXPIRAION-DATE
001474     END-STRING
001475     MOVE CCUP-NEW-CRDSTCD            TO CARD-UPDATE-ACTIVE-STATUS
001476
001477     EXEC CICS
001478          REWRITE FILE(LIT-CARDFILENAME)
001479                  FROM(CARD-UPDATE-RECORD)
001480                  LENGTH(LENGTH OF CARD-UPDATE-RECORD)
001481                  RESP      (WS-RESP-CD)
001482                  RESP2     (WS-REAS-CD)
001483     END-EXEC.
```

## Acceptance Criteria

### Scenario 1: Successful update with no concurrent modification

```gherkin
GIVEN a user has validated changes to card "4000123456789012"
  AND no other user has modified the card since it was loaded
WHEN the user presses PF5 to confirm the update
THEN the record is locked for update
  AND the current record values match the originally loaded values
  AND the new values are written to the CARDDAT file
  AND the message "Changes committed to database" is displayed
```

### Scenario 2: Concurrent modification detected

```gherkin
GIVEN user A has loaded card "4000123456789012" with name "JOHN DOE"
  AND user B has subsequently changed the name to "JANE DOE" and saved
WHEN user A presses PF5 to confirm their changes
THEN the record is locked for update
  AND the system detects that CARD-EMBOSSED-NAME has changed
  AND the message "Record changed by some one else. Please review" is displayed
  AND the screen shows the updated data (name "JANE DOE")
  AND user A can now review the changes and decide whether to resubmit
```

### Scenario 3: Record lock failure

```gherkin
GIVEN a user has validated changes to card "4000123456789012"
  AND another transaction currently holds an update lock on the same record
WHEN the user presses PF5 to confirm the update
THEN the CICS READ UPDATE returns a non-NORMAL response
  AND the message "Could not lock record for update" is displayed
  AND no data is modified
```

### Scenario 4: Rewrite failure after successful lock

```gherkin
GIVEN a user has validated changes and the record lock was acquired
  AND the record has not been modified by another user
WHEN the REWRITE operation fails (non-NORMAL response)
THEN the message "Update of record failed" is displayed
  AND the state transitions to CHANGES-OKAYED-BUT-FAILED
```

### Scenario 5: Case-insensitive name comparison during conflict check

```gherkin
GIVEN a card was loaded with embossed name "john doe" (stored as lowercase)
  AND the system converts stored names to uppercase for comparison
WHEN the conflict check compares the current record name "JOHN DOE"
THEN no conflict is detected (comparison is case-insensitive)
  AND the update proceeds normally
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 §4 | Operational risk management — credit institutions must ensure data integrity in concurrent access scenarios | Optimistic concurrency control prevents lost updates; the lock-compare-write pattern ensures that no concurrent modification is silently overwritten |
| PSD2 | Art. 97 | Strong customer authentication for payment instrument modifications | The two-phase commit (validate → confirm → lock → verify → write) ensures that card data modifications are intentional and protected against race conditions |

## Edge Cases

1. **Lock timeout**: CICS has configurable lock wait times. If the lock wait exceeds the timeout, the READ UPDATE returns SYSIDERR or similar. The COBOL code treats any non-NORMAL response as a lock failure. The migrated system should implement a configurable lock timeout and distinguish between "record locked by another user" and "file/system error."

2. **CVV code comparison**: The conflict check includes CARD-CVV-CD even though the CVV is not user-editable in the update screen. This means if the CVV is changed through a different process (e.g., card reissue), the concurrency check will detect it. The migrated system must include all non-editable fields in the conflict check to maintain this behavior.

3. **Uppercase conversion before comparison**: The COBOL code converts the re-read embossed name to uppercase before comparison (line 1499-1501), matching the uppercase conversion done during the initial read (line 1356-1358). Without this, a case difference would trigger a false conflict. The migrated system should normalize before comparison.

4. **Expiry date format in conflict check**: The expiry date is stored as "YYYY-MM-DD" (10 chars) but compared as three separate components (year:4, month:2, day:2). The migrated system should compare the full normalized date rather than substring positions.

5. **SYNCPOINT after successful update**: The COBOL program issues `EXEC CICS SYNCPOINT` (line 470-471) when transitioning away from the update screen after success. This commits the transaction. The migrated system must ensure the database transaction is committed before navigating away.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The optimistic concurrency pattern should be evaluated against modern alternatives (e.g., ETag-based HTTP concurrency, database row versioning with rowversion/timestamp columns). The current field-by-field comparison is fragile; consider adding a version number or timestamp to the card record in the migrated system.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
