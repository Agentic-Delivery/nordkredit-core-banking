---
id: "USEC-BR-007"
title: "User update with field-level change detection and rewrite"
domain: "user-security"
cobol_source: "COUSR02C.cbl:115-274"
requirement_id: "USEC-BR-007"
regulations:
  - "FFFS 2014:5 Ch. 6 — User access management"
  - "DORA Art. 9 — ICT access control maintenance"
  - "GDPR Art. 5 — Data accuracy"
  - "GDPR Art. 16 — Right to rectification"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# USEC-BR-007: User update with field-level change detection and rewrite

## Summary

The User Update program (COUSR02C) allows admin users to modify existing user records in the USRSEC file. The workflow is two-phase: first the admin enters or receives a User ID (from the User List screen), and the program reads and displays the current record. The admin then modifies fields (First Name, Last Name, Password, User Type) and presses PF5 to save. Before writing, the program performs field-level change detection — comparing each screen field against the stored record. If no fields have changed, the program displays an error message ("Please modify to update ...") and does not write. If at least one field changed, it performs a CICS REWRITE to persist the updated record. All five fields (User ID, First Name, Last Name, Password, User Type) must be non-empty for the update to proceed.

## Business Logic

### Pseudocode

```
// Phase 1: Lookup (Enter key)
RECEIVE screen input

IF User-ID is empty:
    ERROR 'User ID can NOT be empty...'
    Re-display screen

READ USRSEC file with UPDATE intent using User-ID as key

EVALUATE read response:
  WHEN record found (NORMAL):
    Display stored values: First Name, Last Name, Password, User Type
    Display 'Press PF5 key to save your updates ...'
  WHEN record not found (NOTFND):
    ERROR 'User ID NOT found...'
  WHEN other error:
    ERROR 'Unable to lookup User...'

// Phase 2: Save (PF5 key)
VALIDATE all fields are non-empty (sequential):
  IF User-ID empty:    ERROR 'User ID can NOT be empty...'
  IF First-Name empty: ERROR 'First Name can NOT be empty...'
  IF Last-Name empty:  ERROR 'Last Name can NOT be empty...'
  IF Password empty:   ERROR 'Password can NOT be empty...'
  IF User-Type empty:  ERROR 'User Type can NOT be empty...'

READ USRSEC file with UPDATE intent (re-read for REWRITE)

// Field-level change detection:
SET modified = false
IF screen-First-Name != stored-First-Name:  UPDATE stored, SET modified = true
IF screen-Last-Name  != stored-Last-Name:   UPDATE stored, SET modified = true
IF screen-Password   != stored-Password:    UPDATE stored, SET modified = true
IF screen-User-Type  != stored-User-Type:   UPDATE stored, SET modified = true

IF modified:
    CICS REWRITE USRSEC record
    EVALUATE response:
      WHEN success (NORMAL):
        Display (green) 'User <ID> has been updated ...'
      WHEN not found (NOTFND):
        ERROR 'User ID NOT found...'
      WHEN other:
        ERROR 'Unable to Update User...'
ELSE:
    ERROR (red) 'Please modify to update ...'
```

### Decision Table

| Phase | User ID | Fields Valid | Record Found | Fields Changed | Outcome |
|-------|---------|-------------|-------------|----------------|---------|
| Lookup | Empty | - | - | - | Error: "User ID can NOT be empty..." |
| Lookup | Provided | - | No | - | Error: "User ID NOT found..." |
| Lookup | Provided | - | Yes | - | Display current values, prompt PF5 |
| Save | Empty | - | - | - | Error: "User ID can NOT be empty..." |
| Save | Provided | No (any empty) | - | - | Error: "\<field\> can NOT be empty..." |
| Save | Provided | Yes | Yes | No | Error: "Please modify to update ..." |
| Save | Provided | Yes | Yes | Yes | Success: "User \<ID\> has been updated ..." |
| Save | Provided | Yes | No (deleted between lookup and save) | - | Error: "User ID NOT found..." |

### Key Navigation

| Key | Action |
|-----|--------|
| Enter | Look up User ID and display current values |
| PF3 | Save pending changes (if any), then return to calling program |
| PF4 | Clear all fields |
| PF5 | Save/commit updates to USRSEC file |
| PF12 | Return to Admin Menu (COADM01C) without saving |
| Other | Error: invalid key message |

## Source COBOL Reference

**Program:** `COUSR02C.cbl`
**Lines:** 115-274 (PROCESS-ENTER-KEY, UPDATE-USER-INFO, UPDATE-USER-SEC-FILE)

```cobol
       PROCESS-ENTER-KEY.

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'User ID can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO USRIDINL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
               WHEN OTHER
                   MOVE -1       TO USRIDINL OF COUSR2AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE SPACES      TO FNAMEI   OF COUSR2AI
                                   LNAMEI   OF COUSR2AI
                                   PASSWDI  OF COUSR2AI
                                   USRTYPEI OF COUSR2AI
               MOVE USRIDINI  OF COUSR2AI TO SEC-USR-ID
               PERFORM READ-USER-SEC-FILE
           END-IF.

           IF NOT ERR-FLG-ON
               MOVE SEC-USR-FNAME      TO FNAMEI    OF COUSR2AI
               MOVE SEC-USR-LNAME      TO LNAMEI    OF COUSR2AI
               MOVE SEC-USR-PWD        TO PASSWDI   OF COUSR2AI
               MOVE SEC-USR-TYPE       TO USRTYPEI  OF COUSR2AI
               PERFORM SEND-USRUPD-SCREEN
           END-IF.

       UPDATE-USER-INFO.

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'User ID can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO USRIDINL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
               WHEN FNAMEI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'First Name can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO FNAMEL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
               WHEN LNAMEI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Last Name can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO LNAMEL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
               WHEN PASSWDI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Password can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO PASSWDL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
               WHEN USRTYPEI OF COUSR2AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'User Type can NOT be empty...' TO WS-MESSAGE
                   MOVE -1       TO USRTYPEL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
               WHEN OTHER
                   MOVE -1       TO FNAMEL OF COUSR2AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE USRIDINI  OF COUSR2AI TO SEC-USR-ID
               PERFORM READ-USER-SEC-FILE

               IF FNAMEI  OF COUSR2AI NOT = SEC-USR-FNAME
                   MOVE FNAMEI   OF COUSR2AI TO SEC-USR-FNAME
                   SET USR-MODIFIED-YES TO TRUE
               END-IF
               IF LNAMEI  OF COUSR2AI NOT = SEC-USR-LNAME
                   MOVE LNAMEI   OF COUSR2AI TO SEC-USR-LNAME
                   SET USR-MODIFIED-YES TO TRUE
               END-IF
               IF PASSWDI  OF COUSR2AI NOT = SEC-USR-PWD
                   MOVE PASSWDI  OF COUSR2AI TO SEC-USR-PWD
                   SET USR-MODIFIED-YES TO TRUE
               END-IF
               IF USRTYPEI  OF COUSR2AI NOT = SEC-USR-TYPE
                   MOVE USRTYPEI OF COUSR2AI TO SEC-USR-TYPE
                   SET USR-MODIFIED-YES TO TRUE
               END-IF

               IF USR-MODIFIED-YES
                   PERFORM UPDATE-USER-SEC-FILE
               ELSE
                   MOVE 'Please modify to update ...' TO WS-MESSAGE
                   MOVE DFHRED       TO ERRMSGC  OF COUSR2AO
                   PERFORM SEND-USRUPD-SCREEN
               END-IF
           END-IF.

       UPDATE-USER-SEC-FILE.

           EXEC CICS REWRITE
                DATASET   (WS-USRSEC-FILE)
                FROM      (SEC-USER-DATA)
                LENGTH    (LENGTH OF SEC-USER-DATA)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   MOVE SPACES             TO WS-MESSAGE
                   MOVE DFHGREEN           TO ERRMSGC  OF COUSR2AO
                   STRING 'User '     DELIMITED BY SIZE
                          SEC-USR-ID  DELIMITED BY SPACE
                          ' has been updated ...' DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   PERFORM SEND-USRUPD-SCREEN
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'User ID NOT found...' TO WS-MESSAGE
                   MOVE -1       TO USRIDINL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to Update User...' TO WS-MESSAGE
                   MOVE -1       TO FNAMEL OF COUSR2AI
                   PERFORM SEND-USRUPD-SCREEN
           END-EVALUATE.
```

**Copybook:** `CSUSR01Y.cpy` (User security record)

```cobol
 01 SEC-USER-DATA.
   05 SEC-USR-ID                 PIC X(08).
   05 SEC-USR-FNAME              PIC X(20).
   05 SEC-USR-LNAME              PIC X(20).
   05 SEC-USR-PWD                PIC X(08).
   05 SEC-USR-TYPE               PIC X(01).
   05 SEC-USR-FILLER             PIC X(23).
```

## Acceptance Criteria

### Scenario 1: Look up existing user by ID

```gherkin
GIVEN an admin user is on the User Update screen (COUSR2A)
WHEN the admin enters User ID "USER0001" and presses Enter
THEN the system reads the USRSEC file for User ID "USER0001"
  AND displays the stored First Name, Last Name, Password, and User Type
  AND displays "Press PF5 key to save your updates ..."
```

### Scenario 2: User ID not found on lookup

```gherkin
GIVEN an admin user is on the User Update screen (COUSR2A)
WHEN the admin enters User ID "NOTEXIST" and presses Enter
THEN the system displays "User ID NOT found..."
  AND the cursor is positioned on the User ID field
```

### Scenario 3: Successful field update

```gherkin
GIVEN an admin user has looked up User ID "USER0001"
  AND the current Last Name is "Smith"
WHEN the admin changes Last Name to "Johnson" and presses PF5
THEN the system detects the Last Name field has changed
  AND rewrites the USRSEC record with the new Last Name
  AND displays "User USER0001 has been updated ..." in green
```

### Scenario 4: No changes detected

```gherkin
GIVEN an admin user has looked up User ID "USER0001"
  AND no fields have been modified
WHEN the admin presses PF5
THEN the system displays "Please modify to update ..." in red
  AND the USRSEC record is NOT rewritten
```

### Scenario 5: Empty required field on save

```gherkin
GIVEN an admin user has looked up User ID "USER0001"
  AND the admin clears the First Name field
WHEN the admin presses PF5
THEN the system displays "First Name can NOT be empty..."
  AND the cursor is positioned on the First Name field
  AND the USRSEC record is NOT modified
```

### Scenario 6: Empty User ID on lookup

```gherkin
GIVEN an admin user is on the User Update screen (COUSR2A)
  AND the User ID field is empty
WHEN the admin presses Enter
THEN the system displays "User ID can NOT be empty..."
  AND the cursor is positioned on the User ID field
```

### Scenario 7: PF3 saves pending changes before exiting

```gherkin
GIVEN an admin user has modified fields for User ID "USER0001"
WHEN the admin presses PF3
THEN the system saves any pending updates via UPDATE-USER-INFO
  AND transfers control to the calling program (or COADM01C)
```

### Scenario 8: PF4 clears all fields

```gherkin
GIVEN an admin user is on the User Update screen (COUSR2A)
  AND some fields contain data
WHEN the admin presses PF4
THEN all input fields are cleared to spaces
  AND the cursor is positioned on the User ID field
```

### Scenario 9: Pre-populated User ID from User List

```gherkin
GIVEN an admin user selected User ID "USER0001" for update from the User List (COUSR00C)
WHEN the User Update screen (COUSR2A) is first displayed
THEN the User ID field is pre-populated with "USER0001"
  AND the system automatically looks up and displays the user's current data
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 6 | User access management — maintenance of user accounts | Provides admin capability to maintain user credentials and roles |
| DORA | Art. 9 | ICT access control — ongoing maintenance of access rights | Supports modification of user types and credentials as part of access lifecycle management |
| GDPR | Art. 5(1)(d) | Data accuracy — personal data shall be accurate and kept up to date | User update function enables correction of user name and credential data |
| GDPR | Art. 16 | Right to rectification | The update function provides the mechanism to correct inaccurate personal data |

## Edge Cases

1. **User ID is immutable**: The User ID field is used as the lookup key and cannot be changed through this screen. To "change" a User ID, the admin must delete the old record and create a new one. The migrated system should consider whether User ID renaming is a requirement.

2. **READ with UPDATE intent**: The program issues `CICS READ ... UPDATE` which acquires a record-level lock in the VSAM file. This prevents concurrent modifications but also means the record is locked between the read and the rewrite. If the admin abandons the screen without saving, the lock is released when the task ends (pseudo-conversational return). The migrated system should use optimistic concurrency (e.g., ETags or row versions) rather than pessimistic locking.

3. **PF3 auto-saves**: Pressing PF3 (typically "Exit") first calls UPDATE-USER-INFO, which will save any pending changes before returning to the previous screen. This is non-standard UI behavior — most users expect PF3 to exit without saving. The migrated system should clarify save-on-exit behavior (prompt for unsaved changes).

4. **No password change confirmation**: The password field is displayed and can be changed without requiring the current password or a confirmation entry. The migrated system should require current password verification for password changes and a confirmation field to prevent typos.

5. **No change audit trail**: User modifications are not logged. The migrated system must log all user attribute changes (who changed what, when) per DORA Art. 9 and FFFS 2014:5.

6. **Plaintext password displayed**: The existing password is displayed in the Password field in plaintext when the record is loaded. The migrated system must never display stored passwords.

7. **No User Type validation**: Like user creation, the User Type field accepts any single character. Changing a user from type 'U' to type 'A' (or vice versa) has no confirmation or approval workflow. The migrated system should require authorization for privilege escalation.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - Should password changes require re-authentication or a separate workflow?
  - Is the PF3 auto-save behavior intentional, or should exit discard unsaved changes?
  - Should privilege escalation (changing User Type from 'U' to 'A') require additional authorization?
  - Are there any fields that should be read-only after initial creation (e.g., User ID)?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
