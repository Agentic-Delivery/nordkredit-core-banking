---
id: "USEC-BR-008"
title: "User deletion with read-before-delete confirmation pattern"
domain: "user-security"
cobol_source: "COUSR03C.cbl:115-250"
requirement_id: "USEC-BR-008"
regulations:
  - "FFFS 2014:5 Ch. 6 — User access deprovisioning"
  - "DORA Art. 9 — ICT access control revocation"
  - "GDPR Art. 17 — Right to erasure"
  - "GDPR Art. 5 — Storage limitation"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# USEC-BR-008: User deletion with read-before-delete confirmation pattern

## Summary

The User Delete program (COUSR03C) allows admin users to delete user records from the USRSEC file. The workflow follows a read-before-delete pattern: the admin first enters (or receives from the User List) a User ID and presses Enter to look up and display the user's details (First Name, Last Name, User Type — but NOT the password). After reviewing the record, the admin confirms deletion by pressing PF5. The program reads the record again with UPDATE intent (acquiring a lock) and then performs a CICS DELETE. On success, the form is cleared and a confirmation message is displayed. The deletion is immediate and permanent — there is no soft-delete, undo capability, or confirmation dialog beyond the two-step review-then-delete pattern.

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
    Display stored values: First Name, Last Name, User Type
    Display 'Press PF5 key to delete this user ...'
  WHEN record not found (NOTFND):
    ERROR 'User ID NOT found...'
  WHEN other error:
    ERROR 'Unable to lookup User...'

// Phase 2: Delete (PF5 key)
IF User-ID is empty:
    ERROR 'User ID can NOT be empty...'

READ USRSEC file with UPDATE intent (re-read to acquire lock)

IF record found:
    CICS DELETE from USRSEC dataset

    EVALUATE delete response:
      WHEN success (NORMAL):
        Clear all fields
        Display (green) 'User <ID> has been deleted ...'
      WHEN not found (NOTFND):
        ERROR 'User ID NOT found...'
      WHEN other error:
        ERROR 'Unable to Update User...'    // Note: message says "Update" — bug in original
```

### Decision Table

| Phase | User ID | Record Found | Outcome |
|-------|---------|-------------|---------|
| Lookup | Empty | - | Error: "User ID can NOT be empty..." |
| Lookup | Provided | No | Error: "User ID NOT found..." |
| Lookup | Provided | Yes | Display user details, prompt PF5 to confirm |
| Delete | Empty | - | Error: "User ID can NOT be empty..." |
| Delete | Provided | No (deleted between lookup and delete) | Error: "User ID NOT found..." |
| Delete | Provided | Yes | Success: "User \<ID\> has been deleted ..." |

### Key Navigation

| Key | Action |
|-----|--------|
| Enter | Look up User ID and display details for review |
| PF3 | Return to calling program (COADM01C or previous) |
| PF4 | Clear all fields |
| PF5 | Confirm and execute deletion |
| PF12 | Return to Admin Menu (COADM01C) |
| Other | Error: invalid key message |

### Fields Displayed (Read-Only Review)

| Field | Source | Displayed |
|-------|--------|-----------|
| User ID | Screen input / COMMAREA | Yes (input field) |
| First Name | SEC-USR-FNAME | Yes (display only) |
| Last Name | SEC-USR-LNAME | Yes (display only) |
| Password | SEC-USR-PWD | No — not displayed on delete screen |
| User Type | SEC-USR-TYPE | Yes (display only) |

## Source COBOL Reference

**Program:** `COUSR03C.cbl`
**Lines:** 115-250 (PROCESS-ENTER-KEY, DELETE-USER-INFO, DELETE-USER-SEC-FILE)

```cobol
       PROCESS-ENTER-KEY.

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR3AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'User ID can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO USRIDINL OF COUSR3AI
                   PERFORM SEND-USRDEL-SCREEN
               WHEN OTHER
                   MOVE -1       TO USRIDINL OF COUSR3AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE SPACES      TO FNAMEI   OF COUSR3AI
                                   LNAMEI   OF COUSR3AI
                                   USRTYPEI OF COUSR3AI
               MOVE USRIDINI  OF COUSR3AI TO SEC-USR-ID
               PERFORM READ-USER-SEC-FILE
           END-IF.

           IF NOT ERR-FLG-ON
               MOVE SEC-USR-FNAME      TO FNAMEI    OF COUSR3AI
               MOVE SEC-USR-LNAME      TO LNAMEI    OF COUSR3AI
               MOVE SEC-USR-TYPE       TO USRTYPEI  OF COUSR3AI
               PERFORM SEND-USRDEL-SCREEN
           END-IF.

       DELETE-USER-INFO.

           EVALUATE TRUE
               WHEN USRIDINI OF COUSR3AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'User ID can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO USRIDINL OF COUSR3AI
                   PERFORM SEND-USRDEL-SCREEN
               WHEN OTHER
                   MOVE -1       TO USRIDINL OF COUSR3AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE USRIDINI  OF COUSR3AI TO SEC-USR-ID
               PERFORM READ-USER-SEC-FILE
               PERFORM DELETE-USER-SEC-FILE
           END-IF.

       DELETE-USER-SEC-FILE.

           EXEC CICS DELETE
                DATASET   (WS-USRSEC-FILE)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   PERFORM INITIALIZE-ALL-FIELDS
                   MOVE SPACES             TO WS-MESSAGE
                   MOVE DFHGREEN           TO ERRMSGC  OF COUSR3AO
                   STRING 'User '     DELIMITED BY SIZE
                          SEC-USR-ID  DELIMITED BY SPACE
                          ' has been deleted ...' DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   PERFORM SEND-USRDEL-SCREEN
               WHEN DFHRESP(NOTFND)
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'User ID NOT found...' TO
                                   WS-MESSAGE
                   MOVE -1       TO USRIDINL OF COUSR3AI
                   PERFORM SEND-USRDEL-SCREEN
               WHEN OTHER
                   DISPLAY 'RESP:' WS-RESP-CD 'REAS:' WS-REAS-CD
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Unable to Update User...' TO
                                   WS-MESSAGE
                   MOVE -1       TO FNAMEL OF COUSR3AI
                   PERFORM SEND-USRDEL-SCREEN
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

### Scenario 1: Look up existing user for deletion

```gherkin
GIVEN an admin user is on the User Delete screen (COUSR3A)
WHEN the admin enters User ID "USER0001" and presses Enter
THEN the system reads the USRSEC file for User ID "USER0001"
  AND displays the stored First Name, Last Name, and User Type
  AND does NOT display the password
  AND displays "Press PF5 key to delete this user ..."
```

### Scenario 2: Confirm deletion with PF5

```gherkin
GIVEN an admin user has reviewed User ID "USER0001" on the delete screen
WHEN the admin presses PF5
THEN the system reads the USRSEC record with UPDATE intent
  AND deletes the record from the USRSEC file
  AND clears all input fields
  AND displays "User USER0001 has been deleted ..." in green
```

### Scenario 3: User ID not found on lookup

```gherkin
GIVEN an admin user is on the User Delete screen (COUSR3A)
WHEN the admin enters User ID "NOTEXIST" and presses Enter
THEN the system displays "User ID NOT found..."
  AND the cursor is positioned on the User ID field
```

### Scenario 4: Empty User ID on lookup

```gherkin
GIVEN an admin user is on the User Delete screen (COUSR3A)
  AND the User ID field is empty
WHEN the admin presses Enter
THEN the system displays "User ID can NOT be empty..."
  AND the cursor is positioned on the User ID field
```

### Scenario 5: Empty User ID on delete attempt

```gherkin
GIVEN an admin user is on the User Delete screen (COUSR3A)
  AND the User ID field is empty
WHEN the admin presses PF5
THEN the system displays "User ID can NOT be empty..."
  AND the cursor is positioned on the User ID field
  AND no deletion is performed
```

### Scenario 6: User deleted between lookup and delete

```gherkin
GIVEN an admin user has looked up User ID "USER0001" on the delete screen
  AND another admin deletes "USER0001" before this admin presses PF5
WHEN the admin presses PF5
THEN the system attempts to read the record
  AND displays "User ID NOT found..."
  AND no deletion is performed
```

### Scenario 7: Pre-populated User ID from User List

```gherkin
GIVEN an admin user selected User ID "USER0001" for deletion from the User List (COUSR00C)
WHEN the User Delete screen (COUSR3A) is first displayed
THEN the User ID field is pre-populated with "USER0001"
  AND the system automatically looks up and displays the user's details
```

### Scenario 8: PF4 clears all fields

```gherkin
GIVEN an admin user is on the User Delete screen (COUSR3A)
  AND user details are displayed
WHEN the admin presses PF4
THEN all input fields are cleared to spaces
  AND the cursor is positioned on the User ID field
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 6 | User access deprovisioning | Provides formal process for removing user access from the system |
| DORA | Art. 9 | ICT access control — revocation of access rights | Supports timely removal of user accounts when access is no longer required |
| GDPR | Art. 17 | Right to erasure | Provides mechanism to delete user data, supporting data subject erasure requests |
| GDPR | Art. 5(1)(e) | Storage limitation — data kept no longer than necessary | Hard-delete removes the record entirely, ensuring data is not retained beyond its purpose |

## Edge Cases

1. **Hard delete with no soft-delete option**: The deletion is permanent — the VSAM record is physically removed via CICS DELETE. There is no soft-delete, recycle bin, or undo capability. The migrated system should consider implementing soft-delete (status flag) with a configurable retention period before physical deletion, to allow recovery from accidental deletions and to comply with audit trail requirements.

2. **No deletion confirmation dialog**: The two-step pattern (lookup then PF5) serves as the confirmation mechanism. However, there is no explicit "Are you sure?" prompt. The migrated system should implement a confirmation dialog for destructive operations.

3. **Password not displayed on delete screen**: Unlike the update screen, the delete screen does not display the password field. This is a security-conscious design choice — the password is not needed for deletion review. The migrated system should follow this pattern.

4. **Error message bug — "Unable to Update User..."**: The DELETE-USER-SEC-FILE paragraph's WHEN OTHER error handler displays "Unable to Update User..." instead of "Unable to Delete User...". This is a copy-paste bug from COUSR02C. The migrated system should use the correct error message.

5. **No cascade delete**: Deleting a user from the USRSEC file does not cascade to any related records (e.g., transaction history, audit logs). The migrated system must consider referential integrity — should user deletion cascade, or should it be blocked if the user has associated records?

6. **No self-deletion prevention**: An admin user can delete their own account. The migrated system should prevent self-deletion or at minimum require a second admin to approve.

7. **No "last admin" protection**: There is no check to prevent deleting the last remaining admin user, which would lock everyone out of admin functions. The migrated system should enforce that at least one admin account must remain active.

8. **READ with UPDATE for delete**: The program reads with UPDATE intent before DELETE, which is the correct CICS pattern (the DELETE operates on the last READ UPDATE record). This ensures the record is locked during the delete operation.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - Should user deletion be a hard delete or soft delete (deactivation)?
  - Are there regulatory retention requirements that prevent immediate deletion of user records?
  - Should deletion require approval from a second admin (dual-control)?
  - What happens to transactions or audit records associated with a deleted user?
  - Should the system prevent deletion of the last admin account?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
