---
id: "USEC-BR-006"
title: "User creation with required field validation and duplicate detection"
domain: "user-security"
cobol_source: "COUSR01C.cbl:115-274"
requirement_id: "USEC-BR-006"
regulations:
  - "PSD2 Art. 97 — SCA credential provisioning"
  - "FFFS 2014:5 Ch. 6 — User access management"
  - "DORA Art. 9 — ICT access control provisioning"
  - "GDPR Art. 5 — Data accuracy and minimisation"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# USEC-BR-006: User creation with required field validation and duplicate detection

## Summary

The User Add program (COUSR01C) allows admin users to create new user accounts in the USRSEC file. Five fields are required: First Name, Last Name, User ID, Password, and User Type. Validation is sequential — the first empty field triggers an error and halts processing. After validation, the program writes a new record to the USRSEC file using CICS WRITE. If the User ID already exists, a DUPREC/DUPKEY error is returned and the admin is notified. On success, the form is cleared and a confirmation message is displayed.

## Business Logic

### Pseudocode

```
RECEIVE screen input from CICS MAP COUSR1A

// Phase 1: Required field validation (sequential — first error stops)
IF First-Name is empty: ERROR 'First Name can NOT be empty...'
IF Last-Name is empty:  ERROR 'Last Name can NOT be empty...'
IF User-ID is empty:    ERROR 'User ID can NOT be empty...'
IF Password is empty:   ERROR 'Password can NOT be empty...'
IF User-Type is empty:  ERROR 'User Type can NOT be empty...'

// Phase 2: Populate SEC-USER-DATA record
MOVE User-ID   -> SEC-USR-ID
MOVE First-Name -> SEC-USR-FNAME
MOVE Last-Name  -> SEC-USR-LNAME
MOVE Password   -> SEC-USR-PWD
MOVE User-Type  -> SEC-USR-TYPE

// Phase 3: Write to file
CICS WRITE to USRSEC dataset

EVALUATE response:
  WHEN success (NORMAL):
    Clear all fields
    Display (green) 'User <ID> has been added ...'
  WHEN duplicate key (DUPKEY/DUPREC):
    ERROR 'User ID already exist...'
    Position cursor on User ID field
  WHEN other error:
    ERROR 'Unable to Add User...'
```

### Decision Table

| First Name | Last Name | User ID | Password | User Type | User ID Exists | Outcome |
|-----------|----------|---------|----------|-----------|---------------|---------|
| Empty | - | - | - | - | - | Error: "First Name can NOT be empty..." |
| Provided | Empty | - | - | - | - | Error: "Last Name can NOT be empty..." |
| Provided | Provided | Empty | - | - | - | Error: "User ID can NOT be empty..." |
| Provided | Provided | Provided | Empty | - | - | Error: "Password can NOT be empty..." |
| Provided | Provided | Provided | Provided | Empty | - | Error: "User Type can NOT be empty..." |
| Provided | Provided | Provided | Provided | Provided | Yes | Error: "User ID already exist..." |
| Provided | Provided | Provided | Provided | Provided | No | Success: "User <ID> has been added ..." |

## Source COBOL Reference

**Program:** `COUSR01C.cbl`
**Lines:** 115-274

```cobol
       PROCESS-ENTER-KEY.

           EVALUATE TRUE
               WHEN FNAMEI OF COUSR1AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'First Name can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO FNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN LNAMEI OF COUSR1AI = SPACES OR LOW-VALUES
                   MOVE 'Y'     TO WS-ERR-FLG
                   MOVE 'Last Name can NOT be empty...' TO
                                   WS-MESSAGE
                   MOVE -1       TO LNAMEL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN USERIDI OF COUSR1AI = SPACES OR LOW-VALUES
                   ...
               WHEN PASSWDI OF COUSR1AI = SPACES OR LOW-VALUES
                   ...
               WHEN USRTYPEI OF COUSR1AI = SPACES OR LOW-VALUES
                   ...
               WHEN OTHER
                   MOVE -1       TO FNAMEL OF COUSR1AI
                   CONTINUE
           END-EVALUATE

           IF NOT ERR-FLG-ON
               MOVE USERIDI  OF COUSR1AI TO SEC-USR-ID
               MOVE FNAMEI   OF COUSR1AI TO SEC-USR-FNAME
               MOVE LNAMEI   OF COUSR1AI TO SEC-USR-LNAME
               MOVE PASSWDI  OF COUSR1AI TO SEC-USR-PWD
               MOVE USRTYPEI OF COUSR1AI TO SEC-USR-TYPE
               PERFORM WRITE-USER-SEC-FILE
           END-IF.

       WRITE-USER-SEC-FILE.

           EXEC CICS WRITE
                DATASET   (WS-USRSEC-FILE)
                FROM      (SEC-USER-DATA)
                LENGTH    (LENGTH OF SEC-USER-DATA)
                RIDFLD    (SEC-USR-ID)
                KEYLENGTH (LENGTH OF SEC-USR-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN DFHRESP(NORMAL)
                   PERFORM INITIALIZE-ALL-FIELDS
                   MOVE DFHGREEN           TO ERRMSGC  OF COUSR1AO
                   STRING 'User '     DELIMITED BY SIZE
                          SEC-USR-ID  DELIMITED BY SPACE
                          ' has been added ...' DELIMITED BY SIZE
                     INTO WS-MESSAGE
                   PERFORM SEND-USRADD-SCREEN
               WHEN DFHRESP(DUPKEY)
               WHEN DFHRESP(DUPREC)
                   MOVE 'User ID already exist...' TO
                                   WS-MESSAGE
                   MOVE -1       TO USERIDL OF COUSR1AI
                   PERFORM SEND-USRADD-SCREEN
               WHEN OTHER
                   MOVE 'Unable to Add User...' TO
                                   WS-MESSAGE
                   PERFORM SEND-USRADD-SCREEN
           END-EVALUATE.
```

## Acceptance Criteria

### Scenario 1: Successfully add a new user

```gherkin
GIVEN an admin user is on the User Add screen (COUSR1A)
WHEN the admin enters First Name "John", Last Name "Doe", User ID "JDOE0001", Password "PASS1234", User Type "U"
  AND presses Enter
THEN the system writes a new record to the USRSEC file
  AND displays "User JDOE0001 has been added ..." in green
  AND all input fields are cleared for the next entry
```

### Scenario 2: Empty required field — First Name

```gherkin
GIVEN an admin user is on the User Add screen (COUSR1A)
  AND the First Name field is empty
WHEN the admin presses Enter
THEN the system displays "First Name can NOT be empty..."
  AND the cursor is positioned on the First Name field
  AND no file write is performed
```

### Scenario 3: Duplicate User ID

```gherkin
GIVEN an admin user is on the User Add screen (COUSR1A)
  AND user "USER0001" already exists in the USRSEC file
WHEN the admin enters User ID "USER0001" with all other fields filled
  AND presses Enter
THEN the system displays "User ID already exist..."
  AND the cursor is positioned on the User ID field
  AND the existing record is not modified
```

### Scenario 4: PF3 to return to admin menu

```gherkin
GIVEN an admin user is on the User Add screen (COUSR1A)
WHEN the admin presses PF3
THEN the system transfers control to COADM01C (Admin Menu)
  AND no user record is created
```

### Scenario 5: PF4 to clear form

```gherkin
GIVEN an admin user is on the User Add screen (COUSR1A)
  AND some fields contain data
WHEN the admin presses PF4
THEN all input fields are cleared to spaces
  AND the cursor is positioned on the First Name field
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | SCA credential provisioning | User creation is restricted to admin users, ensuring controlled credential provisioning |
| FFFS 2014:5 | Ch. 6 | User access management procedures | Formal user creation process with required fields ensures structured access provisioning |
| DORA | Art. 9 | ICT access control provisioning | Admin-controlled user creation supports formal access provisioning requirements |
| GDPR | Art. 5(1)(c) | Data minimisation | Only essential user attributes are collected (name, ID, password, type) |
| GDPR | Art. 5(1)(d) | Data accuracy | Required field validation ensures complete user records are created |

## Edge Cases

1. **No User Type validation**: The User Type field is required but not validated for specific values. Any single character is accepted, even though only 'A' and 'U' are meaningful. The migrated system should validate against allowed user types.

2. **No password complexity enforcement**: The password is accepted as-is (any 1-8 characters). The migrated system must enforce password complexity policies (length, special characters, etc.).

3. **No User ID format validation**: The User ID is any 1-8 characters. The migrated system should enforce naming conventions if required.

4. **Plaintext password storage**: The password is written directly to the SEC-USR-PWD field in plaintext. The migrated system must hash passwords before storage.

5. **No audit trail**: User creation is not logged to an audit trail. The migrated system must log all user provisioning events per DORA Art. 9 and FFFS 2014:5.

6. **Form cleared on success**: After successful creation, all fields are cleared. The admin must re-enter all fields for the next user. This prevents accidental duplicate creation but requires full re-entry.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - What are the valid User Type values beyond 'A' and 'U'?
  - Is there a naming convention for User IDs (e.g., first initial + last name)?
  - Should user creation trigger any notifications or approval workflows?
  - Is there a maximum number of users the system should support?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
