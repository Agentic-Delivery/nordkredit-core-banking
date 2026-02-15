---
id: "USEC-BR-001"
title: "User authentication via credential validation against USRSEC file"
domain: "user-security"
cobol_source: "COSGN00C.cbl:108-257"
requirement_id: "USEC-BR-001"
regulations:
  - "PSD2 Art. 97 — Strong Customer Authentication (SCA)"
  - "FFFS 2014:5 Ch. 6 — IT security and access control"
  - "DORA Art. 9 — ICT access control policies"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# USEC-BR-001: User authentication via credential validation against USRSEC file

## Summary

The CICS sign-on program (COSGN00C) authenticates users by validating a User ID and Password against the USRSEC VSAM file. Both fields are required and converted to uppercase before lookup. The system performs a keyed read on the USRSEC file using the User ID, then compares the stored password with the entered password. Authentication is a single-factor mechanism (knowledge-based only — no second factor). On success, the user is routed to either the Admin menu (COADM01C) or the regular user menu (COMEN01C) based on their user type.

## Business Logic

### Pseudocode

```
RECEIVE screen input from CICS MAP COSGN0A

// Phase 1: Required field validation
IF User-ID is empty (spaces or low-values):
    ERROR 'Please enter User ID ...'
    Position cursor on User ID field
    Re-display sign-on screen

IF Password is empty (spaces or low-values):
    ERROR 'Please enter Password ...'
    Position cursor on Password field
    Re-display sign-on screen

// Phase 2: Case normalization
CONVERT User-ID to UPPER-CASE
CONVERT Password to UPPER-CASE
STORE User-ID in COMMAREA (CDEMO-USER-ID)

// Phase 3: Credential verification
READ USRSEC file using User-ID as key

EVALUATE read response:
  WHEN record found (RESP = 0):
    IF stored-password = entered-password:
        SET COMMAREA fields (from-tranid, from-program, user-id, user-type)
        RESET program context to 0
        IF user-type = 'A' (Admin):
            XCTL to COADM01C (Admin Menu)
        ELSE:
            XCTL to COMEN01C (Regular Menu)
    ELSE:
        ERROR 'Wrong Password. Try again ...'
        Position cursor on Password field

  WHEN record not found (RESP = 13):
    ERROR 'User not found. Try again ...'
    Position cursor on User ID field

  WHEN other error:
    ERROR 'Unable to verify the User ...'
    Position cursor on User ID field
```

### Decision Table

| User ID Provided | Password Provided | User ID Found | Password Matches | Outcome |
|-------------------|-------------------|---------------|------------------|---------|
| No | - | - | - | Error: "Please enter User ID ..." |
| Yes | No | - | - | Error: "Please enter Password ..." |
| Yes | Yes | No | - | Error: "User not found. Try again ..." |
| Yes | Yes | Yes | No | Error: "Wrong Password. Try again ..." |
| Yes | Yes | Yes | Yes (type=A) | Route to Admin Menu (COADM01C) |
| Yes | Yes | Yes | Yes (type=U) | Route to Regular Menu (COMEN01C) |

## Source COBOL Reference

**Program:** `COSGN00C.cbl`
**Lines:** 108-257

```cobol
       PROCESS-ENTER-KEY.

           EXEC CICS RECEIVE
                     MAP('COSGN0A')
                     MAPSET('COSGN00')
                     RESP(WS-RESP-CD)
                     RESP2(WS-REAS-CD)
           END-EXEC.

           EVALUATE TRUE
               WHEN USERIDI OF COSGN0AI = SPACES OR LOW-VALUES
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Please enter User ID ...' TO WS-MESSAGE
                   MOVE -1       TO USERIDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN PASSWDI OF COSGN0AI = SPACES OR LOW-VALUES
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Please enter Password ...' TO WS-MESSAGE
                   MOVE -1       TO PASSWDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

           MOVE FUNCTION UPPER-CASE(USERIDI OF COSGN0AI) TO
                           WS-USER-ID
                           CDEMO-USER-ID
           MOVE FUNCTION UPPER-CASE(PASSWDI OF COSGN0AI) TO
                           WS-USER-PWD

           IF NOT ERR-FLG-ON
               PERFORM READ-USER-SEC-FILE
           END-IF.

       READ-USER-SEC-FILE.

           EXEC CICS READ
                DATASET   (WS-USRSEC-FILE)
                INTO      (SEC-USER-DATA)
                LENGTH    (LENGTH OF SEC-USER-DATA)
                RIDFLD    (WS-USER-ID)
                KEYLENGTH (LENGTH OF WS-USER-ID)
                RESP      (WS-RESP-CD)
                RESP2     (WS-REAS-CD)
           END-EXEC.

           EVALUATE WS-RESP-CD
               WHEN 0
                   IF SEC-USR-PWD = WS-USER-PWD
                       MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                       MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                       MOVE WS-USER-ID   TO CDEMO-USER-ID
                       MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE
                       MOVE ZEROS        TO CDEMO-PGM-CONTEXT

                       IF CDEMO-USRTYP-ADMIN
                            EXEC CICS XCTL
                              PROGRAM ('COADM01C')
                              COMMAREA(CARDDEMO-COMMAREA)
                            END-EXEC
                       ELSE
                            EXEC CICS XCTL
                              PROGRAM ('COMEN01C')
                              COMMAREA(CARDDEMO-COMMAREA)
                            END-EXEC
                       END-IF
                   ELSE
                       MOVE 'Wrong Password. Try again ...' TO
                                                          WS-MESSAGE
                       MOVE -1       TO PASSWDL OF COSGN0AI
                       PERFORM SEND-SIGNON-SCREEN
                   END-IF
               WHEN 13
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'User not found. Try again ...' TO WS-MESSAGE
                   MOVE -1       TO USERIDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
               WHEN OTHER
                   MOVE 'Y'      TO WS-ERR-FLG
                   MOVE 'Unable to verify the User ...' TO WS-MESSAGE
                   MOVE -1       TO USERIDL OF COSGN0AI
                   PERFORM SEND-SIGNON-SCREEN
           END-EVALUATE.
```

## Acceptance Criteria

### Scenario 1: Successful authentication as admin user

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
  AND user "ADMIN01" exists in the USRSEC file with password "PASS1234" and type "A"
WHEN the user enters User ID "admin01" and Password "pass1234"
  AND presses Enter
THEN the system converts both fields to uppercase
  AND reads the USRSEC file for User ID "ADMIN01"
  AND the password matches
  AND the user is transferred to the Admin Menu (COADM01C)
  AND the COMMAREA contains user-type "A"
```

### Scenario 2: Successful authentication as regular user

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
  AND user "USER0001" exists in the USRSEC file with password "TESTPASS" and type "U"
WHEN the user enters User ID "user0001" and Password "testpass"
  AND presses Enter
THEN the system converts both fields to uppercase
  AND reads the USRSEC file for User ID "USER0001"
  AND the password matches
  AND the user is transferred to the Regular Menu (COMEN01C)
  AND the COMMAREA contains user-type "U"
```

### Scenario 3: Empty User ID

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
  AND the User ID field is empty
WHEN the user presses Enter
THEN the system displays "Please enter User ID ..."
  AND the cursor is positioned on the User ID field
  AND no file read is performed
```

### Scenario 4: Empty Password

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
  AND the User ID field contains a value
  AND the Password field is empty
WHEN the user presses Enter
THEN the system displays "Please enter Password ..."
  AND the cursor is positioned on the Password field
  AND no file read is performed
```

### Scenario 5: User ID not found

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
WHEN the user enters a User ID that does not exist in the USRSEC file
  AND presses Enter
THEN the system displays "User not found. Try again ..."
  AND the cursor is positioned on the User ID field
```

### Scenario 6: Wrong password

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
  AND user "USER0001" exists in the USRSEC file with password "CORRECT1"
WHEN the user enters User ID "USER0001" and Password "WRONG123"
  AND presses Enter
THEN the system displays "Wrong Password. Try again ..."
  AND the cursor is positioned on the Password field
```

### Scenario 7: PF3 pressed to exit

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
WHEN the user presses PF3
THEN the system displays the "Thank you" message
  AND the CICS transaction ends (RETURN without TRANSID)
```

### Scenario 8: Invalid key pressed

```gherkin
GIVEN a user is on the CardDemo sign-on screen (COSGN0A)
WHEN the user presses any key other than Enter or PF3
THEN the system displays the invalid key message
  AND the sign-on screen is re-displayed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 — SCA | Payment service providers shall apply strong customer authentication where the payer accesses its payment account online | The sign-on screen provides the first authentication factor (knowledge: user ID + password). **Gap**: PSD2 SCA requires at least two independent factors — a second factor (possession or inherence) must be added in the migrated system |
| FFFS 2014:5 | Ch. 6 — IT security | Credit institutions must have adequate access control mechanisms | Authentication gate ensures only registered users with valid credentials can access the system |
| DORA | Art. 9 — Protection and prevention | Financial entities shall have ICT access control policies with authentication mechanisms | The USRSEC file-based authentication implements credential-based access control |
| GDPR | Art. 32 — Security of processing | Appropriate technical measures to ensure data security | User authentication prevents unauthorized access to personal financial data |

## Edge Cases

1. **Case-insensitive credentials**: Both User ID and Password are converted to uppercase via `FUNCTION UPPER-CASE` before validation. This means passwords are effectively case-insensitive — "Pass1234" and "PASS1234" are treated identically. The migrated system should enforce case-sensitive passwords per modern security standards.

2. **Plaintext password comparison**: Passwords are stored and compared in plaintext in the USRSEC file (PIC X(08)). The migrated system must hash passwords (e.g., bcrypt, Argon2) and never store plaintext credentials.

3. **No account lockout**: There is no failed login attempt counter or account lockout mechanism. A user can attempt unlimited password guesses. The migrated system must implement account lockout or rate limiting per PSD2 SCA requirements.

4. **No session timeout**: The CICS pseudo-conversational pattern returns control with a TRANSID, which will accept the next input indefinitely. The migrated system should implement session timeouts per DORA Art. 9.

5. **EIBCALEN = 0 (first entry)**: If the program is invoked without a COMMAREA (first CICS transaction start), it displays the sign-on screen. This is the normal entry point for new sessions.

6. **8-character credential limits**: User ID is limited to PIC X(08) and Password to PIC X(08). The migrated system should allow longer credentials per modern security standards.

7. **No password complexity rules**: The COBOL code does not validate password complexity (length, special characters, etc.). The migrated system should enforce password complexity policies.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - Is the case-insensitive password behavior intentional or a limitation of the mainframe environment?
  - Are there any external authentication systems (RACF, ACF2, Top Secret) that supplement USRSEC file authentication?
  - What is the current password rotation policy enforced operationally (outside the code)?
  - Should the migrated system support multi-factor authentication from day one, or can it be phased?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
