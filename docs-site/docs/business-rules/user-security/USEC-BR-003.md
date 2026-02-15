---
id: "USEC-BR-003"
title: "Session validation via COMMAREA presence check"
domain: "user-security"
cobol_source: "COSGN00C.cbl:80-96"
requirement_id: "USEC-BR-003"
regulations:
  - "PSD2 Art. 97 — Strong Customer Authentication (SCA)"
  - "DORA Art. 9 — ICT access control and session management"
  - "FFFS 2014:5 Ch. 6 — IT security"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# USEC-BR-003: Session validation via COMMAREA presence check

## Summary

Every CICS program in the CardDemo application checks `EIBCALEN` (the length of the COMMAREA passed to it) as the first step of execution. If `EIBCALEN = 0`, it means the program was invoked without an established session context (no COMMAREA). In this case, the program redirects to the sign-on screen (COSGN00C), preventing unauthorized access to any function. This COMMAREA-based session check is the primary mechanism for ensuring that all program access goes through the authentication gate. The sign-on program itself uses `EIBCALEN = 0` to distinguish first-time display from returning user input.

## Business Logic

### Pseudocode

```
// In COSGN00C (Sign-on):
IF EIBCALEN = 0:
    // First entry — display sign-on screen
    INITIALIZE screen to LOW-VALUES
    Position cursor on User ID field
    SEND sign-on screen
ELSE:
    // Returning with COMMAREA — process user input
    EVALUATE key pressed...

// In all other programs (COADM01C, COUSR00C, COUSR01C, COUSR02C, COUSR03C):
IF EIBCALEN = 0:
    // No COMMAREA = no authenticated session
    SET target-program to 'COSGN00C'
    XCTL to sign-on screen (RETURN-TO-PREV-SCREEN / RETURN-TO-SIGNON-SCREEN)
ELSE:
    // Valid session — copy COMMAREA and proceed
    MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA
    // Continue with normal processing...
```

### Decision Table

| Program | EIBCALEN = 0 | EIBCALEN > 0 |
|---------|-------------|-------------|
| COSGN00C | Display sign-on screen (first entry) | Process user input (returning) |
| COADM01C | Redirect to COSGN00C | Load COMMAREA, display admin menu |
| COUSR00C | Redirect to COSGN00C | Load COMMAREA, display user list |
| COUSR01C | Redirect to COSGN00C | Load COMMAREA, display user add form |
| COUSR02C | Redirect to COSGN00C | Load COMMAREA, display user update form |
| COUSR03C | Redirect to COSGN00C | Load COMMAREA, display user delete form |

## Source COBOL Reference

**Program:** `COSGN00C.cbl`
**Lines:** 80-96

```cobol
           IF EIBCALEN = 0
               MOVE LOW-VALUES TO COSGN0AO
               MOVE -1       TO USERIDL OF COSGN0AI
               PERFORM SEND-SIGNON-SCREEN
           ELSE
               EVALUATE EIBAID
                   WHEN DFHENTER
                       PERFORM PROCESS-ENTER-KEY
                   WHEN DFHPF3
                       MOVE CCDA-MSG-THANK-YOU        TO WS-MESSAGE
                       PERFORM SEND-PLAIN-TEXT
                   WHEN OTHER
                       MOVE 'Y'                       TO WS-ERR-FLG
                       MOVE CCDA-MSG-INVALID-KEY      TO WS-MESSAGE
                       PERFORM SEND-SIGNON-SCREEN
               END-EVALUATE
           END-IF.
```

**Program:** `COADM01C.cbl`
**Lines:** 86-89

```cobol
           IF EIBCALEN = 0
               MOVE 'COSGN00C' TO CDEMO-FROM-PROGRAM
               PERFORM RETURN-TO-SIGNON-SCREEN
           ELSE
```

**Program:** `COUSR00C.cbl`
**Lines:** 110-112

```cobol
           IF EIBCALEN = 0
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
               PERFORM RETURN-TO-PREV-SCREEN
```

## Acceptance Criteria

### Scenario 1: Direct access to admin menu without session

```gherkin
GIVEN a user attempts to directly invoke the COADM01C program
  AND no COMMAREA is passed (EIBCALEN = 0)
WHEN the program starts
THEN the system redirects to the sign-on screen (COSGN00C)
  AND the user must authenticate before proceeding
```

### Scenario 2: Direct access to user management without session

```gherkin
GIVEN a user attempts to directly invoke COUSR01C (User Add)
  AND no COMMAREA is passed (EIBCALEN = 0)
WHEN the program starts
THEN the system redirects to the sign-on screen (COSGN00C)
  AND the user must authenticate before proceeding
```

### Scenario 3: Valid session with COMMAREA

```gherkin
GIVEN an authenticated user navigates from the Admin Menu to User List
  AND the COMMAREA is passed with EIBCALEN > 0
WHEN COUSR00C starts
THEN the system copies the COMMAREA to local storage
  AND proceeds with normal user list display
  AND the user's authentication context is preserved
```

### Scenario 4: First-time sign-on screen display

```gherkin
GIVEN the CICS transaction CC00 is started for the first time
  AND EIBCALEN = 0 (no prior COMMAREA)
WHEN COSGN00C starts
THEN the system displays an empty sign-on screen
  AND the cursor is positioned on the User ID field
  AND no authentication check is performed (this IS the authentication entry point)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for account access | COMMAREA check ensures every program execution is preceded by authentication through the sign-on flow |
| DORA | Art. 9 | Protection and prevention — session management | COMMAREA presence serves as a session token; absence forces re-authentication |
| FFFS 2014:5 | Ch. 6 | IT security measures for access control | Defense-in-depth: each program independently verifies session validity |

## Edge Cases

1. **COMMAREA presence is not identity verification**: The EIBCALEN check only verifies that *some* COMMAREA was passed, not that it contains a valid authenticated session. A crafted COMMAREA could bypass this check. In the CICS environment, this is mitigated by the fact that COMMAREA is only passed between programs via XCTL/RETURN. The migrated system must use proper session tokens (e.g., JWT) with cryptographic validation.

2. **No session expiry**: Once a COMMAREA is established, there is no timeout mechanism in the code. The CICS pseudo-conversational pattern does return control, but the TRANSID will accept input indefinitely. The migrated system must implement session timeouts.

3. **COMMAREA content not validated**: Programs copy the COMMAREA into local storage but do not validate that `CDEMO-USER-TYPE` or `CDEMO-USER-ID` are still valid (e.g., user hasn't been deleted mid-session). The migrated system should validate session integrity on each request.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - Is there any CICS-level timeout configured for the CC00 transaction?
  - Does the CICS region have any external security manager (RACF) that supplements the COMMAREA check?
  - How long can a session remain idle before the terminal is disconnected?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
