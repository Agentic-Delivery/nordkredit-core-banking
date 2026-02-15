---
id: "USEC-BR-004"
title: "Admin menu access control and option routing"
domain: "user-security"
cobol_source: "COADM01C.cbl:119-158"
requirement_id: "USEC-BR-004"
regulations:
  - "FFFS 2014:5 Ch. 6 — Role-based access control"
  - "DORA Art. 9 — Segregation of duties and least privilege"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# USEC-BR-004: Admin menu access control and option routing

## Summary

The Admin Menu program (COADM01C) displays a numbered list of administrative options (defined in the COADM02Y copybook) and routes the user to the selected program via CICS XCTL. The menu validates that the entered option is numeric, non-zero, and within the valid range (1 to CDEMO-ADMIN-OPT-COUNT, currently 6). It also handles the case where a selected program is not installed (PGMIDERR condition). The admin menu is only reachable by users authenticated with type 'A' through the sign-on routing in COSGN00C.

## Business Logic

### Pseudocode

```
// Screen handling
IF first entry (CDEMO-PGM-REENTER is false):
    SET CDEMO-PGM-REENTER = true
    INITIALIZE screen
    BUILD menu options from COADM02Y copybook
    SEND admin menu screen
ELSE:
    RECEIVE screen input
    EVALUATE key pressed:
        WHEN Enter:
            PERFORM PROCESS-ENTER-KEY
        WHEN PF3:
            XCTL to COSGN00C (return to sign-on)
        WHEN Other:
            ERROR 'Invalid key pressed'
            Re-display menu

// PROCESS-ENTER-KEY:
TRIM and pad option input to 2 digits
REPLACE spaces with '0'

IF option is NOT numeric
   OR option > CDEMO-ADMIN-OPT-COUNT (6)
   OR option = 0:
    ERROR 'Please enter a valid option number...'
    Re-display menu

IF program name for option starts with 'DUMMY':
    Display 'This option is not installed ...'
ELSE:
    SET COMMAREA fields (from-tranid, from-program, context=0)
    XCTL to CDEMO-ADMIN-OPT-PGMNAME(selected-option) with COMMAREA

// PGMIDERR handler:
IF program not found:
    Display 'This option is not installed ...'
    Re-display menu
```

### Decision Table

| Option Value | Numeric? | In Range (1-6)? | Program Installed? | Outcome |
|-------------|----------|-----------------|-------------------|---------|
| Spaces/empty | No | - | - | Error: "Please enter a valid option number..." |
| "AB" | No | - | - | Error: "Please enter a valid option number..." |
| "0" or "00" | Yes | No (= 0) | - | Error: "Please enter a valid option number..." |
| "7" | Yes | No (> 6) | - | Error: "Please enter a valid option number..." |
| "1" | Yes | Yes | Yes | XCTL to COUSR00C (User List) |
| "2" | Yes | Yes | Yes | XCTL to COUSR01C (User Add) |
| "3" | Yes | Yes | Yes | XCTL to COUSR02C (User Update) |
| "4" | Yes | Yes | Yes | XCTL to COUSR03C (User Delete) |
| "5" | Yes | Yes | Yes | XCTL to COTRTLIC (Transaction Type List) |
| "6" | Yes | Yes | Yes | XCTL to COTRTUPC (Transaction Type Maintenance) |
| Any valid | Yes | Yes | No (PGMIDERR) | Message: "This option is not installed ..." |

## Source COBOL Reference

**Program:** `COADM01C.cbl`
**Lines:** 119-158

```cobol
       PROCESS-ENTER-KEY.

           PERFORM VARYING WS-IDX
                   FROM LENGTH OF OPTIONI OF COADM1AI BY -1 UNTIL
                   OPTIONI OF COADM1AI(WS-IDX:1) NOT = SPACES OR
                   WS-IDX = 1
           END-PERFORM
           MOVE OPTIONI OF COADM1AI(1:WS-IDX) TO WS-OPTION-X
           INSPECT WS-OPTION-X REPLACING ALL ' ' BY '0'
           MOVE WS-OPTION-X              TO WS-OPTION
           MOVE WS-OPTION                TO OPTIONO OF COADM1AO

           IF WS-OPTION IS NOT NUMERIC OR
              WS-OPTION > CDEMO-ADMIN-OPT-COUNT OR
              WS-OPTION = ZEROS
               MOVE 'Y'     TO WS-ERR-FLG
               MOVE 'Please enter a valid option number...' TO
                                       WS-MESSAGE
               PERFORM SEND-MENU-SCREEN
           END-IF

           IF NOT ERR-FLG-ON
               IF CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION)(1:5) NOT = 'DUMMY'
                   MOVE WS-TRANID    TO CDEMO-FROM-TRANID
                   MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
                   MOVE ZEROS        TO CDEMO-PGM-CONTEXT
                   EXEC CICS
                       XCTL PROGRAM(CDEMO-ADMIN-OPT-PGMNAME(WS-OPTION))
                       COMMAREA(CARDDEMO-COMMAREA)
                   END-EXEC
               END-IF
               MOVE SPACES             TO WS-MESSAGE
               MOVE DFHGREEN           TO ERRMSGC  OF COADM1AO
               STRING 'This option '       DELIMITED BY SIZE
                       'is not installed ...'   DELIMITED BY SIZE
                  INTO WS-MESSAGE
               PERFORM SEND-MENU-SCREEN
           END-IF.
```

**Copybook:** `COADM02Y.cpy` (Admin menu options)

```cobol
 01 CARDDEMO-ADMIN-MENU-OPTIONS.
   05 CDEMO-ADMIN-OPT-COUNT           PIC 9(02) VALUE 6.
   05 CDEMO-ADMIN-OPTIONS-DATA.
     10 FILLER  PIC 9(02) VALUE 1.
     10 FILLER  PIC X(35) VALUE 'User List (Security)               '.
     10 FILLER  PIC X(08) VALUE 'COUSR00C'.
     ...
   05 CDEMO-ADMIN-OPTIONS REDEFINES CDEMO-ADMIN-OPTIONS-DATA.
     10 CDEMO-ADMIN-OPT OCCURS 9 TIMES.
       15 CDEMO-ADMIN-OPT-NUM           PIC 9(02).
       15 CDEMO-ADMIN-OPT-NAME          PIC X(35).
       15 CDEMO-ADMIN-OPT-PGMNAME       PIC X(08).
```

## Acceptance Criteria

### Scenario 1: Valid option selected — User List

```gherkin
GIVEN an admin user is on the Admin Menu screen (COADM1A)
WHEN the user enters option "1" and presses Enter
THEN the system transfers control to COUSR00C (User List)
  AND the COMMAREA contains the admin user's session context
```

### Scenario 2: Invalid option — non-numeric

```gherkin
GIVEN an admin user is on the Admin Menu screen (COADM1A)
WHEN the user enters "AB" in the option field and presses Enter
THEN the system displays "Please enter a valid option number..."
  AND the admin menu is re-displayed
```

### Scenario 3: Invalid option — out of range

```gherkin
GIVEN an admin user is on the Admin Menu screen (COADM1A)
WHEN the user enters "7" in the option field and presses Enter
THEN the system displays "Please enter a valid option number..."
  AND the admin menu is re-displayed
```

### Scenario 4: PF3 to return to sign-on

```gherkin
GIVEN an admin user is on the Admin Menu screen (COADM1A)
WHEN the user presses PF3
THEN the system transfers control to COSGN00C (sign-on screen)
  AND the admin session ends
```

### Scenario 5: Program not installed

```gherkin
GIVEN an admin user is on the Admin Menu screen (COADM1A)
  AND the selected program is not available in the CICS region
WHEN the user selects that option and presses Enter
THEN the PGMIDERR condition is raised
  AND the system displays "This option is not installed ..."
  AND the admin menu is re-displayed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 6 | Role-based access control | Admin menu is only reachable via admin-type authentication; options 1-4 provide user management capabilities restricted to admins |
| DORA | Art. 9 | Segregation of duties and least privilege | Admin functions (user CRUD) are segregated into a separate menu system, accessible only to admin-type users |

## Edge Cases

1. **Menu options are data-driven**: The admin menu reads options from the COADM02Y copybook, which allows adding/removing options by modifying the copybook and recompiling. The option count (6) and program names are configuration, not hard-coded logic. The migrated system should similarly externalize menu configuration.

2. **DUMMY program check**: The code checks if the program name starts with 'DUMMY' to handle placeholder/disabled options. This is a soft-disable mechanism. The migrated system can use feature flags or permission checks instead.

3. **Array bounds**: The copybook defines `OCCURS 9 TIMES` but only 6 options are populated. The remaining 3 slots are available for future expansion. Input validation correctly limits to CDEMO-ADMIN-OPT-COUNT (6).

4. **No per-option authorization**: All admin users see all admin menu options. There is no granular permission to restrict, for example, user deletion to senior admins only. The migrated system should consider per-operation authorization.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - Should all admin users have access to all admin functions, or should there be sub-roles?
  - Are options 5-6 (Transaction Type management) used in the NordKredit context, or are they CardDemo-specific?
  - What is the process for adding new admin menu options in production?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
