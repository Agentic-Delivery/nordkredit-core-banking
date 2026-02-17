---
id: "sec-br-003"
title: "User session context management via COMMAREA"
domain: "user-security"
cobol_source: "COCRDLIC.cbl:226-262,COCRDLIC.cbl:315-332,COCRDLIC.cbl:604-619,COCRDSLC.cbl:198-205,COCRDSLC.cbl:256-278,COCRDSLC.cbl:393-405,COCRDUPC.cbl:345,COCOM01Y.cpy:19-46,CSUSR01Y.cpy:17-23"
requirement_id: "SEC-BR-003"
regulations:
  - "PSD2 Art. 97"
  - "FFFS 2014:5 Ch. 8 §4"
  - "GDPR Art. 5(1)(f)"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# SEC-BR-003: User session context management via COMMAREA

## Summary

The CardDemo system maintains user session state across CICS program invocations using the COMMAREA (Communication Area), a CICS-managed memory structure passed between programs. The COMMAREA carries the authenticated user's identity, role type, navigation state, current account/card context, and program-specific working data. This is the functional equivalent of a session in the CICS environment — every program transfer (XCTL) and every transaction return (RETURN TRANSID) passes the full COMMAREA, preserving user context across the multi-program workflow. The COMMAREA is the sole mechanism for maintaining security context between screens.

## Business Logic

### Pseudocode

```
SESSION INITIALIZATION (on first entry, EIBCALEN = 0):
    INITIALIZE CARDDEMO-COMMAREA
    INITIALIZE program-specific-COMMAREA
    SET from-transaction-id = current-transaction-id
    SET from-program = current-program-name
    SET user-type = 'USER'
    SET program-enter = TRUE
    SET last-map = current-map
    SET last-mapset = current-mapset

SESSION RESTORATION (on subsequent entries, EIBCALEN > 0):
    MOVE DFHCOMMAREA(1:LENGTH OF CARDDEMO-COMMAREA)
        TO CARDDEMO-COMMAREA
    MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                     LENGTH OF program-specific-COMMAREA)
        TO program-specific-COMMAREA

SESSION PERSISTENCE (on CICS RETURN):
    MOVE CARDDEMO-COMMAREA TO WS-COMMAREA
    MOVE program-specific-COMMAREA TO
        WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                     LENGTH OF program-specific-COMMAREA)
    EXEC CICS RETURN
        TRANSID(current-transaction-id)
        COMMAREA(WS-COMMAREA)
        LENGTH(LENGTH OF WS-COMMAREA)
    END-EXEC

PROGRAM TRANSFER (XCTL with context):
    SET from-transaction-id = current-transaction-id
    SET from-program = current-program-name
    SET user-type = 'USER'
    SET program-enter = TRUE
    SET last-mapset = current-mapset
    SET last-map = current-map
    SET target-program = next-program-name
    SET account-id = selected-account
    SET card-number = selected-card
    EXEC CICS XCTL
        PROGRAM(target-program)
        COMMAREA(CARDDEMO-COMMAREA)
    END-EXEC
```

### COMMAREA Structure (verified from COCOM01Y.cpy)

```cobol
 01 CARDDEMO-COMMAREA.
    05 CDEMO-GENERAL-INFO.
       10 CDEMO-FROM-TRANID             PIC X(04).
       10 CDEMO-FROM-PROGRAM            PIC X(08).
       10 CDEMO-TO-TRANID               PIC X(04).
       10 CDEMO-TO-PROGRAM              PIC X(08).
       10 CDEMO-USER-ID                 PIC X(08).
       10 CDEMO-USER-TYPE               PIC X(01).
          88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
          88 CDEMO-USRTYP-USER          VALUE 'U'.
       10 CDEMO-PGM-CONTEXT             PIC 9(01).
          88 CDEMO-PGM-ENTER            VALUE 0.
          88 CDEMO-PGM-REENTER          VALUE 1.
    05 CDEMO-CUSTOMER-INFO.
       10 CDEMO-CUST-ID                 PIC 9(09).
       10 CDEMO-CUST-FNAME              PIC X(25).
       10 CDEMO-CUST-MNAME              PIC X(25).
       10 CDEMO-CUST-LNAME              PIC X(25).
    05 CDEMO-ACCOUNT-INFO.
       10 CDEMO-ACCT-ID                 PIC 9(11).
       10 CDEMO-ACCT-STATUS             PIC X(01).
    05 CDEMO-CARD-INFO.
       10 CDEMO-CARD-NUM                PIC 9(16).
    05 CDEMO-MORE-INFO.
       10 CDEMO-LAST-MAP                PIC X(7).
       10 CDEMO-LAST-MAPSET             PIC X(7).
```

| Field | Type | Purpose | Security Relevance |
|-------|------|---------|-------------------|
| CDEMO-FROM-TRANID | PIC X(04) | Calling transaction ID | Tracks origin for audit/navigation |
| CDEMO-FROM-PROGRAM | PIC X(08) | Calling program name | Controls PF3 return destination |
| CDEMO-TO-TRANID | PIC X(04) | Target transaction ID | Navigation routing |
| CDEMO-TO-PROGRAM | PIC X(08) | Target program name | Navigation routing |
| CDEMO-USER-ID | PIC X(08) | Authenticated user identity | Carries user identity across all invocations |
| CDEMO-USER-TYPE | PIC X(01) | User type flag ('A'=admin, 'U'=user) | Determines data access level |
| CDEMO-PGM-CONTEXT | PIC 9(01) | First-entry (0) / re-entry (1) flag | Controls initialization vs restore |
| CDEMO-CUST-ID | PIC 9(09) | Customer identifier | Ties session to customer context |
| CDEMO-ACCT-ID | PIC 9(11) | Current account context | Drives account-level filtering |
| CDEMO-ACCT-STATUS | PIC X(01) | Account status | Status-gated operations |
| CDEMO-CARD-NUM | PIC 9(16) | Current card context | Drives card-level filtering |
| CDEMO-LAST-MAPSET | PIC X(7) | Last displayed mapset | UI state preservation |
| CDEMO-LAST-MAP | PIC X(7) | Last displayed map | UI state preservation |
| Program-specific area | Varies | Pagination keys, local state | Program working context |

**Signed-on user data (CSUSR01Y.cpy):**

```cobol
 01 SEC-USER-DATA.
    05 SEC-USR-ID                 PIC X(08).
    05 SEC-USR-FNAME              PIC X(20).
    05 SEC-USR-LNAME              PIC X(20).
    05 SEC-USR-PWD                PIC X(08).
    05 SEC-USR-TYPE               PIC X(01).
    05 SEC-USR-FILLER             PIC X(23).
```

**Key observations from copybooks:**
- `CDEMO-USER-ID` (8 chars) and `SEC-USR-ID` (8 chars) are the same domain — the sign-on program copies the authenticated user's ID into the COMMAREA.
- `CDEMO-USER-TYPE` and `SEC-USR-TYPE` are both single-character fields with the same 'A'/'U' domain — user type is propagated from the user record to the session context.
- `CDEMO-CUSTOMER-INFO` contains customer name fields (FNAME, MNAME, LNAME) carried in the session, representing PII that must be protected under GDPR.
- The COMMAREA is a fixed-size flat structure (no variable-length fields), totaling approximately 160 bytes for the shared section.

### Session Lifecycle

```
[CICS Sign-On] → [Menu Program] → XCTL → [Card List (COCRDLIC)]
    ↕ RETURN TRANSID (same program, preserves COMMAREA)
    → XCTL → [Card Detail (COCRDSLC)]
    ↕ RETURN TRANSID
    → XCTL → [Card Update (COCRDUPC)]
    ↕ RETURN TRANSID
    → XCTL → [Menu Program] (PF3 exit)
```

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 226-227 (COMMAREA and user data copybook includes)

```cobol
000226 *Application Commmarea Copybook
000227  COPY COCOM01Y.
```

**Lines:** 284-285 (Signed-on user data include)

```cobol
000284 *Signed on user data
000285  COPY CSUSR01Y.
```

**Lines:** 315-332 (Session initialization vs restoration)

```cobol
000315      IF EIBCALEN = 0
000316         INITIALIZE CARDDEMO-COMMAREA
000317                    WS-THIS-PROGCOMMAREA
000318         MOVE LIT-THISTRANID        TO CDEMO-FROM-TRANID
000319         MOVE LIT-THISPGM           TO CDEMO-FROM-PROGRAM
000320         SET CDEMO-USRTYP-USER      TO TRUE
000321         SET CDEMO-PGM-ENTER        TO TRUE
000322         MOVE LIT-THISMAP           TO CDEMO-LAST-MAP
000323         MOVE LIT-THISMAPSET        TO CDEMO-LAST-MAPSET
000324         SET CA-FIRST-PAGE          TO TRUE
000325         SET CA-LAST-PAGE-NOT-SHOWN TO TRUE
000326      ELSE
000327         MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA) TO
000328                           CARDDEMO-COMMAREA
000329         MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
000330                          LENGTH OF WS-THIS-PROGCOMMAREA )TO
000331                           WS-THIS-PROGCOMMAREA
000332      END-IF
```

**Lines:** 604-619 (Session persistence on CICS RETURN)

```cobol
000604  COMMON-RETURN.
000605      MOVE  LIT-THISTRANID TO CDEMO-FROM-TRANID
000606      MOVE  LIT-THISPGM     TO CDEMO-FROM-PROGRAM
000607      MOVE  LIT-THISMAPSET  TO CDEMO-LAST-MAPSET
000608      MOVE  LIT-THISMAP     TO CDEMO-LAST-MAP
000609      MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA
000610      MOVE  WS-THIS-PROGCOMMAREA TO
000611             WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
000612                          LENGTH OF WS-THIS-PROGCOMMAREA )
000613
000614
000615      EXEC CICS RETURN
000616           TRANSID (LIT-THISTRANID)
000617           COMMAREA (WS-COMMAREA)
000618           LENGTH(LENGTH OF WS-COMMAREA)
000619      END-EXEC
```

**Program:** `COCRDSLC.cbl`
**Lines:** 256-278 (Session initialization and restoration in card detail)

```cobol
000256      INITIALIZE CC-WORK-AREA
000257                 WS-MISC-STORAGE
000258                 WS-COMMAREA
000259
000260      IF EIBCALEN = 0
000261 *        NEVER SHOULD BE INVOKED DIRECTLY FROM CICS
000262 *        ALWAYS INVOKED FROM CARD LIST OR SOME
000263 *        PROGRAM.
000264         PERFORM SEND-PLAIN-TEXT
000265      END-IF
000266
000267      IF  EIBCALEN NOT = 0
000268 *    PROGRAM HAS BEEN INVOKED WITH COMMAREA
000269         CONTINUE
000270      END-IF
000271
000272         INITIALIZE CARDDEMO-COMMAREA
000273                    WS-THIS-PROGCOMMAREA
000274         MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA)  TO
000275                           CARDDEMO-COMMAREA
000276         MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
000277                          LENGTH OF WS-THIS-PROGCOMMAREA ) TO
000278                           WS-THIS-PROGCOMMAREA
```

**Lines:** 393-405 (Session persistence on CICS RETURN in card detail)

```cobol
000393      MOVE LIT-THISPGM          TO CDEMO-FROM-PROGRAM
000394      MOVE LIT-THISTRANID       TO CDEMO-FROM-TRANID
000395
000396
000397      MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA
000398      MOVE  WS-THIS-PROGCOMMAREA TO
000399             WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
000400                          LENGTH OF WS-THIS-PROGCOMMAREA )
000401
000402      EXEC CICS RETURN
000403           TRANSID (LIT-THISTRANID)
000404           COMMAREA (WS-COMMAREA)
000405           LENGTH(LENGTH OF WS-COMMAREA)
```

## Acceptance Criteria

### Scenario 1: Session initialized on first program entry

```gherkin
GIVEN a CICS transaction is initiated with no COMMAREA (EIBCALEN = 0)
WHEN the card list program (COCRDLIC) is entered
THEN CARDDEMO-COMMAREA is initialized to default values
  AND CDEMO-USRTYP-USER is set to TRUE (regular user)
  AND CDEMO-FROM-PROGRAM is set to 'COCRDLIC'
  AND CDEMO-FROM-TRANID is set to 'CCLI'
```

### Scenario 2: Session restored from COMMAREA on re-entry

```gherkin
GIVEN a CICS transaction is resumed with a COMMAREA (EIBCALEN > 0)
WHEN the card list program is re-entered
THEN CARDDEMO-COMMAREA is restored from the first portion of DFHCOMMAREA
  AND WS-THIS-PROGCOMMAREA is restored from the remaining portion
  AND the user type, account context, and navigation state are preserved
```

### Scenario 3: Context passed to child program via XCTL

```gherkin
GIVEN the user selects a card for detail view
WHEN the card list program transfers to the card detail program
THEN CDEMO-FROM-PROGRAM is set to 'COCRDLIC'
  AND CDEMO-ACCT-ID contains the selected account number
  AND CDEMO-CARD-NUM contains the selected card number
  AND the full CARDDEMO-COMMAREA is passed via CICS XCTL
```

### Scenario 4: Session persisted across pseudo-conversational returns

```gherkin
GIVEN the user is viewing the card list screen
WHEN the CICS transaction ends and waits for user input
THEN the COMMAREA is saved via CICS RETURN TRANSID
  AND both shared and program-specific sections are concatenated into WS-COMMAREA
  AND the next transaction invocation restores the full context
```

### Scenario 5: Card detail program rejects direct invocation

```gherkin
GIVEN the card detail program (COCRDSLC) is invoked directly without COMMAREA
WHEN EIBCALEN = 0 is detected
THEN the program sends a plain-text error message
  AND does not proceed to normal screen processing
  AND the user is informed that direct invocation is not supported
```

### Scenario 6: Program-specific state isolated between programs

```gherkin
GIVEN the card list program has pagination state (first/last card keys, page number)
WHEN control transfers to the card detail program
THEN the card detail program initializes its own WS-THIS-PROGCOMMAREA
  AND the card list's pagination state is not accessible to the detail program
  AND each program only reads its own section of the COMMAREA
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication — session must maintain authenticated state | COMMAREA carries user type and identity across all program invocations, ensuring the authenticated context is never lost during a workflow |
| FFFS 2014:5 | Ch. 8 §4 | Internal controls must ensure continuity and integrity of information flows | Session context is consistently structured, persisted, and restored using the same copybook (COCOM01Y) across all programs |
| GDPR | Art. 5(1)(f) | Integrity and confidentiality of personal data processing | User context (account scope, user type) is maintained in CICS-managed memory, not client-side; the session cannot be tampered with by the terminal user |
| DORA | Art. 11 | ICT systems must have adequate logging and monitoring of operations | COMMAREA fields (CDEMO-FROM-PROGRAM, CDEMO-FROM-TRANID) provide an audit trail of the user's navigation path through the system |

## Edge Cases

1. **COMMAREA size fixed across programs**: All programs use the same COCOM01Y copybook for the shared section, but each appends a different program-specific section. The total COMMAREA size varies by program. CICS manages the length via `LENGTH OF WS-COMMAREA`. The migrated system should use a typed session object with a shared base class and program-specific extensions.

2. **No COMMAREA encryption — PII exposure**: The COMMAREA carries customer PII (`CDEMO-CUST-FNAME`, `CDEMO-CUST-MNAME`, `CDEMO-CUST-LNAME`, `CDEMO-CUST-ID`) and the user password is stored as plain text in `SEC-USR-PWD` (8 chars max). The COMMAREA is stored in CICS temporary storage in clear text. The migrated system must encrypt sensitive session data (user type, account context, customer PII) in transit and at rest, especially for GDPR and PSD2 compliance.

3. **Direct invocation protection only in COCRDSLC**: The card detail program (COCRDSLC) explicitly checks for EIBCALEN = 0 and sends an error (line 260-265). However, COCRDLIC defaults to initialization when EIBCALEN = 0 (line 315), which means it can be invoked directly from CICS (e.g., via CEMT). The migrated system should require authentication context for all endpoints.

4. **Session state not invalidated**: There is no explicit session timeout or invalidation in the COBOL code. CICS manages transaction timeouts at the system level. The migrated system must implement session expiration (PSD2 requires re-authentication after 5 minutes of inactivity for payment operations).

5. **COMMAREA memory safety**: The COMMAREA is extracted using fixed offsets based on `LENGTH OF` declarations. If a copybook is changed without recompiling all programs, the offsets will be misaligned and session data will be corrupted. The migrated system should use a versioned serialization format.

## Domain Expert Notes

- **Copybook analysis complete** — COCOM01Y.cpy confirms the COMMAREA shared section is approximately 160 bytes (well within CICS 32K limit). The structure includes customer PII fields (names, IDs) that constitute GDPR-relevant personal data carried in the session. `CDEMO-USER-ID` (8 chars) and `CDEMO-USER-TYPE` ('A'/'U') are the security-critical fields. **Remaining questions for domain expert:** (1) Is the session tied to the CICS terminal ID (EIBTRMID), and does the mainframe enforce single-session-per-terminal? (2) Does the CICS region have transaction timeout configured, and what is the value? (3) Are there any CICS security exits that validate the COMMAREA contents between program transfers?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
