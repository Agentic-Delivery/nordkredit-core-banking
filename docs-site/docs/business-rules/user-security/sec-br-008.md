---
id: "sec-br-008"
title: "Navigation audit trail and program flow tracking"
domain: "user-security"
cobol_source: "COCRDLIC.cbl:604-608,COCRDLIC.cbl:522-550,COCRDSLC.cbl:323-334,COCRDSLC.cbl:393-394,COCRDUPC.cbl:456-476"
requirement_id: "SEC-BR-008"
regulations:
  - "DORA Art. 11"
  - "FFFS 2014:5 Ch. 8 §4"
  - "GDPR Art. 5(2)"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# SEC-BR-008: Navigation audit trail and program flow tracking

## Summary

The CardDemo system maintains a navigation audit trail through the COMMAREA by recording which program and transaction invoked the current program. Every program sets `CDEMO-FROM-PROGRAM` and `CDEMO-FROM-TRANID` before transferring control to another program or returning to CICS. This creates a traceable chain of program invocations: the calling program records its identity, the called program can read the origin, and the PF3 (exit) key uses this information to route back to the correct caller. Additionally, `CDEMO-LAST-MAPSET` and `CDEMO-LAST-MAP` track which screen was last displayed, allowing the system to detect the display context (e.g., whether the user came from the card list or entered the program directly). Together, these fields provide an application-level audit trail of user navigation through the system.

## Business Logic

### Pseudocode

```
ON EVERY CICS RETURN (pseudo-conversational):
    SET CDEMO-FROM-PROGRAM = current-program-name
    SET CDEMO-FROM-TRANID = current-transaction-id
    SET CDEMO-LAST-MAPSET = current-mapset
    SET CDEMO-LAST-MAP = current-map
    PERSIST TO COMMAREA

ON EVERY XCTL (program transfer):
    SET CDEMO-FROM-PROGRAM = current-program-name
    SET CDEMO-FROM-TRANID = current-transaction-id
    SET CDEMO-USRTYP-USER = TRUE
    SET CDEMO-PGM-ENTER = TRUE
    SET CDEMO-LAST-MAPSET = current-mapset
    SET CDEMO-LAST-MAP = current-map
    -- Additional context: account-id, card-number if applicable
    TRANSFER WITH COMMAREA

ON PF3 (EXIT) — Return to caller:
    IF CDEMO-FROM-TRANID = LOW-VALUES or SPACES
        SET target-transaction = menu-transaction
    ELSE
        SET target-transaction = CDEMO-FROM-TRANID
    END-IF

    IF CDEMO-FROM-PROGRAM = LOW-VALUES or SPACES
        SET target-program = menu-program
    ELSE
        SET target-program = CDEMO-FROM-PROGRAM
    END-IF

    XCTL TO target-program WITH COMMAREA
```

### Navigation Tracking Fields

| Field | Set By | Purpose | Example Value |
|-------|--------|---------|---------------|
| CDEMO-FROM-PROGRAM | Every program on RETURN/XCTL | Identifies the calling program | 'COCRDLIC' |
| CDEMO-FROM-TRANID | Every program on RETURN/XCTL | Identifies the calling transaction | 'CCLI' |
| CDEMO-LAST-MAPSET | Every program on RETURN/XCTL | Last BMS mapset displayed | 'COCRDLI' |
| CDEMO-LAST-MAP | Every program on RETURN/XCTL | Last BMS map displayed | 'CCRDSLA' |
| CDEMO-TO-PROGRAM | Set on PF3 exit | Target program for navigation | 'COMEN01C' |
| CDEMO-TO-TRANID | Set on PF3 exit | Target transaction for navigation | 'CM00' |

### Navigation Flow Trace Example

```
Step 1: Menu (COMEN01C, CM00) → XCTL → Card List (COCRDLIC, CCLI)
    COMMAREA: FROM-PROGRAM='COMEN01C', FROM-TRANID='CM00'

Step 2: Card List → XCTL → Card Detail (COCRDSLC, CCDL)
    COMMAREA: FROM-PROGRAM='COCRDLIC', FROM-TRANID='CCLI',
              ACCT-ID=41000000001, CARD-NUM=4000123456789012

Step 3: Card Detail → PF3 → XCTL → Card List (COCRDLIC, CCLI)
    COMMAREA: FROM-PROGRAM='COCRDSLC', FROM-TRANID='CCDL'

Step 4: Card List → PF3 → XCTL → Menu (COMEN01C, CM00)
    COMMAREA: FROM-PROGRAM='COCRDLIC', FROM-TRANID='CCLI'
```

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 604-608 (Navigation tracking on CICS RETURN)

```cobol
000604  COMMON-RETURN.
000605      MOVE  LIT-THISTRANID TO CDEMO-FROM-TRANID
000606      MOVE  LIT-THISPGM     TO CDEMO-FROM-PROGRAM
000607      MOVE  LIT-THISMAPSET  TO CDEMO-LAST-MAPSET
000608      MOVE  LIT-THISMAP     TO CDEMO-LAST-MAP
```

**Lines:** 522-550 (Navigation tracking on XCTL to card detail)

```cobol
000522             MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
000523             MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM
000524             SET  CDEMO-USRTYP-USER  TO TRUE
000525             SET  CDEMO-PGM-ENTER    TO TRUE
000526             MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET
000527             MOVE LIT-THISMAP        TO CDEMO-LAST-MAP
000528
000529             MOVE CDEMO-ACCT-ID      TO CDEMO-ACCT-ID
000530             MOVE CDEMO-CARD-NUM     TO CDEMO-CARD-NUM
000531
000532 *            CALL CARD DETAIL PROGRAM
000533 *
000534              EXEC CICS XCTL
000535                   PROGRAM (CCARD-NEXT-PROG)
000536                   COMMAREA(CARDDEMO-COMMAREA)
000537              END-EXEC
```

**Program:** `COCRDSLC.cbl`
**Lines:** 309-334 (PF3 exit — uses FROM fields to route back)

```cobol
000309                IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES
000310                OR CDEMO-FROM-TRANID    EQUAL SPACES
000311                   MOVE LIT-MENUTRANID  TO CDEMO-TO-TRANID
000312                ELSE
000313                   MOVE CDEMO-FROM-TRANID  TO CDEMO-TO-TRANID
000314                END-IF
000315
000316                IF CDEMO-FROM-PROGRAM   EQUAL LOW-VALUES
000317                OR CDEMO-FROM-PROGRAM   EQUAL SPACES
000318                   MOVE LIT-MENUPGM     TO CDEMO-TO-PROGRAM
000319                ELSE
000320                   MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM
000321                END-IF
000322
000323                MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
000324                MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM
000325
000326                SET  CDEMO-USRTYP-USER  TO TRUE
000327                SET  CDEMO-PGM-ENTER    TO TRUE
000328                MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET
000329                MOVE LIT-THISMAP        TO CDEMO-LAST-MAP
000330 *
000331                EXEC CICS XCTL
000332                         PROGRAM (CDEMO-TO-PROGRAM)
000333                         COMMAREA(CARDDEMO-COMMAREA)
000334                END-EXEC
```

**Lines:** 393-394 (Navigation tracking on CICS RETURN in card detail)

```cobol
000393      MOVE LIT-THISPGM          TO CDEMO-FROM-PROGRAM
000394      MOVE LIT-THISTRANID       TO CDEMO-FROM-TRANID
```

**Program:** `COCRDUPC.cbl`
**Lines:** 456-476 (PF3 exit with SYNCPOINT and navigation tracking)

```cobol
000456                MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
000457                MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM
000458
000459                IF CDEMO-LAST-MAPSET    EQUAL LIT-CCLISTMAPSET
000460                    MOVE ZEROS          TO CDEMO-ACCT-ID
000461                                           CDEMO-CARD-NUM
000462                END-IF
000463
000464                SET  CDEMO-USRTYP-USER  TO TRUE
000465                SET  CDEMO-PGM-ENTER    TO TRUE
000466                MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET
000467                MOVE LIT-THISMAP        TO CDEMO-LAST-MAP
000468
000469                EXEC CICS
000470                     SYNCPOINT
000471                END-EXEC
000472 *
000473                EXEC CICS XCTL
000474                     PROGRAM (CDEMO-TO-PROGRAM)
000475                     COMMAREA(CARDDEMO-COMMAREA)
000476                END-EXEC
```

## Acceptance Criteria

### Scenario 1: Program origin tracked on every transfer

```gherkin
GIVEN the card list program transfers control to the card detail program
WHEN the XCTL command executes
THEN CDEMO-FROM-PROGRAM is set to 'COCRDLIC'
  AND CDEMO-FROM-TRANID is set to 'CCLI'
  AND the card detail program can read the calling program's identity
```

### Scenario 2: PF3 routes back to calling program

```gherkin
GIVEN the user is on the card detail screen
  AND CDEMO-FROM-PROGRAM = 'COCRDLIC' (card list)
WHEN the user presses PF3
THEN CDEMO-TO-PROGRAM is set to 'COCRDLIC'
  AND control is transferred to the card list program
  AND the card list program receives the COMMAREA context
```

### Scenario 3: PF3 defaults to menu when no caller

```gherkin
GIVEN the user is on a screen
  AND CDEMO-FROM-PROGRAM is LOW-VALUES or SPACES (no caller recorded)
WHEN the user presses PF3
THEN CDEMO-TO-PROGRAM defaults to 'COMEN01C' (main menu)
  AND CDEMO-TO-TRANID defaults to 'CM00' (menu transaction)
  AND control is transferred to the main menu
```

### Scenario 4: Last mapset tracked for context detection

```gherkin
GIVEN the card update program is checking whether the user came from the card list
WHEN CDEMO-LAST-MAPSET equals 'COCRDLI' (card list mapset)
THEN the program knows the user navigated from the card list
  AND it adjusts behavior accordingly (e.g., protecting search fields, clearing context on exit)
```

### Scenario 5: Navigation chain forms an audit trail

```gherkin
GIVEN a user navigates: Menu → Card List → Card Detail → Card Update
WHEN each program sets CDEMO-FROM-PROGRAM and CDEMO-FROM-TRANID
THEN the COMMAREA contains a single-hop navigation record at each step:
  | At Program | FROM-PROGRAM | FROM-TRANID |
  | Card List  | COMEN01C     | CM00        |
  | Card Detail| COCRDLIC     | CCLI        |
  | Card Update| COCRDSLC     | CCDL        |
```

### Scenario 6: Context cleared on exit from update to list

```gherkin
GIVEN the user exits the card update program via PF3
  AND CDEMO-LAST-MAPSET equals the card list mapset
WHEN the exit processing runs
THEN CDEMO-ACCT-ID is set to ZEROS
  AND CDEMO-CARD-NUM is set to ZEROS
  AND the card list program starts fresh without pre-selected card context
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| DORA | Art. 11 | ICT systems must have logging and detection mechanisms for anomalous activities | COMMAREA navigation fields (FROM-PROGRAM, FROM-TRANID) create an application-level audit trail of user actions through the system |
| FFFS 2014:5 | Ch. 8 §4 | Internal controls must ensure traceability of transactions and operations | The navigation chain records which screens the user visited and in what order; each program records its identity before transferring control |
| GDPR | Art. 5(2) | Controller must demonstrate compliance (accountability principle) | Navigation tracking provides evidence of which data screens were accessed and from what context, supporting access auditing |
| PSD2 | Art. 97 | Strong customer authentication — session integrity | The navigation tracking ensures the user follows the authorized workflow path (list → detail → update), preventing direct access to modification screens without proper context |

## Edge Cases

1. **Single-hop trail only**: The COMMAREA only stores the immediately preceding program (FROM-PROGRAM). The full navigation history is not preserved — each XCTL overwrites the previous caller. The migrated system should implement a full audit log (timestamp, user ID, action, screen visited) in a persistent store.

2. **Context clearing on exit**: When exiting the update program back to the card list, CDEMO-ACCT-ID and CDEMO-CARD-NUM are zeroed (line 460-461), but only if the last mapset was the card list. This conditional clearing could leave stale context if the navigation path changes. The migrated system should always clear sensitive context on navigation boundaries.

3. **Default to menu as fallback**: If CDEMO-FROM-PROGRAM is blank (LOW-VALUES or SPACES), the PF3 exit defaults to the menu program (COMEN01C). This is a safe fallback but means direct invocations lose their return context. The migrated system should always have a well-defined return route.

4. **No timestamp in navigation trail**: The COMMAREA navigation fields have no timestamp. Without timestamps, there is no way to determine when the user accessed each screen or how long they spent. The migrated system must add timestamps to the audit trail for regulatory compliance (DORA incident timelines, GDPR access logging).

5. **Terminal ID not in application trail**: The CICS terminal ID (EIBTRMID) is available via the EIB but is not stored in the COMMAREA or application-level audit fields. CICS system logs may capture this, but the application-level trail does not include the source terminal. The migrated system should include the client IP and device identifier in the audit trail.

## Domain Expert Notes

- **null** — Awaiting domain expert review. Key questions: (1) Does the CICS system log (CICS auxiliary trace or SMF records) capture the full navigation chain including timestamps and terminal IDs? (2) How long are CICS audit logs retained, and are they available for regulatory review? (3) Are there additional menu programs beyond COMEN01C that serve as entry points for the card management subsystem? (4) Is the navigation tracking used for any operational reporting or anomaly detection today?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
