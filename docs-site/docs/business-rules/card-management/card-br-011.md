---
id: "card-br-011"
title: "Inter-program navigation and COMMAREA data contract"
domain: "card-management"
cobol_source: "COCRDLIC.cbl:384-406,458-482,COCRDSLC.cbl:268-348,COCRDUPC.cbl:429-543"
requirement_id: "CARD-BR-011"
regulations:
  - "FFFS 2014:5 Ch. 8 §4"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# CARD-BR-011: Inter-program navigation and COMMAREA data contract

## Summary

The three card management programs communicate via CICS XCTL (transfer control) and a shared COMMAREA (communication area). The COMMAREA carries context between programs including: the originating program name, account ID, card number, user type, and program state (first entry vs. re-entry). Each program checks where it was called from and adjusts its behavior accordingly — for example, when the detail view (COCRDSLC) is invoked from the card list (COCRDLIC), the search fields are pre-populated and protected. PF3 (exit) returns to the calling program or the main menu if no caller context exists. This navigation pattern implements a hub-and-spoke workflow centered on the card list screen.

## Business Logic

### Pseudocode

```
NAVIGATION FLOW:

    Main Menu (COMEN01C)
         |
         | XCTL (Enter)
         v
    Card List (COCRDLIC) <--+
         |                   |
         | 'S' selection     | PF3
         | XCTL              | XCTL
         v                   |
    Card Detail (COCRDSLC) --+
         |
         | (via list)
         |
    Card List (COCRDLIC) <--+
         |                   |
         | 'U' selection     | PF3
         | XCTL              | XCTL
         v                   |
    Card Update (COCRDUPC) --+

COMMAREA DATA CONTRACT (CARDDEMO-COMMAREA):
    CDEMO-FROM-PROGRAM    PIC X(08)  -- Who called us
    CDEMO-FROM-TRANID     PIC X(04)  -- Calling transaction ID
    CDEMO-TO-PROGRAM      PIC X(08)  -- Where we're going
    CDEMO-TO-TRANID       PIC X(04)  -- Destination transaction ID
    CDEMO-ACCT-ID         PIC 9(11)  -- Selected account ID
    CDEMO-CARD-NUM        PIC 9(16)  -- Selected card number
    CDEMO-PGM-REENTER     flag       -- Re-entry into same program
    CDEMO-PGM-ENTER       flag       -- First entry
    CDEMO-USRTYP-USER     flag       -- User type
    CDEMO-LAST-MAPSET     PIC X(07)  -- Last BMS mapset displayed
    CDEMO-LAST-MAP        PIC X(07)  -- Last BMS map displayed

PROGRAM-SPECIFIC COMMAREA (appended after CARDDEMO-COMMAREA):
    Each program appends its own local state to the shared COMMAREA:
    - COCRDLIC: pagination keys, screen number, page indicators
    - COCRDSLC: calling program context
    - COCRDUPC: update state machine, old/new card data
```

### Decision Table — Program Entry Behavior

| Target Program | Source Program | COMMAREA State | Behavior |
|---------------|---------------|----------------|----------|
| COCRDLIC | COMEN01C (menu) | PGM-ENTER, fresh | Initialize, show first page of all cards |
| COCRDLIC | COCRDSLC/COCRDUPC | PGM-ENTER | Re-initialize, show first page |
| COCRDSLC | COCRDLIC (action 'S') | PGM-ENTER, acct+card set | Pre-populate and protect search fields, auto-read data |
| COCRDSLC | COMEN01C (direct) | PGM-ENTER, no data | Show empty search form, user must enter criteria |
| COCRDSLC | Self (re-entry) | PGM-REENTER | Process user inputs, validate, read data |
| COCRDUPC | COCRDLIC (action 'U') | PGM-ENTER, acct+card set | Pre-populate, protect search fields, fetch data for edit |
| COCRDUPC | COMEN01C (direct) | PGM-ENTER, no data | Show empty form, prompt for search keys |
| COCRDUPC | Self (re-entry) | PGM-REENTER | Process inputs based on state machine |

### Decision Table — PF3 Exit Behavior

| Current Program | CDEMO-FROM-PROGRAM | Behavior |
|----------------|-------------------|----------|
| Any | Populated (e.g., COCRDLIC) | XCTL back to calling program |
| Any | SPACES or LOW-VALUES | XCTL to main menu (COMEN01C) |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 384-406 (PF3 exit to menu), 458-482 (returning from detail/update)

PF3 exit from card list:
```cobol
000384     IF  (CCARD-AID-PFK03
000385     AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM)
000386        MOVE LIT-THISTRANID   TO CDEMO-FROM-TRANID
000387        MOVE LIT-THISPGM      TO CDEMO-FROM-PROGRAM
000388        SET  CDEMO-USRTYP-USER TO TRUE
000389        SET  CDEMO-PGM-ENTER  TO TRUE
000390        MOVE LIT-THISMAPSET   TO CDEMO-LAST-MAPSET
000391        MOVE LIT-THISMAP      TO CDEMO-LAST-MAP
000392        MOVE LIT-MENUPGM      TO CDEMO-TO-PROGRAM
000393
000394*       CALL MENU PROGRAM
000395        SET CDEMO-PGM-ENTER   TO TRUE
000396
000397        EXEC CICS XCTL
000398                  PROGRAM (LIT-MENUPGM)
000399                  COMMAREA(CARDDEMO-COMMAREA)
000400        END-EXEC
000401     END-IF
```

**Program:** `COCRDSLC.cbl`
**Lines:** 304-348 (entry point handling)

Entry from card list with pre-validated keys:
```cobol
000339        WHEN CDEMO-PGM-ENTER
000340         AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM
000341             SET INPUT-OK TO TRUE
000342             MOVE CDEMO-ACCT-ID       TO CC-ACCT-ID-N
000343             MOVE CDEMO-CARD-NUM      TO CC-CARD-NUM-N
000344             PERFORM 9000-READ-DATA
000345                THRU 9000-READ-DATA-EXIT
000346             PERFORM 1000-SEND-MAP
000347               THRU 1000-SEND-MAP-EXIT
000348             GO TO COMMON-RETURN
```

Field protection when coming from list:
```cobol
000505     IF  CDEMO-LAST-MAPSET  EQUAL LIT-CCLISTMAPSET
000506     AND CDEMO-FROM-PROGRAM EQUAL LIT-CCLISTPGM
000507        MOVE DFHBMPRF     TO ACCTSIDA OF CCRDSLAI
000508        MOVE DFHBMPRF     TO CARDSIDA OF CCRDSLAI
000509     ELSE
000510        MOVE DFHBMFSE      TO ACCTSIDA OF CCRDSLAI
000511        MOVE DFHBMFSE      TO CARDSIDA OF CCRDSLAI
000512     END-IF
```

**Program:** `COCRDUPC.cbl`
**Lines:** 429-543 (main EVALUATE for entry handling)

Entry from card list for update:
```cobol
000482        WHEN CDEMO-PGM-ENTER
000483         AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM
000484        WHEN CCARD-AID-PFK12
000485         AND CDEMO-FROM-PROGRAM  EQUAL LIT-CCLISTPGM
000486             SET CDEMO-PGM-REENTER    TO TRUE
000487             SET INPUT-OK             TO TRUE
000488             SET FLG-ACCTFILTER-ISVALID  TO TRUE
000489             SET FLG-CARDFILTER-ISVALID  TO TRUE
000490             MOVE CDEMO-ACCT-ID       TO CC-ACCT-ID-N
000491             MOVE CDEMO-CARD-NUM      TO CC-CARD-NUM-N
000492             PERFORM 9000-READ-DATA
000493                THRU 9000-READ-DATA-EXIT
000494             SET CCUP-SHOW-DETAILS TO TRUE
```

PF3 exit back to calling program:
```cobol
000435        WHEN CCARD-AID-PFK03
000442             IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES
000443             OR CDEMO-FROM-TRANID    EQUAL SPACES
000444                MOVE LIT-MENUTRANID  TO CDEMO-TO-TRANID
000445             ELSE
000446                MOVE CDEMO-FROM-TRANID  TO CDEMO-TO-TRANID
000447             END-IF
000448
000449             IF CDEMO-FROM-PROGRAM   EQUAL LOW-VALUES
000450             OR CDEMO-FROM-PROGRAM   EQUAL SPACES
000451                MOVE LIT-MENUPGM     TO CDEMO-TO-PROGRAM
000452             ELSE
000453                MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM
000454             END-IF
000455
000456             MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
000457             MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM
000469
000470             EXEC CICS
000471                  SYNCPOINT
000472             END-EXEC
000473
000474             EXEC CICS XCTL
000475                  PROGRAM (CDEMO-TO-PROGRAM)
000476                  COMMAREA(CARDDEMO-COMMAREA)
000477             END-EXEC
```

## Acceptance Criteria

### Scenario 1: Navigation from list to detail via 'S' selection

```gherkin
GIVEN the card list displays cards including card "4000123456789012" on account "12345678901"
  AND the user enters 'S' next to that card
WHEN the system transfers control to COCRDSLC
THEN the COMMAREA contains CDEMO-FROM-PROGRAM = "COCRDLIC"
  AND the COMMAREA contains CDEMO-ACCT-ID = "12345678901"
  AND the COMMAREA contains CDEMO-CARD-NUM = "4000123456789012"
  AND the detail screen shows the card data with search fields protected
```

### Scenario 2: Navigation from list to update via 'U' selection

```gherkin
GIVEN the card list displays cards including card "4000123456789012"
  AND the user enters 'U' next to that card
WHEN the system transfers control to COCRDUPC
THEN the COMMAREA contains CDEMO-FROM-PROGRAM = "COCRDLIC"
  AND the update screen shows the card data with account/card fields protected
  AND the name, status, and expiry fields are editable
```

### Scenario 3: PF3 returns to calling program

```gherkin
GIVEN the user is on the card detail screen (COCRDSLC)
  AND the screen was reached from the card list (CDEMO-FROM-PROGRAM = "COCRDLIC")
WHEN the user presses PF3
THEN control transfers back to COCRDLIC (the card list program)
  AND the list screen is re-displayed
```

### Scenario 4: PF3 returns to main menu when no caller context

```gherkin
GIVEN the user is on the card detail screen (COCRDSLC)
  AND the screen was accessed directly from the main menu
  AND CDEMO-FROM-PROGRAM is SPACES
WHEN the user presses PF3
THEN control transfers to the main menu (COMEN01C)
```

### Scenario 5: Direct entry to detail screen requires manual input

```gherkin
GIVEN the user navigates to the card detail screen directly (not via card list)
WHEN the screen loads
THEN the account number and card number fields are editable (not protected)
  AND the prompt "Please enter Account and Card Number" is displayed
  AND no card data is shown until the user submits search criteria
```

### Scenario 6: PF12 reloads original data in update screen

```gherkin
GIVEN the user is on the card update screen with modified card data
  AND the user arrived from the card list (CDEMO-FROM-PROGRAM = "COCRDLIC")
WHEN the user presses PF12
THEN the original card data is re-read from the database
  AND all user modifications are discarded
  AND the screen displays the original values
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 8 §4 | Operational risk management — systems must ensure controlled access to functions | The COMMAREA-based navigation enforces a controlled workflow: users must navigate through the list to reach detail/update, maintaining an audit trail of the navigation path; direct access is also possible but requires manual entry of search criteria |
| PSD2 | Art. 97 | Strong customer authentication context must be maintained across payment operations | The COMMAREA carries authentication context (user type) across program transfers, ensuring that the authenticated session is not lost during navigation between card management screens |

## Edge Cases

1. **COMMAREA truncation**: If the COMMAREA length (EIBCALEN) is shorter than expected, the receiving program may read garbage data for the program-specific section. The COBOL code checks EIBCALEN = 0 as a special case (first entry) but does not validate partial lengths. The migrated system should validate all input parameters regardless of source.

2. **SYNCPOINT on exit from update**: The update program issues `EXEC CICS SYNCPOINT` before XCTL on PF3 exit (line 470-471). This commits any pending database changes. If the user navigates away after a failed update without SYNCPOINT, the locked record would remain locked until CICS task termination. The migrated system must ensure proper transaction cleanup on navigation.

3. **Program-specific COMMAREA alignment**: Each program appends its local state after the shared CARDDEMO-COMMAREA. The alignment depends on the LENGTH OF CARDDEMO-COMMAREA being consistent across all programs (they all COPY the same COCOM01Y). A change to the shared copybook affects all programs simultaneously. The migrated system should use a versioned data contract.

4. **Re-entry vs. first entry confusion**: The CDEMO-PGM-REENTER and CDEMO-PGM-ENTER flags control whether the program processes user input or displays a fresh screen. If these flags are corrupted (e.g., by a COMMAREA alignment issue), the program may skip input processing or attempt to process stale data. The migrated system should use explicit routing rather than flag-based dispatching.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The COMMAREA-based navigation pattern is specific to CICS transaction processing. The migrated REST API / web application will use HTTP request routing, session state, and URL-based navigation instead. Key mapping: COMMAREA context → session/JWT claims; XCTL → HTTP redirect or client-side routing; PF keys → API endpoints or button actions.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
