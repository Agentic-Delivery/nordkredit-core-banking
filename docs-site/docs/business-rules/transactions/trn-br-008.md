---
id: "trn-br-008"
title: "Transaction inter-program navigation and COMMAREA contract"
domain: "transactions"
cobol_source: "COTRN00C.cbl:62-71,107-141,510-521,COTRN01C.cbl:53-61,94-139,197-208,COTRN02C.cbl:72-80,115-159,500-511"
requirement_id: "TRN-BR-008"
regulations:
  - "PSD2 Art. 97"
  - "FSA FFFS 2014:5"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# TRN-BR-008: Transaction inter-program navigation and COMMAREA contract

## Summary

The three CICS transaction processing programs (COTRN00C, COTRN01C, COTRN02C) share a common COMMAREA structure for maintaining state and navigating between screens. The COMMAREA carries pagination context, selected transaction IDs, and program routing information. Each program uses CICS XCTL (transfer control) to navigate to other programs while preserving the COMMAREA, and CICS RETURN with TRANSID to maintain pseudo-conversational state within the same program. This inter-program contract defines the navigation flow and data passing protocol for the transaction management subsystem. Extracted from `COTRN00C.cbl`, `COTRN01C.cbl`, and `COTRN02C.cbl`.

## Business Logic

### Navigation Map

```
                    COMEN01C (Main Menu)
                         │
                    PF3  │  PF3
                         ▼
                    COTRN00C (Transaction List)
                    Trans ID: CT00
                         │
                    SEL='S'│  PF5 (return)
                         ▼
                    COTRN01C (Transaction View)
                    Trans ID: CT01
                         │
                    PF3  │
                         ▼
                    Return to calling program
                    (CDEMO-FROM-PROGRAM)

    COTRN02C (Transaction Add) — accessed separately
    Trans ID: CT02
         │
    PF3  │
         ▼
    Return to calling program
```

### COMMAREA Structure

Each program defines its own section within the shared CARDDEMO-COMMAREA:

#### COTRN00C (Transaction List) — CDEMO-CT00-INFO

| Field | PIC | Purpose |
|---|---|---|
| CDEMO-CT00-TRNID-FIRST | X(16) | First transaction ID on current page |
| CDEMO-CT00-TRNID-LAST | X(16) | Last transaction ID on current page |
| CDEMO-CT00-PAGE-NUM | 9(08) | Current page number |
| CDEMO-CT00-NEXT-PAGE-FLG | X(01) | 'Y' if more pages exist forward |
| CDEMO-CT00-TRN-SEL-FLG | X(01) | Selection flag entered by user |
| CDEMO-CT00-TRN-SELECTED | X(16) | Transaction ID of selected row |

#### COTRN01C (Transaction View) — CDEMO-CT01-INFO

| Field | PIC | Purpose |
|---|---|---|
| CDEMO-CT01-TRNID-FIRST | X(16) | Reserved (mirrors list structure) |
| CDEMO-CT01-TRNID-LAST | X(16) | Reserved |
| CDEMO-CT01-PAGE-NUM | 9(08) | Reserved |
| CDEMO-CT01-NEXT-PAGE-FLG | X(01) | Reserved |
| CDEMO-CT01-TRN-SEL-FLG | X(01) | Reserved |
| CDEMO-CT01-TRN-SELECTED | X(16) | Transaction ID to auto-lookup |

#### COTRN02C (Transaction Add) — CDEMO-CT02-INFO

| Field | PIC | Purpose |
|---|---|---|
| CDEMO-CT02-TRNID-FIRST | X(16) | Reserved |
| CDEMO-CT02-TRNID-LAST | X(16) | Reserved |
| CDEMO-CT02-PAGE-NUM | 9(08) | Reserved |
| CDEMO-CT02-NEXT-PAGE-FLG | X(01) | Reserved |
| CDEMO-CT02-TRN-SEL-FLG | X(01) | Reserved |
| CDEMO-CT02-TRN-SELECTED | X(16) | Pre-selected card number |

### Common COMMAREA Fields (from COCOM01Y)

| Field | Purpose |
|---|---|
| CDEMO-TO-PROGRAM | Target program for XCTL |
| CDEMO-FROM-PROGRAM | Calling program (for PF3 return) |
| CDEMO-FROM-TRANID | Calling transaction ID |
| CDEMO-PGM-REENTER | Flag: TRUE if re-entering program |
| CDEMO-PGM-CONTEXT | Context value (set to 0 on navigation) |

### Pseudocode — Navigation Protocol

```
RETURN-TO-PREV-SCREEN (common pattern in all 3 programs):
    IF CDEMO-TO-PROGRAM is empty
        Default to 'COSGN00C' (sign-on screen)
    END-IF
    MOVE current TRANID to CDEMO-FROM-TRANID
    MOVE current PGMNAME to CDEMO-FROM-PROGRAM
    MOVE ZEROS to CDEMO-PGM-CONTEXT
    EXEC CICS XCTL
        PROGRAM(CDEMO-TO-PROGRAM)
        COMMAREA(CARDDEMO-COMMAREA)
    END-EXEC

CICS RETURN (end of each transaction):
    EXEC CICS RETURN
        TRANSID(WS-TRANID)
        COMMAREA(CARDDEMO-COMMAREA)
    END-EXEC
    (Preserves state for next user interaction)
```

### Transaction IDs

| Program | CICS Trans ID | Purpose |
|---|---|---|
| COTRN00C | CT00 | Transaction list |
| COTRN01C | CT01 | Transaction view |
| COTRN02C | CT02 | Transaction add |

## Source COBOL Reference

**Programs:** `COTRN00C.cbl`, `COTRN01C.cbl`, `COTRN02C.cbl`

COTRN00C — COMMAREA definition (lines 62-71):
```cobol
000062          05 CDEMO-CT00-INFO.
000063             10 CDEMO-CT00-TRNID-FIRST     PIC X(16).
000064             10 CDEMO-CT00-TRNID-LAST      PIC X(16).
000065             10 CDEMO-CT00-PAGE-NUM        PIC 9(08).
000066             10 CDEMO-CT00-NEXT-PAGE-FLG   PIC X(01) VALUE 'N'.
000067                88 NEXT-PAGE-YES                     VALUE 'Y'.
000068                88 NEXT-PAGE-NO                      VALUE 'N'.
000069             10 CDEMO-CT00-TRN-SEL-FLG     PIC X(01).
000070             10 CDEMO-CT00-TRN-SELECTED    PIC X(16).
```

COTRN00C — Selection and XCTL (lines 186-195):
```cobol
000186                   WHEN 'S'
000187                   WHEN 's'
000188                        MOVE 'COTRN01C'   TO CDEMO-TO-PROGRAM
000189                        MOVE WS-TRANID    TO CDEMO-FROM-TRANID
000190                        MOVE WS-PGMNAME   TO CDEMO-FROM-PROGRAM
000191                        MOVE 0        TO CDEMO-PGM-CONTEXT
000192                        EXEC CICS
000193                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
000194                            COMMAREA(CARDDEMO-COMMAREA)
000195                        END-EXEC
```

## Acceptance Criteria

### Scenario 1: List to view navigation

GIVEN the user is on the transaction list screen (COTRN00C)
  AND selects a transaction by entering 'S'
WHEN the ENTER key is pressed
THEN control transfers to COTRN01C via CICS XCTL
  AND the selected transaction ID is passed in CDEMO-CT00-TRN-SELECTED
  AND CDEMO-FROM-PROGRAM is set to 'COTRN00C'

### Scenario 2: View to list return

GIVEN the user is on the transaction view screen (COTRN01C)
  AND the calling program was COTRN00C
WHEN the user presses PF5
THEN control returns to COTRN00C
  AND the list's pagination state is preserved in COMMAREA

### Scenario 3: Return to main menu

GIVEN the user is on any transaction screen
WHEN the user presses PF3
THEN control transfers to the calling program (CDEMO-FROM-PROGRAM)
  OR to COMEN01C if no calling program is recorded

### Scenario 4: Empty COMMAREA protection

GIVEN a program is entered with EIBCALEN = 0 (no COMMAREA)
WHEN the program starts
THEN it immediately transfers to the sign-on screen (COSGN00C)
  AND no transaction data is displayed

### Scenario 5: Pseudo-conversational state preservation

GIVEN the user interacts with a transaction screen
WHEN the CICS RETURN is issued with TRANSID and COMMAREA
THEN the program state is preserved across the pseudo-conversational boundary
  AND the next user interaction resumes with the saved COMMAREA

### Scenario 6: Auto-lookup on view entry

GIVEN the transaction view (COTRN01C) is entered from the list
  AND CDEMO-CT01-TRN-SELECTED contains a transaction ID
WHEN the program initializes (first entry, not reenter)
THEN the transaction is automatically looked up and displayed
  AND the user does not need to enter the ID manually

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| PSD2 | Art. 97 | Session authentication | Empty COMMAREA check prevents unauthorized access; CICS authentication gates all transaction access |
| FSA FFFS 2014:5 | Ch. 7 | Access control for financial data | Navigation protocol ensures users can only access transactions through authenticated program flow |

## Edge Cases

1. **COMMAREA size**: The LINKAGE SECTION defines `LK-COMMAREA PIC X(01) OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN`. The actual COMMAREA is shared across all CardDemo programs, so each program's section occupies a specific offset. The migrated system should use explicit session state or a typed DTO.

2. **XCTL vs RETURN**: XCTL transfers control permanently (the calling program is removed from the chain), while RETURN suspends the program for the next transaction. The migrated system must distinguish between page navigation (XCTL → HTTP redirect or SPA routing) and state preservation (RETURN → session state).

3. **Case-insensitive selection**: The selection flag accepts both 'S' and 's'. The migrated system should preserve this case-insensitivity.

4. **CDEMO-PGM-CONTEXT**: Set to 0 on every navigation but never checked. This may be used by other programs in the CardDemo suite. The migrated system should document whether context values are needed.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The COMMAREA contract is critical for the migrated system's session management. Questions: (1) Should the migrated REST API maintain pagination state server-side or use client-side pagination parameters? (2) Is the COTRN02C (add) screen accessible from the main menu only, or also from the list? (3) Should the auto-lookup behavior on COTRN01C be preserved, or should the migrated UI always show an empty form first?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
