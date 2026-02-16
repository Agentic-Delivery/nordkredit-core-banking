---
id: "sec-br-004"
title: "Function key validation and authorized actions"
domain: "user-security"
cobol_source: "COCRDLIC.cbl:346-375,COCRDLIC.cbl:389-517,COCRDSLC.cbl:291-299,COCRDSLC.cbl:304-381,COCRDUPC.cbl:413-424,COCRDUPC.cbl:429-543"
requirement_id: "SEC-BR-004"
regulations:
  - "PSD2 Art. 97"
  - "FFFS 2014:5 Ch. 8 §4"
  - "FFFS 2014:5 Ch. 4 §3"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# SEC-BR-004: Function key validation and authorized actions

## Summary

Each CICS program in the CardDemo system validates the terminal function key (PF key / AID byte) before performing any action. Only specific keys are accepted per program, and invalid keys are silently normalized to ENTER. This validation prevents users from triggering unauthorized operations by pressing unexpected keys. The allowed key set varies by program and by the current workflow state, creating a context-sensitive action authorization model. In the card update program, PF5 (save) is only accepted when changes have been validated and PF12 (cancel) is only accepted when data has been fetched, implementing a state-gated authorization pattern.

## Business Logic

### Pseudocode

```
CARD LIST (COCRDLIC) — Allowed Keys:
    SET PFK-INVALID = TRUE
    IF key = ENTER OR key = PF3 OR key = PF7 OR key = PF8
        SET PFK-VALID = TRUE
    END-IF
    IF PFK-INVALID
        SET key = ENTER       -- Normalize invalid key to ENTER
    END-IF

CARD DETAIL (COCRDSLC) — Allowed Keys:
    SET PFK-INVALID = TRUE
    IF key = ENTER OR key = PF3
        SET PFK-VALID = TRUE
    END-IF
    IF PFK-INVALID
        SET key = ENTER       -- Normalize invalid key to ENTER
    END-IF

CARD UPDATE (COCRDUPC) — Allowed Keys (state-dependent):
    SET PFK-INVALID = TRUE
    IF key = ENTER
    OR key = PF3
    OR (key = PF5 AND changes-validated-but-not-confirmed)
    OR (key = PF12 AND details-already-fetched)
        SET PFK-VALID = TRUE
    END-IF
    IF PFK-INVALID
        SET key = ENTER       -- Normalize invalid key to ENTER
    END-IF
```

### Allowed Actions Per Program

| Program | PF Key | Action | Condition Required |
|---------|--------|--------|--------------------|
| COCRDLIC | ENTER | Refresh/apply filters | Always |
| COCRDLIC | PF3 | Exit to menu/caller | Always |
| COCRDLIC | PF7 | Page up (previous page) | Always |
| COCRDLIC | PF8 | Page down (next page) | Always |
| COCRDSLC | ENTER | Submit search/refresh | Always |
| COCRDSLC | PF3 | Exit to card list/menu | Always |
| COCRDUPC | ENTER | Submit search/validate changes | Always |
| COCRDUPC | PF3 | Exit to card list/menu | Always |
| COCRDUPC | PF5 | Confirm and save changes | Only when CCUP-CHANGES-OK-NOT-CONFIRMED |
| COCRDUPC | PF12 | Cancel/re-fetch data | Only when details have been fetched |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 346-375 (PF key validation — card list)

```cobol
000346         SET PFK-INVALID TO TRUE
000347         IF CCARD-AID-ENTER OR
000348            CCARD-AID-PFK03 OR
000349            CCARD-AID-PFK07 OR
000350            CCARD-AID-PFK08
000351            SET PFK-VALID TO TRUE
000352         END-IF
000353
000354         IF PFK-INVALID
000355            SET CCARD-AID-ENTER TO TRUE
000356         END-IF
```

**Lines:** 389-517 (Action dispatch based on PF key)

```cobol
000389         EVALUATE TRUE
000390             WHEN CCARD-AID-PFK03
000391 *            EXIT
000392             ...
000393             WHEN CDEMO-PGM-ENTER
000394             ...
000395             WHEN CDEMO-PGM-REENTER
000396                 WHEN CCARD-AID-ENTER
000397 *                PROCESS USER INPUT
000398             ...
000399             WHEN CCARD-AID-PFK07
000400 *                PAGE UP
000401             ...
000402             WHEN CCARD-AID-PFK08
000403 *                PAGE DOWN
000404             ...
```

**Program:** `COCRDSLC.cbl`
**Lines:** 291-299 (PF key validation — card detail)

```cobol
000291         SET PFK-INVALID TO TRUE
000292         IF CCARD-AID-ENTER OR
000293            CCARD-AID-PFK03
000294            SET PFK-VALID TO TRUE
000295         END-IF
000296
000297         IF PFK-INVALID
000298            SET CCARD-AID-ENTER TO TRUE
000299         END-IF
```

**Program:** `COCRDUPC.cbl`
**Lines:** 413-424 (PF key validation — card update, state-gated)

```cobol
000413         SET PFK-INVALID TO TRUE
000414         IF CCARD-AID-ENTER OR
000415            CCARD-AID-PFK03 OR
000416            (CCARD-AID-PFK05 AND CCUP-CHANGES-OK-NOT-CONFIRMED)
000417                            OR
000418            (CCARD-AID-PFK12 AND NOT CCUP-DETAILS-NOT-FETCHED)
000419            SET PFK-VALID TO TRUE
000420         END-IF
000421
000422         IF PFK-INVALID
000423            SET CCARD-AID-ENTER TO TRUE
000424         END-IF
```

## Acceptance Criteria

### Scenario 1: Valid PF key is accepted

```gherkin
GIVEN the user is on the card list screen
WHEN the user presses PF7 (page up)
THEN PFK-VALID is set to TRUE
  AND the page up action is executed
```

### Scenario 2: Invalid PF key is normalized to ENTER

```gherkin
GIVEN the user is on the card list screen
WHEN the user presses PF9 (not in the allowed set)
THEN PFK-INVALID remains TRUE
  AND the key is silently changed to ENTER
  AND the screen is refreshed without performing a special action
```

### Scenario 3: PF5 save rejected when changes not validated

```gherkin
GIVEN the user is on the card update screen
  AND card details are displayed but changes have NOT been validated
WHEN the user presses PF5 (save)
THEN PFK-INVALID is TRUE because CCUP-CHANGES-OK-NOT-CONFIRMED is not set
  AND the key is normalized to ENTER
  AND no save operation is performed
```

### Scenario 4: PF5 save accepted when changes are validated

```gherkin
GIVEN the user is on the card update screen
  AND the user has made changes that passed validation
  AND CCUP-CHANGES-OK-NOT-CONFIRMED is set to TRUE
WHEN the user presses PF5 (save)
THEN PFK-VALID is set to TRUE
  AND the save operation proceeds (9200-WRITE-PROCESSING)
```

### Scenario 5: PF12 cancel rejected before data fetch

```gherkin
GIVEN the user has entered the card update program
  AND no card data has been fetched yet (CCUP-DETAILS-NOT-FETCHED is TRUE)
WHEN the user presses PF12 (cancel)
THEN PFK-INVALID is TRUE because details have not been fetched
  AND the key is normalized to ENTER
```

### Scenario 6: PF3 always exits regardless of state

```gherkin
GIVEN the user is on any CardDemo screen
WHEN the user presses PF3
THEN PFK-VALID is set to TRUE
  AND the program transfers control back to the calling program or menu
  AND no data modification is performed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for payment operations | State-gated PF5 (save) ensures changes can only be committed after validation; the two-step confirm-then-save pattern is a form of transaction authorization |
| FFFS 2014:5 | Ch. 8 §4 | Adequate internal controls including authorization of transactions | Only validated, confirmed changes can be saved via PF5; unauthorized function keys are rejected and normalized |
| FFFS 2014:5 | Ch. 4 §3 | Operational risk management — systems must prevent unauthorized actions | Invalid PF keys are silently normalized to ENTER, preventing the user from bypassing the workflow state machine |

## Edge Cases

1. **Silent normalization vs error feedback**: Invalid PF keys are silently changed to ENTER rather than producing an error message. The user receives no indication that their key was ignored. The migrated system should display a message like "Invalid action" when an unauthorized operation is attempted.

2. **PF5 state gate is application-level only**: The PF5 gate (`CCUP-CHANGES-OK-NOT-CONFIRMED`) is checked at the application level, not by CICS security. A malicious program could bypass this by constructing a COMMAREA with the flag pre-set. The migrated system should enforce state transitions server-side with tamper-proof state management.

3. **No timeout on confirmation state**: Once `CCUP-CHANGES-OK-NOT-CONFIRMED` is set, there is no timeout. The user could leave the terminal unattended and return later to press PF5. PSD2 requires re-authentication after inactivity periods. The migrated system should implement confirmation timeouts.

4. **AID byte mapping via copybook**: The PF key to AID byte mapping is defined in the `CVCRD01Y.cpy` copybook (`CCARD-AID-PFK03`, etc.) and IBM's `DFHAID` copybook. The migrated system should map these to HTTP methods and URL endpoints (e.g., PF3 → navigation back, PF5 → PUT/PATCH, PF7/PF8 → pagination parameters).

## Domain Expert Notes

- **null** — Awaiting domain expert review. Key questions: (1) Are there other CICS transaction-level security profiles that restrict which PF keys a terminal can send? (2) Is PF5 confirmation considered sufficient for PSD2 SCA, or is an additional authentication step required for the migrated system? (3) Does the mainframe have CICS security exits that intercept AID bytes before the application program receives them?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
