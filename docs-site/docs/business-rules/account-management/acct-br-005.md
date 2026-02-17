---
id: "acct-br-005"
title: "Account status transitions and lifecycle management"
domain: "account-management"
cobol_source: "CVACT02Y.cpy:10, COCRDUPC.cbl:845-876, CBTRN02C.cbl:414-420"
requirement_id: "ACCT-BR-005"
regulations:
  - "FSA FFFS 2014:5 Ch. 4 §3"
  - "PSD2 Art. 97"
  - "GDPR Art. 17"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-005: Account status transitions and lifecycle management

## Summary

Account status management in the COBOL system uses a simple binary active/inactive model. The card-level active status (`CARD-ACTIVE-STATUS` in CVACT02Y.cpy) is a single character field accepting only 'Y' (active) or 'N' (inactive), validated in the card update program (COCRDUPC.cbl). At the account level, the `ACCT-ACTIVE-STATUS` field follows the same pattern. Account expiration is enforced separately during batch transaction posting (CBTRN02C.cbl), where transactions for expired accounts are rejected. The expected coverage for the migrated system includes additional status transitions (dormant, frozen, closed) that are not explicitly present in the available COBOL source but are referenced in the .NET domain model and required by Swedish banking regulations.

## Business Logic

### Pseudocode

```
-- Card status validation (COCRDUPC: 1240-EDIT-CARDSTATUS)
PERFORM EDIT-CARD-STATUS:
    IF card-status = LOW-VALUES OR SPACES OR ZEROS
        SET input-error = TRUE
        SET card-status-must-be-yes-no = TRUE
        EXIT PARAGRAPH
    END-IF

    MOVE card-status TO yes-no-check-field

    IF yes-no-check-field IS VALID ('Y' or 'N')
        SET card-status-is-valid = TRUE
    ELSE
        SET input-error = TRUE
        SET card-status-must-be-yes-no = TRUE
        EXIT PARAGRAPH
    END-IF

-- Account expiration check (CBTRN02C: credit limit validation)
PERFORM CHECK-ACCOUNT-EXPIRATION:
    IF ACCT-EXPIRAION-DATE < transaction-date(1:10)
        SET fail-reason = 103
        SET description = "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
    END-IF

-- Account active status (inferred from .NET domain model)
ACCOUNT STATUS VALUES:
    'Y' = Active (or 'A' in extended model)
    'N' = Inactive
    Extended model (not in available COBOL):
        'A' = Active
        'D' = Dormant
        'F' = Frozen
        'C' = Closed
```

### Status Transition Matrix (Available COBOL)

| Current Status | Action | New Status | Validated In |
|---------------|--------|-----------|-------------|
| Y (Active) | Deactivate card | N (Inactive) | COCRDUPC.cbl:845-876 |
| N (Inactive) | Activate card | Y (Active) | COCRDUPC.cbl:845-876 |
| Any | Expired account | Rejected transactions | CBTRN02C.cbl:414-420 |

### Expected Status Model (Extended for Migration)

| Status | Code | Description | Allowed Operations |
|--------|------|-------------|-------------------|
| Active | A/Y | Account in good standing | All operations (transactions, inquiries, updates) |
| Dormant | D | No activity for regulatory period | Inquiry only; reactivation on next transaction |
| Frozen | F | Suspended by bank or regulatory order | Inquiry only; no transactions |
| Closed | C | Account permanently closed | No operations; retained for regulatory period |
| Inactive | N | Card/account deactivated | Inquiry only |

### Validation Rules

| Check | Value | Valid? | Error Message |
|-------|-------|--------|---------------|
| 'Y' | Active | Yes | — |
| 'N' | Inactive | Yes | — |
| 'y' | Lowercase | No | "CARD STATUS MUST BE YES OR NO" |
| ' ' | Space | No | "CARD STATUS MUST BE YES OR NO" |
| 'X' | Other | No | "CARD STATUS MUST BE YES OR NO" |
| LOW-VALUES | Empty | No | "CARD STATUS MUST BE YES OR NO" |

## Source COBOL Reference

**Programs:** `COCRDUPC.cbl`, `CBTRN02C.cbl`
**Copybook:** `CVACT02Y.cpy`

### CVACT02Y.cpy — Status field in card record

```cobol
           05  CARD-ACTIVE-STATUS                PIC X(01).
```
*(Line 10 — single character, 'Y' or 'N')*

### COCRDUPC.cbl — Card status validation

```cobol
000845 1240-EDIT-CARDSTATUS.
000846*    Must be Y or N
000847     SET FLG-CARDSTATUS-NOT-OK      TO TRUE
000848
000849*    Not supplied
000850     IF CCUP-NEW-CRDSTCD   EQUAL LOW-VALUES
000851     OR CCUP-NEW-CRDSTCD   EQUAL SPACES
000852     OR CCUP-NEW-CRDSTCD   EQUAL ZEROS
000853        SET INPUT-ERROR           TO TRUE
000854        SET FLG-CARDSTATUS-BLANK  TO TRUE
000855        IF WS-RETURN-MSG-OFF
000856           SET CARD-STATUS-MUST-BE-YES-NO TO TRUE
000857        END-IF
000858        GO TO  1240-EDIT-CARDSTATUS-EXIT
000859     END-IF
000860
000861     MOVE CCUP-NEW-CRDSTCD          TO FLG-YES-NO-CHECK
000862
000863     IF FLG-YES-NO-VALID
000864        SET FLG-CARDSTATUS-ISVALID  TO TRUE
000865     ELSE
000866        SET INPUT-ERROR             TO TRUE
000867        SET FLG-CARDSTATUS-NOT-OK   TO TRUE
000868        IF WS-RETURN-MSG-OFF
000869           SET CARD-STATUS-MUST-BE-YES-NO  TO TRUE
000870        END-IF
000871        GO TO  1240-EDIT-CARDSTATUS-EXIT
000872     END-IF
000873     .
000874 1240-EDIT-CARDSTATUS-EXIT.
000875     EXIT
000876     .
```

### COCRDUPC.cbl — Yes/No validation flag definition

```cobol
000091        05  FLG-YES-NO-CHECK              PIC X(1).
000092            88  FLG-YES-NO-VALID          VALUES 'Y', 'N'.
```
*(Lines 91-92 — defines the valid values for status fields)*

### CBTRN02C.cbl — Account expiration enforcement

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                 CONTINUE
000416               ELSE
000417                 MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                 MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
000419                   TO WS-VALIDATION-FAIL-REASON-DESC
000420               END-IF
```

### .NET Domain Model — Extended status values

```csharp
/// <summary>Active status ('A' = active, 'D' = dormant). COBOL: ACCT-ACTIVE-STATUS PIC X(01).</summary>
public string ActiveStatus { get; set; } = string.Empty;
```
*(src/NordKredit.Domain/Transactions/Account.cs:14 — note 'A' and 'D' values, extending COBOL 'Y'/'N')*

## Acceptance Criteria

### Scenario 1: Valid active status accepted

```gherkin
GIVEN the user enters card status "Y"
WHEN card status validation is performed
THEN the status is accepted as valid
  AND the card status flag is set to ISVALID
```

### Scenario 2: Valid inactive status accepted

```gherkin
GIVEN the user enters card status "N"
WHEN card status validation is performed
THEN the status is accepted as valid
  AND the card status flag is set to ISVALID
```

### Scenario 3: Invalid status rejected

```gherkin
GIVEN the user enters card status "X"
WHEN card status validation is performed
THEN the input is rejected
  AND the message "CARD STATUS MUST BE YES OR NO" is displayed
```

### Scenario 4: Blank status rejected

```gherkin
GIVEN the user leaves the card status field blank
WHEN card status validation is performed
THEN the input is rejected
  AND the message "CARD STATUS MUST BE YES OR NO" is displayed
```

### Scenario 5: Expired account blocks transactions

```gherkin
GIVEN an account with expiration date "2025-06-30"
  AND a daily transaction with origination date "2025-07-01"
WHEN the batch program validates the transaction
THEN the transaction is rejected with code 103
  AND reason "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"
```

### Scenario 6: Non-expired account allows transactions

```gherkin
GIVEN an account with expiration date "2028-12-31"
  AND a daily transaction with origination date "2026-02-17"
WHEN the batch program validates the transaction
THEN the expiration check passes
  AND the transaction proceeds to the next validation step
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 4 §3 | Operational risk — systems must enforce access controls and prevent unauthorized operations | Status validation prevents transactions on inactive or expired accounts |
| PSD2 | Art. 97 | Payment instruments must be properly managed with clear activation/deactivation controls | The Y/N status flag provides explicit activation/deactivation for card payment instruments |
| GDPR | Art. 17 | Right to erasure — upon account closure, PII must be handled according to data retention policies | The account status model must support a "Closed" state that triggers data retention/erasure workflows (not present in current COBOL but required for compliance) |

## Edge Cases

1. **Case sensitivity**: The COBOL validation accepts only uppercase 'Y' and 'N'. Lowercase 'y' and 'n' are rejected. The migrated system should either enforce uppercase or perform case-insensitive comparison — the approach must be documented and consistent.

2. **Status at card level vs account level**: The available COBOL source validates status at the card level (CARD-ACTIVE-STATUS). The account-level status (ACCT-ACTIVE-STATUS) is referenced in the .NET domain model but the validation logic is not in the available COBOL source. The migrated system must clarify the relationship: can an active account have inactive cards? Can an inactive account have active cards?

3. **Missing dormant/frozen states**: Swedish banking regulations (FSA FFFS 2014:5) require dormancy handling (accounts with no activity for a defined period) and freeze capability (for AML/regulatory holds). These states are not in the available COBOL source but are referenced in the .NET domain model ('D' for dormant). The migrated system must implement the full status lifecycle.

4. **Expiration date string comparison**: The COBOL code compares `ACCT-EXPIRAION-DATE` as a string against the first 10 characters of the transaction timestamp. This works because YYYY-MM-DD format preserves chronological ordering in string comparison. The migrated system should use proper date comparison.

5. **No state machine enforcement**: The COBOL system allows direct Y↔N transitions without intermediate states. There is no audit trail of status changes in the available source. The migrated system should implement a state machine with transition rules and an audit log.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) What account status values exist beyond Y/N — are dormant ('D'), frozen ('F'), and closed ('C') used on the mainframe? (2) Are there state transition rules (e.g., can an account go from Closed to Active)? (3) Is there a dormancy detection batch job? If so, what defines the dormancy period? (4) Can an account be frozen independently of its cards? (5) What happens to pending transactions when an account transitions from Active to Frozen? (6) Is there an account status change audit trail on the mainframe?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
