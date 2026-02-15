---
id: "card-br-003"
title: "Card detail lookup by account and card number"
domain: "card-management"
cobol_source: "COCRDSLC.cbl:608-812"
requirement_id: "CARD-BR-003"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# CARD-BR-003: Card detail lookup by account and card number

## Summary

The card detail view program retrieves and displays a single card record based on the card number (primary key) or account ID (alternate index). When accessed from the card list screen (COCRDLIC), the search fields are pre-populated and protected. When accessed directly, the user must provide a valid 11-digit account number or 16-digit card number. On successful lookup, the program displays the card number, account ID, embossed name, expiration date (YYYY-MM-DD format), and active status. If no matching record is found, a descriptive error message is shown.

## Business Logic

### Pseudocode

```
PERFORM 2200-EDIT-MAP-INPUTS:
    IF coming-from-list-screen (COCRDLIC)
        PRE-POPULATE search fields from COMMAREA
        SET search fields to PROTECTED (non-editable)
    END-IF

    IF account-id IS PROVIDED
        PERFORM 2210-EDIT-ACCOUNT
        IF account-id = SPACES OR LOW-VALUES
            MOVE 'Account number not provided' TO WS-MESSAGE
            EXIT
        END-IF
        IF account-id IS NOT NUMERIC
            MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER'
                TO WS-MESSAGE
            EXIT
        END-IF
    END-IF

    IF card-number IS PROVIDED
        PERFORM 2220-EDIT-CARD
        IF card-number = SPACES OR LOW-VALUES
            MOVE 'Card number not provided' TO WS-MESSAGE
            EXIT
        END-IF
        IF card-number IS NOT NUMERIC
            MOVE 'CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER'
                TO WS-MESSAGE
            EXIT
        END-IF
    END-IF

PERFORM CARD-LOOKUP:
    IF card-number IS PROVIDED
        PERFORM 9100-GETCARD-BYACCTCARD
        EXEC CICS READ FILE('CARDDAT')
            INTO(CARD-RECORD)
            RIDFLD(card-number)
            RESP(WS-RESP-CD)
        END-EXEC
    ELSE IF account-id IS PROVIDED
        PERFORM 9150-GETCARD-BYACCT
        EXEC CICS READ FILE('CARDAIX')
            INTO(CARD-RECORD)
            RIDFLD(account-id)
            RESP(WS-RESP-CD)
        END-EXEC
    END-IF

    IF WS-RESP-CD = DFHRESP(NOTFND)
        MOVE 'Did not find cards for this search condition'
            TO WS-MESSAGE
    ELSE
        DISPLAY card-number, account-id, embossed-name,
                expiration-date, active-status
    END-IF
```

### Decision Table

| Input Source | Card Number Provided | Account ID Provided | Lookup Method | Outcome |
|-------------|---------------------|--------------------|--------------|---------|
| From list (COCRDLIC) | Yes (pre-populated) | Yes (pre-populated) | Primary key on CARDDAT | Card detail displayed, fields protected |
| Direct entry | Yes | Optional | Primary key on CARDDAT | Card detail displayed |
| Direct entry | No | Yes | Alternate index CARDAIX | First matching card displayed |
| Direct entry | No | No | N/A | Error: "Card number not provided" |
| Any | Yes (invalid) | N/A | N/A | Error: validation message |
| Any | N/A | Yes (invalid) | N/A | Error: validation message |
| Any | Yes (valid) | N/A | Primary key, NOTFND | Error: "Did not find cards for this search condition" |

## Source COBOL Reference

**Program:** `COCRDSLC.cbl`
**Lines:** 608-724 (input validation), 736-812 (data reads)

```cobol
000608 2200-EDIT-MAP-INPUTS.
000609     IF WS-CA-CALLED-FROM = 'COCRDLIC'
000610         MOVE WS-CA-ACCT-ID TO ACCT-ID-INPUT
000611         MOVE WS-CA-CARD-NUM TO CARD-NUM-INPUT
000612         SET ACCT-ID-PROT TO TRUE
000613         SET CARD-NUM-PROT TO TRUE
000614     END-IF.
```

```cobol
000647 2210-EDIT-ACCOUNT.
000648     IF ACCT-ID-INPUT = SPACES OR LOW-VALUES
000649         MOVE 'Account number not provided'
000650             TO WS-MESSAGE
000651         SET WS-INPUT-ERROR TO TRUE
000652         GO TO 2210-EXIT
000653     END-IF.
000654
000655     IF ACCT-ID-INPUT IS NOT NUMERIC
000656         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
000657           ' 11 DIGIT NUMBER'
000658             TO WS-MESSAGE
000659         SET WS-INPUT-ERROR TO TRUE
000660         GO TO 2210-EXIT
000661     END-IF.
000662
000663     IF ACCT-ID-INPUT = ZEROS
000664         MOVE 'ACCOUNT FILTER,IF SUPPLIED MUST BE A'
000665           ' 11 DIGIT NUMBER'
000666             TO WS-MESSAGE
000667         SET WS-INPUT-ERROR TO TRUE
000668         GO TO 2210-EXIT
000669     END-IF.
000670
000671 2210-EXIT.
000672     EXIT.
```

```cobol
000685 2220-EDIT-CARD.
000686     IF CARD-NUM-INPUT = SPACES OR LOW-VALUES
000687         MOVE 'Card number not provided'
000688             TO WS-MESSAGE
000689         SET WS-INPUT-ERROR TO TRUE
000690         GO TO 2220-EXIT
000691     END-IF.
000692
000693     IF CARD-NUM-INPUT IS NOT NUMERIC
000694         MOVE 'CARD ID FILTER,IF SUPPLIED MUST BE A'
000695           ' 16 DIGIT NUMBER'
000696             TO WS-MESSAGE
000697         SET WS-INPUT-ERROR TO TRUE
000698         GO TO 2220-EXIT
000699     END-IF.
000700
000701     IF CARD-NUM-INPUT = ZEROS
000702         MOVE 'CARD ID FILTER,IF SUPPLIED MUST BE A'
000703           ' 16 DIGIT NUMBER'
000704             TO WS-MESSAGE
000705         SET WS-INPUT-ERROR TO TRUE
000706         GO TO 2220-EXIT
000707     END-IF.
000708
000724 2220-EXIT.
000725     EXIT.
```

```cobol
000736 9100-GETCARD-BYACCTCARD.
000737     EXEC CICS READ
000738         FILE('CARDDAT')
000739         INTO(CARD-RECORD)
000740         RIDFLD(WS-CARD-RID-CARDNUM)
000741         KEYLENGTH(LENGTH OF WS-CARD-RID-CARDNUM)
000742         RESP(WS-RESP-CD)
000743     END-EXEC.
000744
000745     IF WS-RESP-CD = DFHRESP(NOTFND)
000746         MOVE 'Did not find cards for this search'
000747           ' condition' TO WS-MESSAGE
000748         SET WS-NOT-FOUND TO TRUE
000749     END-IF.
000773 9100-EXIT.
000774     EXIT.
```

```cobol
000779 9150-GETCARD-BYACCT.
000780     EXEC CICS READ
000781         FILE('CARDAIX')
000782         INTO(CARD-RECORD)
000783         RIDFLD(WS-CARD-RID-ACCTID)
000784         KEYLENGTH(LENGTH OF WS-CARD-RID-ACCTID)
000785         RESP(WS-RESP-CD)
000786     END-EXEC.
000787
000788     IF WS-RESP-CD = DFHRESP(NOTFND)
000789         MOVE 'Did not find cards for this search'
000790           ' condition' TO WS-MESSAGE
000791         SET WS-NOT-FOUND TO TRUE
000792     END-IF.
000812 9150-EXIT.
000813     EXIT.
```

## Acceptance Criteria

### Scenario 1: Successful lookup by card number

```gherkin
GIVEN a card with number "4000123456789012" exists in the system
  AND it belongs to account "12345678901"
WHEN the user enters card number "4000123456789012" and submits
THEN the card detail screen displays:
  | Field           | Value              |
  | Card Number     | 4000123456789012   |
  | Account ID      | 12345678901        |
  | Embossed Name   | JOHN DOE           |
  | Expiration Date | 2027-12-31         |
  | Active Status   | Y                  |
```

### Scenario 2: Successful lookup by account ID via alternate index

```gherkin
GIVEN an account "12345678901" has at least one card in the system
WHEN the user enters account ID "12345678901" without a card number
THEN the first card associated with that account is displayed
```

### Scenario 3: Card not found

```gherkin
GIVEN no card with number "9999999999999999" exists in the system
WHEN the user enters card number "9999999999999999" and submits
THEN the message "Did not find cards for this search condition" is displayed
```

### Scenario 4: Pre-populated fields from list screen

```gherkin
GIVEN the user selected a card from the list screen (COCRDLIC) with action 'S'
  AND the selected card has account "12345678901" and card number "4000123456789012"
WHEN the card detail screen loads
THEN the account ID field shows "12345678901" and is protected (non-editable)
  AND the card number field shows "4000123456789012" and is protected (non-editable)
  AND the card details are automatically retrieved and displayed
```

### Scenario 5: Invalid account number format

```gherkin
GIVEN the user enters "ABC12345678" as the account number
WHEN the input is validated
THEN the error message "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER" is displayed
  AND no data lookup is performed
```

### Scenario 6: Invalid card number format

```gherkin
GIVEN the user enters "12345" as the card number
WHEN the input is validated
THEN the error message "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER" is displayed
  AND no data lookup is performed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for accessing payment instrument details | Card detail lookup is a protected operation requiring prior authentication; the migrated system must enforce SCA before displaying card data including embossed name and expiration date |
| GDPR | Art. 15 | Right of access to personal data | The card detail view enables data subjects (via authorized staff) to access their card information; the migrated system must log access for audit trail purposes |

## Edge Cases

1. **Account with multiple cards via alternate index**: When looking up by account ID using CARDAIX, the CICS READ returns only the first matching record. If an account has multiple cards, only the first card (by key order) is displayed. The migrated system should clarify whether single-card or multi-card display is expected for account-based lookups.

2. **COMMAREA data integrity from list screen**: When arriving from COCRDLIC, the account and card number are passed via COMMAREA. If the COMMAREA is corrupted or truncated, the pre-populated fields may contain invalid data. The COBOL code does not explicitly validate COMMAREA contents from the calling program. The migrated system should validate input parameters regardless of source.

3. **Concurrent card deletion**: If a card is deleted by another user between the list display and the detail lookup, the READ will return NOTFND. The COBOL program handles this with the "Did not find cards" message. The migrated system must handle this race condition identically.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. The alternate index lookup (CARDAIX) behavior when an account has multiple cards needs clarification -- specifically whether the migrated system should return the first card or present a disambiguation list.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
