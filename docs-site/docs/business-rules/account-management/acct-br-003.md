---
id: "acct-br-003"
title: "Account-card relationship and cross-reference lookup"
domain: "account-management"
cobol_source: "COCRDSLC.cbl:779-812, CVACT03Y.cpy:1-11, CVACT02Y.cpy:1-14"
requirement_id: "ACCT-BR-003"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
  - "AML 2017:11"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-003: Account-card relationship and cross-reference lookup

## Summary

The account-card relationship is maintained through two mechanisms: a direct foreign key (CARD-ACCT-ID in the card record, CVACT02Y.cpy) and a cross-reference structure (CARD-XREF-RECORD in CVACT03Y.cpy) that adds the customer dimension. The card file (CARDDAT) has an alternate index (CARDAIX) on the account ID field, enabling card lookup by account. The cross-reference file (XREFFILE) maps card numbers to customer IDs and account IDs, used primarily by batch transaction processing to resolve card-to-account relationships. This three-way linkage (card ↔ account ↔ customer) is fundamental to the entire system and must be preserved in the migrated relational model.

## Business Logic

### Pseudocode

```
-- Account-based card lookup (COCRDSLC: 9150-GETCARD-BYACCT)
PERFORM GETCARD-BY-ACCOUNT:
    EXEC CICS READ
        FILE('CARDAIX')           -- Alternate index on CARDDAT
        RIDFLD(account-id)        -- 11-digit account ID as key
        KEYLENGTH(11)
        INTO(CARD-RECORD)
    END-EXEC

    EVALUATE CICS-RESPONSE:
        WHEN NORMAL:
            SET found-cards-for-account = TRUE
            -- CARD-RECORD now contains first card for this account
        WHEN NOTFND:
            SET input-error = TRUE
            SET did-not-find-account-in-cardxref = TRUE
        WHEN OTHER:
            SET input-error = TRUE
            MOVE file-error-message TO return-message
    END-EVALUATE

-- Cross-reference lookup (CBTRN01C/02C: batch processing)
PERFORM LOOKUP-XREF:
    READ XREFFILE BY card-number
    IF found:
        account-id = XREF-ACCT-ID
        customer-id = XREF-CUST-ID
    ELSE:
        -- Card not in cross-reference, reject transaction
    END-IF

-- Account filtering in card list (COCRDLIC: 9500-FILTER-RECORDS)
PERFORM FILTER-BY-ACCOUNT:
    IF account-filter-active
        IF CARD-ACCT-ID NOT = filter-account-id
            SET record-skipped = TRUE
            EXIT PARAGRAPH
        END-IF
    END-IF
```

### Relationship Model

```
CUSTOMER (9-digit CUST-ID)
    |
    +--< CARD-XREF-RECORD >-- links to -->
    |       XREF-CARD-NUM (16)
    |       XREF-CUST-ID  (9)  -- FK to customer
    |       XREF-ACCT-ID  (11) -- FK to account
    |
    +--< ACCOUNT (11-digit ACCT-ID)
            |
            +--< CARD-RECORD >-- embedded FK -->
                    CARD-NUM     (16) -- PK
                    CARD-ACCT-ID (11) -- FK to account (direct)
```

### Decision Table

| Lookup Method | Key | Source File | Returns | Use Case |
|--------------|-----|-----------|---------|----------|
| Primary key | CARD-NUM (16) | CARDDAT | Single card record | Card detail/update (COCRDSLC, COCRDUPC) |
| Alternate index | CARD-ACCT-ID (11) | CARDAIX | First card for account | Account-based card lookup (COCRDSLC) |
| Browse + filter | CARD-ACCT-ID (11) | CARDDAT | All matching cards | Card list with account filter (COCRDLIC) |
| Cross-reference | XREF-CARD-NUM (16) | XREFFILE | Account + customer IDs | Batch transaction processing (CBTRN01C/02C) |

## Source COBOL Reference

**Programs:** `COCRDSLC.cbl`, `COCRDLIC.cbl`, `CBTRN01C.cbl`
**Copybooks:** `CVACT02Y.cpy`, `CVACT03Y.cpy`

### CVACT02Y.cpy — Card record with embedded account FK

```cobol
       01  CARD-RECORD.
           05  CARD-NUM                          PIC X(16).
           05  CARD-ACCT-ID                      PIC 9(11).
           05  CARD-CVV-CD                       PIC 9(03).
           05  CARD-EMBOSSED-NAME                PIC X(50).
           05  CARD-EXPIRAION-DATE               PIC X(10).
           05  CARD-ACTIVE-STATUS                PIC X(01).
           05  FILLER                            PIC X(59).
```
*(Lines 1-14 — CARD-ACCT-ID at line 6 is the direct foreign key to account)*

### CVACT03Y.cpy — Cross-reference structure

```cobol
       01 CARD-XREF-RECORD.
           05  XREF-CARD-NUM                     PIC X(16).
           05  XREF-CUST-ID                      PIC 9(09).
           05  XREF-ACCT-ID                      PIC 9(11).
           05  FILLER                            PIC X(14).
```
*(Lines 1-11 — three-way linkage: card → customer → account)*

### COCRDSLC.cbl — Account-based card lookup via alternate index

```cobol
000779 9150-GETCARD-BYACCT.
000780
000781*    Read the Card file. Access via alternate index ACCTID
000782*
000783     EXEC CICS READ
000784          FILE      (LIT-CARDFILENAME-ACCT-PATH)
000785          RIDFLD    (WS-CARD-RID-ACCT-ID)
000786          KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID)
000787          INTO      (CARD-RECORD)
000788          LENGTH    (LENGTH OF CARD-RECORD)
000789          RESP      (WS-RESP-CD)
000790          RESP2     (WS-REAS-CD)
000791     END-EXEC
000792
000793     EVALUATE WS-RESP-CD
000794         WHEN DFHRESP(NORMAL)
000795            SET FOUND-CARDS-FOR-ACCOUNT TO TRUE
000796         WHEN DFHRESP(NOTFND)
000797            SET INPUT-ERROR                 TO TRUE
000798            SET FLG-ACCTFILTER-NOT-OK                TO TRUE
000799            SET DID-NOT-FIND-ACCT-IN-CARDXREF TO TRUE
000800         WHEN OTHER
000801            SET INPUT-ERROR                 TO TRUE
000802            SET FLG-ACCTFILTER-NOT-OK                TO TRUE
000803            MOVE 'READ'                     TO ERROR-OPNAME
000804            MOVE LIT-CARDFILENAME-ACCT-PATH TO ERROR-FILE
000805            MOVE WS-RESP-CD                 TO ERROR-RESP
000806            MOVE WS-REAS-CD                 TO ERROR-RESP2
000807            MOVE WS-FILE-ERROR-MESSAGE      TO WS-RETURN-MSG
000808     END-EVALUATE
000809     .
000810 9150-GETCARD-BYACCT-EXIT.
000811     EXIT
000812     .
```

### COCRDLIC.cbl — Account filter in card list browse

```cobol
001382 9500-FILTER-RECORDS.
001383     SET WS-RECORD-PASSES-FILTER TO TRUE.
001384
001385     IF WS-ACCT-FILTER-ACTIVE
001386         IF CARD-ACCT-ID NOT = WS-FILTER-ACCOUNT-ID
001387             SET WS-RECORD-PASSES-FILTER TO FALSE
001388             GO TO 9500-FILTER-RECORDS-EXIT
001389         END-IF
001390     END-IF.
```
*(Lines 1382-1390 — filters cards by embedded account ID during browse)*

## Acceptance Criteria

### Scenario 1: Lookup cards by account via alternate index

```gherkin
GIVEN an account "12345678901" has associated cards in the system
WHEN a card lookup is performed by account ID using the alternate index
THEN the first card record for that account is returned
  AND CARD-ACCT-ID in the returned record matches "12345678901"
```

### Scenario 2: Account not found in card file

```gherkin
GIVEN an account "99999999999" has no associated cards
WHEN a card lookup is performed by account ID
THEN the system sets DID-NOT-FIND-ACCT-IN-CARDXREF
  AND an input error is raised
```

### Scenario 3: Card list filtered by account

```gherkin
GIVEN the card list contains cards for multiple accounts
  AND the user filters by account "12345678901"
WHEN the card list is browsed
THEN only cards with CARD-ACCT-ID = "12345678901" are displayed
  AND cards belonging to other accounts are excluded
```

### Scenario 4: Cross-reference resolves card to account

```gherkin
GIVEN a card "4000123456789012" exists in the cross-reference file
  AND the cross-reference maps to account "12345678901" and customer "123456789"
WHEN the cross-reference is read by card number
THEN XREF-ACCT-ID = "12345678901"
  AND XREF-CUST-ID = "123456789"
```

### Scenario 5: Multiple cards per account

```gherkin
GIVEN account "12345678901" has 3 associated cards
WHEN the card list is filtered by account "12345678901"
THEN all 3 cards are displayed across paginated results
  AND the alternate index lookup returns only the first card by key order
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for accessing payment instrument data | Account-based card lookup ensures users access only cards associated with their authorized account |
| GDPR | Art. 15 | Data subject's right of access to personal data | The cross-reference structure enables retrieving all cards and accounts for a given customer, supporting data subject access requests |
| AML 2017:11 | Para. 3 | Transaction monitoring requires linking transactions to account holders | The card → account → customer chain enables full transaction attribution for AML monitoring purposes |

## Edge Cases

1. **Alternate index returns only first card**: When looking up by account ID using CARDAIX, CICS READ returns only the first matching record by primary key order. If an account has multiple cards, only one is displayed in the detail view. The migrated system should clarify whether single-card or multi-card display is expected for account-based lookups — a disambiguation list may be more appropriate.

2. **Cross-reference vs direct FK redundancy**: The CARD-ACCT-ID field in the card record provides a direct card-to-account link, making the cross-reference partially redundant for that relationship. The cross-reference adds the customer dimension (XREF-CUST-ID) which is not in the card record. The migrated system should normalize this into proper foreign key relationships.

3. **Orphaned cross-references**: If a cross-reference entry exists for a card that has been deleted, or maps to a non-existent account, the system has no referential integrity enforcement. The COBOL batch program (CBTRN01C) handles this by displaying a warning but continuing processing. The migrated system should enforce referential integrity at the database level.

4. **One-to-many cardinality**: The relationship is: one account can have many cards, one card belongs to one account, and one customer can have multiple accounts and cards. The cross-reference captures the full N:M:N relationship chain.

5. **File path aliasing**: The alternate index is accessed via `LIT-CARDFILENAME-ACCT-PATH` which maps to the CARDAIX DD name. This is a CICS file alias, not a separate physical file. The migrated system replaces this with a SQL index on the account ID column of the cards table.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Should the card detail view display all cards for an account (multi-card list) or just the first one? (2) Is the cross-reference file (XREFFILE) a separate physical file or an alternate index path? (3) Are there any business rules about the maximum number of cards per account? (4) Is the customer-account relationship always 1:1 or can one customer have multiple accounts? (5) Should the migrated system enforce referential integrity between cards, accounts, and customers at the database level?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
