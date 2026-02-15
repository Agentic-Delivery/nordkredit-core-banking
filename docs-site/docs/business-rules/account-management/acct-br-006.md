---
id: "acct-br-006"
title: "Three-file read chain for account lookup"
domain: "account-management"
cobol_source: "COACTUPC.cbl:3608-3800"
requirement_id: "ACCT-BR-006"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 15"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# ACCT-BR-006: Three-file read chain for account lookup

## Summary

Both the account view (COACTVWC) and account update (COACTUPC) programs use a three-file sequential read chain to retrieve all data associated with an account. This read chain is the fundamental data access pattern for account management operations. The sequence reads from three VSAM files in strict order: (1) Card Cross-Reference (CXACAIX) by account ID to obtain the customer ID and card number, (2) Account Master (ACCTDAT) by account ID for financial data, and (3) Customer Master (CUSTDAT) by customer ID (obtained from step 1) for personal data. If any step in the chain fails, subsequent steps are skipped and a specific error message is displayed identifying the failing file and key. After a successful chain, all field values are stored as a snapshot in the COMMAREA for use in change detection and optimistic concurrency control during updates.

## Business Logic

### Pseudocode

```
PERFORM 9000-READ-ACCT:
    SET INPUT-OK TO TRUE
    SET WS-READ-SUCCESS TO TRUE

    -- Step 1: Read Card Cross-Reference by Account ID
    PERFORM 9200-GETCARDXREF-BYACCT
    IF NOT WS-READ-SUCCESS
        EXIT PARAGRAPH  -- Chain broken at step 1
    END-IF

    -- Step 2: Read Account Master by Account ID
    PERFORM 9300-GETACCTDATA-BYACCT
    IF NOT WS-READ-SUCCESS
        EXIT PARAGRAPH  -- Chain broken at step 2
    END-IF

    -- Step 3: Read Customer Master by Customer ID (from step 1)
    PERFORM 9400-GETCUSTDATA-BYCUST
    IF NOT WS-READ-SUCCESS
        EXIT PARAGRAPH  -- Chain broken at step 3
    END-IF

    -- Step 4: Store snapshot for change detection
    PERFORM 9500-STORE-FETCHED-DATA


9200-GETCARDXREF-BYACCT:
    EXEC CICS READ
        FILE('CXACAIX')
        INTO(CARD-XREF-RECORD)
        RIDFLD(WS-ACCT-ID)         -- PIC 9(11)
        KEYLENGTH(LENGTH OF WS-ACCT-ID)
        RESP(WS-RESP-CD)
        RESP2(WS-REAS-CD)
    END-EXEC

    EVALUATE WS-RESP-CD
        WHEN DFHRESP(NORMAL)
            MOVE XREF-CUST-ID TO WS-CUST-ID
            MOVE XREF-CARD-NUM TO WS-CARD-NUM
        WHEN DFHRESP(NOTFND)
            MOVE 'Account ID not found in Cross-Reference file'
                TO WS-MESSAGE
            SET INPUT-ERROR TO TRUE
            SET WS-READ-SUCCESS TO FALSE
        WHEN OTHER
            MOVE 'Error reading Cross-Reference file'
                TO WS-MESSAGE
            MOVE WS-RESP-CD TO WS-ERR-RESP
            MOVE WS-REAS-CD TO WS-ERR-RESP2
            SET WS-READ-SUCCESS TO FALSE
    END-EVALUATE


9300-GETACCTDATA-BYACCT:
    EXEC CICS READ
        FILE('ACCTDAT')
        INTO(ACCT-RECORD)
        RIDFLD(WS-ACCT-ID)         -- PIC 9(11)
        KEYLENGTH(LENGTH OF WS-ACCT-ID)
        RESP(WS-RESP-CD)
        RESP2(WS-REAS-CD)
    END-EXEC

    EVALUATE WS-RESP-CD
        WHEN DFHRESP(NORMAL)
            CONTINUE  -- Account data loaded
        WHEN DFHRESP(NOTFND)
            MOVE 'Account ID not found in Account Master file'
                TO WS-MESSAGE
            SET INPUT-ERROR TO TRUE
            SET WS-READ-SUCCESS TO FALSE
        WHEN OTHER
            MOVE 'Error reading Account Master file'
                TO WS-MESSAGE
            MOVE WS-RESP-CD TO WS-ERR-RESP
            MOVE WS-REAS-CD TO WS-ERR-RESP2
            SET WS-READ-SUCCESS TO FALSE
    END-EVALUATE


9400-GETCUSTDATA-BYCUST:
    EXEC CICS READ
        FILE('CUSTDAT')
        INTO(CUST-RECORD)
        RIDFLD(WS-CUST-ID)         -- PIC 9(09), from step 1
        KEYLENGTH(LENGTH OF WS-CUST-ID)
        RESP(WS-RESP-CD)
        RESP2(WS-REAS-CD)
    END-EXEC

    EVALUATE WS-RESP-CD
        WHEN DFHRESP(NORMAL)
            CONTINUE  -- Customer data loaded
        WHEN DFHRESP(NOTFND)
            MOVE 'Customer ID not found in Customer Master file'
                TO WS-MESSAGE
            SET INPUT-ERROR TO TRUE
            SET WS-READ-SUCCESS TO FALSE
        WHEN OTHER
            MOVE 'Error reading Customer Master file'
                TO WS-MESSAGE
            MOVE WS-RESP-CD TO WS-ERR-RESP
            MOVE WS-REAS-CD TO WS-ERR-RESP2
            SET WS-READ-SUCCESS TO FALSE
    END-EVALUATE


9500-STORE-FETCHED-DATA:
    -- Store account snapshot
    MOVE ACCT-ACTIVE-STATUS   TO ACUP-OLD-ACTIVE-STATUS
    MOVE ACCT-CURR-BAL        TO ACUP-OLD-CURR-BAL
    MOVE ACCT-CREDIT-LIMIT    TO ACUP-OLD-CREDIT-LIMIT
    MOVE ACCT-CASH-CREDIT-LIMIT TO ACUP-OLD-CASH-CREDIT-LIMIT
    MOVE ACCT-CURR-CYC-CREDIT TO ACUP-OLD-CURR-CYC-CREDIT
    MOVE ACCT-CURR-CYC-DEBIT  TO ACUP-OLD-CURR-CYC-DEBIT
    MOVE ACCT-OPEN-DATE       TO ACUP-OLD-OPEN-DATE
    MOVE ACCT-EXPIRAION-DATE  TO ACUP-OLD-EXPIRATION-DATE
    MOVE ACCT-REISSUE-DATE    TO ACUP-OLD-REISSUE-DATE
    MOVE ACCT-GROUP-ID        TO ACUP-OLD-GROUP-ID

    -- Store customer snapshot
    MOVE CUST-FIRST-NAME      TO ACUP-OLD-FIRST-NAME
    MOVE CUST-MIDDLE-NAME     TO ACUP-OLD-MIDDLE-NAME
    MOVE CUST-LAST-NAME       TO ACUP-OLD-LAST-NAME
    MOVE CUST-ADDR-LINE-1     TO ACUP-OLD-ADDR-LINE-1
    MOVE CUST-ADDR-LINE-2     TO ACUP-OLD-ADDR-LINE-2
    MOVE CUST-ADDR-LINE-3     TO ACUP-OLD-ADDR-LINE-3
    MOVE CUST-ADDR-STATE-CD   TO ACUP-OLD-ADDR-STATE-CD
    MOVE CUST-ADDR-ZIP        TO ACUP-OLD-ADDR-ZIP
    MOVE CUST-ADDR-COUNTRY-CD TO ACUP-OLD-ADDR-COUNTRY-CD
    MOVE CUST-PHONE-NUM-1     TO ACUP-OLD-PHONE-NUM-1
    MOVE CUST-PHONE-NUM-2     TO ACUP-OLD-PHONE-NUM-2
    MOVE CUST-SSN             TO ACUP-OLD-SSN
    MOVE CUST-DOB             TO ACUP-OLD-DOB
    MOVE CUST-FICO-CREDIT-SCORE TO ACUP-OLD-FICO-CREDIT-SCORE
    MOVE CUST-EFT-ACCOUNT-ID  TO ACUP-OLD-EFT-ACCOUNT-ID
    MOVE CUST-PRI-CARD-HOLDER-IND TO ACUP-OLD-PRI-CARD-HOLDER-IND

    -- Also copy to NEW fields for initial screen display
    MOVE ACUP-OLD-ACCT-DATA   TO ACUP-NEW-ACCT-DATA
    MOVE ACUP-OLD-CUST-DATA   TO ACUP-NEW-CUST-DATA
```

### Decision Table

| Step | File | Key | Key Type | Success | Not Found Message | Error Handling |
|------|------|-----|----------|---------|-------------------|----------------|
| 1 | CXACAIX | Account ID | PIC 9(11), alternate index | Extract CUST-ID and CARD-NUM, proceed to step 2 | "Account ID not found in Cross-Reference file" | Log RESP/RESP2, abort chain |
| 2 | ACCTDAT | Account ID | PIC 9(11), primary key | Load account financial data, proceed to step 3 | "Account ID not found in Account Master file" | Log RESP/RESP2, abort chain |
| 3 | CUSTDAT | Customer ID | PIC 9(09), primary key (from step 1) | Load customer personal data, proceed to step 4 | "Customer ID not found in Customer Master file" | Log RESP/RESP2, abort chain |
| 4 | N/A | N/A | N/A | Store all fields as snapshot in COMMAREA | N/A | N/A |

### File Relationship Diagram

```
+---------------------+          +---------------------+
|  User enters        |          |                     |
|  Account ID         |          |                     |
|  PIC 9(11)          |          |                     |
+---------------------+          |                     |
         |                        |                     |
         v                        |                     |
+---------------------+          |                     |
| 1. CXACAIX          |          |                     |
| Card Cross-Ref      |          |                     |
| Key: Account ID     |          |                     |
| (alternate index)   |          |                     |
|                     |          |                     |
| Returns:            |          |                     |
|  - XREF-CUST-ID ---------+    |                     |
|  - XREF-CARD-NUM   |     |    |                     |
+---------------------+     |    |                     |
         |                   |    |                     |
    Account ID               |    |                     |
    (same key)               |    |                     |
         |                   |    |                     |
         v                   |    |                     |
+---------------------+     |    |                     |
| 2. ACCTDAT          |     |    |                     |
| Account Master      |     |    |                     |
| Key: Account ID     |     |    |                     |
| (primary key)       |     |    |                     |
|                     |     |    |                     |
| Returns:            |     |    |                     |
|  - Financial data   |     |    |                     |
|  - Status, balances |     |    |                     |
|  - Dates, limits    |     |    |                     |
+---------------------+     |    |                     |
                             |    |                     |
         +-------------------+    |                     |
         |                        |                     |
         v                        |                     |
+---------------------+          |                     |
| 3. CUSTDAT          |          |                     |
| Customer Master     |          |                     |
| Key: Customer ID    |          |                     |
| PIC 9(09)           |          |                     |
| (from CXACAIX)      |          |                     |
|                     |          |                     |
| Returns:            |          |                     |
|  - Personal data    |          |                     |
|  - Address, phone   |          |                     |
|  - SSN, DOB, FICO   |          |                     |
+---------------------+          |                     |
         |                        |                     |
         v                        |                     |
+---------------------+          |                     |
| 4. COMMAREA         |          |                     |
| Snapshot Storage    |          |                     |
| (2000 bytes)        |          |                     |
|                     |          |                     |
| OLD-ACCT-DATA +     |          |                     |
| OLD-CUST-DATA +     |          |                     |
| NEW-ACCT-DATA +     |          |                     |
| NEW-CUST-DATA       |          |                     |
+---------------------+          +---------------------+
```

## Source COBOL Reference

**Programs:** `COACTUPC.cbl` (lines 3608-3887), `COACTVWC.cbl` (lines 687-780)

### Read chain orchestration in COACTUPC (lines 3608-3648)

```cobol
003608 9000-READ-ACCT.
003609     SET INPUT-OK TO TRUE.
003610     SET WS-READ-SUCCESS TO TRUE.
003611
003612     PERFORM 9200-GETCARDXREF-BYACCT.
003613     IF NOT WS-READ-SUCCESS
003614         GO TO 9000-READ-ACCT-EXIT
003615     END-IF.
003616
003617     PERFORM 9300-GETACCTDATA-BYACCT.
003618     IF NOT WS-READ-SUCCESS
003619         GO TO 9000-READ-ACCT-EXIT
003620     END-IF.
003621
003622     PERFORM 9400-GETCUSTDATA-BYCUST.
003623     IF NOT WS-READ-SUCCESS
003624         GO TO 9000-READ-ACCT-EXIT
003625     END-IF.
003626
003627     PERFORM 9500-STORE-FETCHED-DATA.
003628
003648 9000-READ-ACCT-EXIT.
003649     EXIT.
```

### Read chain orchestration in COACTVWC (lines 687-721)

```cobol
000687 9000-READ-ACCT.
000688     SET INPUT-OK TO TRUE.
000689     SET WS-READ-SUCCESS TO TRUE.
000690
000691     PERFORM 9200-GETCARDXREF-BYACCT.
000692     IF NOT WS-READ-SUCCESS
000693         GO TO 9000-READ-ACCT-EXIT
000694     END-IF.
000695
000696     PERFORM 9300-GETACCTDATA-BYACCT.
000697     IF NOT WS-READ-SUCCESS
000698         GO TO 9000-READ-ACCT-EXIT
000699     END-IF.
000700
000701     PERFORM 9400-GETCUSTDATA-BYCUST.
000702     IF NOT WS-READ-SUCCESS
000703         GO TO 9000-READ-ACCT-EXIT
000704     END-IF.
000705
000721 9000-READ-ACCT-EXIT.
000722     EXIT.
```

### Card Cross-Reference read - CXACAIX (lines 3650-3690)

```cobol
003650 9200-GETCARDXREF-BYACCT.
003651     EXEC CICS READ
003652         FILE('CXACAIX')
003653         INTO(CARD-XREF-RECORD)
003654         RIDFLD(WS-ACCT-ID)
003655         KEYLENGTH(LENGTH OF WS-ACCT-ID)
003656         RESP(WS-RESP-CD)
003657         RESP2(WS-REAS-CD)
003658     END-EXEC.
003659
003660     EVALUATE WS-RESP-CD
003661         WHEN DFHRESP(NORMAL)
003662             MOVE XREF-CUST-ID TO WS-CUST-ID
003663             MOVE XREF-CARD-NUM TO WS-CARD-NUM
003664         WHEN DFHRESP(NOTFND)
003665             MOVE 'Account ' TO WS-MSG-FILE
003666             MOVE WS-ACCT-ID TO WS-MSG-KEY
003667             MOVE 'not found in Cross-Reference file'
003668                 TO WS-MSG-DETAIL
003669             SET INPUT-ERROR TO TRUE
003670             SET WS-READ-SUCCESS TO FALSE
003680         WHEN OTHER
003681             MOVE 'READ' TO WS-MSG-OPER
003682             MOVE 'CXACAIX' TO WS-MSG-FILE
003683             MOVE WS-RESP-CD TO WS-MSG-RESP
003684             MOVE WS-REAS-CD TO WS-MSG-RESP2
003685             SET WS-READ-SUCCESS TO FALSE
003690     END-EVALUATE.
```

### Account Master read - ACCTDAT (lines 3700-3740)

```cobol
003700 9300-GETACCTDATA-BYACCT.
003701     EXEC CICS READ
003702         FILE('ACCTDAT')
003703         INTO(ACCT-RECORD)
003704         RIDFLD(WS-ACCT-ID)
003705         KEYLENGTH(LENGTH OF WS-ACCT-ID)
003706         RESP(WS-RESP-CD)
003707         RESP2(WS-REAS-CD)
003708     END-EXEC.
003709
003710     EVALUATE WS-RESP-CD
003711         WHEN DFHRESP(NORMAL)
003712             CONTINUE
003713         WHEN DFHRESP(NOTFND)
003714             MOVE 'Account ' TO WS-MSG-FILE
003715             MOVE WS-ACCT-ID TO WS-MSG-KEY
003716             MOVE 'not found in Account Master file'
003717                 TO WS-MSG-DETAIL
003718             SET INPUT-ERROR TO TRUE
003719             SET WS-READ-SUCCESS TO FALSE
003730         WHEN OTHER
003731             MOVE 'READ' TO WS-MSG-OPER
003732             MOVE 'ACCTDAT' TO WS-MSG-FILE
003733             MOVE WS-RESP-CD TO WS-MSG-RESP
003734             MOVE WS-REAS-CD TO WS-MSG-RESP2
003735             SET WS-READ-SUCCESS TO FALSE
003740     END-EVALUATE.
```

### Customer Master read - CUSTDAT (lines 3750-3790)

```cobol
003750 9400-GETCUSTDATA-BYCUST.
003751     EXEC CICS READ
003752         FILE('CUSTDAT')
003753         INTO(CUST-RECORD)
003754         RIDFLD(WS-CUST-ID)
003755         KEYLENGTH(LENGTH OF WS-CUST-ID)
003756         RESP(WS-RESP-CD)
003757         RESP2(WS-REAS-CD)
003758     END-EXEC.
003759
003760     EVALUATE WS-RESP-CD
003761         WHEN DFHRESP(NORMAL)
003762             CONTINUE
003763         WHEN DFHRESP(NOTFND)
003764             MOVE 'Customer ' TO WS-MSG-FILE
003765             MOVE WS-CUST-ID TO WS-MSG-KEY
003766             MOVE 'not found in Customer Master file'
003767                 TO WS-MSG-DETAIL
003768             SET INPUT-ERROR TO TRUE
003769             SET WS-READ-SUCCESS TO FALSE
003780         WHEN OTHER
003781             MOVE 'READ' TO WS-MSG-OPER
003782             MOVE 'CUSTDAT' TO WS-MSG-FILE
003783             MOVE WS-RESP-CD TO WS-MSG-RESP
003784             MOVE WS-REAS-CD TO WS-MSG-RESP2
003785             SET WS-READ-SUCCESS TO FALSE
003790     END-EVALUATE.
```

### Snapshot storage (lines 3801-3887)

```cobol
003801 9500-STORE-FETCHED-DATA.
003802     MOVE ACCT-ACTIVE-STATUS TO ACUP-OLD-ACTIVE-STATUS.
003803     MOVE ACCT-CURR-BAL      TO ACUP-OLD-CURR-BAL.
003804     MOVE ACCT-CREDIT-LIMIT  TO ACUP-OLD-CREDIT-LIMIT.
003805     MOVE ACCT-CASH-CREDIT-LIMIT
003806                             TO ACUP-OLD-CASH-CREDIT-LIMIT.
003810     MOVE ACCT-CURR-CYC-CREDIT
003811                             TO ACUP-OLD-CURR-CYC-CREDIT.
003812     MOVE ACCT-CURR-CYC-DEBIT
003813                             TO ACUP-OLD-CURR-CYC-DEBIT.
003820     MOVE ACCT-OPEN-DATE     TO ACUP-OLD-OPEN-DATE.
003821     MOVE ACCT-EXPIRAION-DATE
003822                             TO ACUP-OLD-EXPIRATION-DATE.
003823     MOVE ACCT-REISSUE-DATE  TO ACUP-OLD-REISSUE-DATE.
003824     MOVE ACCT-GROUP-ID      TO ACUP-OLD-GROUP-ID.
003830     MOVE CUST-FIRST-NAME    TO ACUP-OLD-FIRST-NAME.
003831     MOVE CUST-MIDDLE-NAME   TO ACUP-OLD-MIDDLE-NAME.
003832     MOVE CUST-LAST-NAME     TO ACUP-OLD-LAST-NAME.
003840     MOVE CUST-ADDR-LINE-1   TO ACUP-OLD-ADDR-LINE-1.
003841     MOVE CUST-ADDR-LINE-2   TO ACUP-OLD-ADDR-LINE-2.
003842     MOVE CUST-ADDR-LINE-3   TO ACUP-OLD-ADDR-LINE-3.
003843     MOVE CUST-ADDR-STATE-CD TO ACUP-OLD-ADDR-STATE-CD.
003844     MOVE CUST-ADDR-ZIP      TO ACUP-OLD-ADDR-ZIP.
003845     MOVE CUST-ADDR-COUNTRY-CD
003846                             TO ACUP-OLD-ADDR-COUNTRY-CD.
003850     MOVE CUST-PHONE-NUM-1   TO ACUP-OLD-PHONE-NUM-1.
003851     MOVE CUST-PHONE-NUM-2   TO ACUP-OLD-PHONE-NUM-2.
003860     MOVE CUST-SSN           TO ACUP-OLD-SSN.
003861     MOVE CUST-DOB           TO ACUP-OLD-DOB.
003870     MOVE CUST-FICO-CREDIT-SCORE
003871                             TO ACUP-OLD-FICO-CREDIT-SCORE.
003872     MOVE CUST-EFT-ACCOUNT-ID
003873                             TO ACUP-OLD-EFT-ACCOUNT-ID.
003880     MOVE CUST-PRI-CARD-HOLDER-IND
003881                             TO ACUP-OLD-PRI-CARD-HOLDER-IND.
003885     MOVE ACUP-OLD-ACCT-DATA TO ACUP-NEW-ACCT-DATA.
003886     MOVE ACUP-OLD-CUST-DATA TO ACUP-NEW-CUST-DATA.
003887 9500-EXIT.
003888     EXIT.
```

## Acceptance Criteria

### Scenario 1: Successful three-file read chain

```gherkin
GIVEN an account with ID "12345678901" exists in the system
  AND a cross-reference record links account "12345678901" to customer "987654321"
  AND account master and customer master records exist for those IDs
WHEN the system performs the three-file read chain
THEN the card cross-reference is read first, yielding customer ID "987654321" and a card number
  AND the account master is read second, yielding financial data (balance, limits, dates)
  AND the customer master is read third using customer ID "987654321", yielding personal data
  AND all account and customer field values are stored as a snapshot in the COMMAREA
  AND the NEW fields are initialized with the same values as the OLD fields
```

### Scenario 2: Cross-reference record not found

```gherkin
GIVEN no cross-reference record exists for account ID "99999999999"
WHEN the system performs the three-file read chain
THEN the CXACAIX read returns NOTFND
  AND the message "Account ID not found in Cross-Reference file" is displayed
  AND the account master and customer master reads are NOT attempted
  AND INPUT-ERROR is set
```

### Scenario 3: Account master record not found

```gherkin
GIVEN a cross-reference record exists for account ID "12345678901"
  AND no account master record exists for that account ID
WHEN the system performs the three-file read chain
THEN the CXACAIX read succeeds
  AND the ACCTDAT read returns NOTFND
  AND the message "Account ID not found in Account Master file" is displayed
  AND the customer master read is NOT attempted
```

### Scenario 4: Customer master record not found (orphaned cross-reference)

```gherkin
GIVEN a cross-reference record links account "12345678901" to customer "987654321"
  AND an account master record exists for "12345678901"
  AND no customer master record exists for customer "987654321"
WHEN the system performs the three-file read chain
THEN the CXACAIX and ACCTDAT reads succeed
  AND the CUSTDAT read returns NOTFND
  AND the message "Customer ID not found in Customer Master file" is displayed
  AND no snapshot is stored
```

### Scenario 5: File unavailable (generic error)

```gherkin
GIVEN the ACCTDAT file is temporarily unavailable (CICS RESP = NOTOPEN)
WHEN the system attempts to read the account master
THEN the RESP code is not NORMAL and not NOTFND
  AND a generic error message is constructed with the operation name ("READ"), file name ("ACCTDAT"), RESP code, and RESP2 code
  AND the read chain is aborted
```

### Scenario 6: Identical read chain in view and update programs

```gherkin
GIVEN the account view program (COACTVWC) and account update program (COACTUPC) both implement the read chain
WHEN either program reads account "12345678901"
THEN the same three-file sequence is followed (CXACAIX -> ACCTDAT -> CUSTDAT)
  AND the same error handling logic applies
  AND the same field data is retrieved
  AND the update program additionally stores the snapshot in the COMMAREA for change detection
```

### Scenario 7: Snapshot includes all fields for concurrency control

```gherkin
GIVEN the three-file read chain completes successfully for account "12345678901"
WHEN the snapshot is stored in the COMMAREA
THEN ALL of the following account fields are preserved:
  | Field | Description |
  | ACCT-ACTIVE-STATUS | Account status flag |
  | ACCT-CURR-BAL | Current balance |
  | ACCT-CREDIT-LIMIT | Credit limit |
  | ACCT-CASH-CREDIT-LIMIT | Cash advance credit limit |
  | ACCT-CURR-CYC-CREDIT | Current cycle credits |
  | ACCT-CURR-CYC-DEBIT | Current cycle debits |
  | ACCT-OPEN-DATE | Account open date |
  | ACCT-EXPIRAION-DATE | Account expiration date |
  | ACCT-REISSUE-DATE | Card reissue date |
  | ACCT-GROUP-ID | Account group identifier |
  AND ALL of the following customer fields are preserved:
  | CUST-FIRST-NAME | First name |
  | CUST-MIDDLE-NAME | Middle name |
  | CUST-LAST-NAME | Last name |
  | CUST-ADDR-LINE-1 through CUST-ADDR-COUNTRY-CD | Full address |
  | CUST-PHONE-NUM-1, CUST-PHONE-NUM-2 | Phone numbers |
  | CUST-SSN | Social security number |
  | CUST-DOB | Date of birth |
  | CUST-FICO-CREDIT-SCORE | Credit score |
  | CUST-EFT-ACCOUNT-ID | EFT account ID |
  | CUST-PRI-CARD-HOLDER-IND | Primary card holder indicator |
  AND the NEW data sections are initialized as copies of the OLD data sections
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication for accessing payment account information | The three-file read chain retrieves sensitive account and customer data; the migrated system must ensure that SCA is enforced before this data access operation is invoked, and that each read in the chain is covered by the same authenticated session |
| GDPR | Art. 15 | Data subject's right of access to personal data | The read chain aggregates personal data (name, address, phone, SSN, DOB) from the customer master file; the migrated system must log this data access for audit trail purposes and ensure the data is only returned to authorized users with a legitimate purpose |

## Edge Cases

1. **Referential integrity violation (orphaned records)**: The three-file chain assumes referential integrity between CXACAIX, ACCTDAT, and CUSTDAT. If the cross-reference contains a customer ID that does not exist in CUSTDAT (orphaned reference), the chain fails at step 3. The COBOL program handles this with a NOTFND message but does not attempt self-healing. The migrated system should log referential integrity violations for investigation and consider whether partial data display (account without customer) is acceptable.

2. **Multiple cross-reference records per account**: The CXACAIX file is read using an alternate index by account ID. If an account has multiple cards (and thus multiple cross-reference records), the CICS READ returns only the first record. The customer ID should be the same across all cross-reference records for the same account. The migrated system should validate this assumption or handle the case where different cross-reference records for the same account point to different customer IDs.

3. **VSAM file I/O errors during chain**: If any file encounters an I/O error (RESP other than NORMAL or NOTFND), the COBOL program captures the RESP and RESP2 codes in the error message. The migrated system should map these to appropriate HTTP status codes (503 for file unavailable, 500 for unexpected errors) and include correlation IDs for troubleshooting.

4. **Concurrent deletion during read chain**: If the account master record is deleted by another transaction between the cross-reference read (step 1) and the account master read (step 2), the chain fails at step 2 with NOTFND. The COBOL program handles this identically to the case where the record never existed. The migrated system should consider whether the error message should distinguish between "record not found" and "record was deleted during access."

5. **COMMAREA overflow**: The snapshot storage (9500-STORE-FETCHED-DATA) copies all fields into the COMMAREA, which has a fixed 2000-byte size. If the combined size of all OLD and NEW fields exceeds the COMMAREA capacity, data truncation occurs silently in COBOL. The migrated system should validate that the session state storage is adequately sized for all snapshot fields.

6. **EBCDIC-to-Unicode conversion for snapshot fields**: Customer name fields (CUST-FIRST-NAME, etc.) stored in EBCDIC on the mainframe must be correctly converted to Unicode in the migrated system. The snapshot comparison logic must use the same encoding for both the stored snapshot and the freshly read data to avoid false concurrency conflicts caused by encoding differences.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) Is the assumption that all cross-reference records for the same account point to the same customer ID always valid in production data? (2) Are there any accounts that exist in ACCTDAT but not in CXACAIX (e.g., accounts without cards)? (3) Should the migrated system implement the read chain as a single SQL JOIN or preserve the sequential read pattern for traceability during parallel-run validation? (4) What is the expected latency impact of converting three sequential VSAM reads to database queries?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
