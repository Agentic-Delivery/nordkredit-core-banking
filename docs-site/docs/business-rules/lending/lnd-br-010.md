---
id: "lnd-br-010"
title: "Lending transaction categorization and balance tracking"
domain: "lending"
cobol_source: "CBTRN02C.cbl:467-542"
requirement_id: "LND-BR-010"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 64"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# LND-BR-010: Lending transaction categorization and balance tracking

## Summary

The transaction categorization system maintains running balances per account, transaction type, and transaction category. This triple-key structure (`ACCT-ID + TRAN-TYPE-CD + TRAN-CAT-CD`) enables granular tracking of lending activity — distinguishing between purchases, cash advances, balance transfers, fees, interest charges, and repayments. During daily batch posting (CBTRN02C), each posted transaction updates the corresponding category balance record in the TCATBAL file. If no record exists for the key combination, a new record is created; otherwise, the existing balance is updated.

This categorization supports FSA regulatory reporting requirements (FFFS 2014:5 Chapter 7 — Financial Systems) by enabling breakdown of lending portfolio activity by type and category. It also supports credit risk analysis by revealing utilization patterns per account (e.g., proportion of cash advances vs. purchases, which may indicate different risk profiles).

## Business Logic

### Pseudocode

```
PERFORM UPDATE-TRANSACTION-CATEGORY-BALANCE:
    -- Build composite key
    MOVE account-id TO TRANCAT-ACCT-ID
    MOVE transaction-type-code TO TRANCAT-TYPE-CD
    MOVE transaction-category-code TO TRANCAT-CD

    -- Attempt to read existing record
    READ TCATBAL-FILE using composite key
    IF record found (status = '00')
        -- Update existing balance
        ADD transaction-amount TO TRAN-CAT-BAL
        REWRITE TCATBAL record
    ELSE IF record not found (status = '23')
        -- Create new category balance record
        INITIALIZE TRAN-CAT-BAL-RECORD
        MOVE account-id TO TRANCAT-ACCT-ID
        MOVE transaction-type-code TO TRANCAT-TYPE-CD
        MOVE transaction-category-code TO TRANCAT-CD
        MOVE transaction-amount TO TRAN-CAT-BAL
        WRITE new TCATBAL record
    ELSE
        -- Unexpected file error
        ABEND with error
    END-IF
```

### Decision Table

| Record Exists | File Status | Action | Outcome |
|--------------|-------------|--------|---------|
| Yes | '00' | ADD amount, REWRITE | Balance updated |
| No | '23' | INITIALIZE, WRITE | New record created |
| Error | Other | ABEND | Processing halted |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 467-542 (category balance update)

```cobol
000467 2700-UPDATE-TCATBAL.
000468      * Update the balances in transaction balance file.
000469           MOVE XREF-ACCT-ID TO FD-TRANCAT-ACCT-ID
000470           MOVE DALYTRAN-TYPE-CD TO FD-TRANCAT-TYPE-CD
000471           MOVE DALYTRAN-CAT-CD TO FD-TRANCAT-CD
000472
000473           MOVE 'N' TO WS-CREATE-TRANCAT-REC
000474           READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD
000475              INVALID KEY
000476                DISPLAY 'TCATBAL record not found for key : '
000477                   FD-TRAN-CAT-KEY '.. Creating.'
000478                MOVE 'Y' TO WS-CREATE-TRANCAT-REC
000479           END-READ.
```
*(Lines 467-479 — composite key construction and record lookup: attempts to read existing category balance, flags for creation if not found)*

```cobol
000495           IF WS-CREATE-TRANCAT-REC = 'Y'
000496              PERFORM 2700-A-CREATE-TCATBAL-REC
000497           ELSE
000498              PERFORM 2700-B-UPDATE-TCATBAL-REC
000499           END-IF
```
*(Lines 495-499 — branching logic: creates new record or updates existing based on lookup result)*

```cobol
000503 2700-A-CREATE-TCATBAL-REC.
000504           INITIALIZE TRAN-CAT-BAL-RECORD
000505           MOVE XREF-ACCT-ID TO TRANCAT-ACCT-ID
000506           MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD
000507           MOVE DALYTRAN-CAT-CD TO TRANCAT-CD
000508           ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000509
000510           WRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
```
*(Lines 503-510 — new record creation: initializes record, sets key fields, adds transaction amount as initial balance)*

```cobol
000526 2700-B-UPDATE-TCATBAL-REC.
000527           ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000528           REWRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
```
*(Lines 526-528 — existing record update: adds transaction amount to running balance and rewrites)*

**Transaction category balance file structure:**

```cobol
000057       FD  TCATBAL-FILE.
000092       01  FD-TRAN-CAT-BAL-RECORD.
000093           05 FD-TRAN-CAT-KEY.
000094              10 FD-TRANCAT-ACCT-ID             PIC 9(11).
000095              10 FD-TRANCAT-TYPE-CD             PIC X(02).
000096              10 FD-TRANCAT-CD                  PIC 9(04).
000097           05 FD-FD-TRAN-CAT-DATA               PIC X(33).
```
*(Lines 92-97 — TCATBAL file record: 17-byte composite key (11-digit account + 2-char type + 4-digit category) + 33-byte data area containing the running balance)*

## Acceptance Criteria

### Scenario 1: First transaction creates new category balance record

```gherkin
GIVEN no transaction category balance record exists for:
  | Account ID     | 12345678901 |
  | Type Code      | PU          |
  | Category Code  | 0001        |
WHEN a purchase transaction of 500.00 is posted
THEN a new TCATBAL record is created with balance 500.00
  AND the composite key is "12345678901PU0001"
```

### Scenario 2: Subsequent transaction updates existing balance

```gherkin
GIVEN a transaction category balance record exists for:
  | Account ID     | 12345678901 |
  | Type Code      | PU          |
  | Category Code  | 0001        |
  | Current Balance | 500.00      |
WHEN another purchase transaction of 300.00 is posted
THEN the TCATBAL balance is updated to 800.00 (500.00 + 300.00)
```

### Scenario 3: Repayment reduces category balance

```gherkin
GIVEN a transaction category balance record for repayments:
  | Account ID     | 12345678901 |
  | Type Code      | PA          |
  | Category Code  | 0001        |
  | Current Balance | -3000.00     |
WHEN a repayment of -1000.00 is posted
THEN the TCATBAL balance is updated to -4000.00 (-3000.00 + (-1000.00))
```

### Scenario 4: Multiple transaction types tracked separately

```gherkin
GIVEN a loan account "12345678901"
WHEN the following transactions are posted:
  | Type | Category | Amount   |
  | PU   | 0001     | 1000.00  |
  | CA   | 0001     | 500.00   |
  | PA   | 0001     | -750.00  |
THEN three separate TCATBAL records exist:
  | Key               | Balance  |
  | 12345678901PU0001 | 1000.00  |
  | 12345678901CA0001 | 500.00   |
  | 12345678901PA0001 | -750.00  |
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — institutions must maintain accurate and detailed records of all financial transactions | The category balance system maintains running totals by transaction type and category, supporting detailed accounting breakdowns |
| FSA FFFS 2014:5 | Ch. 7 | Financial systems — IT systems must support reliable and timely financial reporting | The categorized balances enable regulatory reporting by transaction type (purchases, cash advances, repayments) |
| PSD2 | Art. 64 | Transaction data integrity — payment service providers must ensure integrity of transaction data | Each transaction updates the category balance atomically, maintaining data integrity across the double-entry system |
| DORA | Art. 11 | ICT risk management — data integrity controls | The create-or-update pattern with file status checking ensures data integrity; any unexpected file error triggers an abend rather than silent corruption |

## Edge Cases

1. **Concurrent access to same key**: If two transactions for the same account/type/category are in the same batch, they are processed sequentially by the COBOL loop. Each READ gets the latest value. However, in the migrated system with parallel processing, concurrent updates to the same key must use optimistic locking or atomic increment operations.

2. **INITIALIZE clears balance to zero**: When creating a new TCATBAL record (line 504), INITIALIZE sets all fields to their default values. For the balance field (PIC S9), this means zero. The ADD on line 508 then sets it to the transaction amount. This is correct but the INITIALIZE + ADD pattern must be preserved in the migration.

3. **Category code interpretation**: The TRAN-CAT-CD is a 4-digit numeric code (PIC 9(04)). The meaning of specific category codes (e.g., 0001 = retail purchase, 0002 = online purchase) is not defined in the available COBOL source. A category code master table must be obtained from the mainframe team.

4. **Balance sign convention**: Category balances accumulate signed amounts. Purchase categories will have positive balances; repayment categories will have negative balances. The migrated system must preserve this sign convention for correct financial reconciliation.

5. **File full condition**: If the TCATBAL VSAM file reaches its allocated space and cannot extend, the WRITE for a new record would fail. The COBOL program abends in this case. The migrated Azure SQL database does not have this limitation but should monitor storage capacity.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) What are the valid transaction type codes (PU=purchase, CA=cash advance, PA=payment, etc.) and their meanings? (2) What are the valid category codes and is there a category master file? (3) How are category balances used in regulatory reporting — are they aggregated for FSA submissions? (4) Is the TCATBAL file reset at billing cycle close, or does it maintain cumulative balances across cycles? (5) Are there additional balance tracking dimensions beyond type and category?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
