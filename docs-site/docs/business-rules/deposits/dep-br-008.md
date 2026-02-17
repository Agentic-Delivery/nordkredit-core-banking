---
id: "dep-br-008"
title: "Deposit transaction categorization and balance tracking"
domain: "deposits"
cobol_source: "CBTRN02C.cbl:467-542"
requirement_id: "DEP-BR-008"
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

# DEP-BR-008: Deposit transaction categorization and balance tracking

## Summary

The transaction categorization system maintains aggregate balances by transaction type and category for each account. During daily batch posting (CBTRN02C), after each transaction is validated and posted, the system updates the transaction category balance file (TCATBAL). The category balance record is keyed by account ID, transaction type code, and category code, enabling the bank to track deposit inflows and withdrawal outflows by classification. This supports regulatory reporting, product analytics, and customer statement summaries.

For deposit accounts, transaction categories distinguish between different deposit sources (salary transfers, cash deposits, interest postings) and withdrawal types (ATM, transfer, direct debit). This categorization is directly extracted from CBTRN02C.cbl paragraph 2700-UPDATE-TCATBAL.

## Business Logic

### Pseudocode

```
PERFORM UPDATE-CATEGORY-BALANCE (2700-UPDATE-TCATBAL):
    -- Step 1: Construct category key
    MOVE account-id TO FD-TRANCAT-ACCT-ID
    MOVE transaction-type-code TO FD-TRANCAT-TYPE-CD
    MOVE transaction-category-code TO FD-TRANCAT-CD

    -- Step 2: Attempt to read existing category balance
    READ TCATBAL-FILE using composite key
    IF record not found (INVALID KEY, status '23')
        SET create-flag = 'Y'
    END-IF

    -- Step 3: Create or update category balance
    IF create-flag = 'Y'
        -- New category: initialize and write
        INITIALIZE TRAN-CAT-BAL-RECORD
        MOVE account-id TO TRANCAT-ACCT-ID
        MOVE type-code TO TRANCAT-TYPE-CD
        MOVE category-code TO TRANCAT-CD
        ADD transaction-amount TO TRAN-CAT-BAL
        WRITE TRAN-CAT-BAL-RECORD
    ELSE
        -- Existing category: update balance
        ADD transaction-amount TO TRAN-CAT-BAL
        REWRITE TRAN-CAT-BAL-RECORD
    END-IF
```

### Category Key Structure

```
TRAN-CAT-BAL-RECORD (from CVTRA01Y.cpy, 50-byte VSAM record):
    TRANCAT-ACCT-ID       PIC 9(11)    -- Account identifier
    TRANCAT-TYPE-CD       PIC X(02)    -- Transaction type (e.g., 'CR'=credit, 'DB'=debit)
    TRANCAT-CD            PIC 9(04)    -- Category code (e.g., 1001=salary, 2001=ATM)
    TRAN-CAT-BAL          PIC S9(10)V99 -- Running category balance
```

### Decision Table

| Category Record Exists | Action | Outcome |
|----------------------|--------|---------|
| Yes (status '00') | REWRITE with updated balance | Existing category balance updated |
| No (status '23') | WRITE new record | New category balance record created |
| Error (other status) | ABEND | Batch terminates with file error |

## Source COBOL Reference

**Program:** `CBTRN02C.cbl`
**Lines:** 467-542 (category balance update)

```cobol
000467       2700-UPDATE-TCATBAL.
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
*(Lines 467-479 — category key construction and read attempt. The composite key (account + type + category) is built from the transaction data and the account cross-reference.)*

```cobol
000503       2700-A-CREATE-TCATBAL-REC.
000504           INITIALIZE TRAN-CAT-BAL-RECORD
000505           MOVE XREF-ACCT-ID TO TRANCAT-ACCT-ID
000506           MOVE DALYTRAN-TYPE-CD TO TRANCAT-TYPE-CD
000507           MOVE DALYTRAN-CAT-CD TO TRANCAT-CD
000508           ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000509
000510           WRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
```
*(Lines 503-510 — new category record creation. When no existing record matches the key, a new record is initialized and written with the transaction amount as the initial balance.)*

```cobol
000526       2700-B-UPDATE-TCATBAL-REC.
000527           ADD DALYTRAN-AMT TO TRAN-CAT-BAL
000528           REWRITE FD-TRAN-CAT-BAL-RECORD FROM TRAN-CAT-BAL-RECORD
```
*(Lines 526-528 — existing category balance update. The transaction amount is added to the running category balance and the record is rewritten.)*

## Acceptance Criteria

### Scenario 1: New deposit category record creation

```gherkin
GIVEN a deposit account with no existing category balance for type "CR" category 1001
WHEN a salary deposit of 45000.00 is posted with type "CR" and category 1001
THEN a new TCATBAL record is created:
  | AccountId | (account) |
  | TypeCode  | CR        |
  | Category  | 1001      |
  | Balance   | 45000.00  |
```

### Scenario 2: Existing deposit category balance update

```gherkin
GIVEN a deposit account with existing TCATBAL record:
  | TypeCode | CR    |
  | Category | 1001  |
  | Balance  | 90000.00 |
WHEN another salary deposit of 45000.00 is posted with type "CR" and category 1001
THEN the TCATBAL record is updated:
  | Balance | 135000.00 |
```

### Scenario 3: Withdrawal category tracking

```gherkin
GIVEN a deposit account with no existing category balance for type "DB" category 2001
WHEN an ATM withdrawal of -2000.00 is posted with type "DB" and category 2001
THEN a new TCATBAL record is created:
  | TypeCode | DB       |
  | Category | 2001     |
  | Balance  | -2000.00 |
```

### Scenario 4: Multiple categories for same account

```gherkin
GIVEN a deposit account with transactions in a day:
  | Type | Category | Amount     | Description    |
  | CR   | 1001     | 45000.00   | Salary         |
  | DB   | 2001     | -2000.00   | ATM withdrawal |
  | CR   | 1005     | 5000.00    | Transfer in    |
WHEN all transactions are posted during the daily batch
THEN three separate TCATBAL records are created or updated for the account
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — transactions must be classified and tracked by category for accurate financial reporting | The category balance system provides aggregate tracking by transaction type and category, enabling sub-ledger reporting for deposit inflows and outflows |
| FSA FFFS 2014:5 | Ch. 7 | Financial systems — systems must support categorized transaction reporting | Category-level balances enable regulatory report generation with transaction breakdowns by type |
| PSD2 | Art. 64 | Transaction data integrity — categorization must be consistent and verifiable | Each transaction's type and category codes are preserved in the category balance file, maintaining an audit trail of transaction classification |
| DORA | Art. 11 | ICT risk management — critical data must be reliably processed and stored | The create-or-update pattern with explicit error handling ensures reliable category balance maintenance during batch processing |

## Edge Cases

1. **Category code assignment**: The transaction's type code and category code come from the daily transaction input file (DALYTRAN). The mapping of deposit transactions to specific categories must be consistent. The mainframe team must provide the category code reference table.

2. **Category balance reset**: Unlike account cycle balances, category balances appear to accumulate indefinitely (no reset logic in CBTRN02C). The mainframe team must confirm whether category balances are ever reset (e.g., at year-end) or if they represent lifetime aggregates.

3. **TCATBAL file error handling**: If the WRITE (create) fails (lines 510-524), the batch ABENDs. If the REWRITE (update) fails (lines 528-542), the batch also ABENDs. There is no graceful recovery for category balance errors — the entire batch halts.

4. **Interest posting categorization**: When interest is posted to a deposit account (DEP-BR-004), it should be categorized as an interest transaction (e.g., type "CR", category for interest). The category codes for interest postings must be identified.

5. **Composite key collisions**: The 17-byte composite key (11 account + 2 type + 4 category) must be unique. Different transaction categories should have distinct type+category combinations.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) What are the specific transaction type codes used for deposit transactions (credit, debit, interest, fee)? (2) What are the category codes for common deposit transaction types (salary, transfer, ATM, interest posting)? (3) Are category balances ever reset, or do they accumulate for the lifetime of the account? (4) Is the TCATBAL file used for regulatory reporting, customer statements, or both? (5) How are category codes assigned to incoming transactions — is there a mapping table or is it embedded in the source system?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
