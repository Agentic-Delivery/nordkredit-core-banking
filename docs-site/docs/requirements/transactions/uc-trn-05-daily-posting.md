---
title: "UC-TRN-05: Post Daily Transactions"
sidebar_label: "UC-TRN-05: Post Daily Transactions"
sidebar_position: 5
domain: "transactions"
cobol_source: "CBTRN02C.cbl"
business_rules:
  - "TRN-BR-005"
  - "TRN-BR-006"
  - "TRN-BR-007"
  - "TRN-BR-008"
  - "TRN-BR-009"
regulations:
  - "PSD2 Art. 64 — Transaction data integrity"
  - "PSD2 Art. 73 — Refund and credit rules"
  - "FFFS 2014:5 Ch. 3 — Accurate accounting records"
  - "FFFS 2014:5 Ch. 4 §3 — Credit risk management"
  - "FFFS 2014:5 Ch. 16 — Financial reporting accuracy"
  - "AML 2017:11 — Transaction monitoring"
  - "DORA Art. 11 — ICT risk management"
status: "documented"
---

# UC-TRN-05: Post Daily Transactions

## Use Case Overview

| Field | Value |
|-------|-------|
| **Use Case ID** | UC-TRN-05 |
| **Title** | Post Daily Transactions |
| **Primary Actor** | Batch Processing System |
| **Trigger** | Nightly batch schedule step 2 (after verification completes via CBTRN01C) |
| **Precondition** | DALYTRAN file available; XREF, ACCT, TCATBAL files accessible in I-O mode |
| **Postcondition** | Valid transactions posted to TRANSACT; balances updated; rejects written to DALYREJS |
| **Source Program** | `CBTRN02C.cbl` |
| **Priority** | Critical — core financial posting logic |

## Summary

The daily transaction posting batch job reads the DALYTRAN file (produced by upstream ingestion) and processes each transaction through a four-step validation pipeline. Valid transactions are posted to the TRANSACT master file with a processing timestamp, account balances are updated, and transaction category balances are maintained. Invalid transactions are written to the DALYREJS reject file with coded reasons. This is the most financially critical program in the batch pipeline — it performs the actual balance calculations that affect account standing.

This use case is step 2 of the three-step daily batch pipeline:

1. **CBTRN01C** — Verification (card-to-account cross-reference validation)
2. **CBTRN02C** — Posting (validation, balance updates, transaction recording) ← **this use case**
3. **CBTRN03C** — Reporting (daily transaction detail report generation)

## Main Success Scenario

```
1. Batch scheduler triggers CBTRN02C after CBTRN01C completes successfully
2. System opens DALYTRAN (input), XREF, ACCT, TCATBAL, TRANSACT, DALYREJS files
3. FOR EACH transaction in DALYTRAN:
   3a. Validate card number exists in XREF → obtain account ID
   3b. Validate account exists in ACCT master
   3c. Validate projected balance does not exceed credit limit
   3d. Validate transaction origination date is before account expiration
   3e. If all validations pass:
       i.   Update transaction category balance (TCATBAL) — upsert pattern
       ii.  Update account balance (ACCT-CURR-BAL) and cycle credit/debit totals
       iii. Write transaction to TRANSACT master with DB2-format timestamp
   3f. If any validation fails:
       i.   Write transaction to DALYREJS with reject code and description
       ii.  Increment reject counter
4. Close all files
5. Set return code: RC=0 (all posted) or RC=4 (some rejected)
6. Display processed and rejected counts
```

## Extension Scenarios

| Extension | Trigger | Behavior |
|-----------|---------|----------|
| 3a-reject | Card not in XREF | Reject with code 100: "INVALID CARD NUMBER FOUND" |
| 3b-reject | Account not in ACCT | Reject with code 101: "ACCOUNT RECORD NOT FOUND" |
| 3c-reject | Projected balance exceeds credit limit | Reject with code 102: "OVERLIMIT TRANSACTION" |
| 3d-reject | Transaction after account expiration | Reject with code 103: "TRANSACTION RECEIVED AFTER ACCT EXPIRATION" |
| File-error | Any file open/read/write failure | ABEND with code 999 |

**Validation order note:** Checks 3c and 3d are sequential within the same code block. If both the credit limit and expiration checks fail, the expiration check (code 103) overwrites the credit limit rejection (code 102). The last failing validation "wins." The migrated system should either fail on the first check or collect all failures.

---

## User Stories

### US-TRN-05.1: Validate card and account existence

> As the **batch system**, I want to validate each transaction's card number in XREF and linked account in ACCT master, so that only transactions with valid references are posted.

**Acceptance Criteria:**

```gherkin
Scenario: Valid card with existing account
  Given a daily transaction with card number "4000000000000001"
    And the card exists in the XREF file linked to account "00000000001"
    And account "00000000001" exists in the ACCOUNT file
  When the batch program validates this transaction
  Then validation passes with fail-reason = 0
    And the transaction proceeds to credit limit checking

Scenario: Card number not in cross-reference
  Given a daily transaction with card number "9999999999999999"
    And this card number does not exist in the XREF file
  When the batch program validates this transaction
  Then the transaction is rejected with code 100 "INVALID CARD NUMBER FOUND"
    And the account lookup is skipped
    And the transaction is written to DALYREJS

Scenario: Card valid but account missing
  Given a daily transaction with a valid card in XREF linked to account "00000000099"
    And account "00000000099" does not exist in the ACCOUNT file
  When the batch program validates this transaction
  Then the transaction is rejected with code 101 "ACCOUNT RECORD NOT FOUND"
    And the transaction is written to DALYREJS
```

**Source:** [TRN-BR-005](../../business-rules/transactions/trn-br-005) (CBTRN02C.cbl:370-395), [TRN-BR-006](../../business-rules/transactions/trn-br-006) (CBTRN02C.cbl:380-399)

**Regulation:** PSD2 Art. 97 — Transaction authorization verification; AML/KYC — Transaction source verification

---

### US-TRN-05.2: Validate credit limit

> As the **batch system**, I want to reject transactions where the projected balance exceeds the account credit limit, so that overlimit transactions are prevented.

**Credit limit calculation:**

```
projected_balance = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT

IF ACCT-CREDIT-LIMIT >= projected_balance → APPROVED
ELSE → REJECTED (code 102)
```

**Acceptance Criteria:**

```gherkin
Scenario: Transaction within credit limit
  Given an account with credit limit 10000.00
    And current cycle credits of 5000.00
    And current cycle debits of 3000.00
    And a transaction amount of +1500.00
  When the transaction is validated
  Then projected balance = 5000.00 - 3000.00 + 1500.00 = 3500.00
    And 10000.00 >= 3500.00 is TRUE
    And the transaction passes credit limit validation

Scenario: Transaction exceeds credit limit
  Given an account with credit limit 5000.00
    And current cycle credits of 4000.00
    And current cycle debits of 200.00
    And a transaction amount of +1500.00
  When the transaction is validated
  Then projected balance = 4000.00 - 200.00 + 1500.00 = 5300.00
    And 5000.00 >= 5300.00 is FALSE
    And the transaction is rejected with code 102 "OVERLIMIT TRANSACTION"

Scenario: Negative amount (credit/payment) reduces projected balance
  Given an account with credit limit 5000.00
    And current cycle credits of 4800.00
    And current cycle debits of 100.00
    And a transaction amount of -500.00
  When the transaction is validated
  Then projected balance = 4800.00 - 100.00 + (-500.00) = 4200.00
    And 5000.00 >= 4200.00 is TRUE
    And the transaction passes credit limit validation
```

**Source:** [TRN-BR-006](../../business-rules/transactions/trn-br-006) (CBTRN02C.cbl:403-413)

**Regulation:** FFFS 2014:5 Ch. 4 §3 — Credit risk management; EBA Guidelines on creditworthiness assessment

---

### US-TRN-05.3: Validate account expiration

> As the **batch system**, I want to reject transactions where the origination date is after the account expiration date, so that expired accounts cannot be used.

**Acceptance Criteria:**

```gherkin
Scenario: Transaction before account expiration
  Given an account with expiration date "2027-06-30"
    And a transaction with origination date "2026-02-15"
  When the transaction is validated
  Then "2027-06-30" >= "2026-02-15" is TRUE
    And the transaction passes expiration validation

Scenario: Transaction after account expiration
  Given an account with expiration date "2025-12-31"
    And a transaction with origination timestamp starting with "2026-01-15"
  When the transaction is validated
  Then "2025-12-31" >= "2026-01-15" is FALSE
    And the transaction is rejected with code 103
    And the reject description is "TRANSACTION RECEIVED AFTER ACCT EXPIRATION"

Scenario: Expiration check overwrites credit limit failure
  Given a transaction that fails both credit limit (code 102) and expiration (code 103)
  When the validation pipeline runs
  Then the reject code is 103 (last check wins)
    And the credit limit failure is not recorded
```

**Source:** [TRN-BR-006](../../business-rules/transactions/trn-br-006) (CBTRN02C.cbl:414-420)

**Regulation:** PSD2 Art. 64 — Transaction data integrity

**Migration note:** The COBOL compares `ACCT-EXPIRAION-DATE` (note original typo in field name) with `DALYTRAN-ORIG-TS(1:10)` as string comparison. Both must be in YYYY-MM-DD format. The migrated system should use proper `DateOnly` or `DateTime` comparison.

---

### US-TRN-05.4: Post valid transactions

> As the **batch system**, I want to write validated transactions to the TRANSACT file with a processing timestamp, so that posted transactions are permanently recorded.

**Processing timestamp format:** `YYYY-MM-DD-HH.MM.SS.HH0000` (DB2 format)

Example: `2026-02-15-14.30.45.120000`

**Acceptance Criteria:**

```gherkin
Scenario: Successful transaction posting
  Given a transaction that has passed all validation checks
  When the transaction is posted
  Then the transaction record is written to the TRANSACT master file
    And the following fields are populated from the daily transaction:
      | Field | Source |
      | TRAN-ID | DALYTRAN-ID |
      | TRAN-TYPE-CD | DALYTRAN-TYPE-CD |
      | TRAN-CAT-CD | DALYTRAN-CAT-CD |
      | TRAN-SOURCE | DALYTRAN-SOURCE |
      | TRAN-DESC | DALYTRAN-DESC |
      | TRAN-AMT | DALYTRAN-AMT |
      | TRAN-CARD-NUM | DALYTRAN-CARD-NUM |
      | TRAN-ORIG-TS | DALYTRAN-ORIG-TS |
    And TRAN-PROC-TS contains the current timestamp in DB2 format

Scenario: Processing timestamp generation
  Given a transaction is being posted at 2026-02-15 14:30:45.12
  When the processing timestamp is generated
  Then TRAN-PROC-TS contains "2026-02-15-14.30.45.120000"
    And the last 4 digits are always "0000" (COBOL limitation)

Scenario: Transaction file write error
  Given a validated transaction is ready to post
    And the TRANSACT file write fails
  When the write is attempted
  Then APPL-RESULT is set to 12
    And the program abends with code 999
```

**Source:** [TRN-BR-007](../../business-rules/transactions/trn-br-007) (CBTRN02C.cbl:424-442, 562-570, 692-703)

**Regulation:** PSD2 Art. 94 — Transaction record retention

**Migration note:** The migrated system should use `DateTime.UtcNow` with full microsecond precision instead of the COBOL `CURRENT-DATE` function which only provides centisecond precision.

---

### US-TRN-05.5: Update account balances

> As the **batch system**, I want to update ACCT-CURR-BAL and classify amounts into cycle credit or debit, so that account balances remain accurate after posting.

**Balance update logic:**

```
ACCT-CURR-BAL = ACCT-CURR-BAL + DALYTRAN-AMT

IF DALYTRAN-AMT >= 0:
    ACCT-CURR-CYC-CREDIT = ACCT-CURR-CYC-CREDIT + DALYTRAN-AMT
ELSE:
    ACCT-CURR-CYC-DEBIT = ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT
```

**Acceptance Criteria:**

```gherkin
Scenario: Post a charge (positive amount)
  Given an account with current balance 1000.00
    And current cycle credit 500.00
    And current cycle debit 200.00
    And a validated transaction amount of +150.00
  When the transaction is posted
  Then ACCT-CURR-BAL becomes 1150.00 (1000.00 + 150.00)
    And ACCT-CURR-CYC-CREDIT becomes 650.00 (500.00 + 150.00)
    And ACCT-CURR-CYC-DEBIT remains 200.00

Scenario: Post a payment/credit (negative amount)
  Given an account with current balance 5000.00
    And current cycle credit 4000.00
    And current cycle debit 1000.00
    And a validated transaction amount of -500.00
  When the transaction is posted
  Then ACCT-CURR-BAL becomes 4500.00 (5000.00 + (-500.00))
    And ACCT-CURR-CYC-CREDIT remains 4000.00
    And ACCT-CURR-CYC-DEBIT becomes 500.00 (1000.00 + (-500.00))

Scenario: Zero-amount transaction
  Given a validated transaction with amount 0.00
  When the transaction is posted
  Then ACCT-CURR-BAL is unchanged
    And ACCT-CURR-CYC-CREDIT is unchanged (0.00 >= 0 adds zero to credit)
    And ACCT-CURR-CYC-DEBIT is unchanged
```

**Source:** [TRN-BR-007](../../business-rules/transactions/trn-br-007) (CBTRN02C.cbl:545-559)

**Regulation:** FFFS 2014:5 Ch. 3 — Accurate accounting records; FFFS 2014:5 Ch. 16 — Financial reporting accuracy

---

### US-TRN-05.6: Update transaction category balances

> As the **batch system**, I want to update or create TCATBAL records (key: ACCT-ID + TYPE-CD + CAT-CD), so that category-level balance tracking is maintained.

**Composite key structure:**

| Field | PIC | Source | Example |
|-------|-----|--------|---------|
| TRANCAT-ACCT-ID | 9(11) | XREF-ACCT-ID | 00000000042 |
| TRANCAT-TYPE-CD | X(02) | DALYTRAN-TYPE-CD | 01 |
| TRANCAT-CD | 9(04) | DALYTRAN-CAT-CD | 0001 |

**Upsert pattern:**

```
READ TCATBAL by composite key
IF record not found:
    CREATE new record with initial balance = transaction amount
ELSE:
    ADD transaction amount to existing TRAN-CAT-BAL
    REWRITE record
```

**Acceptance Criteria:**

```gherkin
Scenario: Create new category balance record
  Given account "00000000042" has no existing TCATBAL record for type "01" category "0001"
    And a transaction of +250.00 is being posted
  When the category balance is updated
  Then a new TCATBAL record is created with key "00000000042|01|0001"
    And TRAN-CAT-BAL is set to 250.00

Scenario: Update existing category balance
  Given account "00000000042" has an existing TCATBAL record for type "01" category "0001"
    And the existing TRAN-CAT-BAL is 1000.00
    And a transaction of +150.00 is being posted
  When the category balance is updated
  Then TRAN-CAT-BAL becomes 1150.00 (1000.00 + 150.00)

Scenario: Credit reduces category balance
  Given an existing TCATBAL record with balance 500.00
    And a transaction of -200.00 (credit/return) is being posted
  When the category balance is updated
  Then TRAN-CAT-BAL becomes 300.00 (500.00 + (-200.00))
```

**Source:** [TRN-BR-008](../../business-rules/transactions/trn-br-008) (CBTRN02C.cbl:467-528)

**Regulation:** FFFS 2014:5 Ch. 3 — Accurate accounting records; FFFS 2014:5 Ch. 16 — Financial reporting by category

---

### US-TRN-05.7: Write rejected transactions

> As the **batch system**, I want rejected transactions written to DALYREJS with the full record plus reason code and description, so that rejects are available for morning review.

**Reject record structure:**

| Component | Size | Content |
|-----------|------|---------|
| Original transaction | 350 bytes | Full DALYTRAN record |
| Reject code | 4 bytes | PIC 9(04) — codes 100-103 |
| Reject description | 76 bytes | PIC X(76) — human-readable description |
| **Total** | **430 bytes** | |

**Rejection codes:**

| Code | Description |
|------|-------------|
| 100 | INVALID CARD NUMBER FOUND |
| 101 | ACCOUNT RECORD NOT FOUND |
| 102 | OVERLIMIT TRANSACTION |
| 103 | TRANSACTION RECEIVED AFTER ACCT EXPIRATION |

**Acceptance Criteria:**

```gherkin
Scenario: Write rejected transaction to DALYREJS
  Given a transaction fails validation with code 102 "OVERLIMIT TRANSACTION"
  When the rejection is recorded
  Then the full 350-byte daily transaction record is written to DALYREJS
    And the 4-byte reject reason code is appended (0102)
    And the 76-byte reject description is appended ("OVERLIMIT TRANSACTION")
    And the reject counter is incremented

Scenario: Multiple rejections in a batch run
  Given 100 transactions are processed
    And 3 fail validation (codes 100, 102, 103)
  When the batch completes
  Then DALYREJS contains exactly 3 reject records
    And each record has the correct reject code and description
```

**Source:** [TRN-BR-006](../../business-rules/transactions/trn-br-006) (CBTRN02C.cbl, reject write logic)

**Regulation:** FFFS 2014:5 Ch. 4 §3 — Operational risk management; AML 2017:11 — Transaction monitoring

---

### US-TRN-05.8: Report batch results

> As the **batch system**, I want to return RC=0 (all posted) or RC=4 (some rejected) and display counts, so that downstream steps and operations can assess the batch outcome.

**Acceptance Criteria:**

```gherkin
Scenario: All transactions posted successfully
  Given 500 transactions are processed
    And all pass validation
  When the batch completes
  Then RETURN-CODE is set to 0
    And the display shows "500 processed, 0 rejected"

Scenario: Some transactions rejected
  Given 500 transactions are processed
    And 15 fail validation
  When the batch completes
  Then RETURN-CODE is set to 4
    And the display shows "500 processed, 15 rejected"
    And downstream JCL steps can check RC=4 to trigger reject review
```

**Source:** [TRN-BR-006](../../business-rules/transactions/trn-br-006) (CBTRN02C.cbl, return code logic)

**Regulation:** DORA Art. 11 — ICT risk management (operational monitoring and alerting)

---

## Critical Financial Precision Rules

All financial calculations in this use case **must** use fixed-point decimal types. Floating-point types (`float`, `double`) are **NOT acceptable** for financial calculations.

| Field | COBOL PIC | .NET Equivalent | Max Value | Notes |
|-------|-----------|-----------------|-----------|-------|
| DALYTRAN-AMT | S9(09)V99 | `decimal(11,2)` | ±999,999,999.99 | Transaction amount |
| ACCT-CURR-BAL | S9(10)V99 | `decimal(12,2)` | ±9,999,999,999.99 | Running account balance |
| ACCT-CURR-CYC-CREDIT | S9(10)V99 | `decimal(12,2)` | ±9,999,999,999.99 | Cycle credits |
| ACCT-CURR-CYC-DEBIT | S9(10)V99 | `decimal(12,2)` | ±9,999,999,999.99 | Cycle debits |
| ACCT-CREDIT-LIMIT | S9(10)V99 | `decimal(12,2)` | ±9,999,999,999.99 | Credit limit |
| WS-TEMP-BAL | S9(09)V99 | `decimal(11,2)` | ±999,999,999.99 | Projected balance (truncation risk!) |
| TRAN-CAT-BAL | S9(09)V99 | `decimal(11,2)` | ±999,999,999.99 | Category balance |

**Truncation risk:** `WS-TEMP-BAL` uses `S9(09)V99` (max ±999,999,999.99) while account balance fields use `S9(10)V99` (max ±9,999,999,999.99). The projected balance calculation could theoretically overflow if cycle credits/debits exceed the temp field's capacity. The migrated system should use `decimal(12,2)` for the projected balance calculation.

---

## Regulatory Traceability Matrix

| Regulation | Article/Section | Requirement | User Story | How Satisfied |
|------------|----------------|-------------|------------|---------------|
| PSD2 | Art. 64 | Transaction data integrity | US-TRN-05.3, US-TRN-05.4 | Validation pipeline ensures only valid transactions are posted; rejected transactions are recorded with reasons |
| PSD2 | Art. 73 | Refund and credit rules | US-TRN-05.5 | Credit/debit classification correctly handles negative amounts (payments/refunds) |
| PSD2 | Art. 94 | Transaction record retention | US-TRN-05.4 | Posted transactions are written to TRANSACT master with processing timestamp |
| PSD2 | Art. 97 | Transaction authorization | US-TRN-05.1 | Card-to-account cross-reference validates payment instrument |
| FFFS 2014:5 | Ch. 3 | Accurate accounting records | US-TRN-05.5, US-TRN-05.6 | Balance updates and category balance tracking ensure accurate records |
| FFFS 2014:5 | Ch. 4 §3 | Credit risk management | US-TRN-05.2 | Credit limit validation prevents unauthorized credit extension |
| FFFS 2014:5 | Ch. 16 | Financial reporting accuracy | US-TRN-05.5, US-TRN-05.6 | Cycle-level and category-level balance tracking supports reporting |
| AML 2017:11 | Transaction monitoring | Monitoring for suspicious activity | US-TRN-05.1, US-TRN-05.7 | Transaction validation and reject recording enable monitoring |
| DORA | Art. 11 | ICT risk management | US-TRN-05.8 | Return codes and counts enable operational monitoring and alerting |
| EBA Guidelines | Creditworthiness | Ongoing credit exposure monitoring | US-TRN-05.2 | Real-time balance calculation against credit limit |

---

## Business Rules Traceability

| User Story | Business Rules | COBOL Source |
|------------|---------------|--------------|
| US-TRN-05.1 | [TRN-BR-005](../../business-rules/transactions/trn-br-005), [TRN-BR-006](../../business-rules/transactions/trn-br-006) | CBTRN02C.cbl:370-399 |
| US-TRN-05.2 | [TRN-BR-006](../../business-rules/transactions/trn-br-006) | CBTRN02C.cbl:403-413 |
| US-TRN-05.3 | [TRN-BR-006](../../business-rules/transactions/trn-br-006) | CBTRN02C.cbl:414-420 |
| US-TRN-05.4 | [TRN-BR-007](../../business-rules/transactions/trn-br-007) | CBTRN02C.cbl:424-442, 562-570, 692-703 |
| US-TRN-05.5 | [TRN-BR-007](../../business-rules/transactions/trn-br-007) | CBTRN02C.cbl:545-559 |
| US-TRN-05.6 | [TRN-BR-008](../../business-rules/transactions/trn-br-008) | CBTRN02C.cbl:467-528 |
| US-TRN-05.7 | [TRN-BR-006](../../business-rules/transactions/trn-br-006) | CBTRN02C.cbl (reject write) |
| US-TRN-05.8 | [TRN-BR-009](../../business-rules/transactions/trn-br-009) | CBTRN02C.cbl (return code) |

---

## Migration Considerations

### Atomicity

The COBOL program performs three posting steps sequentially (update TCATBAL, update ACCT, write TRANSACT) without a transaction scope. If the program abends after updating the account but before writing the transaction, the account balance will be incorrect with no transaction record. The migrated system **must** use database transactions to ensure atomicity of the entire posting operation.

### Validation order

The COBOL code checks credit limit and expiration sequentially — if both fail, expiration (code 103) overwrites credit limit (code 102). The migrated system should:
- Either fail on the **first** validation failure (fail-fast), or
- Collect **all** failures and return them together

Both approaches differ from the COBOL behavior and should be documented as an intentional improvement.

### Date comparison

The expiration check compares date strings (`ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS(1:10)`). The migrated system should use proper `DateOnly` comparison. Note the typo in the original COBOL field name (`EXPIRAION` instead of `EXPIRATION`).

### Timestamp precision

The COBOL `CURRENT-DATE` function provides centisecond precision with 4 hardcoded trailing zeros (`HH0000`). The migrated system should use `DateTime.UtcNow` or `DateTimeOffset.UtcNow` for full microsecond/tick precision.

### Error handling

The COBOL program ABENDs (crashes) on file I/O errors. The migrated system should implement graceful error handling with:
- Retry logic for transient failures
- Structured logging (Application Insights)
- Dead-letter queue for permanently failed transactions
- Alerting on batch failures

### Concurrency

The batch processes transactions sequentially. If multiple batch instances run concurrently, VSAM REWRITE without locking could cause lost updates. The migrated system should use proper database concurrency control (optimistic concurrency with row versioning or pessimistic locking).

---

## Domain Expert Validation

:::caution Awaiting Validation
This use case document is awaiting validation by a domain expert (retiring COBOL developer). Key questions requiring sign-off:

1. Must the three posting steps (TCATBAL update, account update, transaction write) be atomic (all-or-nothing)?
2. What is the recovery procedure if the program abends mid-posting?
3. Should the migrated system fail on the first validation failure or collect all failures?
4. Is the debit/credit sign convention documented elsewhere? (Positive = charge to card, Negative = payment received)
5. Should `ACCT-CURR-BAL` ever go negative, and what are the business implications?
6. Are there additional validation rules that should be added (e.g., velocity checks, transaction limits)?
7. What is the business process for handling rejected transactions in the DALYREJS file?
:::

---

**Template version:** 1.0
**Last updated:** 2026-02-15
