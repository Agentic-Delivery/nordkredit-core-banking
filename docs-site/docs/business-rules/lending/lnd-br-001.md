---
id: "lnd-br-001"
title: "Loan account data structure and field definitions"
domain: "lending"
cobol_source: "CVACT01Y.cpy:1-30 (credit limit fields referenced across programs)"
requirement_id: "LND-BR-001"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "GDPR Art. 5(1)(c)"
  - "GDPR Art. 5(1)(d)"
  - "EU Consumer Credit Directive 2008/48/EC Art. 10"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# LND-BR-001: Loan account data structure and field definitions

## Summary

The lending domain relies on the account master record (CVACT01Y copybook) which contains credit-related fields that form the foundation for lending operations. The account record includes credit limit fields (`ACCT-CREDIT-LIMIT`, `ACCT-CASH-CREDIT-LIMIT`), balance tracking fields (`ACCT-CURR-BAL`, `ACCT-CURR-CYC-CREDIT`, `ACCT-CURR-CYC-DEBIT`), and a disclosure group field (`ACCT-GROUP-ID`) that links to interest rate schedules. These fields are referenced across multiple COBOL programs for credit risk management, transaction validation, and balance maintenance.

The loan account structure must support the full lending lifecycle: origination (setting initial credit limits), utilization tracking (current balance vs. credit limit), repayment processing (cycle debit accumulation), and interest calculation (via disclosure group linkage). Dedicated lending COBOL programs for loan origination, servicing, and collections are not yet available in the repository and must be obtained from the mainframe team.

## Business Logic

### Data Structure

```
LOAN-ACCOUNT-FIELDS (extracted from CVACT01Y.cpy, 300-byte account record):
    ACCT-ID                   PIC 9(11)      -- Primary key, 11-digit account number
    ACCT-ACTIVE-STATUS        PIC X(01)      -- 'Y' = active, 'N' = inactive
    ACCT-CURR-BAL             PIC S9(10)V99  -- Current outstanding balance (signed)
    ACCT-CREDIT-LIMIT         PIC S9(10)V99  -- Maximum authorized credit (purchase limit)
    ACCT-CASH-CREDIT-LIMIT    PIC S9(10)V99  -- Cash advance credit limit (separate ceiling)
    ACCT-CURR-CYC-CREDIT      PIC S9(10)V99  -- Current cycle charges (utilization)
    ACCT-CURR-CYC-DEBIT       PIC S9(10)V99  -- Current cycle payments (repayments)
    ACCT-EXPIRAION-DATE       PIC X(10)      -- Account/loan maturity date (YYYY-MM-DD)
    ACCT-GROUP-ID             PIC X(10)      -- Disclosure/interest rate group assignment

DERIVED LENDING METRICS:
    Available Credit = ACCT-CREDIT-LIMIT - (ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT)
    Utilization Rate = ACCT-CURR-BAL / ACCT-CREDIT-LIMIT × 100
    Cash Advance Available = ACCT-CASH-CREDIT-LIMIT - (cash advance portion of balance)
```

### Decision Table

| Field | Lending Function | Used In | Regulatory Relevance |
|-------|-----------------|---------|---------------------|
| ACCT-CREDIT-LIMIT | Maximum borrowing capacity | CBTRN02C.cbl:407 | FSA FFFS 2014:5 Ch. 6 (credit risk) |
| ACCT-CASH-CREDIT-LIMIT | Cash advance ceiling | Account master | Consumer Credit Directive Art. 10 |
| ACCT-CURR-BAL | Outstanding debt | CBTRN02C.cbl:547 | FSA FFFS 2014:5 Ch. 3 (accounting) |
| ACCT-CURR-CYC-CREDIT | Period utilization | CBTRN02C.cbl:403-404,549 | PSD2 Art. 64 (transaction integrity) |
| ACCT-CURR-CYC-DEBIT | Period repayments | CBTRN02C.cbl:404,551 | PSD2 Art. 64 (transaction integrity) |
| ACCT-GROUP-ID | Interest rate group | Account master | Consumer Credit Directive Art. 10 |
| ACCT-EXPIRAION-DATE | Loan maturity | CBTRN02C.cbl:414 | FSA FFFS 2014:5 Ch. 6 |

## Source COBOL Reference

**Copybook:** `CVACT01Y.cpy` (referenced but not fully available in repository — reconstructed from program references)
**Programs referencing credit fields:** `CBTRN02C.cbl`, `COCRDUPC.cbl`

```cobol
000403               COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT
000404                                   - ACCT-CURR-CYC-DEBIT
000405                                   + DALYTRAN-AMT
000407               IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
```
*(Lines 403-407, CBTRN02C.cbl — credit limit comparison for overlimit detection, demonstrating ACCT-CREDIT-LIMIT, ACCT-CURR-CYC-CREDIT, ACCT-CURR-CYC-DEBIT fields)*

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```
*(Lines 547-552, CBTRN02C.cbl — balance update showing credit/debit classification for lending cycle tracking)*

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
```
*(Line 414, CBTRN02C.cbl — account/loan expiration enforcement)*

## Acceptance Criteria

### Scenario 1: Loan account record contains all required lending fields

```gherkin
GIVEN a loan account record is created in the migrated system
WHEN the record is persisted to Azure SQL
THEN all lending fields are present:
  | Field              | SQL Type       | Nullable |
  | AccountId          | CHAR(11)       | NO       |
  | ActiveStatus       | CHAR(1)        | NO       |
  | CurrentBalance     | DECIMAL(12,2)  | NO       |
  | CreditLimit        | DECIMAL(12,2)  | NO       |
  | CashCreditLimit    | DECIMAL(12,2)  | NO       |
  | CycleCredit        | DECIMAL(12,2)  | NO       |
  | CycleDebit         | DECIMAL(12,2)  | NO       |
  | ExpirationDate     | DATE           | NO       |
  | DisclosureGroupId  | VARCHAR(10)    | YES      |
```

### Scenario 2: Financial field precision preservation

```gherkin
GIVEN a loan account with CreditLimit = 9999999999.99
WHEN the value is stored and retrieved from Azure SQL
THEN the value is exactly 9999999999.99
  AND no floating-point precision loss occurs
  AND the SQL type is DECIMAL(12,2)
```

### Scenario 3: Dual credit limit structure

```gherkin
GIVEN a loan account with:
  | CreditLimit     | 50000.00 |
  | CashCreditLimit | 10000.00 |
WHEN the available credit is calculated
THEN the purchase credit available is CreditLimit minus outstanding purchase balance
  AND the cash advance credit available is CashCreditLimit minus outstanding cash advance balance
  AND the two limits are enforced independently
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — institutions must maintain adequate records of credit exposures | The loan account record tracks current balance, credit limits, and cycle utilization providing a complete view of credit exposure per account |
| GDPR | Art. 5(1)(c) | Data minimization — personal data limited to what is necessary | The loan account contains only financial and status fields; customer PII is stored separately, not in the account master record |
| GDPR | Art. 5(1)(d) | Accuracy — data must be kept accurate and up to date | Balance fields are updated atomically during transaction posting (CBTRN02C.cbl:547-552), maintaining accuracy through structured credit/debit classification |
| Consumer Credit Directive | Art. 10 | Information to be included in credit agreements — total amount of credit, borrowing rate, conditions governing application of rate | The credit limit, cash credit limit, and disclosure group ID fields provide the data foundation for credit agreement terms |

## Edge Cases

1. **EBCDIC numeric encoding**: COBOL PIC S9(10)V99 fields use packed decimal format in EBCDIC. The implied decimal point (V99) must be correctly interpreted during migration. Incorrect conversion would cause all balances to be off by a factor of 100.

2. **Negative balances**: All financial fields use signed notation (S prefix). A negative current balance represents an overpayment or credit to the account. The migrated system must allow negative values in balance columns.

3. **Dual credit limit interaction**: The relationship between ACCT-CREDIT-LIMIT and ACCT-CASH-CREDIT-LIMIT is not explicitly defined in available COBOL source. The mainframe team must clarify whether cash advance limit is a subset of the total credit limit or an independent ceiling.

4. **Disclosure group linkage**: The ACCT-GROUP-ID field links to interest rate schedules, but the disclosure group master file and its COBOL programs are not yet available. This linkage is critical for interest calculation rules (LND-BR-004).

5. **Account expiration vs. loan maturity**: The ACCT-EXPIRAION-DATE field is used for both card expiration (in card programs) and potentially loan maturity (in lending context). The mainframe team must confirm whether separate maturity date fields exist for term loans.

6. **Filler area contents**: The 300-byte account record likely contains additional lending-relevant fields not yet identified (origination date, last payment date, minimum payment amount, delinquency counters). A full dump of the CVACT01Y copybook is needed.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The full CVACT01Y.cpy copybook must be obtained from the mainframe team to validate the reconstructed record layout and identify additional lending-specific fields. Key questions: (1) Does the account record contain loan origination date, term length, and maturity date? (2) Is the disclosure group ID the sole linkage to interest rate schedules? (3) Are there separate account types distinguishing revolving credit from term loans? (4) What fields in the filler area relate to delinquency tracking or minimum payment calculations?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
