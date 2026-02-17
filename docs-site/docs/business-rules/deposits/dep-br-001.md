---
id: "dep-br-001"
title: "Deposit account data structure and field definitions"
domain: "deposits"
cobol_source: "CVACT01Y.cpy:1-30 (account master fields referenced across programs)"
requirement_id: "DEP-BR-001"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "GDPR Art. 5(1)(c)"
  - "GDPR Art. 5(1)(d)"
  - "EU Deposit Guarantee Directive 2014/49/EU"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# DEP-BR-001: Deposit account data structure and field definitions

## Summary

The deposits domain relies on the account master record (CVACT01Y copybook) which contains balance and configuration fields forming the foundation for deposit operations. The account record includes balance tracking fields (`ACCT-CURR-BAL`, `ACCT-CURR-CYC-CREDIT`, `ACCT-CURR-CYC-DEBIT`), an active status flag (`ACCT-ACTIVE-STATUS`), an expiration/maturity date (`ACCT-EXPIRAION-DATE`), and a disclosure group field (`ACCT-GROUP-ID`) that links to interest rate schedules. These fields are referenced across multiple COBOL programs for deposit balance management, interest calculation, and regulatory reporting.

The deposit account structure must support the full deposit lifecycle: account opening (KYC and initial deposit), balance maintenance (credits and debits), interest accrual (via disclosure group linkage to rate tiers), maturity processing (for term deposits), and statement generation. Dedicated deposit COBOL programs for account opening, interest calculation, and statement generation are not yet available in the repository and must be obtained from the mainframe team.

## Business Logic

### Data Structure

```
DEPOSIT-ACCOUNT-FIELDS (extracted from CVACT01Y.cpy, 300-byte account record):
    ACCT-ID                   PIC 9(11)      -- Primary key, 11-digit account number
    ACCT-ACTIVE-STATUS        PIC X(01)      -- 'Y' = active, 'N' = inactive/closed
    ACCT-CURR-BAL             PIC S9(10)V99  -- Current deposit balance (signed)
    ACCT-CREDIT-LIMIT         PIC S9(10)V99  -- Not used for pure deposit accounts (zero)
    ACCT-CASH-CREDIT-LIMIT    PIC S9(10)V99  -- Not used for pure deposit accounts (zero)
    ACCT-CURR-CYC-CREDIT      PIC S9(10)V99  -- Current cycle deposits (inflows)
    ACCT-CURR-CYC-DEBIT       PIC S9(10)V99  -- Current cycle withdrawals (outflows)
    ACCT-EXPIRAION-DATE       PIC X(10)      -- Maturity date for term deposits (YYYY-MM-DD)
    ACCT-GROUP-ID             PIC X(10)      -- Disclosure/interest rate group assignment

DERIVED DEPOSIT METRICS:
    Available Balance = ACCT-CURR-BAL (for demand deposits, no credit limit)
    Net Cycle Flow = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT
    Interest Accrual Base = ACCT-CURR-BAL (daily closing balance)
```

### Decision Table

| Field | Deposit Function | Used In | Regulatory Relevance |
|-------|-----------------|---------|---------------------|
| ACCT-ID | Unique deposit account identifier | All programs | FSA FFFS 2014:5 Ch. 3 (account identification) |
| ACCT-ACTIVE-STATUS | Account lifecycle state | CBTRN02C.cbl | FSA FFFS 2014:5 Ch. 3 (active account registry) |
| ACCT-CURR-BAL | Current deposit balance | CBTRN02C.cbl:547 | Deposit Guarantee Directive (coverage calculation) |
| ACCT-CURR-CYC-CREDIT | Period deposit inflows | CBTRN02C.cbl:549 | PSD2 Art. 64 (transaction integrity) |
| ACCT-CURR-CYC-DEBIT | Period withdrawal outflows | CBTRN02C.cbl:551 | PSD2 Art. 64 (transaction integrity) |
| ACCT-GROUP-ID | Interest rate group for deposit product | Account master | FSA FFFS 2014:5 Ch. 3, 6 (rate transparency) |
| ACCT-EXPIRAION-DATE | Term deposit maturity date | CBTRN02C.cbl:414 | FSA FFFS 2014:5 Ch. 3 (contractual terms) |

## Source COBOL Reference

**Copybook:** `CVACT01Y.cpy` (referenced but not fully available in repository — reconstructed from program references)
**Programs referencing deposit-relevant fields:** `CBTRN02C.cbl`, `COTRN02C.cbl`, `COCRDUPC.cbl`

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```
*(Lines 547-552, CBTRN02C.cbl — balance update showing deposit credit/withdrawal debit classification. For deposit accounts, positive amounts represent deposits (inflows) added to cycle credit, negative amounts represent withdrawals added to cycle debit.)*

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
```
*(Line 414, CBTRN02C.cbl — account expiration/maturity enforcement. For term deposits, this prevents transactions on matured accounts.)*

## Acceptance Criteria

### Scenario 1: Deposit account record contains all required fields

```gherkin
GIVEN a deposit account record is created in the migrated system
WHEN the record is persisted to Azure SQL
THEN all deposit fields are present:
  | Field              | SQL Type       | Nullable |
  | AccountId          | CHAR(11)       | NO       |
  | ActiveStatus       | CHAR(1)        | NO       |
  | CurrentBalance     | DECIMAL(12,2)  | NO       |
  | CreditLimit        | DECIMAL(12,2)  | NO       |
  | CashCreditLimit    | DECIMAL(12,2)  | NO       |
  | CycleCredit        | DECIMAL(12,2)  | NO       |
  | CycleDebit         | DECIMAL(12,2)  | NO       |
  | ExpirationDate     | DATE           | YES      |
  | DisclosureGroupId  | VARCHAR(10)    | YES      |
```

### Scenario 2: Financial field precision preservation for deposit balances

```gherkin
GIVEN a deposit account with CurrentBalance = 9999999999.99
WHEN the value is stored and retrieved from Azure SQL
THEN the value is exactly 9999999999.99
  AND no floating-point precision loss occurs
  AND the SQL type is DECIMAL(12,2)
```

### Scenario 3: Deposit account has zero credit limits

```gherkin
GIVEN a pure deposit account (not a credit/lending account)
WHEN the account is created
THEN CreditLimit is set to 0.00
  AND CashCreditLimit is set to 0.00
  AND the account type is distinguishable from lending accounts
```

### Scenario 4: Deposit balance tracks cycle inflows and outflows

```gherkin
GIVEN a deposit account with:
  | CurrentBalance | 50000.00 |
  | CycleCredit    | 10000.00 |
  | CycleDebit     | 2000.00  |
WHEN a deposit of 5000.00 is posted
THEN CurrentBalance becomes 55000.00
  AND CycleCredit becomes 15000.00
  AND CycleDebit remains 2000.00
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — institutions must maintain complete and accurate records of all accounts and balances | The deposit account record tracks current balance, cycle inflows/outflows, and interest rate group providing a complete view of each deposit position |
| GDPR | Art. 5(1)(c) | Data minimization — personal data limited to what is necessary | The deposit account contains only financial and status fields; customer PII (name, address, personnummer) is stored separately, not in the account master record |
| GDPR | Art. 5(1)(d) | Accuracy — data must be kept accurate and up to date | Balance fields are updated atomically during transaction posting (CBTRN02C.cbl:547-552), maintaining accuracy through structured credit/debit classification |
| Deposit Guarantee Directive | 2014/49/EU Art. 6 | Determination of eligible deposits and coverage level — deposit balance must be accurately tracked for guarantee calculations | The ACCT-CURR-BAL field provides the real-time deposit balance required for calculating deposit guarantee coverage per depositor |

## Edge Cases

1. **EBCDIC numeric encoding**: COBOL PIC S9(10)V99 fields use packed decimal format in EBCDIC. The implied decimal point (V99) must be correctly interpreted during migration. Incorrect conversion would cause all balances to be off by a factor of 100.

2. **Negative deposit balances**: While deposit accounts should not normally have negative balances (unlike credit accounts), the signed notation (S prefix) on ACCT-CURR-BAL allows negative values. This could represent temporary overdraft situations or processing errors. The migrated system must decide whether to enforce non-negative constraints on deposit accounts.

3. **Account type differentiation**: The current COBOL account record does not have an explicit account type field distinguishing deposits from lending. The ACCT-CREDIT-LIMIT being zero may implicitly indicate a deposit account. The mainframe team must confirm how account types are differentiated in the existing system.

4. **Disclosure group linkage**: The ACCT-GROUP-ID field links to interest rate schedules. For deposit accounts, this determines the savings interest rate (positive), unlike lending where it determines the borrowing rate (negative from the customer perspective). The rate lookup and tier structure programs are not yet available.

5. **Term deposit vs. demand deposit**: The ACCT-EXPIRAION-DATE field may distinguish term deposits (with a maturity date) from demand deposits (null or far-future date). The mainframe team must confirm the convention for demand deposit accounts.

6. **Filler area contents**: The 300-byte account record likely contains additional deposit-relevant fields not yet identified (account opening date, last interest posting date, product type code, dormancy flag). A full dump of the CVACT01Y copybook is needed.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The full CVACT01Y.cpy copybook must be obtained from the mainframe team to validate the reconstructed record layout and identify additional deposit-specific fields. Key questions: (1) How are deposit accounts distinguished from credit/lending accounts in the account master? (2) Does the account record contain an account opening date and product type code? (3) Is the disclosure group ID the sole linkage to deposit interest rate schedules? (4) What fields in the filler area relate to dormancy tracking, last activity date, or deposit guarantee reporting? (5) Are there separate VSAM files for term deposit metadata (maturity instructions, renewal preferences)?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
