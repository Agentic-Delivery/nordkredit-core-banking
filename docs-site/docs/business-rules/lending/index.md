---
title: Lending
sidebar_position: 7
---

# Lending Business Rules

Business rules for loan origination, credit limit enforcement, interest calculation, collateral management, disbursement, repayment processing, delinquency management, and maturity enforcement extracted from COBOL source programs and regulatory requirements for the NordKredit AB core banking system.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `CBTRN02C.cbl` | Batch | Daily transaction posting with credit limit enforcement, balance updates, and category tracking | LND-BR-001, LND-BR-002, LND-BR-005, LND-BR-009, LND-BR-010 |
| `COCRDUPC.cbl` | CICS Online | Card/account update with account validation | LND-BR-001 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVACT01Y.cpy` | Account master record layout — credit limit, balance, and disclosure group fields (referenced but not fully in repository) | LND-BR-001, LND-BR-002, LND-BR-003, LND-BR-004 |
| `CVACT03Y.cpy` | Card cross-reference record — card-to-account linkage used during credit limit validation | LND-BR-002 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [LND-BR-001](./lnd-br-001) | Loan account data structure and field definitions | Critical | CVACT01Y.cpy (reconstructed) | FSA FFFS 2014:5 Ch. 6, GDPR, Consumer Credit Directive |
| [LND-BR-002](./lnd-br-002) | Credit limit enforcement and overlimit detection | Critical | CBTRN02C.cbl:393-421 | FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive, PSD2 |
| [LND-BR-003](./lnd-br-003) | Loan origination and credit assessment | Critical | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 8, AML |
| [LND-BR-004](./lnd-br-004) | Interest calculation and amortization schedule | Critical | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive Art. 10, 19 |
| [LND-BR-005](./lnd-br-005) | Repayment processing and balance update | Critical | CBTRN02C.cbl:545-560 | FSA FFFS 2014:5 Ch. 3, PSD2, Consumer Credit Directive Art. 16 |
| [LND-BR-006](./lnd-br-006) | Collateral management and valuation | High | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 6, 8, CRR Art. 194-217 |
| [LND-BR-007](./lnd-br-007) | Loan disbursement and fund transfer | High | Dedicated program not yet in repository | PSD2 Art. 83, 89, FSA FFFS 2014:5 Ch. 3, AML |
| [LND-BR-008](./lnd-br-008) | Delinquency management and collections | High | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive, Inkassolagen |
| [LND-BR-009](./lnd-br-009) | Account expiration enforcement for lending | High | CBTRN02C.cbl:414-420 | FSA FFFS 2014:5 Ch. 6, Consumer Credit Directive, PSD2 |
| [LND-BR-010](./lnd-br-010) | Lending transaction categorization and balance tracking | Medium | CBTRN02C.cbl:467-542 | FSA FFFS 2014:5 Ch. 3, 7, PSD2, DORA |

## Status

All 10 business rules have been extracted. Rules LND-BR-001, LND-BR-002, LND-BR-005, LND-BR-009, and LND-BR-010 are extracted directly from available COBOL source code (CBTRN02C.cbl). Rules LND-BR-003, LND-BR-004, LND-BR-006, LND-BR-007, and LND-BR-008 are extracted from regulatory requirements and inferred from the account data structure, pending dedicated COBOL source programs from the mainframe team. All rules have `status: extracted` and are awaiting domain expert validation.

**Note on COBOL source coverage**: The available COBOL source programs are from the CardDemo credit card application and do not include dedicated lending programs. The lending-relevant logic extracted here represents:

1. **Credit limit enforcement** (LND-BR-002): Directly extracted from CBTRN02C.cbl overlimit detection logic
2. **Repayment processing** (LND-BR-005): Directly extracted from CBTRN02C.cbl balance update logic
3. **Account expiration** (LND-BR-009): Directly extracted from CBTRN02C.cbl maturity enforcement
4. **Transaction categorization** (LND-BR-010): Directly extracted from CBTRN02C.cbl category balance tracking
5. **Account data structure** (LND-BR-001): Reconstructed from CVACT01Y.cpy field references

The following dedicated COBOL programs must be obtained from the mainframe team to complete the lending domain extraction:

- **Loan origination/account opening**: Credit assessment, limit determination, account creation
- **Interest calculation batch**: Nightly interest accrual, disclosure group rate lookup, day-count computation
- **Collateral management**: Collateral registration, periodic revaluation, LTV monitoring
- **Disbursement**: Term loan funding, revolving credit activation, external fund transfer
- **Collections/delinquency**: Missed payment detection, dunning, escalation, write-off processing
- **Minimum payment calculation**: Monthly statement generation, minimum payment determination
- **Credit line management**: Limit increases/decreases, temporary limit overrides

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| FSA FFFS 2014:5 Ch. 3 (Accounting Records) | LND-BR-001, LND-BR-005, LND-BR-007, LND-BR-010 |
| FSA FFFS 2014:5 Ch. 6 (Credit Risk Management) | LND-BR-001, LND-BR-002, LND-BR-003, LND-BR-004, LND-BR-006, LND-BR-008, LND-BR-009 |
| FSA FFFS 2014:5 Ch. 7 (Financial Systems) | LND-BR-010 |
| FSA FFFS 2014:5 Ch. 8 (Assets) | LND-BR-006 |
| EU Consumer Credit Directive 2008/48/EC Art. 8 (Creditworthiness) | LND-BR-003 |
| EU Consumer Credit Directive 2008/48/EC Art. 10 (Credit Agreement Information) | LND-BR-001, LND-BR-004, LND-BR-009 |
| EU Consumer Credit Directive 2008/48/EC Art. 16 (Early Repayment) | LND-BR-005 |
| EU Consumer Credit Directive 2008/48/EC Art. 19 (APR Calculation) | LND-BR-004 |
| PSD2 Art. 64 (Transaction Data Integrity) | LND-BR-002, LND-BR-009, LND-BR-010 |
| PSD2 Art. 83 (Execution Time) | LND-BR-007 |
| PSD2 Art. 89 (Value Dating) | LND-BR-005, LND-BR-007 |
| GDPR Art. 5(1)(a) (Lawfulness, Fairness, Transparency) | LND-BR-008 |
| GDPR Art. 5(1)(c) (Data Minimization) | LND-BR-001, LND-BR-006 |
| GDPR Art. 5(1)(d) (Accuracy) | LND-BR-001 |
| GDPR Art. 5(1)(e) (Storage Limitation) | LND-BR-006 |
| GDPR Art. 6(1)(b) (Contract Performance) | LND-BR-003 |
| GDPR Art. 15 (Right of Access) | LND-BR-004 |
| AML 2017:11 (Customer Due Diligence) | LND-BR-003, LND-BR-007 |
| EU CRR Art. 194-217 (Credit Risk Mitigation) | LND-BR-006 |
| Swedish Inkassolagen 1974:182 (Debt Recovery) | LND-BR-008 |
| DORA Art. 11 (ICT Risk Management) | LND-BR-010 |

## Migration Considerations

1. **Dedicated lending programs required**: The most critical gap is the absence of dedicated lending COBOL programs. The mainframe team must provide programs for loan origination, interest calculation, collateral management, and collections. These programs contain the core lending business logic that cannot be fully inferred from the card transaction processing code.

2. **Real-time credit limit enforcement**: The current COBOL system checks credit limits only during daily batch processing (CBTRN02C). The migrated system should implement real-time overlimit detection for online transactions, using optimistic concurrency control to handle concurrent access.

3. **Interest calculation precision**: COBOL PIC S9(10)V99 provides 2-decimal precision. The migrated Azure SQL system should use `DECIMAL(12,4)` for intermediate interest calculations and `DECIMAL(12,2)` for final posted amounts to avoid rounding errors over billing cycles.

4. **Batch SLA compliance**: The nightly interest calculation batch must complete by 06:00 (per current SLAs). Azure Functions with parallel processing should be designed to handle ~2 million accounts within this window.

5. **Repayment allocation**: The current system applies all repayments uniformly to the balance. The migrated system may need to implement payment allocation waterfall (interest → fees → principal) per Swedish Consumer Credit Act requirements.

6. **Collateral data model**: No collateral VSAM file structure is available. The migrated system should design a normalized collateral schema supporting one-to-many (loan-to-collateral) and many-to-many (cross-collateralization) relationships.

7. **Delinquency state machine**: The COBOL account status field (ACCT-ACTIVE-STATUS) supports only Y/N. The migrated system must extend this to a full state machine: Active → Early Delinquent → Stage 1-3 → Collections → Write-off → Closed, with transition rules and audit logging.

8. **Regulatory reporting**: Swedish lending regulations require periodic reporting to Finansinspektionen on credit risk exposures, non-performing loans, and collateral coverage. The migrated system must maintain data structures that support automated regulatory report generation.
