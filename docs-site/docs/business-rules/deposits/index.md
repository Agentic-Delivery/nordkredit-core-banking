---
title: Deposits
sidebar_position: 8
---

# Deposits Business Rules

Business rules for deposit account management, interest calculation, term deposit handling, savings product configuration, statement generation, deposit guarantee compliance, and account dormancy management extracted from COBOL source programs and regulatory requirements for the NordKredit AB core banking system.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `CBTRN02C.cbl` | Batch | Daily transaction posting with balance updates and category tracking | DEP-BR-001, DEP-BR-003, DEP-BR-008 |
| `COTRN02C.cbl` | CICS Online | Transaction add with account validation | DEP-BR-001 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVACT01Y.cpy` | Account master record layout — balance fields, cycle tracking, and disclosure group linkage (referenced but not fully in repository) | DEP-BR-001, DEP-BR-003, DEP-BR-004, DEP-BR-005 |
| `CVACT03Y.cpy` | Card cross-reference record — card-to-account linkage used during deposit transaction routing | DEP-BR-001 |
| `CVTRA01Y.cpy` | Transaction category balance record — category-level balance tracking for deposit transactions | DEP-BR-008 |
| `CVTRA02Y.cpy` | Disclosure group record — interest rate grouping for deposit products (referenced in batch processing) | DEP-BR-004 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [DEP-BR-001](./dep-br-001) | Deposit account data structure and field definitions | Critical | CVACT01Y.cpy (reconstructed) | FSA FFFS 2014:5 Ch. 3, GDPR, Deposit Guarantee Directive |
| [DEP-BR-002](./dep-br-002) | Deposit account opening and KYC verification | Critical | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 3, AML 2017:11, GDPR, Deposit Guarantee Directive |
| [DEP-BR-003](./dep-br-003) | Deposit posting and balance update | Critical | CBTRN02C.cbl:545-560 | FSA FFFS 2014:5 Ch. 3, PSD2 Art. 64, 89 |
| [DEP-BR-004](./dep-br-004) | Interest calculation and accrual for deposit accounts | Critical | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 3, 6, Deposit Guarantee Directive, PSD2 |
| [DEP-BR-005](./dep-br-005) | Term deposit maturity and renewal processing | High | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 3, Consumer Deposit Regulations, PSD2 |
| [DEP-BR-006](./dep-br-006) | Savings product configuration and tiered rates | High | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 3, 6, Deposit Guarantee Directive |
| [DEP-BR-007](./dep-br-007) | Deposit account statement generation | High | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 7, PSD2 Art. 57, GDPR Art. 15 |
| [DEP-BR-008](./dep-br-008) | Deposit transaction categorization and balance tracking | Medium | CBTRN02C.cbl:467-542 | FSA FFFS 2014:5 Ch. 3, 7, PSD2, DORA |
| [DEP-BR-009](./dep-br-009) | Account dormancy detection and management | High | Dedicated program not yet in repository | FSA FFFS 2014:5 Ch. 3, GDPR Art. 5(1)(e), Deposit Guarantee Directive |
| [DEP-BR-010](./dep-br-010) | Deposit guarantee scheme compliance and reporting | Critical | Dedicated program not yet in repository | Deposit Guarantee Directive 2014/49/EU, FSA FFFS 2014:5, DORA |

## Status

All 10 business rules have been extracted. Rules DEP-BR-001, DEP-BR-003, and DEP-BR-008 are extracted directly from available COBOL source code (CBTRN02C.cbl) and copybook references. Rules DEP-BR-002, DEP-BR-004, DEP-BR-005, DEP-BR-006, DEP-BR-007, DEP-BR-009, and DEP-BR-010 are extracted from regulatory requirements and inferred from the account data structure, pending dedicated COBOL source programs from the mainframe team. All rules have `status: extracted` and are awaiting domain expert validation.

**Note on COBOL source coverage**: The available COBOL source programs are from the CardDemo application and do not include dedicated deposit programs. The deposit-relevant logic extracted here represents:

1. **Deposit account data structure** (DEP-BR-001): Reconstructed from CVACT01Y.cpy field references — balance, cycle credit/debit, disclosure group
2. **Deposit posting and balance update** (DEP-BR-003): Directly extracted from CBTRN02C.cbl balance update logic (lines 545-560)
3. **Transaction categorization** (DEP-BR-008): Directly extracted from CBTRN02C.cbl category balance tracking (lines 467-542)

The following dedicated COBOL programs must be obtained from the mainframe team to complete the deposits domain extraction:

- **Deposit account opening**: Customer onboarding, KYC verification, initial deposit, account type assignment
- **Interest calculation batch**: Nightly interest accrual, tiered rate computation, day-count convention (ACT/360 or ACT/365)
- **Term deposit management**: Maturity processing, automatic renewal, early withdrawal penalty calculation
- **Savings product configuration**: Product catalog, rate tier definitions, minimum balance rules
- **Statement generation**: Monthly/quarterly statement creation, interest summary, regulatory disclosures
- **Dormancy management**: Inactivity detection, dormant account flagging, escheatment processing
- **Deposit guarantee reporting**: FSA coverage calculation, depositor reporting, guarantee fund contributions

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| FSA FFFS 2014:5 Ch. 3 (Accounting Records) | DEP-BR-001, DEP-BR-002, DEP-BR-003, DEP-BR-004, DEP-BR-005, DEP-BR-006, DEP-BR-008, DEP-BR-009 |
| FSA FFFS 2014:5 Ch. 6 (Risk Management) | DEP-BR-004, DEP-BR-006 |
| FSA FFFS 2014:5 Ch. 7 (Financial Systems) | DEP-BR-007, DEP-BR-008 |
| EU Deposit Guarantee Directive 2014/49/EU | DEP-BR-001, DEP-BR-002, DEP-BR-005, DEP-BR-006, DEP-BR-009, DEP-BR-010 |
| PSD2 Art. 57 (Information on Transactions) | DEP-BR-007 |
| PSD2 Art. 64 (Transaction Data Integrity) | DEP-BR-003, DEP-BR-008 |
| PSD2 Art. 89 (Value Dating) | DEP-BR-003 |
| GDPR Art. 5(1)(a) (Lawfulness, Fairness, Transparency) | DEP-BR-002, DEP-BR-007 |
| GDPR Art. 5(1)(c) (Data Minimization) | DEP-BR-001 |
| GDPR Art. 5(1)(d) (Accuracy) | DEP-BR-001, DEP-BR-003 |
| GDPR Art. 5(1)(e) (Storage Limitation) | DEP-BR-009 |
| GDPR Art. 15 (Right of Access) | DEP-BR-007 |
| AML 2017:11 (Customer Due Diligence) | DEP-BR-002 |
| DORA Art. 11 (ICT Risk Management) | DEP-BR-008, DEP-BR-010 |

## Migration Considerations

1. **Dedicated deposit programs required**: The most critical gap is the absence of dedicated deposit COBOL programs. The mainframe team must provide programs for deposit account opening, interest calculation, term deposit management, and statement generation. These programs contain the core deposit business logic that cannot be fully inferred from the card transaction processing code.

2. **Interest calculation complexity**: Deposit interest calculation in Swedish banking follows specific day-count conventions. The COBOL system likely uses ACT/360 or ACT/365 — the mainframe team must clarify which convention applies. The migrated system should use `DECIMAL(12,4)` for intermediate interest calculations and `DECIMAL(12,2)` for posted amounts.

3. **Deposit guarantee scheme**: Sweden's deposit guarantee (insättningsgaranti) covers up to SEK 1,050,000 per depositor per institution. The system must track aggregate deposit balances per customer for guarantee reporting to Riksgälden (Swedish National Debt Office). This cross-account aggregation is not visible in the card-centric COBOL source.

4. **Term deposit maturity processing**: Term deposits (bundna konton) have maturity dates and renewal logic. The ACCT-EXPIRAION-DATE field may serve dual purpose for card expiration and deposit maturity. The mainframe team must confirm whether separate fields exist for term deposit maturity and renewal instructions.

5. **Savings product tiers**: Swedish savings products often have tiered interest rates based on balance ranges. The disclosure group (ACCT-GROUP-ID) likely links to rate tier schedules, but the tier structure and rate lookup programs are not in the repository.

6. **Account dormancy and escheatment**: Swedish law requires banks to manage dormant accounts and eventually escheat unclaimed funds. The current COBOL system's approach to dormancy detection (based on ACCT-ACTIVE-STATUS or separate dormancy flags) must be clarified.

7. **Batch SLA compliance**: The nightly interest calculation batch must complete by 06:00 (per current SLAs). For ~2 million deposit accounts, the Azure Functions implementation must process interest accrual within this window using parallel processing.

8. **EBCDIC encoding for Swedish characters**: Deposit account holder names and statement descriptions containing Swedish characters (Å, Ä, Ö) require EBCDIC-to-UTF-8 conversion with correct code page mapping.
