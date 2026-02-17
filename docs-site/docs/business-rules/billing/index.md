---
title: Billing
sidebar_position: 5
---

# Billing Business Rules

Business rules for interest rate assignment, credit limit enforcement, billing cycle balance tracking, transaction category accumulation, account expiration handling, transaction rejection, billing reconciliation reports, fee calculation, interest computation, statement generation, late payment handling, and minimum payment calculation extracted from COBOL source programs and regulatory requirements for the NordKredit AB core banking system.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `CBTRN02C.cbl` | Batch | Daily transaction posting with validation and balance updates | BILL-BR-002, BILL-BR-003, BILL-BR-004, BILL-BR-005, BILL-BR-006 |
| `CBTRN03C.cbl` | Batch | Daily transaction detail report generation | BILL-BR-007 |
| `COCRDUPC.cbl` | CICS Online | Credit card update (expiration date structure) | BILL-BR-005 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVTRA02Y.cpy` | Disclosure group record (interest rate assignment) | BILL-BR-001 |
| `CVTRA01Y.cpy` | Transaction category balance record (50-byte VSAM record) | BILL-BR-004 |
| `CVTRA03Y.cpy` | Transaction type description record | BILL-BR-007 |
| `CVTRA04Y.cpy` | Transaction category type description record | BILL-BR-007 |
| `CVTRA07Y.cpy` | Report layout structures (headers, detail, totals) | BILL-BR-007 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [BILL-BR-001](./bill-br-001) | Interest rate assignment by disclosure group | Critical | CVTRA02Y.cpy | FFFS 2014:5, PSD2, Consumer Credit Directive |
| [BILL-BR-002](./bill-br-002) | Credit limit enforcement and overlimit handling | Critical | CBTRN02C.cbl | FFFS 2014:5, PSD2, Consumer Credit Directive |
| [BILL-BR-003](./bill-br-003) | Billing cycle balance tracking and credit/debit classification | Critical | CBTRN02C.cbl | FFFS 2014:5, PSD2, DORA |
| [BILL-BR-004](./bill-br-004) | Transaction category balance accumulation for billing | High | CBTRN02C.cbl | FFFS 2014:5, PSD2, Consumer Credit Directive |
| [BILL-BR-005](./bill-br-005) | Account expiration enforcement in billing | High | CBTRN02C.cbl, COCRDUPC.cbl | FFFS 2014:5, PSD2, GDPR |
| [BILL-BR-006](./bill-br-006) | Transaction rejection and billing exception handling | High | CBTRN02C.cbl | FFFS 2014:5, AML, DORA |
| [BILL-BR-007](./bill-br-007) | Daily transaction report generation for billing reconciliation | Medium | CBTRN03C.cbl | FFFS 2014:5, AML, DORA |
| [BILL-BR-008](./bill-br-008) | Fee calculation and schedule management | Critical | Dedicated program not yet in repository | FFFS 2014:5, PSD2, Consumer Credit Directive |
| [BILL-BR-009](./bill-br-009) | Interest computation batch for billing accounts | Critical | Dedicated program not yet in repository | FFFS 2014:5, PSD2, Consumer Credit Directive |
| [BILL-BR-010](./bill-br-010) | Statement generation and billing cycle close | Critical | Dedicated program not yet in repository | FFFS 2014:5, PSD2, Consumer Credit Directive, GDPR |
| [BILL-BR-011](./bill-br-011) | Late payment detection and penalty processing | High | Dedicated program not yet in repository | FFFS 2014:5, PSD2, Consumer Credit Directive |
| [BILL-BR-012](./bill-br-012) | Minimum payment calculation | High | Dedicated program not yet in repository | FFFS 2014:5, Consumer Credit Directive |

## Status

All 12 business rules have been extracted. Rules BILL-BR-001 through BILL-BR-007 are extracted directly from available COBOL source code (CBTRN02C.cbl, CBTRN03C.cbl, COCRDUPC.cbl) and copybook references. Rules BILL-BR-008 through BILL-BR-012 are extracted from regulatory requirements and inferred from the account data structure, disclosure group mechanism, and transaction posting patterns, pending dedicated COBOL source programs from the mainframe team. All rules have `status: extracted` and are awaiting domain expert validation.

**Note on COBOL source coverage**: The available COBOL source programs are from the CardDemo application and do not include dedicated billing batch programs. The billing-relevant logic extracted here represents:

1. **Interest rate assignment** (BILL-BR-001): Directly extracted from CVTRA02Y.cpy disclosure group structure
2. **Credit limit enforcement** (BILL-BR-002): Directly extracted from CBTRN02C.cbl overlimit detection logic
3. **Billing cycle balance tracking** (BILL-BR-003): Directly extracted from CBTRN02C.cbl cycle accumulators
4. **Transaction category accumulation** (BILL-BR-004): Directly extracted from CBTRN02C.cbl category balance tracking
5. **Account expiration enforcement** (BILL-BR-005): Directly extracted from CBTRN02C.cbl and COCRDUPC.cbl
6. **Transaction rejection handling** (BILL-BR-006): Directly extracted from CBTRN02C.cbl reject processing
7. **Billing reconciliation report** (BILL-BR-007): Directly extracted from CBTRN03C.cbl report generation

The following dedicated COBOL programs must be obtained from the mainframe team to complete the billing domain extraction with full source traceability:

- **Fee assessment program**: Fee calculation and schedule management (BILL-BR-008 extracted from regulatory inference)
- **Interest computation batch**: Nightly interest calculation on category balances (BILL-BR-009 extracted from regulatory inference and disclosure group structure)
- **Statement generation program**: Billing cycle close and statement production (BILL-BR-010 extracted from regulatory inference and cycle accumulator fields)
- **Late payment processing program**: Late payment detection and penalty application (BILL-BR-011 extracted from regulatory inference)
- **Minimum payment calculation program**: Minimum payment determination logic (BILL-BR-012 extracted from regulatory inference)

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| FSA FFFS 2014:5 Ch. 6 (Credit Risk Management) | BILL-BR-001, BILL-BR-002, BILL-BR-008, BILL-BR-009, BILL-BR-011, BILL-BR-012 |
| FSA FFFS 2014:5 Ch. 7 (Financial Systems) | BILL-BR-003, BILL-BR-004, BILL-BR-006, BILL-BR-007, BILL-BR-008, BILL-BR-009, BILL-BR-010, BILL-BR-011 |
| FSA FFFS 2014:5 Ch. 4 (Operational Risk) | BILL-BR-005 |
| PSD2 Art. 45 (Information on Charges) | BILL-BR-001, BILL-BR-008, BILL-BR-009 |
| PSD2 Art. 57 (Information on Transactions) | BILL-BR-010 |
| PSD2 Art. 64 (Transaction Data Integrity) | BILL-BR-002, BILL-BR-003, BILL-BR-004, BILL-BR-005 |
| PSD2 Art. 83 (Execution Time) | BILL-BR-011 |
| EU Consumer Credit Directive 2008/48/EC Art. 10 | BILL-BR-001, BILL-BR-002, BILL-BR-004, BILL-BR-009, BILL-BR-010, BILL-BR-011, BILL-BR-012 |
| EU Consumer Credit Directive 2008/48/EC Art. 16 | BILL-BR-012 |
| EU Consumer Credit Directive 2008/48/EC Art. 19 | BILL-BR-008 |
| GDPR Art. 15 (Right of Access) | BILL-BR-010 |
| GDPR Art. 17 (Right to Erasure) | BILL-BR-005 |
| AML 2017:11 (Transaction Monitoring) | BILL-BR-006, BILL-BR-007 |
| DORA Art. 11 (ICT Risk Management) | BILL-BR-003, BILL-BR-006, BILL-BR-007 |

## Migration Considerations

1. **Interest calculation precision**: The disclosure group rate field `S9(04)V99` provides 2 decimal places for interest rates. The migrated system must use `decimal(6,2)` for rates and `decimal(12,4)` for intermediate interest calculations. Floating-point types are NOT acceptable.
2. **Credit limit enforcement atomicity**: In the COBOL batch, credit limit checks use the cumulative balance updated by prior transactions in the same run. The migrated system must preserve this sequential dependency or use database-level constraints.
3. **Billing cycle management**: The dedicated COBOL programs for cycle-close, statement generation, fee assessment, late payment processing, and minimum payment calculation must be obtained from the mainframe team. Rules BILL-BR-008 through BILL-BR-012 provide the regulatory framework and inferred business logic to guide the migration, but full source traceability requires the original programs.
4. **Category-level interest**: The combination of disclosure group rates (BILL-BR-001) and category balances (BILL-BR-004) enables tiered interest calculation per transaction type (BILL-BR-009). The migrated system should model this as a lookup table joined to a balance table, with per-category accrual tracking.
5. **Reject handling**: The fixed-width reject file (430 bytes) should be migrated to a database table with structured columns for easier querying and reporting.
6. **Report generation**: The batch report (BILL-BR-007) should be replaced with a database-backed reporting API or scheduled report generation, supporting both PDF output and interactive querying.
7. **Fee schedule management**: The fee schedule structure (BILL-BR-008) should be modeled as a configurable database table linked to account groups, enabling product managers to update fee amounts without code changes.
8. **Statement generation pipeline**: The statement generation process (BILL-BR-010) should follow the batch processing pattern: interest calculation (BILL-BR-009) -> fee assessment (BILL-BR-008) -> minimum payment calculation (BILL-BR-012) -> statement generation (BILL-BR-010). The migrated Azure Functions implementation must maintain this ordering.
9. **Late payment and delinquency**: The late payment system (BILL-BR-011) must integrate with the lending domain's delinquency management (LND-BR-008) for accounts that escalate beyond 90 days. The migrated system should use a unified delinquency state machine across billing and lending.
10. **Batch SLA compliance**: The nightly billing batch (interest calculation, fee assessment, late payment detection) must complete by 06:00 per current SLAs. For ~2 million accounts, the Azure Functions implementation must be designed for parallel processing.
