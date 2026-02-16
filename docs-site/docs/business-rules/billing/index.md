---
title: Billing
sidebar_position: 5
---

# Billing Business Rules

Business rules for interest rate assignment, credit limit enforcement, billing cycle balance tracking, transaction category accumulation, account expiration handling, transaction rejection, and billing reconciliation reports extracted from COBOL source programs in the AWS CardDemo application.

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

## Status

All 7 business rules have been extracted from the COBOL source. All rules have `status: extracted` and are awaiting domain expert validation.

**Note on COBOL source coverage**: The available COBOL source programs do not include dedicated billing batch programs (e.g., interest calculation, statement generation, late payment processing, or fee assessment). The rules extracted here represent billing-adjacent logic embedded within the transaction processing and card management programs. Additional billing-specific COBOL programs may exist on the mainframe that are not yet available in the repository. The following areas require dedicated COBOL source programs to be obtained from the mainframe team:

- **Fee calculation and schedules**: No fee assessment program found in available source
- **Interest computation batch**: The disclosure group structure (BILL-BR-001) defines rates, but the actual interest calculation batch program is not available
- **Statement generation**: No statement generation or cycle-close program found
- **Late payment handling**: No late payment fee or penalty program found
- **Minimum payment calculation**: No minimum payment determination logic found

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| FSA FFFS 2014:5 Ch. 6 (Credit Risk Management) | BILL-BR-001, BILL-BR-002 |
| FSA FFFS 2014:5 Ch. 7 (Financial Systems) | BILL-BR-003, BILL-BR-004, BILL-BR-006, BILL-BR-007 |
| FSA FFFS 2014:5 Ch. 4 (Operational Risk) | BILL-BR-005 |
| PSD2 Art. 45 (Information on Charges) | BILL-BR-001 |
| PSD2 Art. 64 (Transaction Data Integrity) | BILL-BR-002, BILL-BR-003, BILL-BR-004, BILL-BR-005 |
| EU Consumer Credit Directive 2008/48/EC | BILL-BR-001, BILL-BR-002, BILL-BR-004 |
| GDPR Art. 17 (Right to Erasure) | BILL-BR-005 |
| AML 2017:11 (Transaction Monitoring) | BILL-BR-006, BILL-BR-007 |
| DORA Art. 11 (ICT Risk Management) | BILL-BR-003, BILL-BR-006, BILL-BR-007 |

## Migration Considerations

1. **Interest calculation precision**: The disclosure group rate field `S9(04)V99` provides 2 decimal places for interest rates. The migrated system must use `decimal(6,2)` for rates and `decimal` types for all intermediate interest calculations. Floating-point types are NOT acceptable.
2. **Credit limit enforcement atomicity**: In the COBOL batch, credit limit checks use the cumulative balance updated by prior transactions in the same run. The migrated system must preserve this sequential dependency or use database-level constraints.
3. **Billing cycle management**: The available COBOL source does not include cycle-close or statement generation programs. These must be obtained from the mainframe team before the billing domain implementation can proceed.
4. **Category-level interest**: The combination of disclosure group rates (BILL-BR-001) and category balances (BILL-BR-004) enables tiered interest calculation. The migrated system should model this as a lookup table joined to a balance table.
5. **Reject handling**: The fixed-width reject file (430 bytes) should be migrated to a database table with structured columns for easier querying and reporting.
6. **Report generation**: The batch report (BILL-BR-007) should be replaced with a database-backed reporting API or scheduled report generation, supporting both PDF output and interactive querying.
