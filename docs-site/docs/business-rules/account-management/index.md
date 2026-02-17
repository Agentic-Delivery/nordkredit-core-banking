---
title: Account Management
sidebar_position: 1
---

# Account Management Business Rules

Business rules for account lifecycle, balance management, credit limit enforcement, account-card relationships, interest calculation, statement generation, dormancy management, and account data presentation extracted from COBOL source programs in the AWS CardDemo application and related batch processing programs.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COCRDLIC.cbl` | CICS Online | Credit card list with account filtering and display | ACCT-BR-002, ACCT-BR-003, ACCT-BR-008 |
| `COCRDSLC.cbl` | CICS Online | Card detail with account lookup via alternate index | ACCT-BR-002, ACCT-BR-003, ACCT-BR-008 |
| `COCRDUPC.cbl` | CICS Online | Card update with account validation and status management | ACCT-BR-002, ACCT-BR-005, ACCT-BR-006 |
| `CBTRN01C.cbl` | Batch | Daily transaction verification with account and XREF validation | ACCT-BR-014 |
| `CBTRN02C.cbl` | Batch | Daily transaction posting with balance updates, limit checks, and category tracking | ACCT-BR-004, ACCT-BR-006, ACCT-BR-007, ACCT-BR-009, ACCT-BR-015 |
| `CBTRN03C.cbl` | Batch | Transaction detail report with account-level aggregation and date range filtering | ACCT-BR-010 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVACT01Y.cpy` | Account master record layout (300-byte VSAM record, referenced but not in repository) | ACCT-BR-001, ACCT-BR-011, ACCT-BR-012, ACCT-BR-013 |
| `CVACT02Y.cpy` | Card record layout — contains embedded account FK (CARD-ACCT-ID) | ACCT-BR-003 |
| `CVACT03Y.cpy` | Card cross-reference — card-customer-account linkage | ACCT-BR-003, ACCT-BR-014 |
| `CVCRD01Y.cpy` | Card work area — account ID input/output fields | ACCT-BR-002 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [ACCT-BR-001](./acct-br-001) | Account record data structure and field definitions | Critical | CVACT01Y.cpy (reconstructed) | GDPR, FSA FFFS 2014:5, PSD2 |
| [ACCT-BR-002](./acct-br-002) | Account ID input validation rules | High | COCRDLIC/COCRDSLC/COCRDUPC.cbl | FSA FFFS 2014:5, PSD2 |
| [ACCT-BR-003](./acct-br-003) | Account-card relationship and cross-reference lookup | High | COCRDSLC.cbl, CVACT02Y/03Y.cpy | PSD2, GDPR, AML/KYC |
| [ACCT-BR-004](./acct-br-004) | Account balance management and cycle tracking | Critical | CBTRN02C.cbl | FSA FFFS 2014:5, PSD2, DORA |
| [ACCT-BR-005](./acct-br-005) | Account status transitions and lifecycle management | High | COCRDUPC.cbl, CBTRN02C.cbl | FSA FFFS 2014:5, PSD2, GDPR |
| [ACCT-BR-006](./acct-br-006) | Account expiration date management and enforcement | High | CBTRN02C.cbl, COCRDUPC.cbl | FSA FFFS 2014:5, PSD2, GDPR |
| [ACCT-BR-007](./acct-br-007) | Account credit limit enforcement | Critical | CBTRN02C.cbl | FSA FFFS 2014:5, PSD2, Consumer Credit Directive |
| [ACCT-BR-008](./acct-br-008) | Account display and list screen presentation | Medium | COCRDLIC.cbl, COCRDSLC.cbl | PSD2, GDPR, FSA FFFS 2014:5 |
| [ACCT-BR-009](./acct-br-009) | Transaction category balance tracking | High | CBTRN02C.cbl | FSA FFFS 2014:5, PSD2, Consumer Credit Directive |
| [ACCT-BR-010](./acct-br-010) | Account statement cycle aggregation and reporting | High | CBTRN03C.cbl | FSA FFFS 2014:5, PSD2, Consumer Credit Directive |
| [ACCT-BR-011](./acct-br-011) | Account opening and initial setup | Critical | Dedicated program not yet in repository | FSA FFFS 2014:5, AML 2017:11, GDPR, PSD2 |
| [ACCT-BR-012](./acct-br-012) | Account closing and termination | High | Dedicated program not yet in repository | FSA FFFS 2014:5, GDPR, PSD2, AML 2017:11 |
| [ACCT-BR-013](./acct-br-013) | Account dormancy detection and management | High | Dedicated program not yet in repository | FSA FFFS 2014:5, GDPR, AML 2017:11 |
| [ACCT-BR-014](./acct-br-014) | Customer-account relationship management | High | CBTRN01C.cbl, CVACT03Y.cpy | GDPR, AML 2017:11, PSD2, FSA FFFS 2014:5 |
| [ACCT-BR-015](./acct-br-015) | Account interest calculation and disclosure grouping | Critical | CBTRN02C.cbl (TCATBAL structure); dedicated program not yet in repository | FSA FFFS 2014:5, Consumer Credit Directive, PSD2 |

## Status

All 15 business rules have been extracted from available COBOL source and regulatory requirements. Rules ACCT-BR-001 through ACCT-BR-010 and ACCT-BR-014 are extracted directly from COBOL programs in the repository. Rules ACCT-BR-011, ACCT-BR-012, ACCT-BR-013, and ACCT-BR-015 are inferred from existing data structures, field references, and Swedish regulatory requirements — their dedicated COBOL programs are not yet available. All rules have `status: extracted` and are awaiting domain expert validation.

**Note on COBOL source coverage**: The available COBOL source programs cover transaction posting (CBTRN01C, CBTRN02C), transaction reporting (CBTRN03C), and card management (COCRDLIC, COCRDSLC, COCRDUPC). Account management logic has been extracted from these programs, including transaction category balance tracking (ACCT-BR-009), statement aggregation (ACCT-BR-010), and customer-account relationship validation (ACCT-BR-014). The following areas still require dedicated COBOL source programs to be obtained from the mainframe team for full extraction:

- **Account opening**: Inferred from data structures and regulatory requirements (ACCT-BR-011). Dedicated creation program not found in available source.
- **Account closing**: Inferred from status field usage and regulatory requirements (ACCT-BR-012). Dedicated termination program not found.
- **Dormancy management**: Inferred from activity status field and Swedish regulatory requirements (ACCT-BR-013). Dedicated dormancy batch not found.
- **Interest calculation**: Partially inferred from TCATBAL category balance structure (ACCT-BR-015). Dedicated interest calculation batch not found. The CVTRA02Y.cpy copybook (disclosure group structure) is referenced but not in repository.

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| PSD2 Art. 55 (Right to Terminate) | ACCT-BR-012 |
| PSD2 Art. 64 (Transaction Data Integrity) | ACCT-BR-001, ACCT-BR-004, ACCT-BR-007, ACCT-BR-009, ACCT-BR-010, ACCT-BR-015 |
| PSD2 Art. 97 (Strong Customer Authentication) | ACCT-BR-002, ACCT-BR-005, ACCT-BR-008, ACCT-BR-011, ACCT-BR-014 |
| GDPR Art. 5(1)(c) (Data Minimization) | ACCT-BR-001 |
| GDPR Art. 5(1)(d) (Accuracy) | ACCT-BR-001, ACCT-BR-014 |
| GDPR Art. 5(1)(e) (Storage Limitation) | ACCT-BR-006, ACCT-BR-012, ACCT-BR-013 |
| GDPR Art. 6 (Lawful Basis for Processing) | ACCT-BR-011 |
| GDPR Art. 15 (Right of Access) | ACCT-BR-003, ACCT-BR-008 |
| GDPR Art. 17 (Right to Erasure) | ACCT-BR-005, ACCT-BR-012 |
| FSA FFFS 2014:5 Ch. 3 (Accounting Records) | ACCT-BR-001, ACCT-BR-004, ACCT-BR-009, ACCT-BR-011, ACCT-BR-012, ACCT-BR-013, ACCT-BR-014, ACCT-BR-015 |
| FSA FFFS 2014:5 Ch. 4 §3 (Operational Risk) | ACCT-BR-002, ACCT-BR-005, ACCT-BR-006, ACCT-BR-011 |
| FSA FFFS 2014:5 Ch. 6 (Credit Risk Management) | ACCT-BR-007 |
| FSA FFFS 2014:5 Ch. 7 (Financial Systems) | ACCT-BR-004, ACCT-BR-008, ACCT-BR-010 |
| AML 2017:11 Art. 11 (Customer Identification) | ACCT-BR-003, ACCT-BR-011, ACCT-BR-014 |
| AML 2017:11 Art. 25 (Record Retention) | ACCT-BR-012, ACCT-BR-013 |
| EU Consumer Credit Directive 2008/48/EC Art. 6 | ACCT-BR-007, ACCT-BR-009, ACCT-BR-010, ACCT-BR-015 |
| EU Consumer Credit Directive 2008/48/EC Art. 10 | ACCT-BR-015 |
| DORA Art. 11 (ICT Risk Management) | ACCT-BR-004 |

## Migration Considerations

1. **CVACT01Y.cpy acquisition**: The account master record copybook must be obtained from the mainframe team. The reconstructed layout covers fields referenced in available programs, but additional fields (account opening date, last statement date, customer reference, account type/product code) likely exist in the filler area.
2. **VSAM to SQL mapping**: The ACCTFILE KSDS record (300 bytes) should be mapped to a normalized Azure SQL table. The 11-digit ACCT-ID becomes a primary key. Financial fields must use `decimal(12,2)` — no floating-point types.
3. **Account-card foreign key**: The `CARD-ACCT-ID` embedded in card records and the CARDAIX alternate index should be mapped to a proper SQL foreign key with referential integrity.
4. **Status model extension**: The COBOL Y/N status model must be extended to support dormant, frozen, and closed states as required by Swedish banking regulations. A state machine with transition rules and audit logging should be implemented.
5. **Credit limit enforcement**: Currently batch-only in COBOL. The migrated system should consider real-time credit limit checking for online transactions, with appropriate concurrency controls.
6. **Dedicated account UI**: The COBOL system has no standalone account management screens. The migrated system should implement dedicated account management API endpoints and UI for account lifecycle operations.
7. **Character encoding**: EBCDIC to UTF-8 conversion for all text fields. Account IDs are numeric-only, but error messages and descriptions need encoding conversion.
8. **Billing cycle integration**: Account balance management (ACCT-BR-004) is tightly coupled with billing cycle tracking. The cycle reset mechanism must be identified from the mainframe before implementing balance management.
9. **TCATBAL to SQL**: The TCATBAL indexed file (ACCT-BR-009) should be mapped to an Azure SQL table with a composite primary key (AccountId, TransactionTypeCode, CategoryCode). Category-level balances support interest calculation (ACCT-BR-015) and must be maintained with the same precision as the COBOL implementation.
10. **Interest calculation program**: The dedicated interest calculation batch must be obtained from the mainframe team. The TCATBAL structure (ACCT-BR-009) provides the balance breakdown, but the rate table, calculation method (average daily balance vs. cycle-end), and grace period rules are not yet available.
11. **Statement generation modernization**: The CBTRN03C report (ACCT-BR-010) produces 133-character fixed-width output. The migrated system should generate PDF or electronic statements while maintaining the same three-level totaling logic and date range filtering.
12. **Customer-account relationship integrity**: The XREF-based relationship model (ACCT-BR-014) must be migrated to proper SQL foreign keys with cascading rules. The migrated system should enforce referential integrity at the database level rather than relying on application-level validation.
