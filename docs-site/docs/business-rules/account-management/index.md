---
title: Account Management
sidebar_position: 1
---

# Account Management Business Rules

Business rules for account lifecycle, balance management, credit limit enforcement, account-card relationships, and account data presentation extracted from COBOL source programs in the AWS CardDemo application and related batch processing programs.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COCRDLIC.cbl` | CICS Online | Credit card list with account filtering and display | ACCT-BR-002, ACCT-BR-003, ACCT-BR-008 |
| `COCRDSLC.cbl` | CICS Online | Card detail with account lookup via alternate index | ACCT-BR-002, ACCT-BR-003, ACCT-BR-008 |
| `COCRDUPC.cbl` | CICS Online | Card update with account validation and status management | ACCT-BR-002, ACCT-BR-005, ACCT-BR-006 |
| `CBTRN02C.cbl` | Batch | Daily transaction posting with balance updates and limit checks | ACCT-BR-004, ACCT-BR-006, ACCT-BR-007 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVACT01Y.cpy` | Account master record layout (300-byte VSAM record, referenced but not in repository) | ACCT-BR-001 |
| `CVACT02Y.cpy` | Card record layout — contains embedded account FK (CARD-ACCT-ID) | ACCT-BR-003 |
| `CVACT03Y.cpy` | Card cross-reference — card-customer-account linkage | ACCT-BR-003 |
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

## Status

All 8 business rules have been extracted from the COBOL source. All rules have `status: extracted` and are awaiting domain expert validation.

**Note on COBOL source coverage**: The available COBOL source programs do not include dedicated account management programs (e.g., account opening, account closing, dormancy management, or standalone account inquiry). The rules extracted here represent account management logic embedded within card management and transaction processing programs. The CVACT01Y.cpy copybook (account master record layout) is referenced but not present in the repository — the record structure has been reconstructed from field references across multiple programs and the .NET domain model. The following areas require dedicated COBOL source programs to be obtained from the mainframe team:

- **Account opening**: No account creation program found in available source
- **Account closing**: No account closure or termination program found
- **Dormancy management**: No dormancy detection or reactivation batch job found
- **Standalone balance inquiry**: No dedicated balance inquiry screen found (balance shown only in transaction context)
- **Interest calculation**: Account-level interest computation batch program not found (disclosure group structure exists in CVTRA02Y.cpy)
- **Statement generation**: No statement generation or cycle-close program found
- **Account holder management**: No customer-account relationship management program found

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| PSD2 Art. 64 (Transaction Data Integrity) | ACCT-BR-001, ACCT-BR-004, ACCT-BR-007 |
| PSD2 Art. 97 (Strong Customer Authentication) | ACCT-BR-002, ACCT-BR-005, ACCT-BR-008 |
| GDPR Art. 5(1)(c) (Data Minimization) | ACCT-BR-001 |
| GDPR Art. 5(1)(d) (Accuracy) | ACCT-BR-001 |
| GDPR Art. 5(1)(e) (Storage Limitation) | ACCT-BR-006 |
| GDPR Art. 15 (Right of Access) | ACCT-BR-003, ACCT-BR-008 |
| GDPR Art. 17 (Right to Erasure) | ACCT-BR-005 |
| FSA FFFS 2014:5 Ch. 3 (Accounting Records) | ACCT-BR-001, ACCT-BR-004 |
| FSA FFFS 2014:5 Ch. 4 §3 (Operational Risk) | ACCT-BR-002, ACCT-BR-005, ACCT-BR-006 |
| FSA FFFS 2014:5 Ch. 6 (Credit Risk Management) | ACCT-BR-007 |
| FSA FFFS 2014:5 Ch. 7 (Financial Systems) | ACCT-BR-004, ACCT-BR-008 |
| AML 2017:11 (Transaction Monitoring) | ACCT-BR-003 |
| EU Consumer Credit Directive 2008/48/EC | ACCT-BR-007 |
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
