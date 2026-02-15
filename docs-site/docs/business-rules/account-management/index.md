---
title: Account Management
sidebar_position: 1
---

# Account Management Business Rules

Business rules for account viewing, updating, balance management, interest calculation, and batch processing extracted from COBOL source programs in the AWS CardDemo application.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COACTUPC.cbl` | CICS Online | Account update with full field validation | ACCT-BR-001, ACCT-BR-003, ACCT-BR-004, ACCT-BR-005, ACCT-BR-006 |
| `COACTVWC.cbl` | CICS Online | Account view (read-only lookup) | ACCT-BR-002, ACCT-BR-006 |
| `CBACT01C.cbl` | Batch | Account data extract to multiple output formats | ACCT-BR-007 |
| `CBACT02C.cbl` | Batch | Card data sequential report | ACCT-BR-009 |
| `CBACT03C.cbl` | Batch | Cross-reference data sequential report | ACCT-BR-009 |
| `CBACT04C.cbl` | Batch | Interest calculation and account balance update | ACCT-BR-008 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVACT01Y.cpy` | Account record layout (300-byte VSAM record) | ACCT-BR-001 |
| `CVACT02Y.cpy` | Card record layout (150-byte VSAM record) | ACCT-BR-009 |
| `CVACT03Y.cpy` | Card cross-reference (card-customer-account linkage) | ACCT-BR-006, ACCT-BR-009 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [ACCT-BR-001](./acct-br-001) | Account record data structure and field definitions | High | CVACT01Y.cpy | GDPR, FSA FFFS 2014:5 |
| [ACCT-BR-002](./acct-br-002) | Account view read-only lookup | Medium | COACTVWC.cbl | PSD2, GDPR |
| [ACCT-BR-003](./acct-br-003) | Account update field validation rules | High | COACTUPC.cbl | FFFS 2014:5, PSD2, GDPR, AML/KYC |
| [ACCT-BR-004](./acct-br-004) | Account update workflow and state machine | High | COACTUPC.cbl | PSD2, FFFS 2014:5, DORA |
| [ACCT-BR-005](./acct-br-005) | Optimistic concurrency control for account updates | High | COACTUPC.cbl | PSD2, FFFS 2014:5, DORA |
| [ACCT-BR-006](./acct-br-006) | Three-file read chain for account lookup | Medium | COACTUPC.cbl, COACTVWC.cbl | PSD2, GDPR |
| [ACCT-BR-007](./acct-br-007) | Batch account data extract and multi-format export | Medium | CBACT01C.cbl | FSA FFFS 2014:5, DORA |
| [ACCT-BR-008](./acct-br-008) | Batch interest calculation and account balance update | High | CBACT04C.cbl | FSA FFFS 2014:5, EBA Guidelines, PSD2 |
| [ACCT-BR-009](./acct-br-009) | Batch card data and cross-reference reporting | Low | CBACT02C.cbl, CBACT03C.cbl | FSA FFFS 2014:5 |

## Status

All 9 business rules have been extracted from the COBOL source. All rules have `status: extracted` and are awaiting domain expert validation.

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| PSD2 Art. 97 (Strong Customer Authentication) | ACCT-BR-002, ACCT-BR-003, ACCT-BR-004, ACCT-BR-005, ACCT-BR-006, ACCT-BR-008 |
| GDPR Art. 5(1)(c) (Data Minimisation) | ACCT-BR-001, ACCT-BR-003 |
| GDPR Art. 5(1)(d) (Accuracy) | ACCT-BR-003, ACCT-BR-005 |
| GDPR Art. 15 (Right of Access) | ACCT-BR-002, ACCT-BR-006 |
| FFFS 2014:5 Ch. 4 §3 (Operational Risk) | ACCT-BR-001, ACCT-BR-003, ACCT-BR-004, ACCT-BR-005 |
| FFFS 2014:5 Ch. 4 §10 (Interest Rate Risk) | ACCT-BR-008 |
| FFFS 2014:5 Ch. 8 (Reporting) | ACCT-BR-007, ACCT-BR-009 |
| DORA Art. 9 (ICT Risk Management) | ACCT-BR-004, ACCT-BR-005 |
| DORA Art. 11 (Data Management) | ACCT-BR-007 |
| AML/KYC | ACCT-BR-003 |
| EBA Guidelines on Interest Rate Risk | ACCT-BR-008 |

## Migration Considerations

1. **VSAM to SQL mapping**: Account records (ACCTDAT, 300-byte) and customer records (CUSTDAT, 300-byte) should be mapped to normalized Azure SQL tables with proper foreign key relationships. The cross-reference file (CXACAIX) becomes a SQL join or foreign key constraint.
2. **CICS transaction to REST API**: COACTUPC (transaction CAUP) maps to PUT/PATCH endpoints; COACTVWC (transaction CAVW) maps to GET endpoints. The COMMAREA data contract (2000 bytes) becomes the API request/response schema.
3. **Concurrency**: Optimistic concurrency via full-field comparison (27+ fields) should be replaced with SQL rowversion/timestamp columns or ETag-based HTTP concurrency. The two-phase confirmation (PF5) maps to a preview/confirm REST flow.
4. **Transactional integrity**: The CICS SYNCPOINT ROLLBACK on partial failure maps to SQL transactions with proper BEGIN/COMMIT/ROLLBACK semantics across the account and customer table updates.
5. **Interest calculation batch**: CBACT04C monthly interest formula `(balance × rate) / 1200` must be preserved exactly. The batch job maps to an Azure Function with timer trigger. The disclosure group fallback to 'DEFAULT' rate must be maintained.
6. **Character encoding**: EBCDIC to UTF-8 conversion required for all text fields. Swedish characters (Å, Ä, Ö) must be supported in name and address fields.
7. **Date formats**: COBOL stores dates as YYYY-MM-DD strings (10 chars). The .NET migration should use DateOnly/DateTime types with proper parsing of the legacy format.
8. **Validation rules**: The 25+ field-level validations (SSN, FICO, phone, state-ZIP cross-validation) should be implemented as domain validation objects in the .NET model, preserving the exact same rules.
