---
title: Transactions
sidebar_position: 3
---

# Transaction Processing Business Rules

Business rules for transaction listing, viewing, adding, batch processing (verification, posting, reporting), and inter-program navigation extracted from COBOL source programs in the AWS CardDemo application.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COTRN00C.cbl` | CICS Online | Transaction list with pagination | TRN-BR-001, TRN-BR-008 |
| `COTRN01C.cbl` | CICS Online | Transaction detail view | TRN-BR-002, TRN-BR-008 |
| `COTRN02C.cbl` | CICS Online | Transaction add with validation | TRN-BR-003, TRN-BR-008 |
| `CBTRN01C.cbl` | Batch | Daily transaction verification | TRN-BR-004, TRN-BR-009 |
| `CBTRN02C.cbl` | Batch | Daily transaction posting and balance updates | TRN-BR-005, TRN-BR-009 |
| `CBTRN03C.cbl` | Batch | Daily transaction detail report | TRN-BR-006, TRN-BR-009 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVTRA05Y.cpy` | Posted transaction record layout (350-byte VSAM record) | TRN-BR-007 |
| `CVTRA06Y.cpy` | Daily transaction input record layout (350-byte sequential record) | TRN-BR-007 |
| `CVTRA01Y.cpy` | Transaction category balance record (50-byte VSAM record) | TRN-BR-007 |
| `CVTRA02Y.cpy` | Disclosure group record (interest rate grouping) | TRN-BR-007 |
| `CVTRA03Y.cpy` | Transaction type description record | TRN-BR-007 |
| `CVTRA04Y.cpy` | Transaction category type description record | TRN-BR-007 |
| `CVTRA07Y.cpy` | Report layout structures (headers, detail, totals) | TRN-BR-007 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [TRN-BR-001](./trn-br-001) | Transaction list display with pagination | High | COTRN00C.cbl | PSD2, GDPR, FFFS 2014:5 |
| [TRN-BR-002](./trn-br-002) | Transaction detail view and lookup | High | COTRN01C.cbl | PSD2, GDPR, FFFS 2014:5 |
| [TRN-BR-003](./trn-br-003) | Transaction add with validation | High | COTRN02C.cbl | PSD2, FFFS 2014:5, AML |
| [TRN-BR-004](./trn-br-004) | Daily transaction file ingestion and card verification | High | CBTRN01C.cbl | PSD2, FFFS 2014:5, AML |
| [TRN-BR-005](./trn-br-005) | Daily transaction posting with validation and balance updates | Critical | CBTRN02C.cbl | PSD2, FFFS 2014:5, AML, DORA |
| [TRN-BR-006](./trn-br-006) | Daily transaction detail report generation | High | CBTRN03C.cbl | FFFS 2014:5, AML, DORA |
| [TRN-BR-007](./trn-br-007) | Transaction record data structures | High | CVTRA01Y-07Y.cpy | PSD2, GDPR, FFFS 2014:5 |
| [TRN-BR-008](./trn-br-008) | Transaction inter-program navigation and COMMAREA contract | Medium | COTRN00C/01C/02C.cbl | PSD2, FFFS 2014:5 |
| [TRN-BR-009](./trn-br-009) | Daily batch transaction processing pipeline | Critical | CBTRN01C/02C/03C.cbl | PSD2, FFFS 2014:5, AML, DORA |

## Status

All 9 business rules have been extracted from the COBOL source. All rules have `status: extracted` and are awaiting domain expert validation.

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| PSD2 Art. 64 (Transaction Data Integrity) | TRN-BR-003, TRN-BR-004, TRN-BR-005, TRN-BR-007, TRN-BR-009 |
| PSD2 Art. 73 (Refund and Credit Rules) | TRN-BR-005 |
| PSD2 Art. 97 (Strong Customer Authentication) | TRN-BR-001, TRN-BR-002, TRN-BR-003, TRN-BR-008 |
| GDPR Art. 5 (Data Minimization) | TRN-BR-007 |
| GDPR Art. 15 (Right of Access) | TRN-BR-001, TRN-BR-002 |
| FSA FFFS 2014:5 Ch. 7 (Financial Systems) | TRN-BR-001, TRN-BR-002, TRN-BR-003, TRN-BR-004, TRN-BR-005, TRN-BR-006, TRN-BR-007, TRN-BR-008, TRN-BR-009 |
| AML 2017:11 (Transaction Monitoring) | TRN-BR-003, TRN-BR-004, TRN-BR-005, TRN-BR-006, TRN-BR-009 |
| DORA Art. 11 (ICT Risk Management) | TRN-BR-005, TRN-BR-006, TRN-BR-009 |

## Migration Considerations

1. **VSAM to SQL mapping**: Transaction records (TRANSACT file, 350 bytes) should be mapped to a normalized Azure SQL table. The 16-byte TRAN-ID key becomes a primary key; sequential ID generation should use a database sequence.
2. **CICS transaction to REST API**: Each CICS program (COTRN00C/01C/02C) maps to REST API endpoints. The COMMAREA data contract becomes the API request/response schema and session state.
3. **Pagination**: VSAM STARTBR/READNEXT keyset pagination should be mapped to SQL keyset pagination (not OFFSET/FETCH) for equivalent behavior and ordering guarantees.
4. **Financial precision**: Account balance fields use `S9(10)V99` (10 integer digits), transaction amount fields use `S9(09)V99` (9 integer digits). The migrated system must use `decimal(12,2)` for account fields and `decimal(11,2)` for transaction fields. Floating-point types are NOT acceptable.
5. **Batch pipeline**: The three-step JCL batch pipeline (CBTRN01C → CBTRN02C → CBTRN03C) should be migrated to Azure Functions with orchestration (Durable Functions or Logic Apps) preserving the sequential execution and SLA requirements.
6. **Character encoding**: EBCDIC to UTF-8 conversion required for all text fields; Swedish characters (Å, Ä, Ö) must be supported in merchant names and descriptions.
7. **Atomicity**: The COBOL batch has no rollback on mid-run failure. The migrated system should use database transactions to ensure atomicity of the post+balance-update+category-update sequence.
