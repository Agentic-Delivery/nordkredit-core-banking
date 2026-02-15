---
title: Card Management
sidebar_position: 2
---

# Card Management Business Rules

Business rules for card issuance, activation, status changes, card listing, and card-account relationships extracted from COBOL source programs in the AWS CardDemo application.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COCRDLIC.cbl` | CICS Online | Credit card list with pagination and filtering | CARD-BR-001, CARD-BR-002, CARD-BR-004 |
| `COCRDSLC.cbl` | CICS Online | Credit card detail view/select | CARD-BR-003, CARD-BR-004, CARD-BR-005 |
| `COCRDUPC.cbl` | CICS Online | Credit card update | CARD-BR-004, CARD-BR-005, CARD-BR-006, CARD-BR-007, CARD-BR-008 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVCRD01Y.cpy` | Card work area (AID keys, screen fields, program navigation) | CARD-BR-011 |
| `CVACT02Y.cpy` | Card record layout (150-byte VSAM record) | CARD-BR-009 |
| `CVACT03Y.cpy` | Card cross-reference (card-customer-account linkage) | CARD-BR-010 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [CARD-BR-001](./card-br-001) | Card list display with pagination and filtering | Medium | COCRDLIC.cbl | PSD2, GDPR |
| [CARD-BR-002](./card-br-002) | Card list selection validation | Medium | COCRDLIC.cbl | FFFS 2014:5, PSD2 |
| [CARD-BR-003](./card-br-003) | Card detail lookup by account and card number | Medium | COCRDSLC.cbl | PSD2, GDPR |
| [CARD-BR-004](./card-br-004) | Account number input validation | High | Shared (all 3 programs) | FFFS 2014:5 |
| [CARD-BR-005](./card-br-005) | Card number input validation | High | Shared (COCRDSLC, COCRDUPC) | FFFS 2014:5, PSD2 |
| [CARD-BR-006](./card-br-006) | Card update field validation rules | High | COCRDUPC.cbl | FFFS 2014:5, PSD2, GDPR |
| [CARD-BR-007](./card-br-007) | Card update workflow state machine | High | COCRDUPC.cbl | PSD2, FFFS 2014:5 |
| [CARD-BR-008](./card-br-008) | Optimistic concurrency control for card updates | High | COCRDUPC.cbl | FFFS 2014:5, PSD2 |
| [CARD-BR-009](./card-br-009) | Card record data structure and field definitions | High | CVACT02Y.cpy | GDPR, PSD2 |
| [CARD-BR-010](./card-br-010) | Card cross-reference structure | Medium | CVACT03Y.cpy | GDPR, AML/KYC |
| [CARD-BR-011](./card-br-011) | Inter-program navigation and COMMAREA data contract | Medium | All 3 programs + CVCRD01Y.cpy | FFFS 2014:5, PSD2 |

## Status

All 11 business rules have been extracted from the COBOL source. All rules have `status: extracted` and are awaiting domain expert validation.

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| PSD2 Art. 97 (Strong Customer Authentication) | CARD-BR-001, CARD-BR-002, CARD-BR-003, CARD-BR-005, CARD-BR-006, CARD-BR-007, CARD-BR-008, CARD-BR-009, CARD-BR-011 |
| GDPR Art. 5 (Data Principles) | CARD-BR-009, CARD-BR-010 |
| GDPR Art. 15 (Right of Access) | CARD-BR-001, CARD-BR-003 |
| FFFS 2014:5 Ch. 4 §3 (Operational Risk) | CARD-BR-004, CARD-BR-005, CARD-BR-006 |
| FFFS 2014:5 Ch. 8 §4 (Internal Controls) | CARD-BR-002, CARD-BR-007, CARD-BR-008, CARD-BR-011 |
| AML/KYC | CARD-BR-010 |

## Migration Considerations

1. **VSAM to SQL mapping**: Card records (CARDDAT) and cross-references should be mapped to normalized Azure SQL tables with proper foreign key relationships
2. **CICS transaction to REST API**: Each CICS program maps to one or more REST API endpoints; the COMMAREA data contract becomes the API request/response schema
3. **Pagination**: VSAM STARTBR/READNEXT keyset pagination should be mapped to SQL keyset pagination (not OFFSET/FETCH) for equivalent behavior
4. **Concurrency**: Optimistic concurrency via field comparison should be replaced with SQL rowversion/timestamp columns or ETag-based HTTP concurrency
5. **Character encoding**: EBCDIC to UTF-8 conversion required for all text fields; Swedish characters (Å, Ä, Ö) must be supported in the embossed name field
