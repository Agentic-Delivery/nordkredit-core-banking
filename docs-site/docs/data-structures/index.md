---
title: Data Structures
sidebar_position: 1
---

# Data Structures

COBOL copybook definitions and their mapped .NET equivalents.

## Documented Copybooks

### Account Management

| Copybook | Record Length | Key | Business Rule | Purpose |
|----------|-------------|-----|---------------|---------|
| [`CVACT01Y.cpy`](./CVACT01Y) | 300 bytes | ACCT-ID PIC 9(11) | [ACCT-BR-001](../business-rules/account-management/acct-br-001) | Account master record layout |
| [`CVACT02Y.cpy`](./CVACT02Y) | 150 bytes | CARD-NUM PIC X(16) | [ACCT-BR-009](../business-rules/account-management/acct-br-009) | Card record layout |
| [`CVACT03Y.cpy`](./CVACT03Y) | 50 bytes | XREF-CARD-NUM PIC X(16) | [ACCT-BR-009](../business-rules/account-management/acct-br-009) | Card-customer-account cross-reference |
| [`CVCRD01Y.cpy`](./cvcrd01y) | N/A (work area) | N/A | N/A | Card work area (AID keys, screen fields) |
| [`CVCUS01Y.cpy`](./cvcus01y) | 500 bytes | CUST-ID | N/A | Customer record |

### Card Management

| Copybook | Record Length | Key | Business Rule | Purpose |
|----------|-------------|-----|---------------|---------|
| `CVACT02Y.cpy` | 150 bytes | CARD-NUM PIC X(16) | [CARD-BR-009](../business-rules/card-management/card-br-009) | Card record layout |
| `CVACT03Y.cpy` | 50 bytes | XREF-CARD-NUM PIC X(16) | [CARD-BR-010](../business-rules/card-management/card-br-010) | Card cross-reference structure |
| `CVCRD01Y.cpy` | N/A | N/A | [CARD-BR-011](../business-rules/card-management/card-br-011) | Card work area (AID keys, screen fields) |

### Transactions

| Copybook | Record Length | Key | Business Rule | Purpose |
|----------|-------------|-----|---------------|---------|
| [`CVTRA01Y.cpy`](./cvtra01y) | 50 bytes | N/A | N/A | Transaction category balance record |
| [`CVTRA02Y.cpy`](./cvtra02y) | 50 bytes | N/A | N/A | Discount group record |
| [`CVTRA03Y.cpy`](./cvtra03y) | 60 bytes | N/A | N/A | Transaction type record |
| [`CVTRA04Y.cpy`](./cvtra04y) | 60 bytes | N/A | N/A | Transaction category record |
| [`CVTRA05Y.cpy`](./cvtra05y) | 350 bytes | N/A | N/A | Transaction record |
| [`CVTRA06Y.cpy`](./cvtra06y) | 350 bytes | N/A | N/A | Daily transaction record |
| [`CVTRA07Y.cpy`](./cvtra07y) | N/A (report) | N/A | N/A | Transaction report structures |

## BMS Screen Copybooks

BMS (Basic Mapping Support) copybooks define CICS 3270 terminal screen layouts. These are not database records -- they define the UI fields for the CardDemo online transaction screens. Each has input (AI) and output (AO) record variants.

| ID | Copybook | Title | Domain | Used By |
|----|----------|-------|--------|---------|
| [DS-BMS-ACTUP](./bms-coactup) | COACTUP.CPY | Account Update Screen | UI Screens | COACTUPC |
| [DS-BMS-ACTVW](./bms-coactvw) | COACTVW.CPY | Account View Screen | UI Screens | COACTVWC |
| [DS-BMS-ADM01](./bms-coadm01) | COADM01.CPY | Admin Menu Screen | UI Screens | COADM01C |
| [DS-BMS-BIL00](./bms-cobil00) | COBIL00.CPY | Bill Payment Screen | UI Screens | COBIL00C |
| [DS-BMS-CRDLI](./bms-cocrdli) | COCRDLI.CPY | Card List Screen | UI Screens | COCRDLIC |
| [DS-BMS-CRDSL](./bms-cocrdsl) | COCRDSL.CPY | Card Select/View Screen | UI Screens | COCRDSLC |
| [DS-BMS-CRDUP](./bms-cocrdup) | COCRDUP.CPY | Card Update Screen | UI Screens | COCRDUPC |
| [DS-BMS-MEN01](./bms-comen01) | COMEN01.CPY | Main Menu Screen | UI Screens | COMEN01C |
| [DS-BMS-RPT00](./bms-corpt00) | CORPT00.CPY | Report Request Screen | UI Screens | CORPT00C |
| [DS-BMS-SGN00](./bms-cosgn00) | COSGN00.CPY | Sign-On Screen | UI Screens | COSGN00C |
| [DS-BMS-TRN00](./bms-cotrn00) | COTRN00.CPY | Transaction List Screen | UI Screens | COTRN00C |
| [DS-BMS-TRN01](./bms-cotrn01) | COTRN01.CPY | Transaction View Screen | UI Screens | COTRN01C |
| [DS-BMS-TRN02](./bms-cotrn02) | COTRN02.CPY | Transaction Add Screen | UI Screens | COTRN02C |
| [DS-BMS-USR00](./bms-cousr00) | COUSR00.CPY | User List Screen | UI Screens | COUSR00C |
| [DS-BMS-USR01](./bms-cousr01) | COUSR01.CPY | User Add Screen | UI Screens | COUSR01C |
| [DS-BMS-USR02](./bms-cousr02) | COUSR02.CPY | User Update Screen | UI Screens | COUSR02C |
| [DS-BMS-USR03](./bms-cousr03) | COUSR03.CPY | User Delete Screen | UI Screens | COUSR03C |

## Coverage

### Data Record Copybooks

Each documented data record copybook includes:

- COBOL copybook field definitions with PIC clauses and offsets
- Field-by-field descriptions and notes
- EBCDIC-to-Unicode conversion rules
- Referential integrity mapping
- Target Azure SQL DDL (CREATE TABLE)
- Data quality validation rules
- Post-migration validation queries
- Compliance considerations (GDPR, PCI-DSS, AML/KYC, FSA)

### BMS Screen Copybooks

Each documented BMS screen copybook includes:

- Screen purpose and user interaction overview
- Key screen field definitions (excluding BMS attribute bytes)
- Screen layout notes and workflow description
- Target architecture mapping (CICS/3270 to .NET web UI)
- Migration considerations (compliance, PCI-DSS, GDPR, UX improvements)
