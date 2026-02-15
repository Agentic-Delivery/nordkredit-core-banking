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
| [`CVEXPORT.cpy`](./cvexport) | 500 bytes | N/A | N/A | Export record (multi-format data export) |

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

## Utility / Common / System Copybooks

Utility and system-level copybooks that provide shared data structures, validation tables, UI constants, and infrastructure patterns used across the CardDemo application. These are generally not database records -- they define session state, configuration, error handling, and other cross-cutting concerns.

| ID | Copybook | Title | Domain | Target |
|----|----------|-------|--------|--------|
| [DS-COMM-001](./cocom01y) | COCOM01Y.cpy | Common Communication Area | System | N/A (session state) |
| [DS-ADMN-001](./coadm02y) | COADM02Y.cpy | Admin Menu Options | System | N/A (configuration) |
| [DS-MENU-001](./comen02y) | COMEN02Y.cpy | Main Menu Options | System | N/A (configuration) |
| [DS-STMT-001](./costm01) | COSTM01.CPY | Statement/Transaction Reporting Layout | Transactions | N/A (report view) |
| [DS-TITL-001](./cottl01y) | COTTL01Y.cpy | Screen Title | System | N/A (UI constant) |
| [DS-DATE-001](./csdat01y) | CSDAT01Y.cpy | Date/Time Structure | System | N/A (utility) |
| [DS-LKUP-001](./cslkpcdy) | CSLKPCDY.cpy | Lookup Code Repository | System | N/A (validation rules) |
| [DS-MSG1-001](./csmsg01y) | CSMSG01Y.cpy | Common Messages | System | N/A (UI messages) |
| [DS-MSG2-001](./csmsg02y) | CSMSG02Y.cpy | Abend/Error Message Structure | System | N/A (error handling) |
| [DS-SETA-001](./cssetaty) | CSSETATY.cpy | Set Attribute Macro | System | N/A (UI pattern) |
| [DS-STRP-001](./csstrpfy) | CSSTRPFY.cpy | Store PFKey Utility | System | N/A (input handling) |
| [DS-USEC-001](./csusr01y) | CSUSR01Y.cpy | User Security Record | User Security | dbo.UserSecurity / Azure AD |
| [DS-UNUSED-001](./unused1y) | UNUSED1Y.cpy | Unused/Deprecated | System | N/A (deprecated -- do NOT migrate) |

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

### Utility / Common / System Copybooks

Each documented utility/system copybook includes:

- COBOL source with field definitions
- Purpose and usage context across the application
- Target architecture mapping (CICS patterns to .NET equivalents)
- Migration notes (what to migrate, what to replace, what to discard)
- Compliance considerations (GDPR, PCI-DSS, FSA, DORA) where applicable
