---
title: User/Security
sidebar_position: 4
---

# User/Security Business Rules

Business rules for authentication, authorization, session management, data isolation, and audit trails extracted from COBOL source programs in the AWS CardDemo application.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COCRDLIC.cbl` | CICS Online | Credit card list with pagination and filtering | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-007, SEC-BR-008 |
| `COCRDSLC.cbl` | CICS Online | Credit card detail view/select | SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-007, SEC-BR-008 |
| `COCRDUPC.cbl` | CICS Online | Credit card update | SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-006, SEC-BR-007, SEC-BR-008 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `COCOM01Y.cpy` (not in workspace) | COMMAREA structure — session context contract | SEC-BR-001, SEC-BR-003, SEC-BR-005 |
| `CSUSR01Y.cpy` (not in workspace) | Signed-on user data — authenticated user identity and attributes | SEC-BR-001, SEC-BR-003, SEC-BR-005 |
| `CVCRD01Y.cpy` | Card work area (AID keys, screen fields, program navigation) | SEC-BR-004, SEC-BR-008 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [SEC-BR-001](./sec-br-001) | Role-based access control (admin vs regular user) | Critical | COCRDLIC.cbl | PSD2, GDPR, FFFS 2014:5 |
| [SEC-BR-002](./sec-br-002) | Account-level data isolation and filtering | Critical | COCRDLIC.cbl | GDPR, PSD2, FFFS 2014:5 |
| [SEC-BR-003](./sec-br-003) | User session context management via COMMAREA | High | Shared (all 3 programs) | PSD2, FFFS 2014:5, GDPR, DORA |
| [SEC-BR-004](./sec-br-004) | Function key validation and authorized actions | High | Shared (all 3 programs) | PSD2, FFFS 2014:5 |
| [SEC-BR-005](./sec-br-005) | CICS terminal authentication and PSD2 SCA mapping | Critical | Shared (all 3 programs) | PSD2, FFFS 2014:5, DORA |
| [SEC-BR-006](./sec-br-006) | Data modification authorization and confirmation workflow | Critical | COCRDUPC.cbl | PSD2, FFFS 2014:5 |
| [SEC-BR-007](./sec-br-007) | Abend handling and secure error management | High | Shared (all 3 programs) | FFFS 2014:5, DORA, GDPR |
| [SEC-BR-008](./sec-br-008) | Navigation audit trail and program flow tracking | High | Shared (all 3 programs) | DORA, FFFS 2014:5, GDPR, PSD2 |

## Status

All 8 business rules have been extracted from the COBOL source. All rules have `status: extracted` and are awaiting domain expert validation.

### Critical Gaps Identified

- **COCOM01Y.cpy** (COMMAREA structure): Referenced in all programs but not in workspace. Defines `CARDDEMO-COMMAREA` including `CDEMO-USRTYP-USER` flag. Must be obtained from mainframe team.
- **CSUSR01Y.cpy** (Signed-on user data): Referenced in all programs but not in workspace. Contains authenticated user identity and attributes populated by CICS CESN sign-on. Must be obtained from mainframe team.
- **Sign-on program**: The authentication entry point (e.g., COSGN00C) is not in the workspace. Current programs delegate authentication entirely to CICS/RACF.
- **PSD2 SCA gap**: Current mainframe uses single-factor authentication (password only via CICS CESN). Migration to Azure AD B2C must add a second factor to comply with PSD2 Art. 97.

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| PSD2 Art. 97 (Strong Customer Authentication) | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-006, SEC-BR-008 |
| PSD2 Art. 98 (Dynamic Linking) | SEC-BR-005, SEC-BR-006 |
| GDPR Art. 5(1)(c) (Data Minimization) | SEC-BR-002 |
| GDPR Art. 5(1)(f) (Integrity & Confidentiality) | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-007 |
| GDPR Art. 5(2) (Accountability) | SEC-BR-008 |
| GDPR Art. 25 (Data Protection by Design) | SEC-BR-001, SEC-BR-002 |
| FFFS 2014:5 Ch. 8 §4 (Internal Controls) | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-006, SEC-BR-007, SEC-BR-008 |
| FFFS 2014:5 Ch. 4 §3 (Operational Risk) | SEC-BR-004, SEC-BR-006, SEC-BR-007 |
| DORA Art. 9 (Strong Authentication) | SEC-BR-005 |
| DORA Art. 11 (ICT Risk Management) | SEC-BR-003, SEC-BR-007, SEC-BR-008 |

## Migration Considerations

1. **CICS authentication to Azure AD**: CICS terminal authentication (CESN/RACF) maps to Azure AD B2C for customers and Azure AD for internal users. The single-factor CESN sign-on must be enhanced with a second factor (MFA) to comply with PSD2 SCA requirements.
2. **COMMAREA to session/token**: The CICS COMMAREA session mechanism (carrying user identity, role, and context across pseudo-conversational returns) maps to JWT tokens or server-side session state in the REST API layer.
3. **RBAC model**: The two-tier admin/user role model should be mapped to Azure AD roles with potential expansion to more granular roles (viewer, operator, admin, auditor) as identified by domain experts.
4. **Function key authorization to API authorization**: CICS function key validation (PF3, PF5, PF7, PF8, ENTER) maps to HTTP method + endpoint authorization. State-gated operations (e.g., PF5 save only when changes are confirmed) become API-level state machine checks.
5. **Abend handling to exception middleware**: CICS HANDLE ABEND maps to ASP.NET Core exception handling middleware with structured error responses. Error messages must not leak internal system details (GDPR, DORA compliance).
6. **Audit trail**: COMMAREA-based navigation tracking (FROM-PROGRAM, FROM-TRANID) maps to application-level audit logging with Application Insights, capturing user identity, action, timestamp, and correlation IDs.
7. **Optimistic concurrency**: The confirm-before-write pattern with re-read verification (COCRDUPC) maps to ETag-based HTTP concurrency or SQL rowversion columns.
