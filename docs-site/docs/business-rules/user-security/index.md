---
title: User/Security
sidebar_position: 4
---

# User/Security Business Rules

Business rules for authentication, authorization, session management, data isolation, and audit trails extracted from COBOL source programs in the AWS CardDemo application.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COCRDLIC.cbl` | CICS Online | Credit card list with pagination and filtering | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-007, SEC-BR-008, SEC-BR-009 |
| `COCRDSLC.cbl` | CICS Online | Credit card detail view/select | SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-007, SEC-BR-008, SEC-BR-009 |
| `COCRDUPC.cbl` | CICS Online | Credit card update | SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-006, SEC-BR-007, SEC-BR-008, SEC-BR-009 |
| `COTRN00C.cbl` | CICS Online | Transaction list | SEC-BR-010 |
| `COTRN01C.cbl` | CICS Online | Transaction detail | SEC-BR-010 |
| `COTRN02C.cbl` | CICS Online | Transaction add | SEC-BR-010 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `COCOM01Y.cpy` | COMMAREA structure — session context contract with user identity fields (`CDEMO-USER-ID`, `CDEMO-USER-TYPE`) | SEC-BR-001, SEC-BR-003, SEC-BR-005, SEC-BR-009, SEC-BR-010 |
| `CSUSR01Y.cpy` | Signed-on user data — user record with ID, name, password (`SEC-USR-PWD`), and type | SEC-BR-001, SEC-BR-003, SEC-BR-005, SEC-BR-009, SEC-BR-010 |
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
| [SEC-BR-009](./sec-br-009) | Password storage and credential management | Critical | CSUSR01Y.cpy | PSD2, GDPR, FFFS 2014:5, DORA |
| [SEC-BR-010](./sec-br-010) | User identity propagation from sign-on to application | High | COCOM01Y.cpy, CSUSR01Y.cpy, COTRN*.cbl | PSD2, GDPR, FFFS 2014:5, DORA |

## Status

10 business rules extracted from COBOL source (SEC-BR-001 through SEC-BR-010). All rules have `status: extracted` and are awaiting domain expert validation.

### Resolved Gaps

- **COCOM01Y.cpy** (COMMAREA structure): Now available in `docs/cobol-source/cpy/COCOM01Y.cpy`. Confirmed `CARDDEMO-COMMAREA` structure with `CDEMO-USER-ID` (PIC X(08)), `CDEMO-USER-TYPE` (PIC X(01), 'A'=admin, 'U'=user), customer info (name, ID), account/card context, and navigation tracking fields. SEC-BR-001, SEC-BR-003, SEC-BR-005 updated with actual field definitions.
- **CSUSR01Y.cpy** (Signed-on user data): Now available in `docs/cobol-source/cpy/CSUSR01Y.cpy`. Confirmed `SEC-USER-DATA` structure with `SEC-USR-ID` (PIC X(08)), `SEC-USR-FNAME`/`SEC-USR-LNAME`, `SEC-USR-PWD` (PIC X(08), plain-text password), `SEC-USR-TYPE` (PIC X(01)). Two new rules extracted: SEC-BR-009 (password storage) and SEC-BR-010 (identity propagation).

### Remaining Gaps

- **Sign-on program (COSGN00C)**: Not in the workspace. Referenced by COTRN00C, COTRN01C, and COTRN02C as the authentication entry point and session recovery fallback. Authentication logic (password validation method, session establishment) cannot be verified without this program. Should be obtained from mainframe team if available.
- **PSD2 SCA gap**: Current mainframe uses single-factor authentication (password only, with 8-character limit per SEC-USR-PWD). Migration to Azure AD B2C must add a second factor (MFA) and enforce modern password policies to comply with PSD2 Art. 97. Plain-text password storage (SEC-BR-009) must be replaced with hashed credentials.
- **Password migration**: Plain-text passwords in SEC-USR-PWD cannot be migrated to Azure AD B2C's hashed store. All users will need to reset passwords on first login to the migrated system.

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| PSD2 Art. 97 (Strong Customer Authentication) | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-006, SEC-BR-008, SEC-BR-009, SEC-BR-010 |
| PSD2 Art. 98 (Dynamic Linking) | SEC-BR-005, SEC-BR-006 |
| GDPR Art. 5(1)(c) (Data Minimization) | SEC-BR-002 |
| GDPR Art. 5(1)(f) (Integrity & Confidentiality) | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-007, SEC-BR-009 |
| GDPR Art. 5(2) (Accountability) | SEC-BR-008 |
| GDPR Art. 25 (Data Protection by Design) | SEC-BR-001, SEC-BR-002, SEC-BR-010 |
| GDPR Art. 32 (Security of Processing) | SEC-BR-009 |
| FFFS 2014:5 Ch. 8 §4 (Internal Controls) | SEC-BR-001, SEC-BR-002, SEC-BR-003, SEC-BR-004, SEC-BR-005, SEC-BR-006, SEC-BR-007, SEC-BR-008, SEC-BR-009, SEC-BR-010 |
| FFFS 2014:5 Ch. 4 §3 (Operational Risk) | SEC-BR-004, SEC-BR-006, SEC-BR-007 |
| DORA Art. 9 (Strong Authentication) | SEC-BR-005, SEC-BR-009, SEC-BR-010 |
| DORA Art. 11 (ICT Risk Management) | SEC-BR-003, SEC-BR-007, SEC-BR-008 |

## Migration Considerations

1. **CICS authentication to Azure AD**: CICS terminal authentication (CESN/RACF) maps to Azure AD B2C for customers and Azure AD for internal users. The single-factor CESN sign-on must be enhanced with a second factor (MFA) to comply with PSD2 SCA requirements.
2. **COMMAREA to session/token**: The CICS COMMAREA session mechanism (carrying user identity, role, and context across pseudo-conversational returns) maps to JWT tokens or server-side session state in the REST API layer.
3. **RBAC model**: The two-tier admin/user role model should be mapped to Azure AD roles with potential expansion to more granular roles (viewer, operator, admin, auditor) as identified by domain experts.
4. **Function key authorization to API authorization**: CICS function key validation (PF3, PF5, PF7, PF8, ENTER) maps to HTTP method + endpoint authorization. State-gated operations (e.g., PF5 save only when changes are confirmed) become API-level state machine checks.
5. **Abend handling to exception middleware**: CICS HANDLE ABEND maps to ASP.NET Core exception handling middleware with structured error responses. Error messages must not leak internal system details (GDPR, DORA compliance).
6. **Audit trail**: COMMAREA-based navigation tracking (FROM-PROGRAM, FROM-TRANID) maps to application-level audit logging with Application Insights, capturing user identity, action, timestamp, and correlation IDs.
7. **Optimistic concurrency**: The confirm-before-write pattern with re-read verification (COCRDUPC) maps to ETag-based HTTP concurrency or SQL rowversion columns.
8. **Password migration**: Passwords are stored as plain text in `SEC-USR-PWD` (8-character PIC X field). These cannot be migrated to Azure AD B2C's hashed credential store. All users must reset passwords on first login. Consider a staged migration: (a) import user profiles (ID, name, type) to Azure AD B2C, (b) trigger password reset flow on first login, (c) enforce new password policy (12+ chars, complexity, MFA).
9. **User identity mapping**: Legacy `SEC-USR-ID` / `CDEMO-USER-ID` (8-character EBCDIC) must be mapped to Azure AD object IDs (GUID format). A mapping table is required for the parallel-run period to correlate mainframe and Azure AD sessions.
10. **Identity propagation trust model**: The mainframe uses a trust-once model (COSGN00C authenticates, downstream programs trust COMMAREA). The migrated system must validate JWT tokens on every API request (zero-trust model). This is a fundamental architectural change — ASP.NET Core authentication middleware replaces the COMMAREA trust chain.
