---
id: "sec-br-005"
title: "CICS terminal authentication and PSD2 SCA mapping"
domain: "user-security"
cobol_source: "COCRDLIC.cbl:284-285,COCRDSLC.cbl:226-227,COCRDUPC.cbl:345-346,COCRDLIC.cbl:315-321"
requirement_id: "SEC-BR-005"
regulations:
  - "PSD2 Art. 97"
  - "PSD2 Art. 98"
  - "FFFS 2014:5 Ch. 8 §4"
  - "DORA Art. 9"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# SEC-BR-005: CICS terminal authentication and PSD2 SCA mapping

## Summary

Authentication in the CardDemo system is delegated to the CICS terminal sign-on infrastructure, not implemented within the application programs. All three COBOL programs include the `CSUSR01Y.cpy` copybook (signed-on user data structure), which references user identity data set during the CICS CESN (sign-on) process. The application assumes the user is already authenticated when it receives control — no login logic exists in the extracted programs. This is the equivalent of delegated authentication (CICS terminal security → RACF/ESM), which must be mapped to modern identity providers (Azure AD B2C for customers, Azure AD for internal users) with PSD2 Strong Customer Authentication (SCA) for the migrated system.

## Business Logic

### Pseudocode

```
AUTHENTICATION FLOW (CICS infrastructure, not in application code):
    1. User connects to CICS terminal
    2. CICS presents CESN sign-on screen
    3. User provides user-id and password
    4. CICS delegates to External Security Manager (RACF/ACF2/TopSecret)
    5. ESM validates credentials against security database
    6. If valid: CICS establishes session, populates EIBUSERID, sets security context
    7. If invalid: CICS rejects sign-on, no application code is reached
    8. User selects transaction from menu (e.g., CCLI for card list)
    9. CICS checks transaction-level security (RACF profile for CCLI)
    10. If authorized: application program receives control with user context
    11. If not authorized: CICS rejects with security violation (no app code reached)

APPLICATION BEHAVIOR (what the COBOL programs do):
    -- Include signed-on user data structure
    COPY CSUSR01Y.       -- Contains user ID, user type, session data

    -- On first entry, default to regular user
    SET CDEMO-USRTYP-USER TO TRUE

    -- Application NEVER prompts for credentials
    -- Application NEVER validates passwords
    -- Application relies entirely on CICS-established security context
```

### Authentication Layer Mapping (CICS → Azure)

| CICS Component | Function | Azure Target |
|----------------|----------|-------------|
| CESN (sign-on transaction) | User authentication | Azure AD B2C sign-in flow (customer) / Azure AD (internal) |
| RACF/ACF2/TopSecret | Credential validation and security profiles | Azure AD identity store + conditional access policies |
| EIBUSERID | Authenticated user identity | JWT claims (sub, email) |
| Transaction-level security | Authorization by transaction code | API endpoint authorization via Azure AD roles/scopes |
| CSUSR01Y copybook | Signed-on user data structure | Claims principal / user context object |
| CICS terminal session | Session management | OAuth 2.0 access tokens + refresh tokens |
| CDEMO-USRTYP-USER | Application-level role flag | Azure AD app roles (Admin, User) |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 284-285 (Signed-on user data inclusion)

```cobol
000284 *Signed on user data
000285  COPY CSUSR01Y.
```

**Program:** `COCRDSLC.cbl`
**Lines:** 226-227 (Signed-on user data inclusion)

```cobol
000226 *Signed on user data
000227  COPY CSUSR01Y.
```

**Program:** `COCRDUPC.cbl`
**Lines:** 345-346 (Signed-on user data inclusion)

```cobol
000345 *Signed on user data
000346  COPY CSUSR01Y.
```

**Program:** `COCRDLIC.cbl`
**Lines:** 315-321 (Default user type — application assumes authentication is done)

```cobol
000315      IF EIBCALEN = 0
000316         INITIALIZE CARDDEMO-COMMAREA
000317                    WS-THIS-PROGCOMMAREA
000318         MOVE LIT-THISTRANID        TO CDEMO-FROM-TRANID
000319         MOVE LIT-THISPGM           TO CDEMO-FROM-PROGRAM
000320         SET CDEMO-USRTYP-USER      TO TRUE
000321         SET CDEMO-PGM-ENTER        TO TRUE
```

### Key Observation

The `CSUSR01Y.cpy` copybook is not in the workspace but is referenced by all three programs. It defines the data structure that CICS populates with the signed-on user's identity after successful authentication. The actual authentication logic resides in the CICS infrastructure layer (CESN transaction + ESM), not in the application.

## Acceptance Criteria

### Scenario 1: Authenticated user accesses card list

```gherkin
GIVEN a user has completed CICS sign-on via CESN
  AND the External Security Manager has validated their credentials
  AND the user is authorized for transaction CCLI (card list)
WHEN the user initiates the CCLI transaction
THEN the COCRDLIC program receives control
  AND CSUSR01Y data structure contains the authenticated user's identity
  AND no additional authentication prompt is displayed
```

### Scenario 2: Unauthenticated access is blocked by CICS

```gherkin
GIVEN a CICS terminal has not completed sign-on
WHEN a transaction code is entered
THEN CICS rejects the request before the application program is invoked
  AND no application code is executed
  AND the user is redirected to the CESN sign-on screen
```

### Scenario 3: User not authorized for transaction is blocked

```gherkin
GIVEN a user has completed CICS sign-on
  BUT the user's RACF profile does not include authorization for transaction CCLI
WHEN the user enters the CCLI transaction code
THEN CICS rejects with a security violation
  AND no application code is executed
  AND the rejection is logged by the security manager
```

### Scenario 4: PSD2 SCA required for payment account access

```gherkin
GIVEN the migrated system replaces CICS terminal authentication
WHEN a customer accesses card data via the REST API
THEN Azure AD B2C enforces Strong Customer Authentication
  AND at least two of three SCA factors are verified:
    | Factor | CICS Equivalent | Azure Target |
    | Knowledge (password) | CESN password | Azure AD B2C password |
    | Possession (device) | Physical terminal | MFA (authenticator app, SMS) |
    | Inherence (biometric) | N/A in mainframe | Azure AD B2C biometric |
```

### Scenario 5: Session timeout enforced for PSD2 compliance

```gherkin
GIVEN a user has authenticated and accessed card data
WHEN 5 minutes of inactivity have elapsed (PSD2 requirement)
THEN the session is invalidated
  AND the user must re-authenticate to continue
  AND the CICS equivalent (transaction timeout) is replaced by OAuth token expiry
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Payment service providers shall apply strong customer authentication where the payer accesses its payment account online | CICS terminal authentication (CESN + ESM) provides single-factor authentication (password); migrated system MUST add second factor for PSD2 SCA compliance |
| PSD2 | Art. 98 | Regulatory technical standards for authentication including dynamic linking for payment transactions | The current COBOL system has no dynamic linking for transaction authorization; migrated system must implement transaction-specific authorization codes |
| FFFS 2014:5 | Ch. 8 §4 | Credit institutions must have systems to authenticate users and protect information | CICS-level authentication ensures only authenticated users reach application programs; this delegation pattern maps to Azure AD middleware in the migrated system |
| DORA | Art. 9 | Financial entities shall use strong authentication mechanisms and implement access control policies | RACF-based security profiles enforce transaction-level authorization; migrated system must replicate this with Azure AD role-based access control |

## Edge Cases

1. **No password complexity in application code**: Password policies (length, complexity, expiration, lockout) are entirely managed by the ESM (RACF/ACF2). The COBOL programs have no visibility into credential management. The migrated system must configure Azure AD B2C password policies to match or exceed the current RACF policies.

2. **Single-factor authentication gap**: The current system uses password-only authentication (CICS CESN). PSD2 Art. 97 requires two-factor authentication for accessing payment account data online. The migration MUST add a second authentication factor (possession or inherence) to achieve SCA compliance.

3. **Terminal-based session binding**: CICS sessions are bound to a physical terminal (EIBTRMID). A user cannot access the same session from a different terminal. The migrated system should implement device binding or token binding to replicate this security property.

4. **CSUSR01Y copybook missing**: The signed-on user data structure is not available in the workspace. This is critical for understanding what user attributes are available to the application after authentication. The production CSUSR01Y copybook must be obtained from the mainframe team to complete the identity mapping.

5. **No multi-tenant isolation**: The mainframe system serves a single organization (NordKredit). The migrated system on Azure may need to support multi-tenancy if the platform is shared. Tenant isolation must be considered in the Azure AD B2C configuration.

## Domain Expert Notes

- **null** — Awaiting domain expert review. Key questions: (1) What External Security Manager is used in production — RACF, ACF2, or TopSecret? (2) What are the current RACF password policies (length, complexity, expiration, lockout thresholds)? (3) Which CICS transaction codes are defined in the security profiles, and what user groups have access to each? (4) Is there a CICS security exit that intercepts sign-on events for audit logging? (5) Has NordKredit already implemented PSD2 SCA for its online banking channels, and if so, what second factor is used?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
