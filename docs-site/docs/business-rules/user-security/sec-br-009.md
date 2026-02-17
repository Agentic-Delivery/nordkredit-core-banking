---
id: "sec-br-009"
title: "Password storage and credential management"
domain: "user-security"
cobol_source: "CSUSR01Y.cpy:17-23,COCRDLIC.cbl:284-285,COCRDSLC.cbl:226-227,COCRDUPC.cbl:345-346"
requirement_id: "SEC-BR-009"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 32"
  - "GDPR Art. 5(1)(f)"
  - "FFFS 2014:5 Ch. 8 §4"
  - "DORA Art. 9"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# SEC-BR-009: Password storage and credential management

## Summary

The CSUSR01Y copybook defines the user authentication data structure (`SEC-USER-DATA`) used by the CardDemo sign-on program (COSGN00C). This structure reveals that passwords are stored as plain-text 8-character fields (`SEC-USR-PWD PIC X(08)`) in the user data record. All three card management programs include this copybook, meaning the password field is accessible in working storage during program execution. The 8-character limit and plain-text storage represent the legacy mainframe credential management approach, which must be completely replaced during migration to meet modern security standards and PSD2 SCA requirements.

## Business Logic

### Pseudocode

```
USER DATA RECORD STRUCTURE (CSUSR01Y.cpy):
    RECORD SEC-USER-DATA:
        user-id        : CHAR(8)    -- Unique user identifier
        first-name     : CHAR(20)   -- User's first name (PII)
        last-name      : CHAR(20)   -- User's last name (PII)
        password       : CHAR(8)    -- Plain-text password (CRITICAL)
        user-type      : CHAR(1)    -- 'A' = admin, 'U' = regular user
        filler         : CHAR(23)   -- Reserved space

SIGN-ON PROCESS (inferred from COSGN00C references):
    1. User enters user-id and password at CICS terminal
    2. COSGN00C reads SEC-USER-DATA record for the given user-id
    3. Compare entered password with SEC-USR-PWD
    4. IF match:
        SET CDEMO-USER-ID = SEC-USR-ID
        SET CDEMO-USER-TYPE = SEC-USR-TYPE
        TRANSFER to menu program with populated COMMAREA
    5. IF no match:
        Display error, allow retry

PASSWORD CONSTRAINTS (from data structure):
    - Maximum length: 8 characters (PIC X(08))
    - Character set: Any EBCDIC character (PIC X)
    - No complexity enforcement visible in data structure
    - No password history or expiration fields in the record
    - No failed attempt counter in the record
    - No account lockout flag in the record
```

### Credential Data Flow

```
[User Data File] → SEC-USER-DATA (CSUSR01Y)
    ↓
[COSGN00C - Sign-on Program]
    ↓ (validates SEC-USR-PWD against entered password)
    ↓ (copies SEC-USR-ID → CDEMO-USER-ID)
    ↓ (copies SEC-USR-TYPE → CDEMO-USER-TYPE)
    ↓
[COMMAREA - COCOM01Y] → All application programs
    ↓
[COCRDLIC / COCRDSLC / COCRDUPC]
    (SEC-USER-DATA still in WORKING-STORAGE via COPY CSUSR01Y)
```

### Password Policy Gap Analysis

| Aspect | Current (Mainframe) | Required (PSD2/DORA) | Migration Target |
|--------|-------------------|---------------------|-----------------|
| Storage | Plain text (PIC X(08)) | Hashed with salt | Azure AD B2C (bcrypt/PBKDF2) |
| Max length | 8 characters | No practical limit | Azure AD B2C (256 chars) |
| Complexity | Not enforced in data structure | Required (upper, lower, digit, special) | Azure AD B2C custom policy |
| Expiration | Not tracked in record | Required (90-day recommendation) | Azure AD conditional access |
| History | Not tracked | Required (prevent reuse) | Azure AD B2C password history |
| Lockout | Not in record (may be ESM-level) | Required after N failures | Azure AD B2C smart lockout |
| MFA | Not present (single factor) | Required for SCA (PSD2 Art. 97) | Azure AD B2C MFA |

## Source COBOL Reference

**Copybook:** `CSUSR01Y.cpy`
**Lines:** 17-23 (Complete user data structure)

```cobol
 01 SEC-USER-DATA.
    05 SEC-USR-ID                 PIC X(08).
    05 SEC-USR-FNAME              PIC X(20).
    05 SEC-USR-LNAME              PIC X(20).
    05 SEC-USR-PWD                PIC X(08).
    05 SEC-USR-TYPE               PIC X(01).
    05 SEC-USR-FILLER             PIC X(23).
```

**Program:** `COCRDLIC.cbl`
**Lines:** 284-285 (Copybook inclusion — password accessible in working storage)

```cobol
000284 *Signed on user data
000285  COPY CSUSR01Y.
```

**Program:** `COCRDSLC.cbl`
**Lines:** 226-227 (Copybook inclusion)

```cobol
000226 *Signed on user data
000227  COPY CSUSR01Y.
```

**Program:** `COCRDUPC.cbl`
**Lines:** 345-346 (Copybook inclusion)

```cobol
000345 *Signed on user data
000346  COPY CSUSR01Y.
```

### Sign-on Program References (COSGN00C)

COSGN00C is referenced as the fallback return destination in multiple programs, confirming its role as the authentication entry point:

```cobol
* COTRN00C.cbl:108 — Return to sign-on when no COMMAREA
  MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM

* COTRN01C.cbl:95 — Return to sign-on when no COMMAREA
  MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM

* COTRN02C.cbl:116 — Return to sign-on when no COMMAREA
  MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
```

This pattern confirms: when a program detects no session context (EIBCALEN = 0) or no return target, it routes back to COSGN00C for re-authentication.

## Acceptance Criteria

### Scenario 1: Password validates against user record

```gherkin
GIVEN a user enters their user-id and password at the CICS sign-on screen
  AND a SEC-USER-DATA record exists for the given user-id
WHEN the sign-on program compares the entered password to SEC-USR-PWD
THEN if the passwords match, the user is authenticated
  AND CDEMO-USER-ID is set to SEC-USR-ID
  AND CDEMO-USER-TYPE is set to SEC-USR-TYPE
  AND control transfers to the menu program
```

### Scenario 2: Invalid password is rejected

```gherkin
GIVEN a user enters a valid user-id but incorrect password
WHEN the sign-on program compares the entered password to SEC-USR-PWD
THEN the authentication fails
  AND the user is prompted to re-enter credentials
  AND the failed attempt is handled (lockout policy may apply at ESM level)
```

### Scenario 3: Password field accessible in all programs

```gherkin
GIVEN any card management program includes COPY CSUSR01Y
WHEN the copybook is expanded during compilation
THEN SEC-USR-PWD is allocated in the program's WORKING-STORAGE
  AND the password field is technically accessible (though not used by the card programs)
  AND this represents a data exposure risk in the current architecture
```

### Scenario 4: Migrated system enforces modern password policy

```gherkin
GIVEN the system is migrated from COBOL to .NET 8 on Azure
WHEN user authentication is handled by Azure AD B2C
THEN passwords are stored using industry-standard hashing (bcrypt/PBKDF2)
  AND passwords support at least 12 characters minimum
  AND complexity requirements are enforced (upper, lower, digit, special)
  AND password history prevents reuse of recent passwords
  AND account lockout occurs after consecutive failed attempts
```

### Scenario 5: Password migration requires forced reset

```gherkin
GIVEN existing user passwords are stored as plain text in SEC-USR-PWD
WHEN the system migrates to Azure AD B2C
THEN plain-text passwords CANNOT be migrated to the hashed store
  AND all users must reset their passwords on first login to the new system
  AND a secure password reset flow is provided (email/SMS verification)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication including knowledge element (password) | Current system uses password-only authentication; SEC-USR-PWD is the knowledge factor. Migration must add possession/inherence factor for SCA |
| GDPR | Art. 32 | Security of processing — implement appropriate technical measures including encryption and pseudonymization | Plain-text password storage in SEC-USR-PWD violates the principle of appropriate security measures; migration must use hashed passwords |
| GDPR | Art. 5(1)(f) | Integrity and confidentiality — appropriate security of personal data | User names (SEC-USR-FNAME, SEC-USR-LNAME) and passwords are stored without encryption in the same record; migration must separate and protect credentials |
| FFFS 2014:5 | Ch. 8 §4 | Credit institutions must have adequate internal controls including information security | Password storage, complexity, and lifecycle management are foundational security controls that must be documented and enforced |
| DORA | Art. 9 | Strong authentication mechanisms and access control policies | Password policy (length, complexity, expiration, lockout) must meet DORA requirements for ICT system access control |

## Edge Cases

1. **Plain-text password in WORKING-STORAGE**: Every program that includes `COPY CSUSR01Y` has `SEC-USR-PWD` allocated in WORKING-STORAGE. Even though the card management programs do not use this field, it is technically accessible. A malicious or buggy program modification could read or display passwords. The migrated system must never expose credential data to application code.

2. **8-character password limit**: `PIC X(08)` limits passwords to 8 characters. Modern security standards recommend minimum 12-character passwords. During migration, users must be forced to create longer passwords. Legacy 8-character passwords should not be accepted by the new system.

3. **No password hashing**: The `PIC X(08)` format and the apparent comparison pattern suggest plain-text comparison. There is no salt, hash algorithm, or key derivation function. A database breach would expose all user passwords. This is a critical vulnerability by modern standards.

4. **FILLER field**: The 23-byte `SEC-USR-FILLER` field suggests the record was designed for future extension. If the production system has evolved, additional fields (e.g., last login date, failed attempt count, password change date) may exist in this space. The domain expert should confirm the production record layout.

5. **No separation between identity and credential stores**: `SEC-USER-DATA` combines identity (ID, name, type) and credential (password) in a single record. Modern architecture separates these concerns — Azure AD B2C manages credentials independently from application user profiles.

## Domain Expert Notes

- **Copybook analysis complete** — CSUSR01Y confirms plain-text password storage in an 8-character field. The sign-on program COSGN00C is not in the workspace but is confirmed as the authentication entry point by references in COTRN00C, COTRN01C, and COTRN02C. **Questions for domain expert:** (1) Does COSGN00C perform direct password comparison against SEC-USR-PWD, or does it delegate to RACF/ESM? (2) Has the 23-byte FILLER in SEC-USER-DATA been used in production for additional fields (e.g., password expiry date, failed login count)? (3) How are new user accounts provisioned — is there an admin program that writes SEC-USER-DATA records? (4) What is the current password reset process on the mainframe? (5) Can the COSGN00C program be obtained for analysis?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
