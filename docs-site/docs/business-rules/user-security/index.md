---
title: "User/Security Domain — Business Rules"
domain: "user-security"
status: "extracted"
cobol_programs:
  - "COSGN00C.cbl"
  - "COADM01C.cbl"
  - "COUSR00C.cbl"
  - "COUSR01C.cbl"
  - "COUSR02C.cbl"
  - "COUSR03C.cbl"
copybooks:
  - "CSUSR01Y.cpy"
  - "COADM02Y.cpy"
  - "COMEN02Y.cpy"
---

# User/Security Domain — Business Rules

## Overview

The User/Security domain covers authentication, authorization, admin functions, and user lifecycle management (CRUD) in the CardDemo application. These programs manage access control for the entire system — no other program can be reached without first passing through the sign-on gate (COSGN00C).

**Regulatory scope**: PSD2 Strong Customer Authentication (SCA), GDPR (user data handling), DORA Art. 9 (ICT access control), FFFS 2014:5 Ch. 6 (IT security).

## COBOL Program Inventory

| Program | Transaction ID | Function | Business Rules |
|---------|---------------|----------|----------------|
| COSGN00C.cbl | CC00 | Sign-on / Authentication | USEC-BR-001, USEC-BR-002, USEC-BR-003 |
| COADM01C.cbl | CA00 | Admin Menu | USEC-BR-004 |
| COUSR00C.cbl | CU00 | User List | USEC-BR-005 |
| COUSR01C.cbl | CU01 | User Add | USEC-BR-006 |
| COUSR02C.cbl | CU02 | User Update | USEC-BR-007 |
| COUSR03C.cbl | CU03 | User Delete | USEC-BR-008 |

## Extracted Business Rules

| Rule ID | Title | Source Program | Priority | Status |
|---------|-------|---------------|----------|--------|
| [USEC-BR-001](USEC-BR-001.md) | User authentication via credential validation against USRSEC file | COSGN00C.cbl:108-257 | Critical | Extracted |
| [USEC-BR-002](USEC-BR-002.md) | User type-based authorization routing after authentication | COSGN00C.cbl:230-240 | Critical | Extracted |
| [USEC-BR-003](USEC-BR-003.md) | Session validation via COMMAREA presence check | COSGN00C.cbl:80-96 | Critical | Extracted |
| [USEC-BR-004](USEC-BR-004.md) | Admin menu access control and option routing | COADM01C.cbl:119-158 | High | Extracted |
| [USEC-BR-005](USEC-BR-005.md) | User list display with pagination from USRSEC file | COUSR00C.cbl:149-332 | High | Extracted |
| [USEC-BR-006](USEC-BR-006.md) | User creation with required field validation and duplicate detection | COUSR01C.cbl:115-274 | High | Extracted |
| [USEC-BR-007](USEC-BR-007.md) | User update with field-level change detection and rewrite | COUSR02C.cbl:115-274 | High | Extracted |
| [USEC-BR-008](USEC-BR-008.md) | User deletion with read-before-delete confirmation pattern | COUSR03C.cbl:115-250 | High | Extracted |

## Key Data Structures

### USRSEC File (CSUSR01Y.cpy)

The USRSEC VSAM KSDS file stores all user credentials and roles. Key structure:

| Field | PIC | Length | Description |
|-------|-----|--------|-------------|
| SEC-USR-ID | X(08) | 8 | User ID (primary key) |
| SEC-USR-FNAME | X(20) | 20 | First name |
| SEC-USR-LNAME | X(20) | 20 | Last name |
| SEC-USR-PWD | X(08) | 8 | Password (plaintext) |
| SEC-USR-TYPE | X(01) | 1 | User type ('A' = Admin, 'U' = User) |
| SEC-USR-FILLER | X(23) | 23 | Reserved filler |

**Total record length**: 80 bytes

### COMMAREA Session Context (COCOM01Y.cpy)

Session state carried between programs via CICS COMMAREA:

| Field | Description |
|-------|-------------|
| CDEMO-USER-ID | Authenticated user ID |
| CDEMO-USER-TYPE | User type ('A'/'U') with 88-level conditions |
| CDEMO-FROM-TRANID | Originating transaction ID |
| CDEMO-FROM-PROGRAM | Originating program name |
| CDEMO-PGM-CONTEXT | Program context counter |
| CDEMO-PGM-REENTER | Re-entry flag (first display vs. input processing) |

## Regulatory Traceability Summary

| Regulation | Article | Rules Mapped | Coverage |
|------------|---------|-------------|----------|
| PSD2 | Art. 97 — SCA | USEC-BR-001, USEC-BR-002, USEC-BR-003, USEC-BR-006 | Authentication, authorization, session management, credential provisioning |
| FFFS 2014:5 | Ch. 6 — IT Security | USEC-BR-001 through USEC-BR-008 | All user/security operations |
| DORA | Art. 9 — ICT Access Control | USEC-BR-001 through USEC-BR-008 | Access control lifecycle (provision, modify, revoke, monitor) |
| GDPR | Art. 5, 16, 17, 32 | USEC-BR-005, USEC-BR-006, USEC-BR-007, USEC-BR-008 | Data minimisation, accuracy, rectification, erasure |

## Migration Gaps Identified

The following security gaps must be addressed in the .NET 8 migration:

1. **Single-factor authentication**: COBOL system uses password-only auth. PSD2 requires at least two independent factors (SCA). The migrated system must implement MFA.
2. **Plaintext passwords**: Passwords stored and compared in plaintext (PIC X(08)). Must use bcrypt/Argon2 hashing.
3. **No account lockout**: Unlimited login attempts allowed. Must implement lockout/rate limiting.
4. **No session timeout**: CICS pseudo-conversational pattern has no expiry. Must implement configurable session timeouts.
5. **No audit trail**: User CRUD operations are not logged. Must implement comprehensive audit logging per DORA Art. 9.
6. **Case-insensitive passwords**: FUNCTION UPPER-CASE applied to passwords. Must enforce case-sensitive passwords.
7. **No password complexity**: No validation of password strength. Must enforce complexity policies.
8. **Binary role model**: Only 'A' (Admin) and 'U' (User) roles. Should implement granular RBAC.
9. **No per-operation authorization**: Admin menu doesn't check granular permissions. Must implement per-endpoint authorization.
10. **No self-deletion/last-admin protection**: Admin can delete own account or last admin. Must add safeguards.
11. **PF3 auto-save on update**: Non-standard UX — exit saves changes. Must clarify save-on-exit behavior.
12. **Error message bug**: Delete screen shows "Unable to Update User..." — copy-paste error from update program.

## Domain Expert Questions (Pending Validation)

1. Is the case-insensitive password behavior intentional or a mainframe limitation?
2. Are there external authentication systems (RACF, ACF2, Top Secret) supplementing USRSEC auth?
3. What is the current operational password rotation policy?
4. Should MFA be required from day one of migration?
5. Can an admin user also perform regular user operations?
6. Should user deletion be hard-delete or soft-delete (deactivation)?
7. Are there regulatory retention requirements preventing immediate user record deletion?
8. Should privilege escalation (U -> A) require additional authorization?

---

**Extraction date:** 2026-02-15
**Template version:** 1.0
**Total COBOL lines analyzed:** ~2,315 (6 programs) + 189 (3 copybooks)
