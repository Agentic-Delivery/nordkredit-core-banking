---
id: "sec-br-010"
title: "User identity propagation from sign-on to application"
domain: "user-security"
cobol_source: "COCOM01Y.cpy:25-28,CSUSR01Y.cpy:17-23,COTRN00C.cbl:108,COTRN00C.cbl:513,COTRN01C.cbl:95,COTRN01C.cbl:200,COTRN02C.cbl:116,COTRN02C.cbl:503"
requirement_id: "SEC-BR-010"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 25"
  - "FFFS 2014:5 Ch. 8 §4"
  - "DORA Art. 9"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# SEC-BR-010: User identity propagation from sign-on to application

## Summary

The CardDemo system propagates authenticated user identity from the sign-on program (COSGN00C) to all application programs via two mechanisms: (1) the COMMAREA fields `CDEMO-USER-ID` and `CDEMO-USER-TYPE` defined in COCOM01Y.cpy, which carry the user identity and role across all CICS program transfers; and (2) the `SEC-USER-DATA` structure from CSUSR01Y.cpy, which is allocated in each program's WORKING-STORAGE. When a program loses its session context (EIBCALEN = 0) or has no return target, it routes back to COSGN00C for re-authentication, establishing COSGN00C as the trusted authentication boundary. This identity propagation pattern — where the sign-on program populates a shared data area that all downstream programs read but never re-validate — is the CICS equivalent of a session token. In the migrated system, this maps to JWT token claims populated during Azure AD authentication and validated by API middleware.

## Business Logic

### Pseudocode

```
IDENTITY PROPAGATION FLOW:

SIGN-ON (COSGN00C — not in workspace, inferred from references):
    READ SEC-USER-DATA for entered user-id
    VALIDATE password (SEC-USR-PWD vs entered password)
    IF valid:
        SET CDEMO-USER-ID = SEC-USR-ID         -- PIC X(08) → PIC X(08)
        SET CDEMO-USER-TYPE = SEC-USR-TYPE      -- PIC X(01) → PIC X(01)
        -- 88 CDEMO-USRTYP-ADMIN VALUE 'A'
        -- 88 CDEMO-USRTYP-USER  VALUE 'U'
        TRANSFER to menu program with COMMAREA

APPLICATION PROGRAMS (all downstream):
    -- Receive identity via COMMAREA
    IF EIBCALEN > 0
        RESTORE CARDDEMO-COMMAREA from DFHCOMMAREA
        -- CDEMO-USER-ID and CDEMO-USER-TYPE are now available
        -- Programs trust these values without re-validation
    END-IF

    -- Use identity for access control decisions
    IF CDEMO-USRTYP-ADMIN
        -- Admin: no account filtering, full access
    ELSE IF CDEMO-USRTYP-USER
        -- Regular user: filter by own account
    END-IF

SESSION RECOVERY (fallback to sign-on):
    -- When session context is lost, return to COSGN00C
    IF EIBCALEN = 0 (no COMMAREA = no session)
        MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
        PERFORM RETURN-TO-PREV-SCREEN
    END-IF

    -- When return target is unknown, default to COSGN00C
    IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
        MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
    END-IF
```

### Identity Field Mapping

| Source (CSUSR01Y) | COMMAREA (COCOM01Y) | Purpose | Migration Target |
|-------------------|---------------------|---------|-----------------|
| SEC-USR-ID (X(08)) | CDEMO-USER-ID (X(08)) | User identifier | JWT `sub` / `oid` claim |
| SEC-USR-TYPE (X(01)) | CDEMO-USER-TYPE (X(01)) | Role ('A'/'U') | JWT `roles` claim (Azure AD app role) |
| SEC-USR-FNAME (X(20)) | CDEMO-CUST-FNAME (X(25)) | First name | JWT `given_name` claim |
| SEC-USR-LNAME (X(20)) | CDEMO-CUST-LNAME (X(25)) | Last name | JWT `family_name` claim |

### Sign-On as Authentication Boundary

```
                 ┌─────────────────────────────────┐
                 │  AUTHENTICATION BOUNDARY         │
                 │                                  │
  [Terminal] ──→ │  COSGN00C (Sign-on Program)      │
                 │    ↓ validates SEC-USR-PWD       │
                 │    ↓ populates COMMAREA          │
                 └──────────────┬────────────────────┘
                                │ XCTL with COMMAREA
                 ┌──────────────▼────────────────────┐
                 │  APPLICATION ZONE                  │
                 │  (trusts COMMAREA identity)        │
                 │                                    │
                 │  Menu ──→ Card List ──→ Card Detail│
                 │           ──→ Card Update          │
                 │  Transaction Programs (COTRN*)     │
                 │                                    │
                 │  On session loss → back to COSGN00C│
                 └────────────────────────────────────┘
```

## Source COBOL Reference

**Copybook:** `COCOM01Y.cpy`
**Lines:** 25-28 (User identity fields in COMMAREA)

```cobol
       10 CDEMO-USER-ID                 PIC X(08).
       10 CDEMO-USER-TYPE               PIC X(01).
          88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
          88 CDEMO-USRTYP-USER          VALUE 'U'.
```

**Copybook:** `CSUSR01Y.cpy`
**Lines:** 17-23 (Source user data record)

```cobol
 01 SEC-USER-DATA.
    05 SEC-USR-ID                 PIC X(08).
    05 SEC-USR-FNAME              PIC X(20).
    05 SEC-USR-LNAME              PIC X(20).
    05 SEC-USR-PWD                PIC X(08).
    05 SEC-USR-TYPE               PIC X(01).
    05 SEC-USR-FILLER             PIC X(23).
```

**Program:** `COTRN00C.cbl`
**Lines:** 107-109 (Return to sign-on when no session)

```cobol
000107           IF EIBCALEN = 0
000108               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
000109               PERFORM RETURN-TO-PREV-SCREEN
```

**Lines:** 512-513 (Default to sign-on when no return target)

```cobol
000512           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
000513               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
```

**Program:** `COTRN01C.cbl`
**Lines:** 94-96 (Return to sign-on when no session)

```cobol
000094           IF EIBCALEN = 0
000095               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
000096               PERFORM RETURN-TO-PREV-SCREEN
```

**Lines:** 199-200 (Default to sign-on when no return target)

```cobol
000199           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
000200               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
```

**Program:** `COTRN02C.cbl`
**Lines:** 115-117 (Return to sign-on when no session)

```cobol
000115           IF EIBCALEN = 0
000116               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
000117               PERFORM RETURN-TO-PREV-SCREEN
```

**Lines:** 502-503 (Default to sign-on when no return target)

```cobol
000502           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
000503               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
```

## Acceptance Criteria

### Scenario 1: Sign-on populates COMMAREA with user identity

```gherkin
GIVEN a user successfully authenticates via COSGN00C
WHEN the sign-on program transfers control to the menu
THEN CDEMO-USER-ID contains the authenticated user's ID (8 characters)
  AND CDEMO-USER-TYPE contains the user's role ('A' for admin, 'U' for regular)
  AND the COMMAREA is passed to the menu program via CICS XCTL
```

### Scenario 2: Application programs trust COMMAREA identity

```gherkin
GIVEN a program receives control with a valid COMMAREA (EIBCALEN > 0)
WHEN the program restores CARDDEMO-COMMAREA from DFHCOMMAREA
THEN CDEMO-USER-ID and CDEMO-USER-TYPE are available
  AND the program does NOT re-validate the user's credentials
  AND access control decisions are based on the COMMAREA values
```

### Scenario 3: Lost session triggers re-authentication

```gherkin
GIVEN a CICS transaction program is invoked without a COMMAREA (EIBCALEN = 0)
WHEN the program detects no session context
THEN CDEMO-TO-PROGRAM is set to 'COSGN00C'
  AND the program performs RETURN-TO-PREV-SCREEN
  AND control returns to the sign-on program for re-authentication
```

### Scenario 4: Unknown return target defaults to sign-on

```gherkin
GIVEN a program needs to return to the calling program
  AND CDEMO-TO-PROGRAM is LOW-VALUES or SPACES (no target recorded)
WHEN the return logic executes
THEN CDEMO-TO-PROGRAM defaults to 'COSGN00C'
  AND the user is routed back to the sign-on screen
  AND this acts as a safety net ensuring unauthenticated sessions cannot persist
```

### Scenario 5: Identity propagated across full program chain

```gherkin
GIVEN a user authenticated as admin (CDEMO-USER-TYPE = 'A')
WHEN the user navigates: Sign-on → Menu → Card List → Card Detail → Card Update
THEN CDEMO-USER-ID remains the same across all program transfers
  AND CDEMO-USER-TYPE remains 'A' across all transfers
  AND no program in the chain re-authenticates or re-reads SEC-USER-DATA
```

### Scenario 6: Migrated system validates identity on every request

```gherkin
GIVEN the system is migrated to .NET 8 on Azure
WHEN a user makes an API request
THEN the JWT token is validated by ASP.NET Core authentication middleware
  AND the user identity (sub claim) and roles (roles claim) are extracted
  AND EVERY request is validated (unlike the mainframe trust model)
  AND expired or tampered tokens are rejected with 401 Unauthorized
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Strong customer authentication — session integrity must be maintained | COSGN00C acts as the authentication boundary; all downstream programs inherit the authenticated identity via COMMAREA. Migration must replicate with JWT validation on every request |
| GDPR | Art. 25 | Data protection by design and by default | The sign-on boundary ensures only authenticated users reach application programs. The migrated system enhances this with per-request token validation |
| FFFS 2014:5 | Ch. 8 §4 | Adequate internal controls including user identification and authentication | The COMMAREA-based identity propagation provides a consistent user identification mechanism across all programs; the sign-on fallback ensures sessions cannot persist without authentication |
| DORA | Art. 9 | Strong authentication mechanisms and access control policies | The authentication boundary (COSGN00C) and re-authentication fallback pattern enforce that only authenticated users can access application functions |

## Edge Cases

1. **No per-request validation**: Once the COMMAREA is populated by COSGN00C, downstream programs trust the identity without re-validation. A compromised or modified COMMAREA could impersonate another user. The migrated system must validate the JWT token on every API request, not just at login.

2. **User type hardcoded on XCTL**: Several programs set `CDEMO-USRTYP-USER TO TRUE` on every XCTL call (e.g., COCRDLIC line 320, COCRDSLC line 326, COCRDUPC line 464). This means the user type is reset to 'U' (regular) regardless of the actual role. The true role from COSGN00C may be overwritten. The domain expert must clarify whether admin users maintain their role through the card management flow.

3. **COSGN00C not in workspace**: The actual sign-on program is referenced but not available for analysis. The authentication logic, password validation method, and session establishment code cannot be verified. This is a gap in the extraction that should be closed if the program becomes available.

4. **Identity fields are fixed-width**: `CDEMO-USER-ID` is PIC X(08) — user IDs are limited to 8 characters, right-padded with spaces. The migrated system (Azure AD) uses GUIDs (36 chars) or email addresses. A mapping table from legacy user IDs to Azure AD object IDs will be required during migration.

5. **No session token invalidation**: There is no mechanism in the COMMAREA to invalidate a specific session. CICS manages session lifecycle at the terminal level. The migrated system must support token revocation (e.g., for password change, account lockout, or admin force-logout).

## Domain Expert Notes

- **Copybook analysis complete** — COCOM01Y confirms `CDEMO-USER-ID` (X(08)) and `CDEMO-USER-TYPE` (X(01), 'A'/'U') as the identity propagation fields. CSUSR01Y confirms the source user data structure. COSGN00C is referenced by COTRN00C, COTRN01C, and COTRN02C as the authentication entry point and session recovery fallback. **Questions for domain expert:** (1) Does COSGN00C populate CDEMO-USER-ID from SEC-USR-ID, or from EIBUSERID? (2) Why do downstream programs reset CDEMO-USRTYP-USER to TRUE on XCTL — is admin role intentionally dropped for the card management flow? (3) Can COSGN00C be obtained from the mainframe team for direct analysis? (4) Is there a user provisioning program that creates/modifies SEC-USER-DATA records?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
