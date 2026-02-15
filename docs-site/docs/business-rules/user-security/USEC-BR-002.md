---
id: "USEC-BR-002"
title: "User type-based authorization routing after authentication"
domain: "user-security"
cobol_source: "COSGN00C.cbl:230-240"
requirement_id: "USEC-BR-002"
regulations:
  - "PSD2 Art. 97 — Strong Customer Authentication (SCA)"
  - "FFFS 2014:5 Ch. 6 — Role-based access control"
  - "DORA Art. 9 — ICT access control policies"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# USEC-BR-002: User type-based authorization routing after authentication

## Summary

After successful authentication, the sign-on program (COSGN00C) routes the user to one of two menu systems based on the `SEC-USR-TYPE` field from the USRSEC file. Admin users (type `'A'`) are transferred to the Admin Menu (COADM01C), which provides access to user management functions. All other users (type `'U'` or any non-'A' value) are transferred to the Regular User Menu (COMEN01C), which provides access to account, credit card, and transaction functions. The user type is stored in the COMMAREA (`CDEMO-USER-TYPE`) and carried through the entire session.

## Business Logic

### Pseudocode

```
// After successful password match:
SET CDEMO-USER-TYPE = SEC-USR-TYPE (from USRSEC record)

IF CDEMO-USER-TYPE = 'A':      // Admin
    XCTL to COADM01C (Admin Menu) with COMMAREA
ELSE:                           // Regular user (type 'U' or any other)
    XCTL to COMEN01C (Regular User Menu) with COMMAREA
```

### Decision Table

| User Type (SEC-USR-TYPE) | 88-Level Condition | Target Program | Menu System |
|--------------------------|-------------------|----------------|-------------|
| 'A' | CDEMO-USRTYP-ADMIN | COADM01C | Admin Menu — User CRUD, Transaction Types |
| 'U' | CDEMO-USRTYP-USER | COMEN01C | Regular Menu — Account/Card/Transaction operations |
| Any other value | (falls through to ELSE) | COMEN01C | Regular Menu (treated as non-admin) |

### Admin Menu Options (from COADM02Y copybook)

| # | Option | Program |
|---|--------|---------|
| 1 | User List (Security) | COUSR00C |
| 2 | User Add (Security) | COUSR01C |
| 3 | User Update (Security) | COUSR02C |
| 4 | User Delete (Security) | COUSR03C |
| 5 | Transaction Type List/Update (Db2) | COTRTLIC |
| 6 | Transaction Type Maintenance (Db2) | COTRTUPC |

### Regular User Menu Options (from COMEN02Y copybook)

| # | Option | Program | User Type |
|---|--------|---------|-----------|
| 1 | Account View | COACTVWC | U |
| 2 | Account Update | COACTUPC | U |
| 3-5 | Credit Card List/View/Update | COCRDLIC/COCRDSLC/COCRDUPC | U |
| 6-8 | Transaction List/View/Add | COTRN00C/COTRN01C/COTRN02C | U |
| 9 | Transaction Reports | CORPT00C | U |
| 10 | Bill Payment | COBIL00C | U |
| 11 | Pending Authorization View | COPAUS0C | U |

## Source COBOL Reference

**Program:** `COSGN00C.cbl`
**Lines:** 230-240

```cobol
                       MOVE SEC-USR-TYPE TO CDEMO-USER-TYPE
                       MOVE ZEROS        TO CDEMO-PGM-CONTEXT

                       IF CDEMO-USRTYP-ADMIN
                            EXEC CICS XCTL
                              PROGRAM ('COADM01C')
                              COMMAREA(CARDDEMO-COMMAREA)
                            END-EXEC
                       ELSE
                            EXEC CICS XCTL
                              PROGRAM ('COMEN01C')
                              COMMAREA(CARDDEMO-COMMAREA)
                            END-EXEC
                       END-IF
```

**Copybook:** `COCOM01Y.cpy` (COMMAREA definition)

```cobol
       01 CARDDEMO-COMMAREA.
          05 CDEMO-GENERAL-INFO.
             10 CDEMO-USER-TYPE               PIC X(01).
                88 CDEMO-USRTYP-ADMIN         VALUE 'A'.
                88 CDEMO-USRTYP-USER          VALUE 'U'.
```

## Acceptance Criteria

### Scenario 1: Admin user routed to admin menu

```gherkin
GIVEN a user has provided valid credentials
  AND the USRSEC record has SEC-USR-TYPE = 'A'
WHEN authentication succeeds
THEN the system sets CDEMO-USER-TYPE to 'A' in the COMMAREA
  AND transfers control to COADM01C (Admin Menu)
  AND the Admin Menu displays 6 options including User CRUD operations
```

### Scenario 2: Regular user routed to user menu

```gherkin
GIVEN a user has provided valid credentials
  AND the USRSEC record has SEC-USR-TYPE = 'U'
WHEN authentication succeeds
THEN the system sets CDEMO-USER-TYPE to 'U' in the COMMAREA
  AND transfers control to COMEN01C (Regular User Menu)
  AND the Regular Menu displays 11 options for account/card/transaction operations
```

### Scenario 3: Unknown user type defaults to regular menu

```gherkin
GIVEN a user has provided valid credentials
  AND the USRSEC record has SEC-USR-TYPE = 'X' (or any non-'A' value)
WHEN authentication succeeds
THEN the system sets CDEMO-USER-TYPE to 'X' in the COMMAREA
  AND transfers control to COMEN01C (Regular User Menu)
  AND the user is treated as a non-admin user
```

### Scenario 4: User type persists in session

```gherkin
GIVEN an authenticated admin user (type 'A') navigating within the Admin Menu
WHEN the user selects any admin option
THEN the COMMAREA carries CDEMO-USER-TYPE = 'A' to the target program
  AND the user type remains consistent throughout the session
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Access to payment accounts must be controlled | User type segregation ensures admin functions (user management) are separated from operational functions (payments, accounts) |
| FFFS 2014:5 | Ch. 6 | Role-based access control for IT systems | Two-tier role system (Admin/User) restricts administrative operations to authorized personnel |
| DORA | Art. 9 | Least privilege access and segregation of duties | Admin users have access to user management; regular users are restricted to their operational scope |

## Edge Cases

1. **Only two roles**: The system supports only two roles — Admin ('A') and User ('U'). There is no granular RBAC (e.g., read-only admin, supervisor). The migrated system should consider more granular role definitions per DORA Art. 9 requirements.

2. **No cross-menu access**: Admin users cannot access the regular user menu functions directly from the admin menu (and vice versa). However, the admin menu includes transaction type management functions. The migrated system should evaluate whether admins need access to all user-level functions as well.

3. **User type stored in single character**: The `SEC-USR-TYPE` field is PIC X(01), limiting role representation to a single character. The migrated system can use a more expressive role/permission model.

4. **No runtime authorization checks**: Once routed to a menu, individual programs do not re-verify the user's role. Any program invoked via XCTL with a valid COMMAREA will execute. The migrated system should implement per-endpoint authorization checks.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - Can an admin user also perform regular user operations (e.g., view accounts)?
  - Are there any operational procedures for managing the admin role assignment?
  - Should the migrated system implement more granular roles (e.g., Security Admin, Operations Admin)?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
