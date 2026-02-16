---
id: "sec-br-001"
title: "Role-based access control (admin vs regular user)"
domain: "user-security"
cobol_source: "COCRDLIC.cbl:4-7,COCRDLIC.cbl:315-321,COCRDLIC.cbl:1382-1410"
requirement_id: "SEC-BR-001"
regulations:
  - "PSD2 Art. 97"
  - "GDPR Art. 25"
  - "GDPR Art. 5(1)(f)"
  - "FFFS 2014:5 Ch. 8 §4"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# SEC-BR-001: Role-based access control (admin vs regular user)

## Summary

The CardDemo system implements a two-tier role-based access control (RBAC) model distinguishing between admin users and regular users. The user type is stored in the COMMAREA field `CDEMO-USRTYP-USER` and determines what data the user can access. Admin users see all card records regardless of account association, while regular users see only records belonging to their own account. This is the foundational security boundary for data access throughout the card management subsystem.

## Business Logic

### Pseudocode

```
ON PROGRAM ENTRY (COCRDLIC - Card List):
    IF no COMMAREA passed (EIBCALEN = 0)
        -- First entry, no context
        INITIALIZE CARDDEMO-COMMAREA
        SET user-type = 'USER'          -- Default to regular user
        SET program-enter = TRUE
    ELSE
        -- Context passed from calling program
        RESTORE CARDDEMO-COMMAREA from DFHCOMMAREA
    END-IF

DURING DATA RETRIEVAL (9500-FILTER-RECORDS):
    SET include-record = TRUE

    IF account-filter-is-valid
        -- Regular user: filter by their account
        IF card-account-id ≠ user-account-id
            SET exclude-record = TRUE
            EXIT FILTER
        END-IF
    ELSE
        -- No account filter = admin user sees all records
        CONTINUE (no filtering applied)
    END-IF

    IF card-filter-is-valid
        -- Additional card number filter
        IF card-number ≠ filter-card-number
            SET exclude-record = TRUE
            EXIT FILTER
        END-IF
    ELSE
        CONTINUE
    END-IF
```

### Role Matrix

| User Type | Account Filter Set | Behavior | Records Visible |
|-----------|-------------------|----------|-----------------|
| Admin | No (blank) | No account filter applied | All card records in system |
| Admin | Yes (specific account) | Filter by specified account | Cards for specified account only |
| Regular User | Yes (own account from COMMAREA) | Filter by own account | Only own account's cards |
| Regular User | No | Default to own account context | Only own account's cards |

## Source COBOL Reference

**Program:** `COCRDLIC.cbl`
**Lines:** 4-7 (Program header documenting role-based behavior)

```cobol
000004 * Function:    List Credit Cards
000005 *              a) All cards if no context passed and admin user
000006 *              b) Only the ones associated with ACCT in COMMAREA
000007 *                 if user is not admin
```

**Lines:** 315-321 (Default user type initialization on first entry)

```cobol
000315      IF EIBCALEN = 0
000316         INITIALIZE CARDDEMO-COMMAREA
000317                    WS-THIS-PROGCOMMAREA
000318         MOVE LIT-THISTRANID        TO CDEMO-FROM-TRANID
000319         MOVE LIT-THISPGM           TO CDEMO-FROM-PROGRAM
000320         SET CDEMO-USRTYP-USER      TO TRUE
000321         SET CDEMO-PGM-ENTER        TO TRUE
```

**Lines:** 1382-1410 (Core data filtering logic based on user context)

```cobol
001382  9500-FILTER-RECORDS.
001383      SET WS-DONOT-EXCLUDE-THIS-RECORD TO TRUE
001384
001385      IF FLG-ACCTFILTER-ISVALID
001386         IF  CARD-ACCT-ID = CC-ACCT-ID
001387             CONTINUE
001388         ELSE
001389             SET WS-EXCLUDE-THIS-RECORD  TO TRUE
001390             GO TO 9500-FILTER-RECORDS-EXIT
001391         END-IF
001392      ELSE
001393        CONTINUE
001394      END-IF
001395
001396      IF FLG-CARDFILTER-ISVALID
001397         IF  CARD-NUM = CC-CARD-NUM-N
001398             CONTINUE
001399         ELSE
001400             SET WS-EXCLUDE-THIS-RECORD TO TRUE
001401             GO TO 9500-FILTER-RECORDS-EXIT
001402         END-IF
001403      ELSE
001404        CONTINUE
001405      END-IF
001406
001407      .
001408
001409  9500-FILTER-RECORDS-EXIT.
001410      EXIT
```

### Supporting References

- **COCOM01Y.cpy** (not in workspace): Defines `CARDDEMO-COMMAREA` including `CDEMO-USRTYP-USER` flag
- **CSUSR01Y.cpy** (not in workspace): Signed-on user data structure with user identity and type
- **COCRDSLC.cbl:326**: Sets `CDEMO-USRTYP-USER` to TRUE on program transfer to card detail
- **COCRDUPC.cbl:464**: Sets `CDEMO-USRTYP-USER` to TRUE on program transfer to card update

## Acceptance Criteria

### Scenario 1: Admin user sees all card records

```gherkin
GIVEN the user is authenticated as an admin user
  AND the account filter field is left blank
WHEN the user accesses the card list screen
THEN all card records across all accounts are displayed
  AND no account-based filtering is applied
```

### Scenario 2: Regular user sees only own account's cards

```gherkin
GIVEN the user is authenticated as a regular user
  AND the user's account ID is set in the COMMAREA (CC-ACCT-ID)
WHEN the user accesses the card list screen
THEN only card records where CARD-ACCT-ID matches CC-ACCT-ID are displayed
  AND cards belonging to other accounts are excluded
```

### Scenario 3: Admin user applies voluntary account filter

```gherkin
GIVEN the user is authenticated as an admin user
  AND the user enters a specific account number in the filter field
WHEN the card list is retrieved
THEN only cards matching the specified account number are displayed
  AND the admin can clear the filter to see all cards again
```

### Scenario 4: Default user type on first program entry

```gherkin
GIVEN a CICS transaction is initiated with no COMMAREA (EIBCALEN = 0)
WHEN the card list program initializes
THEN CDEMO-USRTYP-USER is set to TRUE (regular user)
  AND the user type defaults to the most restrictive access level
```

### Scenario 5: User type propagated across program transfers

```gherkin
GIVEN the user selects a card for detail view or update
WHEN the card list program transfers control via CICS XCTL
THEN CDEMO-USRTYP-USER is set to TRUE in the COMMAREA
  AND the called program receives the user type context
```

### Scenario 6: Filtered record is excluded from display

```gherkin
GIVEN the account filter is valid (FLG-ACCTFILTER-ISVALID = TRUE)
  AND a card record has CARD-ACCT-ID = "12345678901"
  AND the filter account is CC-ACCT-ID = "99999999999"
WHEN the filter routine (9500-FILTER-RECORDS) is executed
THEN WS-EXCLUDE-THIS-RECORD is set to TRUE
  AND the record is not shown on the card list screen
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 97 | Payment service providers must apply strong customer authentication when accessing payment instrument information | RBAC ensures authenticated users can only access data appropriate to their role; regular users are restricted to their own account data |
| GDPR | Art. 25 | Data protection by design and by default — appropriate technical measures to ensure only necessary personal data is processed | Default user type is regular (most restrictive); data filtering is applied at the data retrieval layer before any display |
| GDPR | Art. 5(1)(f) | Integrity and confidentiality — appropriate security of personal data including protection against unauthorized access | Account-level filtering prevents cross-account data access for regular users |
| FFFS 2014:5 | Ch. 8 §4 | Credit institutions must have adequate internal controls including access control and segregation of duties | Two-tier role model (admin/user) provides segregation between administrative and operational access to card data |

## Edge Cases

1. **No explicit admin flag in source**: The COBOL code defaults `CDEMO-USRTYP-USER` to TRUE (regular user) on every program entry. The admin role appears to be determined by whether an account filter is passed in context — admin users arrive without an account context, meaning the filter is blank and no records are excluded. The migrated system should implement an explicit admin role rather than relying on the absence of an account context.

2. **User type reset on every transfer**: Every CICS XCTL call sets `CDEMO-USRTYP-USER` to TRUE (lines 320, 388, 466, 522, 550). This means the user type is hardcoded as "USER" in COCRDLIC regardless of the actual logged-in user's role. The true role determination likely happens upstream in the sign-on program (not in the workspace). The migrated system must preserve role context across API calls without resetting it.

3. **Filter bypass when both filters blank**: When both `FLG-ACCTFILTER-BLANK` and `FLG-CARDFILTER-BLANK` are set, the 9500-FILTER-RECORDS paragraph allows all records through (both IF conditions fall to the ELSE/CONTINUE branch). This is the admin "see all" path. The migrated system should enforce explicit role checks rather than relying on filter state.

4. **CSUSR01Y copybook not in workspace**: The signed-on user data structure (CSUSR01Y) is referenced in all three programs but is not present in the workspace. This copybook likely contains the authenticated user identity, user type, and session data that drives the access control decisions upstream. It must be located and analyzed for the migration.

5. **COMMAREA size boundary**: The CARDDEMO-COMMAREA is fixed-size and the program-specific data is appended after it (line 610-612). If the COMMAREA structure changes, all programs must be recompiled together. The migrated system should use a versioned session/context API.

## Domain Expert Notes

- **null** — Awaiting domain expert review. Key questions: (1) How is the admin role actually determined at sign-on time — is it RACF-based, CICS transaction security, or application-level? (2) Does the CSUSR01Y copybook contain explicit admin/user type flags or is it inferred from the terminal profile? (3) Are there additional RBAC levels beyond admin/user in the production system? (4) Should the migrated system implement a more granular role model (e.g., viewer, operator, admin, auditor)?

---

**Template version:** 1.0
**Last updated:** 2026-02-16
