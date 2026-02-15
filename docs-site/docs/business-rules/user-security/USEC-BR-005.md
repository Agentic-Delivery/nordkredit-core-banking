---
id: "USEC-BR-005"
title: "User list display with pagination from USRSEC file"
domain: "user-security"
cobol_source: "COUSR00C.cbl:149-332"
requirement_id: "USEC-BR-005"
regulations:
  - "FFFS 2014:5 Ch. 6 — IT security audit trail"
  - "DORA Art. 9 — ICT access control monitoring"
  - "GDPR Art. 5 — Data minimisation in display"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# USEC-BR-005: User list display with pagination from USRSEC file

## Summary

The User List program (COUSR00C) displays up to 10 user records per page from the USRSEC VSAM file. It supports forward pagination (PF8) and backward pagination (PF7), maintaining the current page number and first/last User IDs for navigation. The list displays User ID, First Name, Last Name, and User Type for each record. Users can filter the list by entering a starting User ID in the input field. The list also allows selecting users for Update ('U') or Delete ('D') operations.

## Business Logic

### Pseudocode

```
// Initial display or Enter key:
IF User ID filter is empty:
    START browse from beginning of file (LOW-VALUES)
ELSE:
    START browse from entered User ID

RESET page number to 0
PERFORM page-forward logic

// Page Forward (PF8):
IF more pages exist (NEXT-PAGE-YES):
    START browse from last displayed User ID
    READ next 10 records
    Track if more records exist beyond current page
    Increment page counter
ELSE:
    Display 'You are already at the bottom of the page...'

// Page Backward (PF7):
IF current page > 1:
    START browse from first displayed User ID
    READ previous 10 records (in reverse)
    Decrement page counter
ELSE:
    Display 'You are already at the top of the page...'

// Each page displays:
FOR each record (up to 10):
    Display: [Selection field] [User ID] [First Name] [Last Name] [User Type]
```

### Decision Table

| Key Pressed | Condition | Action |
|-------------|-----------|--------|
| Enter | No selection | Refresh list from entered User ID filter |
| Enter | Selection = 'U'/'u' | Transfer to COUSR02C (User Update) with selected User ID |
| Enter | Selection = 'D'/'d' | Transfer to COUSR03C (User Delete) with selected User ID |
| Enter | Selection = other | Error: "Invalid selection. Valid values are U and D" |
| PF7 | Page > 1 | Navigate to previous page |
| PF7 | Page = 1 | Message: "You are already at the top of the page..." |
| PF8 | More records exist | Navigate to next page |
| PF8 | No more records | Message: "You are already at the bottom of the page..." |
| PF3 | - | Return to Admin Menu (COADM01C) |
| Other | - | Error: invalid key message |

## Source COBOL Reference

**Program:** `COUSR00C.cbl`
**Lines:** 149-232 (PROCESS-ENTER-KEY), 282-331 (PROCESS-PAGE-FORWARD)

```cobol
       PROCESS-ENTER-KEY.

           EVALUATE TRUE
               WHEN SEL0001I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0001I OF COUSR0AI TO CDEMO-CU00-USR-SEL-FLG
                   MOVE USRID01I OF COUSR0AI TO CDEMO-CU00-USR-SELECTED
               WHEN SEL0002I OF COUSR0AI NOT = SPACES AND LOW-VALUES
                   ...
               WHEN OTHER
                   MOVE SPACES   TO CDEMO-CU00-USR-SEL-FLG
                   MOVE SPACES   TO CDEMO-CU00-USR-SELECTED
           END-EVALUATE

           IF (CDEMO-CU00-USR-SEL-FLG NOT = SPACES AND LOW-VALUES) AND
              (CDEMO-CU00-USR-SELECTED NOT = SPACES AND LOW-VALUES)
               EVALUATE CDEMO-CU00-USR-SEL-FLG
                   WHEN 'U'
                   WHEN 'u'
                        MOVE 'COUSR02C'   TO CDEMO-TO-PROGRAM
                        ...
                        EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                   WHEN 'D'
                   WHEN 'd'
                        MOVE 'COUSR03C'   TO CDEMO-TO-PROGRAM
                        ...
                   WHEN OTHER
                       MOVE
                       'Invalid selection. Valid values are U and D' TO
                                       WS-MESSAGE
               END-EVALUATE
           END-IF

           IF USRIDINI OF COUSR0AI = SPACES OR LOW-VALUES
               MOVE LOW-VALUES TO SEC-USR-ID
           ELSE
               MOVE USRIDINI  OF COUSR0AI TO SEC-USR-ID
           END-IF

           MOVE 0       TO CDEMO-CU00-PAGE-NUM
           PERFORM PROCESS-PAGE-FORWARD
```

## Acceptance Criteria

### Scenario 1: Display first page of users

```gherkin
GIVEN an admin user navigates to the User List screen (COUSR0A)
  AND the USRSEC file contains 25 user records
WHEN the screen is first displayed
THEN the system shows the first 10 users sorted by User ID
  AND the page number shows "1"
  AND forward navigation is available (more pages exist)
```

### Scenario 2: Page forward

```gherkin
GIVEN the User List displays page 1 of 3
WHEN the admin presses PF8 (page forward)
THEN the system displays the next 10 user records
  AND the page number increments to 2
```

### Scenario 3: Page backward

```gherkin
GIVEN the User List displays page 2
WHEN the admin presses PF7 (page backward)
THEN the system displays the previous 10 user records
  AND the page number decrements to 1
```

### Scenario 4: Already at top of list

```gherkin
GIVEN the User List displays page 1
WHEN the admin presses PF7 (page backward)
THEN the system displays "You are already at the top of the page..."
  AND the page remains at 1
```

### Scenario 5: Filter by User ID

```gherkin
GIVEN an admin is on the User List screen
WHEN the admin enters "USER00" in the User ID filter field and presses Enter
THEN the system starts browsing from "USER00"
  AND displays up to 10 users starting from or after "USER00" alphabetically
```

### Scenario 6: Select user for update

```gherkin
GIVEN the User List displays users
WHEN the admin enters "U" in the selection field next to user "USER0001"
  AND presses Enter
THEN the system transfers control to COUSR02C (User Update)
  AND passes "USER0001" as the selected user in the COMMAREA
```

### Scenario 7: Select user for delete

```gherkin
GIVEN the User List displays users
WHEN the admin enters "D" in the selection field next to user "USER0001"
  AND presses Enter
THEN the system transfers control to COUSR03C (User Delete)
  AND passes "USER0001" as the selected user in the COMMAREA
```

### Scenario 8: Invalid selection value

```gherkin
GIVEN the User List displays users
WHEN the admin enters "X" in the selection field next to a user
  AND presses Enter
THEN the system displays "Invalid selection. Valid values are U and D"
  AND the user list is re-displayed
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 6 | IT security — ability to review user access | User list provides admin visibility into all registered users and their types, supporting access reviews |
| DORA | Art. 9 | ICT access control monitoring | Admin interface for reviewing user accounts supports access control monitoring requirements |
| GDPR | Art. 5(1)(c) | Data minimisation | List displays only User ID, name, and type — minimal PII necessary for user management |

## Edge Cases

1. **Case-insensitive selection**: The selection flag accepts both uppercase and lowercase 'U'/'u' and 'D'/'d'. The migrated system should similarly be case-insensitive for user convenience.

2. **10 records per page**: The page size is hardcoded to 10 records (limited by BMS screen real estate). The migrated system can support configurable page sizes and infinite scroll.

3. **VSAM sequential browse**: The list uses CICS STARTBR/READNEXT/READPREV for pagination, which reads records in key order (User ID). The migrated system should use SQL pagination (OFFSET/FETCH or cursor-based).

4. **Only first selection processed**: If multiple rows have selection flags, only the first non-empty selection in the EVALUATE chain is processed. The migrated system could support batch selection.

5. **Page tracking in COMMAREA**: Page number, first/last User IDs, and next-page flag are stored in the COMMAREA for pseudo-conversational pagination state. The migrated system should use server-side pagination tokens or stateless cursor-based pagination.

## Domain Expert Notes

- **Pending**: No domain expert review yet. Key questions for validation:
  - Should the user list be accessible to non-admin users (e.g., for self-service profile viewing)?
  - Are there any sorting requirements beyond User ID order?
  - Should the migrated system support search by name in addition to User ID?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
