---
id: DS-BMS-USR00
title: "User List Screen"
copybook_name: "COUSR00.CPY"
domain: "ui-screens"
used_by_programs: [COUSR00C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 23
---

# DS-BMS-USR00: User List Screen (COUSR00)

## Overview

The `COUSR00.CPY` copybook defines the **User List Screen**, a CICS BMS screen that displays a paginated list of system users. The screen shows up to 10 user rows per page, each with a selection field, user ID, first name, last name, and user type. This is an admin-only screen accessed from the Admin Menu.

The copybook defines two symbolic map records:
- **CUSR00AI** -- Input record (receives row selection inputs and pagination keys)
- **CUSR00AO** -- Output record (sends user list data and messages to the terminal)

**Source file:** `COUSR00.CPY`
**BMS map name:** COUSR00
**Used by:** `COUSR00C` (User List CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `SEL0001` - `SEL0010` | Input | 1 | Row selection field (action code) |
| `USID0001` - `USID0010` | Display | 8 | User ID for each row |
| `FNAME0001` - `FNAME0010` | Display | 20 | First name for each row |
| `LNAME0001` - `LNAME0010` | Display | 20 | Last name for each row |
| `UTYP0001` - `UTYP0010` | Display | 1 | User type for each row (`A` = Admin, `U` = User) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **List layout:** The screen displays up to 10 user records per page in a tabular format. Each row shows the user ID, first name, last name, and user type.

2. **Pagination:** PF7 scrolls backward and PF8 scrolls forward through the user list. The current page position is maintained in the CICS COMMAREA.

3. **Row selection:** The user enters an action code in the `SEL` field to select a row. Different action codes may route to different operations:
   - `U` or `S`: Select for update (routes to User Update Screen - COUSR02)
   - `D`: Select for delete (routes to User Delete Screen - COUSR03)

4. **User type display:** The `UTYP` field shows a single character: `A` for administrator users and `U` for regular users.

5. **Admin-only access:** This screen is only accessible to users with administrative privileges. The CICS program verifies authorization before displaying the list.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COUSR00` on 3270 terminal | **Replaced by Azure AD user management** or admin Blazor/Razor page |
| **Program** | `COUSR00C` (COBOL CICS program) | Azure AD Graph API for user listing, or admin `UserController.Index` |
| **Pagination** | PF7/PF8 keys, COMMAREA state | Server-side pagination or Azure AD Graph API paging |
| **Row selection** | Action code in selection field | Clickable action links/buttons per row |
| **User type** | Single character (`A`/`U`) | Azure AD roles/groups displayed as labels |
| **Authorization** | CICS RACF check | `[Authorize(Roles = "UserAdmin")]` policy |

### Migration Considerations

- **Azure AD integration:** The user management screens (list, add, update, delete) are largely replaced by Azure AD user management. The .NET application may provide a simplified admin view that wraps Azure AD Graph API calls, or administrators may use the Azure Portal directly.
- **If custom admin UI is needed:** Build a lightweight admin page that lists users from Azure AD via Microsoft Graph API, showing display name, email, assigned roles, and account status. This replaces the custom USRSEC file-based user management.
- **Role model upgrade:** Replace the binary Admin/User type with Azure AD role-based groups supporting more granular permissions (e.g., `AccountViewer`, `AccountEditor`, `TransactionOperator`, `AMLAnalyst`, `SystemAdmin`).
- **GDPR considerations:** User data displayed in the list (names, user IDs) is personal data subject to GDPR. Access to the user list must be restricted and logged.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
