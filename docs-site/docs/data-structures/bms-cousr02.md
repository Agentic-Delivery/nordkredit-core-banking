---
id: DS-BMS-USR02
title: "User Update Screen"
copybook_name: "COUSR02.CPY"
domain: "ui-screens"
used_by_programs: [COUSR02C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 25
---

# DS-BMS-USR02: User Update Screen (COUSR02)

## Overview

The `COUSR02.CPY` copybook defines the **User Update Screen**, a CICS BMS screen for modifying existing user accounts. The screen displays the user ID as read-only and allows editing of the user's name, password, and user type.

The copybook defines two symbolic map records:
- **CUSR02AI** -- Input record (receives updated user data from the terminal)
- **CUSR02AO** -- Output record (sends current user data for display/editing)

**Source file:** `COUSR02.CPY`
**BMS map name:** COUSR02
**Used by:** `COUSR02C` (User Update CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `USIDTEFN` | Display | 8 | User ID (display-only, identifies the user being edited) |
| `FNAMETEFN` | Input | 20 | First name (editable) |
| `LNAMETEFN` | Input | 20 | Last name (editable) |
| `PASTEFN` | Input | 8 | Password (dark field, editable; leave blank to keep current) |
| `UTYPTEFN` | Input | 1 | User type (`A` = Admin, `U` = User, editable) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Edit form:** The user ID is displayed as read-only (it is the primary key and cannot be changed). The first name, last name, password, and user type fields are editable.

2. **Password handling:** The password field is a dark (non-display) input. If the administrator leaves it blank, the existing password is retained. If a new value is entered, the password is updated. This is a common CICS pattern for optional password changes.

3. **User type change:** Changing the user type from `U` to `A` (or vice versa) immediately changes the user's access level on their next login. The COBOL program does not require additional confirmation for this privilege escalation.

4. **Pre-population:** When the screen is first displayed, the current values for first name, last name, and user type are pre-populated from the USRSEC file. The password field is always blank (passwords cannot be displayed).

5. **Validation:** The program validates that at least the first name, last name, and user type are populated, and that the user type is valid (`A` or `U`).

6. **Admin-only access:** Only administrators can access this screen.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COUSR02` on 3270 terminal | **Replaced by Azure AD user management** or admin Blazor/Razor form |
| **Program** | `COUSR02C` (COBOL CICS program) | Azure AD Graph API user update, or admin `UserController.Edit` |
| **User ID** | Display-only PIC X(8) | Azure AD Object ID (GUID) or UPN |
| **Password update** | Optional 8-char dark field | Azure AD password reset flow (self-service or admin-initiated) |
| **User type** | `A`/`U` toggle | Azure AD role/group assignment changes |
| **Authorization** | CICS RACF check | `[Authorize(Roles = "UserAdmin")]` policy |

### Migration Considerations

- **Azure AD Graph API:** User profile updates (name, display attributes) are performed via Microsoft Graph API calls. Password changes are handled through Azure AD's password reset mechanisms, not application-level password storage.
- **Privilege escalation controls:** Changing a user's role (especially elevating to admin) must require additional authorization in the .NET system. Consider implementing a maker-checker pattern for role changes per FSA (FFFS 2014:5) Ch. 8. The COBOL system's single-step privilege change is insufficient for regulatory compliance.
- **Self-service vs. admin:** In the .NET architecture, users should be able to update their own profile (name) via self-service. Only user type/role changes and password resets should require admin intervention. This split was not possible in the COBOL system.
- **Audit trail:** All user profile modifications must be logged with before/after values, the modifying administrator's identity, and timestamp. Azure AD provides built-in audit logs for these operations.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
