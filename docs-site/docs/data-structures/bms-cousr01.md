---
id: DS-BMS-USR01
title: "User Add Screen"
copybook_name: "COUSR01.CPY"
domain: "ui-screens"
used_by_programs: [COUSR01C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 24
---

# DS-BMS-USR01: User Add Screen (COUSR01)

## Overview

The `COUSR01.CPY` copybook defines the **User Add Screen**, a CICS BMS screen for creating new system user accounts. This admin-only screen provides input fields for user ID, name, password, and user type.

The copybook defines two symbolic map records:
- **CUSR01AI** -- Input record (receives new user data from the terminal)
- **CUSR01AO** -- Output record (sends form labels and messages to the terminal)

**Source file:** `COUSR01.CPY`
**BMS map name:** COUSR01
**Used by:** `COUSR01C` (User Add CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `FNAMETEFN` | Input | 20 | First name |
| `LNAMETEFN` | Input | 20 | Last name |
| `USIDTEFN` | Input | 8 | User ID (unique identifier) |
| `PASTEFN` | Input | 8 | Password (dark field -- non-display) |
| `UTYPTEFN` | Input | 1 | User type (`A` = Admin, `U` = User) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **User creation form:** The administrator fills in all fields (first name, last name, user ID, password, user type) and presses Enter. The program validates the input and creates the user record in the USRSEC file.

2. **User ID uniqueness:** The program checks that the entered user ID does not already exist. If a duplicate is found, an error is displayed.

3. **Password field:** The `PASTEFN` field uses the BMS `DARK` attribute so the password is not visible on screen. The 8-character limit is a CICS/RACF constraint.

4. **User type:** The `UTYPTEFN` field accepts `A` (Admin) or `U` (regular User). This determines the user's access level throughout the application.

5. **Validation:** The program validates that all required fields are populated, the user ID is unique, and the user type is valid (`A` or `U`). Errors are displayed in `ERRMSG`.

6. **Admin-only access:** Only administrators can access this screen, enforced by the CICS program's authorization check.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COUSR01` on 3270 terminal | **Replaced by Azure AD user provisioning** or admin Blazor/Razor form |
| **Program** | `COUSR01C` (COBOL CICS program) | Azure AD Graph API user creation, or admin `UserController.Create` |
| **User store** | USRSEC VSAM file | Azure AD / Azure AD B2C directory |
| **Password** | 8-char, stored in USRSEC | Azure AD password policies (no application-level storage) |
| **User type** | `A`/`U` flag | Azure AD role/group assignment |
| **Authorization** | CICS RACF check | `[Authorize(Roles = "UserAdmin")]` policy |

### Migration Considerations

- **Azure AD provisioning:** User creation is replaced by Azure AD user provisioning via Microsoft Graph API or Azure Portal. The .NET application should not store passwords or manage authentication credentials directly.
- **If custom admin UI is retained:** Build a form that calls Microsoft Graph API to create the user in Azure AD, assign roles/groups, and send an invitation email with a temporary password or passwordless setup link.
- **Enhanced user attributes:** Azure AD supports richer user profiles (email, department, manager, phone) beyond the limited COBOL fields. The .NET admin form can capture these additional attributes.
- **Role assignment:** Replace the binary `A`/`U` type with granular Azure AD role assignment. The admin form should allow selecting from available application roles defined in the Azure AD app registration.
- **Password policy:** Azure AD enforces modern password policies automatically. The application does not need to implement password complexity validation; Azure AD handles this.
- **Audit logging:** User creation events must be logged for FSA (FFFS 2014:5) Ch. 8 and DORA compliance. Azure AD provides built-in audit logs for directory operations.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
