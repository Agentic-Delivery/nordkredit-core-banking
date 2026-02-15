---
id: DS-BMS-USR03
title: "User Delete Screen"
copybook_name: "COUSR03.CPY"
domain: "ui-screens"
used_by_programs: [COUSR03C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 26
---

# DS-BMS-USR03: User Delete Screen (COUSR03)

## Overview

The `COUSR03.CPY` copybook defines the **User Delete Screen**, a CICS BMS screen for confirming the deletion of a system user account. All fields on this screen are display-only, showing the user's details for verification before the administrator confirms the deletion.

The copybook defines two symbolic map records:
- **CUSR03AI** -- Input record (receives the confirmation action from the terminal)
- **CUSR03AO** -- Output record (sends user data for review and messages to the terminal)

**Source file:** `COUSR03.CPY`
**BMS map name:** COUSR03
**Used by:** `COUSR03C` (User Delete CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `USIDTEFN` | Display | 8 | User ID (display-only) |
| `FNAMETEFN` | Display | 20 | First name (display-only) |
| `LNAMETEFN` | Display | 20 | Last name (display-only) |
| `UTYPTEFN` | Display | 1 | User type (display-only: `A` = Admin, `U` = User) |
| `CONFIRM` | Input | 1 | Confirmation flag (`Y`/`N`) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Confirmation screen:** This screen is a deletion confirmation dialog. All user fields (ID, name, type) are displayed as read-only so the administrator can verify they are deleting the correct user.

2. **Confirmation required:** The administrator must enter `Y` in the `CONFIRM` field and press Enter to proceed with deletion. Any other value cancels the operation.

3. **Hard delete:** The COBOL program performs a hard delete of the user record from the USRSEC file. There is no soft-delete or recycle bin in the mainframe implementation.

4. **No self-deletion guard:** The COBOL program may or may not prevent an administrator from deleting their own account. The .NET replacement must enforce a guard against self-deletion to prevent lockout.

5. **Navigation:** The user arrives at this screen by selecting a user for deletion from the User List Screen (COUSR00). After successful deletion, the program returns to the user list with a confirmation message.

6. **Admin-only access:** Only administrators can access this screen.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COUSR03` on 3270 terminal | **Replaced by Azure AD user management** or admin confirmation dialog |
| **Program** | `COUSR03C` (COBOL CICS program) | Azure AD Graph API user disable/delete, or admin `UserController.Delete` |
| **Confirmation** | `Y`/`N` text field | Confirmation modal dialog with user details |
| **Delete action** | Hard delete from USRSEC | Azure AD: disable account (soft delete) with retention period |
| **Authorization** | CICS RACF check | `[Authorize(Roles = "UserAdmin")]` policy |

### Migration Considerations

- **Soft delete, not hard delete:** The .NET implementation should disable (soft delete) the Azure AD account rather than permanently deleting it. Azure AD retains deleted users for 30 days, allowing recovery from accidental deletions. This is also important for audit trail preservation.
- **GDPR right to erasure:** While user accounts are disabled rather than deleted for operational purposes, GDPR right-to-erasure requests may require permanent deletion of personal data after the regulatory retention period expires. The .NET system must support a separate GDPR erasure workflow that respects retention requirements.
- **Self-deletion prevention:** The .NET implementation must prevent administrators from disabling their own account or the last remaining administrator account, preventing system lockout.
- **Cascading effects:** Deleting/disabling a user in the COBOL system only removes the USRSEC record. In the .NET system, consider the cascading effects: revoke active sessions, invalidate tokens, reassign owned resources.
- **Audit logging:** User deletion is a high-impact administrative action. The event must be logged with the deleted user's details, the performing administrator, timestamp, and reason for deletion, per FSA (FFFS 2014:5) Ch. 8 and DORA requirements.
- **Maker-checker:** Consider requiring a second administrator to approve user deletions (maker-checker pattern) to prevent unauthorized or accidental account removal.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
