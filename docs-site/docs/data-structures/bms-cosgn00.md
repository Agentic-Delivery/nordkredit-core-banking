---
id: DS-BMS-SGN00
title: "Sign-On Screen"
copybook_name: "COSGN00.CPY"
domain: "ui-screens"
used_by_programs: [COSGN00C]
record_length: 0
status: "extracted"
target_schema: "N/A (screen layout)"
sidebar_position: 19
---

# DS-BMS-SGN00: Sign-On Screen (COSGN00)

## Overview

The `COSGN00.CPY` copybook defines the **Sign-On Screen**, the authentication entry point for the CardDemo CICS application. This screen collects user credentials (user ID and password) and displays system identification information. Upon successful authentication, the user is routed to the Main Menu Screen (COMEN01).

The copybook defines two symbolic map records:
- **CSGN00AI** -- Input record (receives user ID and password from the terminal)
- **CSGN00AO** -- Output record (sends system info and messages to the terminal)

**Source file:** `COSGN00.CPY`
**BMS map name:** COSGN00
**Used by:** `COSGN00C` (Sign-On CICS program)

## Key Screen Fields

| Field Name | Type | Length | Description |
|------------|------|--------|-------------|
| `TRNNAME` | Display | 4 | Transaction name (CICS transaction ID) |
| `PGMNAME` | Display | 8 | Program name display |
| `TITLE01` | Display | 40 | Screen title text |
| `CURDATE` | Display | 8 | Current date display |
| `CURTIME` | Display | 8 | Current time display |
| `APPLIDTEFN` | Display | 8 | CICS Application ID (APPLID) |
| `SYSIDTEFN` | Display | 4 | CICS System ID (SYSID) |
| `USETEFN` | Input | 8 | User ID (PIC X(8)) |
| `PASTEFN` | Input | 8 | Password (PIC X(8), dark field -- non-display) |
| `INFOMSG` | Display | 40 | Informational message area |
| `ERRMSG` | Display | 40 | Error message area |

## Screen Layout Notes

1. **Authentication flow:** The user enters their user ID and password, then presses Enter. The program `COSGN00C` validates the credentials against the user data file (USRSEC). On success, the user is routed to the Main Menu (COMEN01). On failure, an error message is displayed (e.g., "Invalid credentials") and the user can retry.

2. **Password field:** The `PASTEFN` field uses the BMS `DARK` attribute, meaning characters typed into this field are not displayed on the 3270 terminal (equivalent to `<input type="password">`).

3. **System identification:** The `APPLIDTEFN` and `SYSIDTEFN` fields display the CICS application and system identifiers, helping operators identify which CICS region they are connected to. This is important in environments with multiple CICS regions (development, test, production).

4. **Credential limits:** Both user ID and password are limited to 8 characters (`PIC X(8)`), which is a common CICS/RACF constraint. The .NET replacement removes this limitation.

5. **No lockout in BMS:** Account lockout logic (if any) is implemented in the `COSGN00C` program, not in the BMS map. The BMS map only defines the screen layout.

## Target Architecture Mapping

| Aspect | CICS/3270 (Current) | .NET (Target) |
|--------|---------------------|---------------|
| **Screen** | BMS map `COSGN00` on 3270 terminal | **Replaced entirely by Azure AD B2C (customer) / Azure AD (internal)** |
| **Program** | `COSGN00C` (COBOL CICS program) | ASP.NET authentication middleware (no custom login page) |
| **User store** | USRSEC VSAM file | Azure AD / Azure AD B2C directory |
| **Password handling** | 8-char password, custom validation | Azure AD password policies (complexity, length, rotation) |
| **Session creation** | CICS COMMAREA with user context | JWT/cookie-based authentication via ASP.NET Identity |
| **MFA** | Not supported | Azure AD MFA / BankID integration |

### Migration Considerations

- **Eliminate custom authentication:** The sign-on screen and its backing program are **fully replaced** by Azure AD B2C (for customer-facing access) and Azure AD (for internal bank staff). No custom login page should be built in the .NET application. Instead, the application redirects to the Azure AD sign-in experience.
- **BankID integration:** For Swedish customers, the .NET system should integrate with BankID as an identity provider through Azure AD B2C, providing the strong authentication required by PSD2 Art. 97 (Strong Customer Authentication).
- **Password policy upgrade:** The 8-character password limit in the COBOL system is a significant security limitation. Azure AD enforces modern password policies (minimum length, complexity, banned password lists) and supports passwordless authentication (FIDO2, Microsoft Authenticator).
- **User migration:** Existing user accounts from the USRSEC VSAM file must be migrated to Azure AD. Passwords cannot be migrated (COBOL password hashing is incompatible); users must reset passwords or use a migration flow with temporary credentials.
- **Audit logging:** All authentication events (successful login, failed login, lockout) must be logged per FSA (FFFS 2014:5) Ch. 8 and DORA requirements. Azure AD provides built-in sign-in logs that satisfy this requirement.
- **GDPR consent:** The .NET login flow should incorporate GDPR consent capture for data processing, which was not part of the original CICS sign-on process.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
