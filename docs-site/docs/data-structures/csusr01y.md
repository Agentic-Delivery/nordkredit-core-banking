---
id: DS-USEC-001
title: "User Security Record"
copybook_name: "CSUSR01Y.cpy"
domain: "user-security"
used_by_programs: [COSGN00C, COUSR00C, COUSR01C, COUSR02C, COUSR03C]
record_length: 80
status: "extracted"
target_schema: "dbo.UserSecurity"
sidebar_position: 31
---

# DS-USEC-001: User Security Record (CSUSR01Y)

## Overview

The `CSUSR01Y.cpy` copybook defines the **User Security Record**, the authentication and authorization data structure for users of the CardDemo CICS application. This record is stored in the VSAM file `USRSEC` and contains user credentials (user ID and password), user identity (first and last name), and role information (user type).

This is a critical security-sensitive record. The password is stored in **plaintext** in the COBOL system, which is a significant security vulnerability that must be addressed in the .NET migration.

**Source file:** `CSUSR01Y.cpy`
**VSAM file:** `USRSEC`
**Record length:** 80 bytes
**Used by:** `COSGN00C` (Sign-On), `COUSR00C` (User List), `COUSR01C` (User Add), `COUSR02C` (User Update), `COUSR03C` (User Delete)

## Source COBOL

```cobol
01 SEC-USER-DATA.
  05 SEC-USR-ID                 PIC X(08).
  05 SEC-USR-FNAME              PIC X(20).
  05 SEC-USR-LNAME              PIC X(20).
  05 SEC-USR-PWD                PIC X(08).
  05 SEC-USR-TYPE               PIC X(01).
  05 SEC-USR-FILLER             PIC X(23).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Nullable | Target Column |
|---|------------|-----------|--------|--------|------|-------------|----------|---------------|
| 1 | `SEC-USR-ID` | `X(08)` | 0 | 8 | Alphanumeric | User identifier (primary key) | No | `UserId` (NVARCHAR(50)) |
| 2 | `SEC-USR-FNAME` | `X(20)` | 8 | 20 | Alphanumeric | User first name | No | `FirstName` (NVARCHAR(50)) |
| 3 | `SEC-USR-LNAME` | `X(20)` | 28 | 20 | Alphanumeric | User last name | No | `LastName` (NVARCHAR(50)) |
| 4 | `SEC-USR-PWD` | `X(08)` | 48 | 8 | Alphanumeric | Password (**PLAINTEXT -- CRITICAL SECURITY ISSUE**) | No | **NOT MIGRATED** (replaced by Azure AD) |
| 5 | `SEC-USR-TYPE` | `X(01)` | 56 | 1 | Alphanumeric | User type: `A` = Admin, `U` = User | No | `UserType` (NVARCHAR(10)) / Azure AD role |
| 6 | `SEC-USR-FILLER` | `X(23)` | 57 | 23 | Filler | Reserved/unused space | N/A | Not migrated |

**Total record length:** 80 bytes

## Field Notes

1. **SEC-USR-ID** (`PIC X(08)`): The primary key for user identification. Limited to 8 characters due to CICS/RACF conventions. Values are typically uppercase alphanumeric (e.g., `ADMIN001`, `USER0001`). In Azure AD, user identifiers are GUIDs or UPNs (user principal names), which are significantly longer.

2. **SEC-USR-FNAME / SEC-USR-LNAME** (`PIC X(20)` each): User's first and last name, right-padded with spaces. Limited to 20 characters each. In Azure AD, display names support much longer values and Unicode characters (important for Swedish names with characters like a-ring and o-umlaut).

3. **SEC-USR-PWD** (`PIC X(08)`): **CRITICAL SECURITY ISSUE.** The password is stored in plaintext with a maximum length of 8 characters. There is no hashing, salting, or encryption. This is the most significant security finding in the CardDemo system. The COBOL sign-on program (`COSGN00C`) simply compares the entered password character-by-character against this stored value. In the .NET migration, this field is **NOT migrated** -- authentication is replaced entirely by Azure AD / Azure AD B2C.

4. **SEC-USR-TYPE** (`PIC X(01)`): User role flag. Values:
   - `A` = Administrator (access to admin menu, user management, all operations)
   - `U` = Regular user (access to main menu, view/limited operations)

   This binary role model is replaced by Azure AD role-based access control with more granular role definitions.

5. **SEC-USR-FILLER** (`PIC X(23)`): Reserved space to pad the record to 80 bytes. Not migrated. May contain residual data from prior record layouts.

## EBCDIC Encoding Notes

| Field | EBCDIC Consideration | Migration Action |
|-------|---------------------|-----------------|
| `SEC-USR-ID` | EBCDIC uppercase letters `C1`-`C9`/`D1`-`D9`, digits `F0`-`F9` | Convert to ASCII/UTF-8; preserve case |
| `SEC-USR-FNAME` / `SEC-USR-LNAME` | EBCDIC alphanumeric with spaces (`40`) | Convert EBCDIC to UTF-8; trim trailing spaces; verify Swedish character support (a-ring=`8C`, o-umlaut=`9C` in EBCDIC CP 1147) |
| `SEC-USR-PWD` | EBCDIC alphanumeric | **DO NOT MIGRATE** -- passwords cannot be converted. Users must re-authenticate via Azure AD. |
| `SEC-USR-TYPE` | EBCDIC `C1`=`A`, `E4`=`U` | Convert to ASCII; map to Azure AD role |
| `SEC-USR-FILLER` | Any byte values | Discard entirely |

## Migration Notes

### Target Architecture: Azure AD B2C (Customers) / Azure AD (Internal Staff)

The `USRSEC` VSAM file and its associated programs are **fully replaced** by Azure AD. There is no `dbo.UserSecurity` table in the target schema for authentication purposes. User identity and authentication are delegated entirely to Azure AD.

### User Migration Strategy

```
USRSEC VSAM file
    |
    +-- Extract user records (SEC-USR-ID, SEC-USR-FNAME, SEC-USR-LNAME, SEC-USR-TYPE)
    |
    +-- Create Azure AD user accounts:
    |     - DisplayName = FNAME + LNAME
    |     - UserPrincipalName = derived from SEC-USR-ID + @nordkredit.se
    |     - Role = map SEC-USR-TYPE to Azure AD role
    |     - Password = TEMPORARY (force reset on first login)
    |
    +-- DO NOT migrate SEC-USR-PWD (plaintext passwords)
    |
    +-- Notify users of new credentials via secure channel
```

### Azure AD Role Mapping

| COBOL Value | COBOL Meaning | Azure AD Role | Permissions |
|-------------|---------------|---------------|-------------|
| `A` | Admin | `CardDemo.Admin` | Full access: user management, all CRUD operations, reports, configuration |
| `U` | User | `CardDemo.User` | Read access: account view, card list/view, transaction list/view, reports |
| (new) | (not in COBOL) | `CardDemo.AccountManager` | Account and card update operations |
| (new) | (not in COBOL) | `CardDemo.TransactionEntry` | Transaction add and bill payment |
| (new) | (not in COBOL) | `CardDemo.UserAdmin` | User management only (COUSR00-03 equivalent) |

### Interim Migration Table (Optional)

If a local user reference table is needed during the migration period (for parallel-run comparison or audit purposes), the following minimal schema can be used:

```sql
CREATE TABLE dbo.UserSecurity (
    UserId              NVARCHAR(50)    NOT NULL,
    FirstName           NVARCHAR(50)    NOT NULL,
    LastName            NVARCHAR(50)    NOT NULL,
    UserType            NVARCHAR(10)    NOT NULL,  -- 'Admin' or 'User'
    AzureAdObjectId     UNIQUEIDENTIFIER NULL,     -- Link to Azure AD
    MigratedFromCobol   BIT             NOT NULL DEFAULT 1,
    MigratedAt          DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),

    -- Audit columns
    CreatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),
    UpdatedAt           DATETIME2       NOT NULL DEFAULT SYSUTCDATETIME(),

    CONSTRAINT PK_UserSecurity PRIMARY KEY (UserId)
);

-- NOTE: No password column. Authentication is handled by Azure AD.
```

### Post-Migration Validation

```sql
-- Verify all COBOL users were migrated
-- Compare against extracted USRSEC record count
SELECT COUNT(*) AS MigratedUserCount FROM dbo.UserSecurity WHERE MigratedFromCobol = 1;

-- Verify user type distribution matches COBOL source
SELECT UserType, COUNT(*) AS Cnt FROM dbo.UserSecurity GROUP BY UserType;

-- Verify Azure AD linkage
SELECT COUNT(*) AS UnlinkedUsers FROM dbo.UserSecurity WHERE AzureAdObjectId IS NULL;
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **GDPR** Art. 5(1)(f) | Integrity and confidentiality -- appropriate security of personal data | **CRITICAL:** The plaintext password storage in `SEC-USR-PWD` violates GDPR security requirements. The migration to Azure AD with proper password hashing (bcrypt/PBKDF2) and MFA support resolves this violation. This finding should be documented in the migration audit trail. |
| **GDPR** Art. 32 | Security of processing -- encryption, pseudonymization | User data (names, IDs) must be protected with appropriate technical measures. Azure AD provides encryption at rest and in transit. The interim migration table must also be encrypted (Azure SQL TDE). |
| **GDPR** Art. 17 | Right to erasure | User records must support deletion requests. Azure AD supports user account deletion. The interim migration table must also support record deletion with cascading cleanup. |
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- access management and authentication | The 8-character plaintext password with no complexity requirements is insufficient for financial system access control. Azure AD enforces modern password policies, MFA, and conditional access. All authentication events must be logged. |
| **PSD2** Art. 97 | Strong Customer Authentication (SCA) | The current system does not support SCA (single-factor plaintext password only). Azure AD B2C with BankID integration provides compliant SCA for customer-facing operations. |
| **DORA** Art. 9 | ICT security policies -- authentication and access control | Financial entities must implement strong authentication. The migration from plaintext passwords to Azure AD with MFA directly addresses DORA authentication requirements. The migration timeline should be documented as a risk remediation action. |
| **AML/KYC** (FFFS 2017:11) | User identification for audit trail | All user actions must be attributable to an identified individual. Azure AD provides non-repudiable user identification via signed JWT tokens with unique user identifiers. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
