---
title: CSUSR01Y — User Data
sidebar_position: 7
---

# CSUSR01Y — User Data

## Overview

| Attribute         | Value                                    |
|-------------------|------------------------------------------|
| **Copybook**      | CSUSR01Y.cpy                             |
| **Record Name**   | Signed-on User Data                      |
| **Record Length**  | Variable (working storage)               |
| **VSAM File**     | N/A — CICS security context              |
| **Domain**        | User/Security (cross-cutting)            |
| **Status**        | Copybook not yet in repo (depends on #125) |

## Purpose

Defines the signed-on user data structure used across all CICS programs for authentication
and authorization context. Contains the current user's identity information as retrieved from
the CICS security facility (RACF/Top Secret). Every program that performs user-specific
operations includes this copybook.

## Field Definitions

The exact field layout of CSUSR01Y is not yet available in the repository (tracked in #125).
Based on standard CardDemo patterns, the typical user data structure includes:

| # | Field Name          | PIC Clause   | Offset | Length | Description                              |
|---|---------------------|--------------|--------|--------|------------------------------------------|
| 1 | USR-USERID          | PIC X(8)     | 0      | 8      | CICS user ID (from EIBUSERID)            |
| 2 | USR-FIRST-NAME      | PIC X(20)    | 8      | 20     | User's first name                        |
| 3 | USR-LAST-NAME       | PIC X(20)    | 28     | 20     | User's last name                         |
| 4 | USR-TYPE            | PIC X(1)     | 48     | 1      | User type: 'A' = admin, 'U' = regular   |

**Note:** Field definitions are approximate — verify against actual copybook when available in #125.

## .NET Class Mapping

The CICS user data pattern is replaced by Azure AD / Azure AD B2C identity:

| COBOL Concept        | .NET Replacement                          |
|----------------------|-------------------------------------------|
| USR-USERID           | `ClaimsPrincipal.Identity.Name` / Azure AD `oid` claim |
| USR-FIRST-NAME       | Azure AD `given_name` claim               |
| USR-LAST-NAME        | Azure AD `family_name` claim              |
| USR-TYPE             | Azure AD roles / app roles                |
| RACF/Top Secret      | Azure AD B2C (customer) / Azure AD (internal) |

No direct .NET entity maps to CSUSR01Y. User identity flows through ASP.NET Core's `ClaimsPrincipal` infrastructure.

## EBCDIC-to-Unicode Conversion Notes

User data fields contain names that may include Swedish characters (Å, Ä, Ö). Use `EbcdicConverter.ConvertToUnicode()` with IBM Code Page 1143 if extracting user data from CICS logs or audit trails during migration.

## Referencing Programs

| Program     | Usage                                    |
|-------------|------------------------------------------|
| COCRDLIC    | Card list — user context for authorization |
| COCRDSLC    | Card detail — user context for audit trail |
| COCRDUPC    | Card update — user context for change authorization |
| *(all CICS programs)* | Every program that needs user identity |

## Regulatory Traceability

| Regulation          | Requirement                                |
|---------------------|--------------------------------------------|
| GDPR Art. 5(1)(c)  | Minimize user data stored in working storage |
| GDPR Art. 5(1)(f)  | Secure handling of user identity data      |
| PSD2 Art. 97       | SCA — user identity for strong authentication |
| FFFS 2014:5 Ch.6   | Audit trail — user identity for all operations |
