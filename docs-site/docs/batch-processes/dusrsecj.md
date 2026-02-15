---
id: "PROC-DUSRSECJ-001"
title: "User Security Definition"
sidebar_position: 26
process_type: batch
domain: account-management
jcl_source: "DUSRSECJ.jcl"
cobol_programs: []
schedule: "on-demand"
sla: "N/A"
status: extracted
target_implementation: Azure AD B2C / Azure AD
---

# User Security Definition

## Overview

The User Security Definition job creates and populates the user security VSAM KSDS file used by the CardDemo application for authentication and authorization. The job creates a flat file from inline (in-stream) user data, defines a VSAM KSDS cluster, and loads the data into it.

The inline data contains 10 user records: 5 administrator users (ADMIN001 through ADMIN005) and 5 regular users (USER0001 through USER0005), each with associated passwords and security attributes. The VSAM file uses an 8-byte key at position 0 with fixed 80-byte records.

> **SECURITY WARNING:** This JCL contains hardcoded user credentials (usernames and passwords) in plain text within the in-stream data. This is a significant security concern that must be addressed during migration. In the Azure target, all user identity and credential management must be handled by Azure AD B2C (customer users) and Azure AD (internal/admin users) with proper credential storage, hashing, and rotation policies. Hardcoded credentials must never be migrated to the target system.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (IEFBR14/IEBGENER/IDCAMS utilities)
**Frequency:** On-demand (initial setup or security file refresh)
**Business owner:** Security Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 (PREDEL) | IEFBR14 | Delete previous USRSEC.PS flat file (catalog cleanup) | Catalog entry for USRSEC.PS | USRSEC.PS deleted | Normal if file does not exist |
| 2 (STEP01) | IEBGENER | Create sequential (PS) file from in-stream user data containing 5 admin users (ADMIN001-005) and 5 regular users (USER0001-0005) with passwords | In-stream data (SYSIN) | USRSEC.PS (sequential file, LRECL=80) | Job fails if output allocation fails |
| 3 (STEP02) | IDCAMS | DELETE existing USRSEC.VSAM.KSDS then DEFINE CLUSTER with KEYS(8,0), RECORDSIZE(80,80), REUSE, INDEXED | IDCAMS control statements | Empty VSAM KSDS cluster | DELETE failure ignored (may not exist); DEFINE failure aborts job |
| 4 (STEP03) | IDCAMS | REPRO from USRSEC.PS to USRSEC.VSAM.KSDS (load user records into VSAM) | USRSEC.PS | Populated USRSEC.VSAM.KSDS | Job fails if REPRO encounters key duplicates or I/O errors |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| CLOSEFIL | Process | If USRSEC is open in CICS, it must be closed before redefinition |
| Security policy | Process | User data must be approved by security team before loading |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| OPENFIL | Process | USRSEC file must be opened in CICS after loading |
| CICS sign-on (COSGN00C) | System | User authentication depends on USRSEC VSAM file being populated and open |
| All CICS transactions | System | Every CICS transaction requires authenticated user session |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand | Security operations schedule |
| Completion deadline | N/A (utility job) | N/A |
| Maximum duration | Less than 1 minute | Operational expectation |
| Retry policy | Re-run after resolving errors (job is idempotent) | Operational runbook |
| Escalation | Security administrator if user data load fails | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| In-stream user data | JCL SYSIN | Fixed 80-byte records | JCL inline data | 10 user records: 5 admin (ADMIN001-005), 5 regular (USER0001-0005) with passwords |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| USRSEC.PS | Sequential (PS) | LRECL=80, fixed-length | STEP03 (IDCAMS REPRO input) | Intermediate flat file of user security records |
| USRSEC.VSAM.KSDS | VSAM KSDS | KEYS(8,0), RECORDSIZE(80,80), INDEXED | CICS sign-on transaction (COSGN00C) | User security file for authentication and authorization |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Expected condition | USRSEC.PS does not exist on PREDEL | IEFBR14 completes normally | No action needed |
| Expected condition | USRSEC.VSAM.KSDS does not exist on DELETE | IDCAMS continues to DEFINE | No action needed |
| Data error | Duplicate keys in user data | IDCAMS REPRO fails | Review inline data for duplicate user IDs, correct and re-run |
| System error | VSAM catalog or volume error | DEFINE or REPRO fails | Verify catalog and volume availability, re-run |

### Restart/Recovery Procedure

1. Check return codes for each step (PREDEL, STEP01, STEP02, STEP03)
2. Job is fully idempotent: re-running deletes and recreates everything from scratch
3. After successful completion, run OPENFIL to make USRSEC available to CICS
4. Verify user sign-on works by testing with a known user ID

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure AD B2C (customer identity) / Azure AD (internal staff)
**Trigger:** N/A (identity platform configuration, not a batch job)
**CRON expression:** N/A

The VSAM-based user security file is completely replaced by cloud identity services in the Azure target. Customer-facing authentication uses Azure AD B2C with modern authentication flows (OAuth 2.0/OIDC). Internal/admin user authentication uses Azure AD with role-based access control (RBAC). There is no file-based user store in the target architecture.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| USRSEC.VSAM.KSDS | Azure AD B2C (customers) / Azure AD (staff) | Cloud identity replaces file-based auth |
| User ID (8-byte key) | Azure AD object ID / user principal name | Unique identifier per identity provider |
| Hardcoded passwords in JCL | Azure AD B2C password policies | Passwords managed by identity platform with hashing, complexity rules, and rotation |
| Admin users (ADMIN001-005) | Azure AD security group "CardDemo Admins" | RBAC role assignment replaces file-based role flag |
| Regular users (USER0001-0005) | Azure AD B2C consumer accounts | Self-service registration with MFA |
| CICS sign-on (COSGN00C) | Azure AD B2C sign-in flow | OAuth 2.0 authorization code flow with PKCE |
| IEBGENER/IDCAMS user load | Azure AD B2C Graph API / bulk user creation | Automated provisioning via Microsoft Graph API |

### Security Migration Requirements

- All hardcoded credentials must be eliminated; no plaintext passwords in any configuration
- Implement Azure AD B2C custom policies for authentication flows
- Enable Multi-Factor Authentication (MFA) for all admin users
- Implement Azure AD Conditional Access policies for risk-based authentication
- User migration requires password reset flow (existing VSAM passwords cannot be securely migrated)
- Audit logging via Azure AD sign-in logs replaces mainframe SMF records

### Parallel-Run Considerations

- **Comparison strategy:** User authentication success/failure rates compared between mainframe and Azure AD B2C
- **Tolerance:** Zero tolerance for authentication failures that succeed on mainframe (no user lockout regression)
- **Comparison frequency:** Daily during parallel-run period
- During parallel-run, both authentication systems must be operational; users may authenticate against either system

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| USEC-BR-001 | GDPR Art. 32 | User credentials must be stored with appropriate security measures (encryption, hashing); hardcoded plaintext passwords violate this requirement |
| USEC-BR-002 | PSD2 Art. 97 | Strong Customer Authentication (SCA) required for payment initiation; Azure AD B2C MFA satisfies this requirement |
| USEC-BR-003 | FFFS 2014:5 Ch. 8 | Access control to payment systems must be role-based with audit trail |
| USEC-BR-004 | DORA Art. 9 | ICT systems must implement robust authentication and access control mechanisms |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
