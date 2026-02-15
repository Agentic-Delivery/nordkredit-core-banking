---
id: "PROC-CBADMCDJ-001"
title: "Admin Card Processing (CICS Resource Definitions)"
sidebar_position: 29
process_type: batch
domain: account-management
jcl_source: "CBADMCDJ.jcl"
cobol_programs: []
schedule: "on-demand (initial setup)"
sla: "N/A"
status: extracted
target_implementation: Azure App Service / Azure AD
---

# Admin Card Processing (CICS Resource Definitions)

## Overview

The Admin Card Processing job defines all CICS resources required by the CardDemo application using the CICS System Definition (CSD) utility DFHCSDUP. This is an initial setup job that registers the application's programs, mapsets, transactions, and library with the CICS region so that online transaction processing can function.

The job defines the complete CardDemo CICS resource set including: a program library (LOADLIB), mapset definitions for all screen layouts, program definitions for all COBOL transaction programs, transaction definitions for the main application and test programs, and a group list that bundles all resources together for installation.

This is a foundational infrastructure job that must run before any CardDemo CICS transactions can be used.

**Source system:** IBM z/OS mainframe
**Transaction type:** JCL batch (CICS CSD utility)
**Frequency:** On-demand (initial setup, or after resource changes)
**Business owner:** Systems Operations Team

## Process Steps

| Step | Program | Description | Input | Output | Error Handling |
|------|---------|-------------|-------|--------|----------------|
| 1 | DFHCSDUP | Define all CICS resources for CardDemo application with PARM='CSD(READWRITE),PAGESIZE(60),NOCOMPAT'. Defines: LIBRARY (COM2DOLL -> LOADLIB), 13 MAPSETs (COSGN00M, COACT00S, COACTVWS, COACTUPS, COACTDES, COTRN00S, COTRNVWS, COTRNVDS, COTRNATS, COBIL00S, COADM00S, COTSTP1S-COTSTP4S), 15 PROGRAMs (COSGN00C, COACT00C, COACTVWC, COACTUPC, COACTDEC, COTRN00C, COTRNVWC, COTRNVDC, COTRNATC, COBIL00C, COADM00C, COTSTP1C-COTSTP4C), 5 TRANSACTIONs (CCDM, CCT1-CCT4), and LIST GROUP(CARDDEMO) | In-stream CSD commands (SYSIN) | Updated CICS CSD file | Job fails if CSD is locked or commands contain syntax errors |

## CICS Resource Definitions

### Library

| Resource | Name | Description |
|----------|------|-------------|
| LIBRARY | COM2DOLL | Points to LOADLIB containing all CardDemo COBOL programs |

### Mapsets (Screen Layouts)

| Mapset | Description |
|--------|-------------|
| COSGN00M | Sign-on screen |
| COACT00S | Account list screen |
| COACTVWS | Account view screen |
| COACTUPS | Account update screen |
| COACTDES | Account detail screen |
| COTRN00S | Transaction list screen |
| COTRNVWS | Transaction view screen |
| COTRNVDS | Transaction view detail screen |
| COTRNATS | Transaction add screen |
| COBIL00S | Billing screen |
| COADM00S | Administration screen |
| COTSTP1S - COTSTP4S | Test program screens (4 screens) |

### Programs

| Program | Description |
|---------|-------------|
| COSGN00C | Sign-on processing |
| COACT00C | Account list processing |
| COACTVWC | Account view processing |
| COACTUPC | Account update processing |
| COACTDEC | Account detail processing |
| COTRN00C | Transaction list processing |
| COTRNVWC | Transaction view processing |
| COTRNVDC | Transaction view detail processing |
| COTRNATC | Transaction add processing |
| COBIL00C | Billing processing |
| COADM00C | Administration processing |
| COTSTP1C - COTSTP4C | Test programs (4 programs) |

### Transactions

| Transaction ID | Description |
|---------------|-------------|
| CCDM | Main CardDemo application transaction |
| CCT1 | Test program 1 transaction |
| CCT2 | Test program 2 transaction |
| CCT3 | Test program 3 transaction |
| CCT4 | Test program 4 transaction |

### Group

| Group | Description |
|-------|-------------|
| CARDDEMO | List group containing all CardDemo resources for batch installation |

## Dependencies

### Upstream

| Dependency | Type | Description |
|-----------|------|-------------|
| COBOL program compilation | Process | All COBOL programs must be compiled and available in LOADLIB |
| CICS region | Infrastructure | CICS region must be available (CSD can be updated while region is active) |
| Mapset assembly | Process | All BMS mapsets must be assembled and linked into LOADLIB |

### Downstream

| Dependent | Type | Description |
|-----------|------|-------------|
| CICS NEWCOPY/INSTALL | Process | After CSD update, resources must be installed (CICS INSTALL GROUP) |
| OPENFIL | Process | File definitions referenced by programs must be opened |
| DUSRSECJ | Process | User security file must be populated for sign-on program |
| All CardDemo CICS transactions | System | Online transactions cannot execute until resources are defined and installed |

## SLA Requirements

| Requirement | Value | Source |
|------------|-------|--------|
| Start time | On-demand (initial setup or after code changes) | Development/operations schedule |
| Completion deadline | N/A (utility job) | N/A |
| Maximum duration | Less than 1 minute | Operational expectation |
| Retry policy | Re-run after resolving CSD lock or syntax errors | Operational runbook |
| Escalation | CICS systems programmer if CSD errors occur | Incident management |

## Input/Output Files

### Input

| File | Type | Format | Source | Description |
|------|------|--------|--------|-------------|
| CSD commands | In-stream (SYSIN) | DFHCSDUP command syntax | JCL | DEFINE statements for LIBRARY, MAPSET, PROGRAM, TRANSACTION, and LIST resources |
| DFHCSD | VSAM KSDS | CICS CSD format | CICS region | Existing CICS System Definition file |

### Output

| File | Type | Format | Consumer | Description |
|------|------|--------|----------|-------------|
| DFHCSD | VSAM KSDS | CICS CSD format | CICS region | Updated CSD with all CardDemo resource definitions |

## Error Handling and Recovery

### Error Categories

| Category | Condition | Action | Recovery |
|----------|-----------|--------|----------|
| Lock contention | CSD locked by another job or CICS transaction | Job fails or waits | Wait for lock release, re-run |
| Syntax error | Invalid DFHCSDUP command syntax | Command rejected, job continues | Correct syntax in JCL, re-run |
| Duplicate definition | Resource already defined in group | Command returns warning | Existing definition is replaced (READWRITE mode) |
| System error | CSD file unavailable or corrupted | Job fails | Restore CSD from backup, re-run |

### Restart/Recovery Procedure

1. Check DFHCSDUP return codes and messages in job output
2. Individual command failures are logged but may not abort the job
3. Re-run is safe: DEFINE with existing resources replaces the definitions
4. After successful CSD update, install the group in CICS: CEMT SET GROUP(CARDDEMO) INSTALL

## Target Azure Implementation Notes

### Architecture

**Target service:** Azure App Service configuration / Azure AD application registration
**Trigger:** N/A (one-time infrastructure setup via Bicep/ARM templates and CI/CD pipeline)
**CRON expression:** N/A

The CICS CSD resource definitions map to Azure infrastructure configuration. CICS programs become Azure App Service hosted .NET applications, mapsets become web UI components (Razor Pages/Blazor), transactions become REST API endpoints, and the group/library concept maps to application deployment units.

### Migration Mapping

| Mainframe Component | Azure Component | Notes |
|--------------------|----------------|-------|
| DFHCSDUP CSD utility | Bicep templates + CI/CD pipeline | Infrastructure-as-code replaces CSD utility |
| LIBRARY (COM2DOLL) | Azure App Service deployment package | .NET application published via CI/CD |
| MAPSET definitions (13 screens) | Razor Pages / Blazor components | Web UI replaces BMS map screens |
| PROGRAM definitions (15 programs) | C# controller classes / services | Business logic in .NET classes |
| TRANSACTION CCDM | REST API endpoints | HTTP routes replace CICS transaction IDs |
| TRANSACTION CCT1-CCT4 (test) | Integration test endpoints / test harness | Test transactions become automated tests |
| GROUP CARDDEMO | Azure resource group + App Service | Logical grouping via Azure resource group |
| CICS INSTALL GROUP | Azure App Service deployment slot swap | Zero-downtime deployment |

### Program-to-Service Migration Reference

| CICS Program | Function | Target C# Service |
|-------------|----------|------------------|
| COSGN00C | Sign-on | Azure AD B2C authentication flow |
| COACT00C | Account list | AccountListService |
| COACTVWC | Account view | AccountViewService |
| COACTUPC | Account update | AccountUpdateService |
| COACTDEC | Account detail | AccountDetailService |
| COTRN00C | Transaction list | TransactionListService |
| COTRNVWC | Transaction view | TransactionViewService |
| COTRNVDC | Transaction view detail | TransactionDetailService |
| COTRNATC | Transaction add | TransactionCreateService |
| COBIL00C | Billing | BillingService |
| COADM00C | Administration | AdminService |

### Parallel-Run Considerations

- **Comparison strategy:** Not applicable (infrastructure setup, no business data output)
- **Tolerance:** N/A
- **Comparison frequency:** N/A
- Functional equivalence validated through end-to-end testing of each migrated transaction

### Regulatory Traceability

| Requirement ID | Regulation | Description |
|---------------|------------|-------------|
| N/A | DORA Art. 9 | ICT systems access control and authentication infrastructure must be properly configured and documented |
| N/A | FFFS 2014:5 Ch. 8 | Payment system application configuration must follow change management procedures |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
