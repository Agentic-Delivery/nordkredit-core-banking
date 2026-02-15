---
id: DS-PCB-AUT-001
title: "PAUT PCB - Auth Base"
copybook_name: "PAUTBPCB.CPY"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 0
status: "extracted"
target_schema: "N/A (IMS infrastructure)"
sidebar_position: 44
---

# DS-PCB-AUT-001: PAUT PCB - Auth Base (PAUTBPCB)

## Overview

The `PAUTBPCB.CPY` copybook defines the **Program Communication Block (PCB)** for the authorization base database. This PCB manages the program's connection to the IMS database containing the base authorization data. It has the same structure as the detail file PCB (PADFLPCB), with the standard 255-byte key feedback area.

This is **not a data record** -- it is IMS infrastructure that is replaced by Entity Framework Core's `DbContext` in the .NET target architecture.

**Source file:** `PAUTBPCB.CPY`
**Used by:** `CCPAUTH1` (IMS authorization program)
**Related PCBs:** `PADFLPCB.CPY` (detail file), `PASFLPCB.CPY` (summary file)

## Source COBOL

```cobol
01 PAUTBPCB.
   05 PAUTB-DBDNAME             PIC X(08).
   05 PAUTB-SEG-LEVEL           PIC X(02).
   05 PAUTB-PCB-STATUS          PIC X(02).
   05 PAUTB-PCB-PROCOPT         PIC X(04).
   05 FILLER                    PIC S9(05) COMP.
   05 PAUTB-SEG-NAME            PIC X(08).
   05 PAUTB-KEYFB-NAME          PIC S9(05) COMP.
   05 PAUTB-NUM-SENSEGS         PIC S9(05) COMP.
   05 PAUTB-KEYFB               PIC X(255).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `PAUTB-DBDNAME` | `X(08)` | 8 | Alphanumeric | IMS Database Description (DBD) name |
| 2 | `PAUTB-SEG-LEVEL` | `X(02)` | 2 | Alphanumeric | Current segment level in hierarchy |
| 3 | `PAUTB-PCB-STATUS` | `X(02)` | 2 | Alphanumeric | DL/I call status code |
| 4 | `PAUTB-PCB-PROCOPT` | `X(04)` | 4 | Alphanumeric | Processing options (G/I/R/D/A) |
| 5 | `FILLER` | `S9(05) COMP` | 2 | Binary | Reserved (JCB pointer) |
| 6 | `PAUTB-SEG-NAME` | `X(08)` | 8 | Alphanumeric | Name of the last accessed segment |
| 7 | `PAUTB-KEYFB-NAME` | `S9(05) COMP` | 2 | Binary | Length of key feedback area |
| 8 | `PAUTB-NUM-SENSEGS` | `S9(05) COMP` | 2 | Binary | Number of sensitive segments |
| 9 | `PAUTB-KEYFB` | `X(255)` | 255 | Alphanumeric | Key feedback area (concatenated keys) |

## Field Notes

The field descriptions are identical to those in `PADFLPCB.CPY` (DS-PCB-DFL-001). This PCB follows the standard IMS PCB layout. Key points:

1. **PAUTB-PCB-STATUS** -- Checked after every DL/I call to the authorization base database. Standard IMS status codes apply (spaces = success, `GE` = not found, etc.).

2. **PAUTB-PCB-PROCOPT** -- Processing options for the base authorization database. Determines whether the program can read, insert, replace, or delete segments in this database.

3. **PAUTB-KEYFB** (`PIC X(255)`) -- Same size as the detail file PCB's key feedback area, indicating the base authorization database may have a similarly complex hierarchical structure with compound keys.

## PCB Comparison

The authorization program (`CCPAUTH1`) uses three PCBs to access three separate IMS databases:

| PCB Copybook | Database Purpose | Key Feedback Size | Data Copybook |
|-------------|-----------------|-------------------|---------------|
| `PADFLPCB.CPY` | Authorization detail (individual transactions) | 255 bytes | `CIPAUDTY.cpy` |
| `PASFLPCB.CPY` | Authorization summary (account-level aggregates) | 100 bytes | `CIPAUSMY.cpy` |
| `PAUTBPCB.CPY` | Authorization base (reference data) | 255 bytes | (program-specific) |

## Target Architecture Mapping

| Aspect | IMS PCB (Current) | .NET (Target) |
|--------|-------------------|---------------|
| **Database connection** | PAUTBPCB in PSB | `DbSet` in `AuthorizationDbContext` |
| **Status checking** | `IF PAUTB-PCB-STATUS = '  '` | Exception handling / null checking |
| **Three separate PCBs** | PADFLPCB + PASFLPCB + PAUTBPCB | Single `AuthorizationDbContext` with multiple `DbSet` properties |

### .NET Equivalent

```csharp
// All three PCBs consolidated into one DbContext
public class AuthorizationDbContext : DbContext
{
    // Replaces PADFLPCB
    public DbSet<PendingAuthorizationDetail> PendingAuthorizationDetails { get; set; } = default!;

    // Replaces PASFLPCB
    public DbSet<PendingAuthorizationSummary> PendingAuthorizationSummaries { get; set; } = default!;

    // Replaces PAUTBPCB
    public DbSet<AuthorizationBase> AuthorizationBases { get; set; } = default!;
}
```

## Migration Notes

### PCB Consolidation

In IMS, separate databases require separate PCBs because each IMS database is an independent physical entity with its own DBD. In the relational model, all authorization data can reside in a single Azure SQL database with multiple tables, managed by a single `DbContext`.

### Status Code Migration

```csharp
// COBOL: CALL 'CBLTDLI' USING FUNC-GU PAUTBPCB AUTH-BASE-RECORD SSA-BASE
//        IF PAUTB-PCB-STATUS = '  ' ...

// .NET:
var authBase = await context.AuthorizationBases
    .FindAsync(key);
if (authBase is not null)
{
    // Process authorization base data
}
```

### IMS PSB (Program Specification Block) Context

The three PCBs (PADFLPCB, PASFLPCB, PAUTBPCB) are defined in the PSB for the `CCPAUTH1` program. The PSB specifies:
- Which databases the program can access (via PCBs)
- What operations are allowed on each database (via PROCOPT)
- The program's view of each database hierarchy (via SENSEG)

In .NET, the PSB's role is replaced by:
- `DbContext` configuration (which tables/entities are accessible)
- Authorization middleware (what operations are permitted)
- Entity configuration (how entities relate to each other)

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls over data access. | IMS PROCOPT controlled database-level access. The .NET replacement must implement equivalent access control via EF Core, SQL Server roles, and application-level authorization. |
| **DORA** Art. 9 | ICT risk management -- availability. | The authorization base database supports real-time authorization decisions. Azure SQL must meet the same availability requirements. |
| **DORA** Art. 11 | ICT data integrity. | IMS PCB status checking provided data integrity feedback. The .NET replacement must implement equivalent error detection and handling. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
