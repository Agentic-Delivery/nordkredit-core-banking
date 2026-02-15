---
id: DS-PCB-SFL-001
title: "PASFL PCB - Summary File"
copybook_name: "PASFLPCB.CPY"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 0
status: "extracted"
target_schema: "N/A (IMS infrastructure)"
sidebar_position: 43
---

# DS-PCB-SFL-001: PASFL PCB - Summary File (PASFLPCB)

## Overview

The `PASFLPCB.CPY` copybook defines the **Program Communication Block (PCB)** for the authorization summary file database. This PCB manages the program's connection to the IMS database containing the pending authorization summary segments (CIPAUSMY). It has the same structure as the detail file PCB (PADFLPCB) but with a smaller key feedback area, reflecting the simpler key structure of the summary database.

This is **not a data record** -- it is IMS infrastructure that is replaced by Entity Framework Core's `DbContext` in the .NET target architecture.

**Source file:** `PASFLPCB.CPY`
**Used by:** `CCPAUTH1` (IMS authorization program)
**Related PCBs:** `PADFLPCB.CPY` (detail file), `PAUTBPCB.CPY` (auth base)

## Source COBOL

```cobol
01 PASFLPCB.
   05 PASFL-DBDNAME             PIC X(08).
   05 PASFL-SEG-LEVEL           PIC X(02).
   05 PASFL-PCB-STATUS          PIC X(02).
   05 PASFL-PCB-PROCOPT         PIC X(04).
   05 FILLER                    PIC S9(05) COMP.
   05 PASFL-SEG-NAME            PIC X(08).
   05 PASFL-KEYFB-NAME          PIC S9(05) COMP.
   05 PASFL-NUM-SENSEGS         PIC S9(05) COMP.
   05 PASFL-KEYFB               PIC X(100).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `PASFL-DBDNAME` | `X(08)` | 8 | Alphanumeric | IMS Database Description (DBD) name |
| 2 | `PASFL-SEG-LEVEL` | `X(02)` | 2 | Alphanumeric | Current segment level in hierarchy |
| 3 | `PASFL-PCB-STATUS` | `X(02)` | 2 | Alphanumeric | DL/I call status code |
| 4 | `PASFL-PCB-PROCOPT` | `X(04)` | 4 | Alphanumeric | Processing options (G/I/R/D/A) |
| 5 | `FILLER` | `S9(05) COMP` | 2 | Binary | Reserved (JCB pointer) |
| 6 | `PASFL-SEG-NAME` | `X(08)` | 8 | Alphanumeric | Name of the last accessed segment |
| 7 | `PASFL-KEYFB-NAME` | `S9(05) COMP` | 2 | Binary | Length of key feedback area |
| 8 | `PASFL-NUM-SENSEGS` | `S9(05) COMP` | 2 | Binary | Number of sensitive segments |
| 9 | `PASFL-KEYFB` | `X(100)` | 100 | Alphanumeric | Key feedback area (concatenated keys) |

## Key Difference from PADFLPCB

| Aspect | PADFLPCB (Detail File) | PASFLPCB (Summary File) |
|--------|----------------------|------------------------|
| **Key feedback area** | `PIC X(255)` | `PIC X(100)` |
| **Database** | Authorization detail segments | Authorization summary segments |
| **Segment structure** | Complex (multiple child levels) | Simpler (summary by account) |
| **Key complexity** | Longer composite keys (date/time + detail fields) | Shorter keys (account ID based) |

The smaller key feedback area (100 bytes vs. 255 bytes) reflects the simpler hierarchical structure of the summary database, which uses account-based keys rather than the compound date/time keys in the detail database.

## Field Notes

The field descriptions are identical to those in `PADFLPCB.CPY` (DS-PCB-DFL-001). The PCB fields serve the same purpose regardless of which database they are associated with:

1. **PASFL-PCB-STATUS** -- The status code checked after every DL/I call to the summary database. The same status codes apply (spaces = success, `GE` = not found, etc.).

2. **PASFL-PCB-PROCOPT** -- Processing options for the summary database. The authorization program typically needs full access (GIRD) to the summary database to read current limits/balances and update them after each authorization decision.

3. **PASFL-KEYFB** (`PIC X(100)`) -- Smaller than the detail file's key feedback area because the summary database has a flatter hierarchy with shorter key paths. The primary key is the account ID (COMP-3 packed decimal), which requires only 6 bytes.

## Target Architecture Mapping

| Aspect | IMS PCB (Current) | .NET (Target) |
|--------|-------------------|---------------|
| **Database connection** | PASFLPCB in PSB | `DbSet<PendingAuthorizationSummary>` in `AuthorizationDbContext` |
| **Status checking** | `IF PASFL-PCB-STATUS = '  '` | Exception handling / null checking |
| **Processing options** | `PASFL-PCB-PROCOPT` | EF Core configuration + authorization middleware |
| **Key feedback** | `PASFL-KEYFB` (100 bytes) | Entity `AccountId` property |

### .NET Equivalent

```csharp
// Both PASFLPCB and PADFLPCB are consolidated into a single DbContext
public class AuthorizationDbContext : DbContext
{
    // Replaces PASFLPCB (summary file PCB)
    public DbSet<PendingAuthorizationSummary> PendingAuthorizationSummaries { get; set; } = default!;

    // Replaces PADFLPCB (detail file PCB)
    public DbSet<PendingAuthorizationDetail> PendingAuthorizationDetails { get; set; } = default!;
}
```

## Migration Notes

### PCB Consolidation

In IMS, each database requires a separate PCB. The authorization program uses three PCBs:
- `PADFLPCB` -- Detail file (pending authorization transactions)
- `PASFLPCB` -- Summary file (account-level summaries)
- `PAUTBPCB` -- Auth base (base authorization data)

In .NET, all three are consolidated into a single `DbContext` with multiple `DbSet` properties. The PCB's role (connection management, status tracking, access control) is handled by EF Core's built-in infrastructure.

### Status Code Migration

The same status code migration pattern applies as documented in `PADFLPCB.CPY`:

```csharp
// COBOL: CALL 'CBLTDLI' USING FUNC-GHU PASFLPCB PA-SUMMARY-RECORD SSA-SUMMARY
//        IF PASFL-PCB-STATUS = '  ' ...

// .NET:
var summary = await context.PendingAuthorizationSummaries
    .FindAsync(accountId);
if (summary is not null)
{
    // Process summary - entity is tracked for update
}
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 4 | Risk management -- real-time access to credit limits and balances. | The summary file PCB provides access to the account-level authorization data used for real-time credit decisions. The .NET replacement must provide equivalent or better performance for these critical lookups. |
| **DORA** Art. 9 | ICT risk management -- availability of critical systems. | The authorization summary database is read on every authorization request. The Azure SQL replacement must meet the same availability SLA. Connection pooling and health checks replace the PCB's connection management role. |
| **DORA** Art. 11 | ICT data integrity. | PCB status checking is the primary data integrity control in IMS programs. The .NET replacement must implement equivalent error detection via exception handling and retry policies. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
