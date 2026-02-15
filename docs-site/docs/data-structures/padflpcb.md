---
id: DS-PCB-DFL-001
title: "PADFL PCB - Detail File"
copybook_name: "PADFLPCB.CPY"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 0
status: "extracted"
target_schema: "N/A (IMS infrastructure)"
sidebar_position: 42
---

# DS-PCB-DFL-001: PADFL PCB - Detail File (PADFLPCB)

## Overview

The `PADFLPCB.CPY` copybook defines the **Program Communication Block (PCB)** for the authorization detail file database. A PCB is an IMS control structure that maintains the state of a program's connection to an IMS database. It contains the database name, current segment level, operation status code, processing options, and key feedback area.

This is **not a data record** -- it is IMS infrastructure that manages database access for the pending authorization detail segments (CIPAUDTY). In the .NET target architecture, this is replaced by Entity Framework Core's `DbContext` and repository configuration.

**Source file:** `PADFLPCB.CPY`
**Used by:** `CCPAUTH1` (IMS authorization program)

## Source COBOL

```cobol
01 PADFLPCB.
   05 PADFL-DBDNAME             PIC X(08).
   05 PADFL-SEG-LEVEL           PIC X(02).
   05 PADFL-PCB-STATUS          PIC X(02).
   05 PADFL-PCB-PROCOPT         PIC X(04).
   05 FILLER                    PIC S9(05) COMP.
   05 PADFL-SEG-NAME            PIC X(08).
   05 PADFL-KEYFB-NAME          PIC S9(05) COMP.
   05 PADFL-NUM-SENSEGS         PIC S9(05) COMP.
   05 PADFL-KEYFB               PIC X(255).
```

## Field Definitions

| # | Field Name | PIC Clause | Length | Type | Description |
|---|------------|-----------|--------|------|-------------|
| 1 | `PADFL-DBDNAME` | `X(08)` | 8 | Alphanumeric | IMS Database Description (DBD) name |
| 2 | `PADFL-SEG-LEVEL` | `X(02)` | 2 | Alphanumeric | Current segment level in hierarchy |
| 3 | `PADFL-PCB-STATUS` | `X(02)` | 2 | Alphanumeric | DL/I call status code |
| 4 | `PADFL-PCB-PROCOPT` | `X(04)` | 4 | Alphanumeric | Processing options (read, update, insert, delete) |
| 5 | `FILLER` | `S9(05) COMP` | 2 | Binary | Reserved (JCB pointer) |
| 6 | `PADFL-SEG-NAME` | `X(08)` | 8 | Alphanumeric | Name of the last accessed segment |
| 7 | `PADFL-KEYFB-NAME` | `S9(05) COMP` | 2 | Binary | Length of key feedback area |
| 8 | `PADFL-NUM-SENSEGS` | `S9(05) COMP` | 2 | Binary | Number of sensitive segments |
| 9 | `PADFL-KEYFB` | `X(255)` | 255 | Alphanumeric | Key feedback area (concatenated keys) |

## Field Notes

1. **PADFL-DBDNAME** -- The name of the IMS Database Description (DBD) that this PCB is associated with. For the authorization detail file, this identifies the IMS database containing pending authorization detail segments.

2. **PADFL-SEG-LEVEL** -- After each DL/I call, IMS sets this field to the hierarchical level number of the segment that was accessed. Level `01` is the root segment. This helps the program understand its position in the IMS hierarchy.

3. **PADFL-PCB-STATUS** -- The status code returned by IMS after each DL/I call. This is the primary mechanism for error detection:

   | Status | Meaning | .NET Equivalent |
   |--------|---------|----------------|
   | `'  '` (spaces) | Successful call | Normal return / entity found |
   | `'GE'` | Segment not found | `null` return / empty result |
   | `'II'` | Insert rule violation (duplicate key) | `DbUpdateException` (unique constraint) |
   | `'AI'` | I/O error | `SqlException` |
   | `'AM'` | Call issued against unopen database | `InvalidOperationException` (disposed context) |

4. **PADFL-PCB-PROCOPT** -- Processing options that define what operations the program is allowed to perform on this database:
   - `G` = Get (read)
   - `I` = Insert
   - `R` = Replace (update)
   - `D` = Delete
   - `A` = All (GIRD)

5. **PADFL-SEG-NAME** -- After each DL/I call, IMS sets this to the name of the segment that was processed. Useful for debugging and for programs that access multiple segment types.

6. **PADFL-KEYFB** -- The key feedback area contains the concatenated keys of all segments in the current path from the root to the accessed segment. This is 255 bytes long to accommodate deeply nested hierarchies. The `PADFL-KEYFB-NAME` field contains the actual length of the key data.

## Target Architecture Mapping

| Aspect | IMS PCB (Current) | .NET (Target) |
|--------|-------------------|---------------|
| **Database connection** | PCB in PSB (Program Specification Block) | `DbContext` with `DbSet<PendingAuthorizationDetail>` |
| **Status checking** | `IF PADFL-PCB-STATUS = '  '` | Exception handling / null checking |
| **Processing options** | `PADFL-PCB-PROCOPT` (G/I/R/D) | EF Core access patterns + authorization middleware |
| **Segment navigation** | Level/segment name feedback | LINQ navigation properties |
| **Key feedback** | Concatenated key area | Entity primary key properties |
| **Database identity** | `PADFL-DBDNAME` | Connection string + DbContext configuration |

### .NET Equivalent

```csharp
// The PCB is replaced by DbContext configuration
public class AuthorizationDbContext : DbContext
{
    public DbSet<PendingAuthorizationDetail> PendingAuthorizationDetails { get; set; } = default!;

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.Entity<PendingAuthorizationDetail>(entity =>
        {
            entity.HasKey(e => e.PendingAuthDetailId);
            entity.HasIndex(e => e.TransactionId).IsUnique();
            // Additional configuration replaces PCB PROCOPT and DBD definitions
        });
    }
}
```

## Migration Notes

### Why This Copybook Exists

In IMS, every program that accesses a database must have a PCB defined for each database it uses. The PCB is allocated by IMS at program startup (via the PSB -- Program Specification Block) and is passed to the program as a parameter. The program uses the PCB to check the status of each DL/I call and to understand its position in the database hierarchy.

### Migration Strategy

PCB copybooks are **not migrated** as data structures. They represent IMS connection and state management that is entirely replaced by:

1. **Entity Framework DbContext** -- Replaces the PCB's database connection and state tracking role
2. **Exception handling** -- Replaces PCB status code checking
3. **Authorization middleware** -- Replaces PROCOPT access control
4. **EF Core model configuration** -- Replaces DBD segment definitions

### Status Code Migration

Every `IF PADFL-PCB-STATUS` check in the COBOL source must be mapped to appropriate .NET error handling:

```csharp
// COBOL: IF PADFL-PCB-STATUS = 'GE'  (not found)
// .NET:
var detail = await context.PendingAuthorizationDetails
    .FindAsync(transactionId);
if (detail is null)
{
    // Handle not found
}

// COBOL: IF PADFL-PCB-STATUS NOT = '  '  (any error)
// .NET:
try
{
    await context.SaveChangesAsync();
}
catch (DbUpdateException ex)
{
    // Handle database error
}
```

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls -- database access must be controlled and audited. | The PROCOPT field controlled access in IMS. In .NET, equivalent access control must be implemented via EF Core interceptors, authorization middleware, and SQL Server row-level security where appropriate. |
| **DORA** Art. 11 | ICT data integrity -- database operations must be reliable and recoverable. | IMS provides automatic recovery via log records. Entity Framework must be configured with appropriate transaction isolation levels and retry policies. Azure SQL provides equivalent recovery capabilities. |
| **DORA** Art. 9 | ICT risk management -- database availability. | The PCB represents a database connection. The .NET replacement must implement connection pooling, health checks, and failover to meet the same availability requirements. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
