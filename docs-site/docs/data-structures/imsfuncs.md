---
id: DS-IMS-001
title: "IMS Function Codes"
copybook_name: "IMSFUNCS.cpy"
domain: "authorization"
used_by_programs: [CCPAUTH1]
record_length: 0
status: "extracted"
target_schema: "N/A (IMS DL/I constants)"
sidebar_position: 41
---

# DS-IMS-001: IMS Function Codes (IMSFUNCS)

## Overview

The `IMSFUNCS.cpy` copybook defines **IMS DL/I Function Codes**, the constants used to invoke IMS database operations (Data Language/Interface calls). These are the IMS equivalents of SQL statements -- each function code tells IMS what type of database operation to perform (get, insert, replace, delete).

This copybook is included in any COBOL program that makes IMS DL/I calls. The function codes are passed as the first argument to the `CALL 'CBLTDLI'` statement, which is the COBOL interface to IMS.

**Source file:** `IMSFUNCS.cpy`
**Used by:** `CCPAUTH1` (IMS authorization program)

## Source COBOL

```cobol
01 FUNC-CODES.
   05 FUNC-GU                   PIC X(04)      VALUE 'GU  '.
   05 FUNC-GHU                  PIC X(04)      VALUE 'GHU '.
   05 FUNC-GN                   PIC X(04)      VALUE 'GN  '.
   05 FUNC-GHN                  PIC X(04)      VALUE 'GHN '.
   05 FUNC-GNP                  PIC X(04)      VALUE 'GNP '.
   05 FUNC-GHNP                 PIC X(04)      VALUE 'GHNP'.
   05 FUNC-REPL                 PIC X(04)      VALUE 'REPL'.
   05 FUNC-ISRT                 PIC X(04)      VALUE 'ISRT'.
   05 FUNC-DLET                 PIC X(04)      VALUE 'DLET'.
   05 PARMCOUNT                 PIC S9(05) VALUE +4 COMP-5.
```

## Field Definitions

| # | Field Name | PIC Clause | Value | IMS Function | Description |
|---|------------|-----------|-------|-------------|-------------|
| 1 | `FUNC-GU` | `X(04)` | `'GU  '` | Get Unique | Retrieve a specific segment by key |
| 2 | `FUNC-GHU` | `X(04)` | `'GHU '` | Get Hold Unique | Retrieve a specific segment for update |
| 3 | `FUNC-GN` | `X(04)` | `'GN  '` | Get Next | Retrieve the next segment sequentially |
| 4 | `FUNC-GHN` | `X(04)` | `'GHN '` | Get Hold Next | Retrieve the next segment for update |
| 5 | `FUNC-GNP` | `X(04)` | `'GNP '` | Get Next within Parent | Retrieve next child segment |
| 6 | `FUNC-GHNP` | `X(04)` | `'GHNP'` | Get Hold Next within Parent | Retrieve next child segment for update |
| 7 | `FUNC-REPL` | `X(04)` | `'REPL'` | Replace | Update a previously held segment |
| 8 | `FUNC-ISRT` | `X(04)` | `'ISRT'` | Insert | Insert a new segment |
| 9 | `FUNC-DLET` | `X(04)` | `'DLET'` | Delete | Delete a previously held segment |
| 10 | `PARMCOUNT` | `S9(05) COMP-5` | `+4` | Parameter Count | Number of parameters in DL/I call |

## Field Notes

### IMS DL/I Function Code Reference

1. **FUNC-GU (Get Unique)** -- Retrieves a specific segment by its key value. The segment search argument (SSA) specifies the key. This is the IMS equivalent of `SELECT ... WHERE key = value`. In Entity Framework, this maps to `FindAsync(key)` or `SingleOrDefaultAsync(x => x.Key == value)`.

2. **FUNC-GHU (Get Hold Unique)** -- Same as GU but establishes a "hold" on the segment, which is required before a REPL (update) or DLET (delete) operation. The hold is IMS's pessimistic locking mechanism. In Entity Framework, this maps to loading an entity with change tracking enabled (the default behavior).

3. **FUNC-GN (Get Next)** -- Retrieves the next segment in hierarchical sequence. Used for sequential processing of segments. Maps to iterating through query results (LINQ `foreach` over `IQueryable`).

4. **FUNC-GHN (Get Hold Next)** -- Sequential retrieval with hold for update. Combines GN navigation with the ability to update the retrieved segment.

5. **FUNC-GNP (Get Next within Parent)** -- Retrieves the next child segment under the current parent. Used to iterate over child segments without leaving the parent context. Maps to navigating a one-to-many relationship (`parent.Children.OrderBy(...)`).

6. **FUNC-GHNP (Get Hold Next within Parent)** -- Same as GNP but with hold for update. Used when iterating child segments and potentially updating some of them.

7. **FUNC-REPL (Replace)** -- Updates a segment that was previously retrieved with a "hold" call (GHU, GHN, GHNP). The modified segment data is written back to the database. Maps to `SaveChangesAsync()` in Entity Framework after modifying a tracked entity.

8. **FUNC-ISRT (Insert)** -- Inserts a new segment into the IMS database. The segment type and parent position are determined by the SSA. Maps to `AddAsync()` + `SaveChangesAsync()` in Entity Framework.

9. **FUNC-DLET (Delete)** -- Deletes a segment that was previously retrieved with a "hold" call. Maps to `Remove()` + `SaveChangesAsync()` in Entity Framework.

10. **PARMCOUNT** -- The number of parameters passed to the `CBLTDLI` call. The value `+4` indicates the standard parameter list: function code, PCB, I/O area, and SSA. `COMP-5` is a native binary format (little-endian on most platforms, but big-endian on z/OS).

### IMS Call Pattern

```cobol
*--- Typical IMS DL/I call in COBOL ---
CALL 'CBLTDLI' USING
    FUNC-GHU           *> Function code (e.g., Get Hold Unique)
    PADFLPCB            *> PCB (Program Communication Block)
    PA-DETAIL-RECORD    *> I/O area (data segment)
    SSA-DETAIL          *> Segment Search Argument (key)

IF PADFL-PCB-STATUS = '  '    *> Status '  ' = success
    PERFORM PROCESS-DETAIL
END-IF
```

## IMS to Entity Framework Mapping

| IMS Function | SQL Equivalent | Entity Framework Method | Use Case |
|-------------|---------------|------------------------|----------|
| `GU` | `SELECT ... WHERE key = ?` | `FindAsync(key)` | Load by primary key |
| `GHU` | `SELECT ... WHERE key = ? FOR UPDATE` | `FindAsync(key)` (tracked) | Load for update |
| `GN` | `SELECT ... ORDER BY key` (next row) | `AsQueryable().OrderBy(...)` | Sequential scan |
| `GHN` | `SELECT ... ORDER BY key FOR UPDATE` | Iterate tracked entities | Sequential update scan |
| `GNP` | `SELECT ... WHERE parent_key = ? ORDER BY child_key` | `parent.Children.OrderBy(...)` | Child iteration |
| `GHNP` | Same as GNP + `FOR UPDATE` | Iterate tracked child entities | Child update iteration |
| `REPL` | `UPDATE ... SET ... WHERE key = ?` | `SaveChangesAsync()` | Update entity |
| `ISRT` | `INSERT INTO ... VALUES (...)` | `AddAsync()` + `SaveChangesAsync()` | Insert entity |
| `DLET` | `DELETE FROM ... WHERE key = ?` | `Remove()` + `SaveChangesAsync()` | Delete entity |

## Target Architecture Mapping

| Aspect | IMS (Current) | .NET (Target) |
|--------|--------------|---------------|
| **Database interface** | DL/I calls via `CBLTDLI` | Entity Framework Core DbContext |
| **Function codes** | String constants (`'GU  '`, `'REPL'`, etc.) | EF Core methods (`Find`, `Add`, `Remove`, `SaveChanges`) |
| **Locking** | "Hold" calls (GHU, GHN, GHNP) | Optimistic concurrency via `RowVersion` or pessimistic via `SELECT ... FOR UPDATE` |
| **Navigation** | Hierarchical (parent/child segments) | Relational (foreign keys, navigation properties) |
| **Status codes** | PCB status bytes (`'  '` = success, `'GE'` = not found) | Exceptions or null return values |
| **Parameter count** | `PARMCOUNT COMP-5` | Not needed (method signatures define parameters) |

## Migration Notes

### Why This Copybook Exists

IMS DL/I function codes are 4-character string constants. Rather than hard-coding these strings in every program, they are centralized in this copybook. Every IMS program includes this copybook to get standardized function code names.

### Migration Strategy

This copybook is **not migrated** as a separate component. The function codes are IMS infrastructure that is entirely replaced by Entity Framework Core's API. The mapping between IMS operations and EF Core operations must be verified during the migration of each IMS program (primarily `CCPAUTH1`).

### Key Migration Considerations

1. **Hold/Lock semantics** -- IMS "hold" calls implement pessimistic locking. The .NET system should use optimistic concurrency (via `RowVersion`) by default, falling back to pessimistic locking (`SELECT ... FOR UPDATE` via EF Core raw SQL) only where contention analysis demonstrates the need.

2. **Sequential processing** -- IMS GN/GNP calls navigate the database hierarchically. In the relational model, these map to ORDER BY queries with cursors or pagination. Performance testing must verify that the relational query patterns meet the same throughput as IMS sequential access.

3. **Status code mapping** -- IMS PCB status codes (`'  '` = success, `'GE'` = segment not found, `'AI'` = I/O error) must be mapped to appropriate .NET exception handling or null-return patterns.

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **FSA (FFFS 2014:5)** Ch. 8 | Internal controls over data access. | IMS DL/I calls are replaced by Entity Framework operations. The same access control and audit logging requirements apply. All database operations must be logged for audit trail. |
| **DORA** Art. 11 | ICT data integrity. | The mapping from IMS DL/I operations to Entity Framework operations must preserve data integrity. Specifically, the "hold" + "replace" pattern must be mapped to a concurrency-safe update pattern in .NET to prevent lost updates. |
| **DORA** Art. 9 | ICT risk management -- availability. | IMS provides built-in recovery (backward/forward) via log records. The .NET replacement must implement equivalent transaction management and recovery capability via Azure SQL transaction logs and Entity Framework transactions. |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
