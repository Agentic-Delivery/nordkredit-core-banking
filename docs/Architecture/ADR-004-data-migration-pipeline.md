# ADR-004: Data Migration Pipeline (Db2 to Azure SQL)

**Date:** 2026-02-17

**Status:** Accepted

**Deciders:** Engineering team

**Tags:** architecture, backend, infrastructure, data-migration, compliance

---

## Context

**What is the issue we're trying to solve?**

The core banking migration requires an incremental data sync pipeline that operates during the parallel-run period. Data must flow from Db2 for z/OS to Azure SQL Database, with EBCDIC-to-Unicode conversion, referential integrity validation, rollback capability, and GDPR/DORA-compliant audit logging. The pipeline must support changed-record-only sync (not full table scans) and maintain EU data residency.

**What forces are at play?**

- Parallel-run requires continuous data synchronization between mainframe and Azure.
- EBCDIC conversion utilities (#138) and schema mapping (#143) are already available.
- Incremental sync must detect changed records via timestamp-based delta detection.
- Rollback capability must allow reverting to a previous sync state.
- GDPR Art.17 requires preserving right-to-erasure capability across migrated data.
- DORA Art.11 requires full audit trail of data changes during migration.
- All data must remain in EU/Sweden Central (data residency).
- Pipeline must handle all 9 mapped tables from the schema mapping document.

---

## Decision

**What did we decide?**

Implement the data migration pipeline as domain-layer orchestration with infrastructure abstractions, following the established patterns from the parallel-run framework (#155):

1. **`MigrationPipeline`** — Orchestrates incremental sync: reads changed records from source, applies field conversions, writes to target, validates referential integrity, and logs all changes. Lives in `NordKredit.Domain/DataMigration/`.

2. **`ISourceDataReader`** — Interface for reading changed records from Db2. Returns raw row data with metadata (table, change type, timestamp). Infrastructure implements with Db2 connectivity.

3. **`ITargetDataWriter`** — Interface for writing converted records to Azure SQL. Supports batch writes with transactional semantics. Infrastructure implements with EF Core.

4. **`IMigrationAuditLog`** — Persists audit records of all data changes for GDPR/DORA compliance. Every record migration is logged with source, target, and conversion details.

5. **`FieldConverter`** — Applies EBCDIC-to-Unicode, packed decimal, and zoned decimal conversions using the existing `EbcdicConverter`. Maps COBOL field types to .NET types per the schema mapping document.

6. **`ReferentialIntegrityValidator`** — Post-sync validation that checks foreign key relationships, record counts, and data completeness across all migrated tables.

7. **`MigrationSyncState`** — Tracks sync checkpoints (last sync timestamp per table) for incremental sync and rollback. Each sync batch creates a checkpoint that can be reverted.

**Incremental sync strategy:** Timestamp-based delta detection. Each table's sync state tracks the last processed timestamp. On each run, only records with a modified timestamp after the checkpoint are processed. This avoids full table scans while being simpler than CDC (which requires Db2 log access that may not be available from Azure).

**Rollback strategy:** Each sync batch is wrapped in a transaction. Sync state checkpoints are versioned. Rolling back means restoring the previous checkpoint and deleting records written in the failed batch.

---

## Alternatives Considered

### Alternative 1: Change Data Capture (CDC) from Db2

**Description:** Use Db2 native CDC to stream changes.

**Pros:**
- Real-time change streaming
- No timestamp column requirement

**Cons:**
- Requires Db2 log reader access (may not be available from Azure)
- Complex infrastructure setup
- Vendor-specific coupling

**Why rejected:** Timestamp-based delta is simpler, works over standard SQL connectivity, and meets the incremental sync requirement. CDC can be adopted later if needed.

### Alternative 2: Azure Data Factory

**Description:** Use Azure Data Factory for ETL pipeline.

**Pros:**
- Managed service, visual pipeline designer
- Built-in connectors

**Cons:**
- No EBCDIC-to-Unicode conversion support
- Cannot apply domain-specific validation rules
- Limited rollback capability
- Additional Azure cost

**Why rejected:** Custom conversion logic (COMP-3, zoned decimal, Swedish EBCDIC) requires code-level control. Domain validation rules need business context.

---

## Consequences

### Positive
- Follows existing domain-layer patterns (interfaces, DI, async)
- Reuses proven EbcdicConverter utilities
- Full audit trail for GDPR/DORA compliance
- Testable with unit tests using stub implementations
- Incremental sync minimizes data transfer volume
- Rollback capability provides safety net during parallel-run

### Negative
- Timestamp-based sync requires reliable timestamp columns in Db2
- Custom pipeline requires more implementation effort than managed ETL

### Risks
- Large batch sizes could cause memory pressure
  - **Mitigation:** Configurable batch size with streaming reads
- Network latency to Db2 could slow sync
  - **Mitigation:** Parallel table sync, configurable timeouts

---

## Implementation Notes

- Domain models: `src/NordKredit.Domain/DataMigration/`
- Infrastructure: `src/NordKredit.Infrastructure/DataMigration/`
- Unit tests: `tests/NordKredit.UnitTests/DataMigration/`
- Reuses: `EbcdicConverter` from `NordKredit.Infrastructure.DataMigration`
- Schema mapping reference: `docs-site/docs/data-structures/db2-to-azure-sql-schema-mapping.md`

---

## References

- Issue #156: Implement data migration pipeline
- Issue #155: Parallel-run orchestration framework (dependency)
- Issue #138: EBCDIC converter utilities
- Issue #143: Db2-to-Azure SQL schema mapping
- GDPR Art.5(1)(d) — Accuracy during migration
- GDPR Art.17 — Right to erasure
- DORA Art.11 — ICT system testing
- FFFS 2014:5 — FSA operational risk

---

**Template version:** 1.0
**Last updated:** 2026-02-17
