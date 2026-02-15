---
id: DS-XXX-000
title: "[Copybook Title]"
copybook_name: "EXAMPLE.cpy"
domain: "[payments | deposits | lending | account-management]"
used_by_programs:
  - PROGRAM1
  - PROGRAM2
record_length: 0
status: "extracted"
target_schema: "dbo.TargetTableName"
---

# [Copybook Title]

## Overview

_Brief description of this data structure's purpose, the business entity it represents, and its role in the mainframe system. Include which subsystem owns it (IMS, CICS, batch) and how it relates to other data structures._

## Field Definitions

| Field Name | PIC Clause | Length (bytes) | Type | Description | Nullable | Target Column |
|------------|-----------|----------------|------|-------------|----------|---------------|
| FIELD-NAME-1 | X(10) | 10 | Alphanumeric | _Description_ | No | `column_name` |
| FIELD-NAME-2 | 9(05) | 5 | Numeric (unsigned) | _Description_ | No | `column_name` |
| FIELD-NAME-3 | S9(07)V99 | 9 | Packed decimal (signed, 2 decimal places) | _Description_ | Yes | `column_name` |
| FILLER | X(nn) | nn | Filler | Unused padding to reach record length | N/A | N/A |

### Field Notes

_Document any field-specific considerations here, such as:_

- _Implicit decimal positions (e.g., PIC 9(5)V99 stores 7 digits but represents a value with 2 decimal places)_
- _Redefines or overlapping fields_
- _Computed or derived fields_
- _Business rules encoded in field values (e.g., status codes, type indicators)_

## EBCDIC Encoding Notes

_Document character encoding considerations for the migration from EBCDIC to Unicode/UTF-8._

| Consideration | Detail |
|---------------|--------|
| Source encoding | EBCDIC (IBM Code Page _nnn_) |
| Target encoding | UTF-8 |
| Special characters | _List any Swedish characters (å, ä, ö) or special symbols that require attention_ |
| Packed decimal fields | _List fields using COMP-3 that need binary-to-decimal conversion_ |
| Binary fields | _List any COMP/COMP-4 fields requiring endian conversion_ |
| Sign handling | _Describe sign representation (trailing overpunch, separate sign byte, etc.)_ |

## Referential Integrity

_Document relationships to other data structures and the expected integrity constraints in the target Azure SQL schema._

| Relationship | Source Field | Target Table | Target Column | Constraint Type |
|-------------|-------------|-------------|---------------|-----------------|
| _Description_ | FIELD-NAME | dbo.OtherTable | other_column | FK / Lookup / Soft reference |

### Integrity Notes

_Document any referential integrity considerations:_

- _Relationships that are enforced by COBOL program logic rather than database constraints_
- _Cross-file validations performed in batch jobs_
- _Orphan record handling strategy_

## Sample Data

_Provide representative (non-PII) sample records showing typical field values. Use synthetic data only._

```
Record 1:
  FIELD-NAME-1: "VALUE1    "
  FIELD-NAME-2: 00123
  FIELD-NAME-3: +0001234.56
```

### Data Characteristics

- _Estimated record count: nnn_
- _Growth rate: nnn records/month_
- _Key distribution notes (e.g., sequential, sparse, partitioned by date)_

## Migration Notes

_Document migration-specific considerations for moving this data structure to Azure SQL._

### Schema Mapping

- **Source**: _VSAM file / Db2 table name_
- **Target**: `dbo.TargetTableName`
- **Migration approach**: _Full extract / incremental sync / CDC_

### Data Quality

- _Known data quality issues (e.g., orphan references, invalid dates, truncated fields)_
- _Cleanup rules to apply during migration_

### Validation Strategy

- _Record count reconciliation_
- _Checksum or hash comparison_
- _Business rule validation queries_
- _Parallel-run comparison approach_

### Performance Considerations

- _Batch window impact during extraction_
- _Index strategy for target table_
- _Partitioning recommendations_
