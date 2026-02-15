---
id: DS-UNUSED-001
title: "Unused/Deprecated"
copybook_name: "UNUSED1Y.cpy"
domain: "system"
used_by_programs: []
record_length: 80
status: "deprecated"
target_schema: "N/A (deprecated)"
sidebar_position: 40
---

# DS-UNUSED-001: Unused/Deprecated (UNUSED1Y)

## Overview

The `UNUSED1Y.cpy` copybook defines an **unused and deprecated data structure** that is structurally identical to the User Security Record (`CSUSR01Y.cpy`). This copybook is not referenced by any program in the CardDemo application and should **NOT be migrated** to the .NET target system.

The structure appears to be a leftover from an earlier version of the user security implementation, possibly a copy that was superseded by `CSUSR01Y.cpy`. Its continued presence in the copybook library is a maintenance artifact.

**Source file:** `UNUSED1Y.cpy`
**Status:** DEPRECATED -- Do NOT migrate
**Used by:** None (no program references this copybook)

## Source COBOL

```cobol
01 UNUSED-DATA.
  05 UNUSED-ID                 PIC X(08).
  05 UNUSED-FNAME              PIC X(20).
  05 UNUSED-LNAME              PIC X(20).
  05 UNUSED-PWD                PIC X(08).
  05 UNUSED-TYPE               PIC X(01).
  05 UNUSED-FILLER             PIC X(23).
```

## Field Definitions

| # | Field Name | PIC Clause | Offset | Length | Type | Description | Migrate? |
|---|------------|-----------|--------|--------|------|-------------|----------|
| 1 | `UNUSED-ID` | `X(08)` | 0 | 8 | Alphanumeric | User identifier (same as `SEC-USR-ID`) | No |
| 2 | `UNUSED-FNAME` | `X(20)` | 8 | 20 | Alphanumeric | First name (same as `SEC-USR-FNAME`) | No |
| 3 | `UNUSED-LNAME` | `X(20)` | 28 | 20 | Alphanumeric | Last name (same as `SEC-USR-LNAME`) | No |
| 4 | `UNUSED-PWD` | `X(08)` | 48 | 8 | Alphanumeric | Password - plaintext (same as `SEC-USR-PWD`) | No |
| 5 | `UNUSED-TYPE` | `X(01)` | 56 | 1 | Alphanumeric | User type (same as `SEC-USR-TYPE`) | No |
| 6 | `UNUSED-FILLER` | `X(23)` | 57 | 23 | Filler | Reserved space | No |

**Total record length:** 80 bytes

## Field Notes

1. **Identical structure to CSUSR01Y:** This copybook has the exact same field layout, PIC clauses, and record length as `CSUSR01Y.cpy` (User Security Record, DS-USEC-001). The only differences are the field name prefixes (`UNUSED-` vs. `SEC-USR-`) and the top-level record name (`UNUSED-DATA` vs. `SEC-USER-DATA`).

2. **No program references:** A search of all COBOL programs in the CardDemo application confirms that no program includes or references `UNUSED1Y.cpy`. This copybook is dead code.

3. **Probable history:** This is likely a prior version of the user security record that was copied, renamed, and replaced by `CSUSR01Y.cpy`. The `UNUSED-` prefix in the field names suggests it was intentionally marked as deprecated by the original developers.

4. **Plaintext password field:** Like its active counterpart, this copybook contains a plaintext password field (`UNUSED-PWD`). If any historical VSAM data files exist that use this layout, those files may contain plaintext passwords and should be securely disposed of per GDPR data minimization requirements.

## Migration Notes

### Decision: DO NOT MIGRATE

This copybook is excluded from migration for the following reasons:

1. **No active usage:** No program references this copybook. There is no runtime dependency.
2. **Duplicate of active copybook:** The functionality is fully covered by `CSUSR01Y.cpy`, which is the active user security record.
3. **Dead code removal:** Per project coding standards (KISS, "Remove, Don't Skip"), unused code artifacts should not be carried forward to the target system.

### Cleanup Actions

| Action | Responsibility | Timeline |
|--------|---------------|----------|
| Confirm no program references exist | Migration team | Pre-migration validation |
| Check for VSAM data files using this layout | Infrastructure team | Pre-migration validation |
| Securely delete any data files with plaintext passwords | Security team | Before mainframe decommission |
| Remove copybook from source library | COBOL maintenance team | Post-migration cleanup |
| Document removal in migration audit trail | Migration team | At removal time |

## Compliance Considerations

| Regulation | Requirement | Implementation |
|-----------|------------|----------------|
| **GDPR** Art. 5(1)(c) | Data minimization -- data should be adequate, relevant, and limited | Deprecated data structures should not be migrated. If any VSAM data files use this layout, they should be reviewed for PII content and securely disposed of. |
| **GDPR** Art. 5(1)(e) | Storage limitation -- data should not be kept longer than necessary | Any historical data associated with this deprecated structure should be deleted if no longer needed for a specific, documented purpose. |
| **GDPR** Art. 5(1)(f) | Integrity and confidentiality | If historical data files contain plaintext passwords (`UNUSED-PWD`), they represent a security risk and must be securely deleted (not just logically deleted -- physical media sanitization per NIST SP 800-88). |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
