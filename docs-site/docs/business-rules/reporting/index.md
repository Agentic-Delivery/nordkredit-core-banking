---
title: Reporting
sidebar_position: 6
---

# Reporting Business Rules

Business rules for transaction reporting, data export/import, and batch report generation extracted from COBOL source programs and JCL batch jobs in the AWS CardDemo application.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `CORPT00C.cbl` | CICS Online | Report generation menu and job submission | REPT-BR-001, REPT-BR-002, REPT-BR-003 |
| `CBEXPORT.cbl` | Batch | Multi-file customer data export | REPT-BR-008, REPT-BR-010 |
| `CBIMPORT.cbl` | Batch | Data import with validation and dispatch | REPT-BR-009 |

### Related JCL Jobs

| Job | Function | Rules Extracted |
|-----|----------|----------------|
| `TRANREPT.jcl` | Transaction report pipeline (unload, filter, format) | REPT-BR-004 |
| `REPTFILE.jcl` | Report file GDG definition | REPT-BR-005 |
| `PRTCATBL.jcl` | Transaction category balance report | REPT-BR-006 |
| `TRANCATG.jcl` | Transaction category reference data management | REPT-BR-007 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVEXPORT.cpy` | 500-byte multi-record export layout with REDEFINES overlays | REPT-BR-010 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [REPT-BR-001](./rept-br-001) | Transaction report type selection and date range | High | CORPT00C.cbl | FFFS 2014:5, DORA |
| [REPT-BR-002](./rept-br-002) | Custom date range validation | High | CORPT00C.cbl | FFFS 2014:5, DORA |
| [REPT-BR-003](./rept-br-003) | Report job submission via TDQ | High | CORPT00C.cbl | FFFS 2014:5, DORA |
| [REPT-BR-004](./rept-br-004) | Transaction report date filtering and sorting | High | TRANREPT.jcl | FFFS 2014:5, PSD2 |
| [REPT-BR-005](./rept-br-005) | Report file GDG management | Medium | REPTFILE.jcl | FFFS 2014:5, DORA |
| [REPT-BR-006](./rept-br-006) | Transaction category balance report | High | PRTCATBL.jcl | FFFS 2014:5, PSD2 |
| [REPT-BR-007](./rept-br-007) | Transaction category reference data management | Medium | TRANCATG.jcl | FFFS 2014:5 |
| [REPT-BR-008](./rept-br-008) | Customer data export for branch migration | High | CBEXPORT.cbl | GDPR, AML/KYC |
| [REPT-BR-009](./rept-br-009) | Customer data import with error handling | High | CBIMPORT.cbl | GDPR, AML/KYC |
| [REPT-BR-010](./rept-br-010) | Multi-record export data structure | Medium | CVEXPORT.cpy | GDPR |

## Status

All 10 business rules have been extracted from the COBOL source. All rules have `status: extracted` and are awaiting domain expert validation.

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| FFFS 2014:5 Ch. 8 §4 (Internal Controls) | REPT-BR-001, REPT-BR-002, REPT-BR-003, REPT-BR-004, REPT-BR-005, REPT-BR-006, REPT-BR-007 |
| DORA Art. 11 (Data Management) | REPT-BR-001, REPT-BR-003, REPT-BR-005 |
| PSD2 Art. 97 (Strong Customer Authentication) | REPT-BR-004, REPT-BR-006 |
| GDPR Art. 5 (Data Principles) | REPT-BR-008, REPT-BR-009, REPT-BR-010 |
| GDPR Art. 17 (Right to Erasure) | REPT-BR-008, REPT-BR-010 |
| AML/KYC (4th Directive) | REPT-BR-008, REPT-BR-009 |

## Migration Considerations

1. **CICS TDQ to Azure Service Bus**: The TDQ-based job submission (REPT-BR-003) should be replaced with Azure Service Bus queue triggers for Azure Functions, preserving the asynchronous submission pattern
2. **JCL batch to Azure Functions**: SORT/IDCAMS/REPROC steps in JCL jobs map to Azure Functions with timer triggers; the multi-step pipeline pattern (unload → filter → format) should be preserved as a Durable Functions orchestration
3. **VSAM KSDS to Azure SQL**: All VSAM key-sequenced datasets should be mapped to Azure SQL tables with clustered indexes matching the original key structure
4. **GDG to Azure Blob versioning**: Generation Data Groups for report output should be mapped to Azure Blob Storage with versioning enabled, maintaining the 10-generation retention limit
5. **SORT utility to LINQ/SQL**: DFSORT INCLUDE/OMIT conditions and OUTREC formatting translate to LINQ queries or SQL WHERE clauses with column projections
6. **500-byte REDEFINES to polymorphic DTOs**: The multi-record export layout (CVEXPORT.cpy) should be mapped to a record-type discriminated union or polymorphic DTO hierarchy in C#
7. **COMP/COMP-3 to .NET types**: Binary (COMP) and packed decimal (COMP-3) fields require explicit conversion to `decimal` or `long` types with appropriate precision
8. **Character encoding**: EBCDIC to UTF-8 conversion required for all text fields; Swedish characters (Å, Ä, Ö) must be preserved in customer name fields
