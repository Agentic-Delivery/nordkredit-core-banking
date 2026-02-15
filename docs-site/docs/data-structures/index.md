---
title: Data Structures
sidebar_position: 1
---

# Data Structures

COBOL copybook definitions and their mapped .NET equivalents.

## Documented Copybooks

### Account Management

| Copybook | Record Length | Key | Business Rule | Purpose |
|----------|-------------|-----|---------------|---------|
| `CVACT01Y.cpy` | 300 bytes | ACCT-ID PIC 9(11) | [ACCT-BR-001](../business-rules/account-management/acct-br-001) | Account master record layout |
| `CVACT02Y.cpy` | 150 bytes | CARD-NUM PIC X(16) | [ACCT-BR-009](../business-rules/account-management/acct-br-009) | Card record layout |
| `CVACT03Y.cpy` | 50 bytes | XREF-CARD-NUM PIC X(16) | [ACCT-BR-009](../business-rules/account-management/acct-br-009) | Card-customer-account cross-reference |

### Card Management

| Copybook | Record Length | Key | Business Rule | Purpose |
|----------|-------------|-----|---------------|---------|
| `CVACT02Y.cpy` | 150 bytes | CARD-NUM PIC X(16) | [CARD-BR-009](../business-rules/card-management/card-br-009) | Card record layout |
| `CVACT03Y.cpy` | 50 bytes | XREF-CARD-NUM PIC X(16) | [CARD-BR-010](../business-rules/card-management/card-br-010) | Card cross-reference structure |
| `CVCRD01Y.cpy` | N/A | N/A | [CARD-BR-011](../business-rules/card-management/card-br-011) | Card work area (AID keys, screen fields) |

## Expected Coverage

- COBOL copybook field definitions
- .NET class/record mappings
- EBCDIC-to-Unicode conversion rules
- Field-level validation constraints
- Db2-to-Azure SQL schema mappings
