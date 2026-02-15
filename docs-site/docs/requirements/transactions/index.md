---
title: Transaction Use Cases
sidebar_position: 1
---

# Transaction Processing Use Cases

Use cases and user stories for the Transaction Processing domain, derived from extracted COBOL business rules.

## Use Cases

| Use Case ID | Title | Primary Actor | Source Program | Priority |
|-------------|-------|---------------|----------------|----------|
| [UC-TRN-05](./uc-trn-05-daily-posting) | Post Daily Transactions | Batch Processing System | CBTRN02C.cbl | Critical |

## Batch Pipeline Context

The transaction use cases cover the three-step daily batch pipeline:

| Step | Program | Function | Use Case |
|------|---------|----------|----------|
| 1 | CBTRN01C | Verification — card-to-account cross-reference validation | _Pending_ |
| **2** | **CBTRN02C** | **Posting — validation, balance updates, transaction recording** | **[UC-TRN-05](./uc-trn-05-daily-posting)** |
| 3 | CBTRN03C | Reporting — daily transaction detail report generation | _Pending_ |

## Status

| Status | Count |
|--------|-------|
| Documented | 1 |
| Pending | — |
