---
id: "billing-index"
title: "Billing Domain — Extracted Business Rules"
domain: "billing"
status: "in-progress"
---

# Billing Domain — Extracted Business Rules

## Overview

The Billing domain covers bill payment processing, interest calculation, statement generation, and related data structures. These rules were extracted from COBOL programs in the CardDemo application running on IBM z/OS.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `COBIL00C.cbl` | CICS | Online bill payment | BILL-BR-001 |
| `CBACT04C.cbl` | Batch | Interest calculation | BILL-BR-002 |
| `CBSTM03A.CBL` | Batch | Statement generation (main) | BILL-BR-003 |
| `CBSTM03B.CBL` | Batch | Statement generation (file I/O subroutine) | BILL-BR-003 |
| `COSTM01.CPY` | Copybook | Transaction layout for reporting | BILL-BR-004 |

### Related JCL Jobs

| Job | Function | Programs Called |
|-----|----------|---------------|
| `INTCALC.jcl` | Interest calculation batch | CBACT04C |
| `CREASTMT.JCL` | Create statements batch | SORT, IDCAMS, CBSTM03A/CBSTM03B |

## Extracted Rules

| Rule ID | Title | Priority | Status |
|---------|-------|----------|--------|
| [BILL-BR-001](BILL-BR-001.md) | Online bill payment pays full current balance and creates transaction record | High | Extracted |
| [BILL-BR-002](BILL-BR-002.md) | Monthly interest calculated per transaction category using disclosure group rates | Critical | Extracted |
| [BILL-BR-003](BILL-BR-003.md) | Batch statement generation produces text and HTML statements per card | High | Extracted |
| [BILL-BR-004](BILL-BR-004.md) | Transaction record layout for statement reporting uses card-first key structure | Medium | Extracted |

## Regulatory Coverage

| Regulation | Rules |
|------------|-------|
| FFFS 2014:5 (FSA) | BILL-BR-001, BILL-BR-002, BILL-BR-003, BILL-BR-004 |
| PSD2 | BILL-BR-001, BILL-BR-002, BILL-BR-003, BILL-BR-004 |
| GDPR | BILL-BR-001, BILL-BR-003, BILL-BR-004 |
| EBA GL 2020/06 | BILL-BR-002 |

## Key Risks

1. **Interest calculation precision** (BILL-BR-002): COBOL truncation behavior must be exactly replicated during parallel run. Rounding differences will cause output comparison failures and potential regulatory issues.
2. **Fee computation gap** (BILL-BR-002): The `1400-COMPUTE-FEES` paragraph is a stub. Fee logic may need to be designed as a new requirement.
3. **Statement memory limits** (BILL-BR-003): In-memory table limited to 51 cards x 10 transactions. Production data volumes must be verified.
4. **Full balance payment only** (BILL-BR-001): No partial payment support exists. This may be a business requirement for the new system.

## Validation Status

All rules are in `extracted` status and require domain expert validation before implementation.
