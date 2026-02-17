---
title: Reporting
sidebar_position: 6
---

# Reporting Business Rules

Business rules for regulatory reports, customer statements, AML screening, management analytics, and operational incident reporting extracted from COBOL source programs and regulatory requirements.

## Source Programs

| Program | Type | Function | Rules Extracted |
|---------|------|----------|----------------|
| `CBTRN03C.cbl` | Batch | Daily transaction detail report generation (step 3 of nightly pipeline) | RPT-BR-001, RPT-BR-004, RPT-BR-007, RPT-BR-008 |
| `CBTRN02C.cbl` | Batch | Daily transaction posting — provides source data for all reports | RPT-BR-002, RPT-BR-005, RPT-BR-008 |
| `CBTRN01C.cbl` | Batch | Transaction verification — pipeline step 1 with error handling patterns | RPT-BR-006, RPT-BR-008 |

### Related Copybooks

| Copybook | Purpose | Rules Extracted |
|----------|---------|----------------|
| `CVACT01Y.cpy` | Account master record layout (referenced, not in repository) | RPT-BR-003, RPT-BR-005 |
| `CVACT02Y.cpy` | Card record layout — contains embedded account FK | RPT-BR-003 |
| `CVACT03Y.cpy` | Card cross-reference — card-customer-account linkage | RPT-BR-003, RPT-BR-004 |

## Extracted Rules

| Rule ID | Title | Priority | Source | Regulation |
|---------|-------|----------|--------|------------|
| [RPT-BR-001](./rpt-br-001) | FSA regulatory reporting structure and calendar compliance | Critical | CBTRN03C.cbl (report framework) | FSA FFFS 2014:5 Ch. 3/7, EBA Guidelines |
| [RPT-BR-002](./rpt-br-002) | AML/KYC screening report generation | Critical | CBTRN02C.cbl, CBTRN03C.cbl (pipeline pattern) | AML 2017:11 Para. 3/4, GDPR Art. 5 |
| [RPT-BR-003](./rpt-br-003) | Customer statement generation | High | CBTRN03C.cbl (report pattern), CVACT01Y.cpy | PSD2 Art. 45/57, GDPR Art. 15 |
| [RPT-BR-004](./rpt-br-004) | Daily transaction detail report generation | High | CBTRN03C.cbl:158-374 | FSA FFFS 2014:5 Ch. 7, AML 2017:11 |
| [RPT-BR-005](./rpt-br-005) | Internal management reporting and analytics | Medium | CBTRN02C.cbl, CBTRN03C.cbl (framework) | FSA FFFS 2014:5 Ch. 3/7, DORA Art. 11 |
| [RPT-BR-006](./rpt-br-006) | DORA incident reporting and ICT risk management | Critical | CBTRN01C/02C/03C.cbl (error handling) | DORA Art. 11/17/19 |
| [RPT-BR-007](./rpt-br-007) | Report output formatting and structure standards | Medium | CBTRN03C.cbl:324-374 | FSA FFFS 2014:5 Ch. 3/7 |
| [RPT-BR-008](./rpt-br-008) | Batch reporting SLA and scheduling rules | Critical | CBTRN01C/02C/03C.cbl (pipeline) | FSA FFFS 2014:5 Ch. 7, AML 2017:11, DORA Art. 11 |

## Status

All 8 business rules have been extracted. All rules have `status: extracted` and are awaiting domain expert validation.

**Note on COBOL source coverage**: The available COBOL source programs include card management programs (COCRDLIC, COCRDSLC, COCRDUPC) and the batch transaction pipeline is documented via extracted references (CBTRN01C, CBTRN02C, CBTRN03C). Dedicated reporting COBOL programs for FSA regulatory filings, AML screening, customer statements, and management reports are not yet available in the repository. The business rules for these reports have been extracted from:
- The CBTRN03C report generation framework (the reference implementation for batch reporting)
- The batch pipeline orchestration pattern (TRN-BR-009)
- Regulatory requirements (FSA FFFS 2014:5, AML 2017:11, DORA, PSD2, GDPR)
- Cross-references to existing domain extractions (transactions, billing, account management)

The following COBOL programs should be obtained from the mainframe team for complete extraction:
- **FSA reporting programs**: Dedicated batch programs for regulatory data extracts
- **AML screening programs**: Nightly screening batch with watchlist matching
- **Statement generation programs**: Monthly customer statement batch
- **Management reporting programs**: Daily/weekly/monthly management extract programs

## Regulatory Coverage

| Regulation | Rules Covering |
|------------|---------------|
| FSA FFFS 2014:5 Ch. 3 (Accounting Records) | RPT-BR-001, RPT-BR-005, RPT-BR-007 |
| FSA FFFS 2014:5 Ch. 7 (Financial Reporting) | RPT-BR-001, RPT-BR-004, RPT-BR-005, RPT-BR-007, RPT-BR-008 |
| AML 2017:11 Para. 3 (Transaction Monitoring) | RPT-BR-002, RPT-BR-004, RPT-BR-008 |
| AML 2017:11 Para. 4 (Structuring Detection) | RPT-BR-002 |
| PSD2 Art. 45 (Information to Users) | RPT-BR-003 |
| PSD2 Art. 57 (Transaction Information) | RPT-BR-003 |
| PSD2 Art. 97 (Strong Customer Authentication) | RPT-BR-002 |
| GDPR Art. 5 (Data Minimization) | RPT-BR-002, RPT-BR-003 |
| GDPR Art. 15 (Right of Access) | RPT-BR-003 |
| DORA Art. 11 (ICT Risk Management) | RPT-BR-005, RPT-BR-006, RPT-BR-008 |
| DORA Art. 17 (Incident Classification) | RPT-BR-006 |
| DORA Art. 19 (Incident Reporting) | RPT-BR-006 |
| EBA Outsourcing Guidelines | RPT-BR-001 |

## Cross-Domain References

The reporting domain has significant dependencies on other extracted domains:

| Domain | Related Rules | Relationship |
|--------|--------------|-------------|
| Transactions | TRN-BR-006, TRN-BR-009 | Daily transaction report (CBTRN03C) is step 3 of the transaction pipeline |
| Billing | BILL-BR-001 through BILL-BR-007 | Interest rates and fees feed into customer statements (RPT-BR-003) |
| Account Management | ACCT-BR-001, ACCT-BR-004, ACCT-BR-007 | Account balances and credit limits feed into portfolio and management reports |

## Migration Considerations

1. **Azure batch architecture**: The mainframe JCL scheduler maps to Azure Durable Functions orchestration with timer triggers. See RPT-BR-008 for the detailed migration architecture.
2. **Output format modernization**: The 133-column fixed-width report format (RPT-BR-007) should be supplemented with modern formats (PDF, CSV, structured data/JSON) while preserving the original format for comparison testing.
3. **SLA preservation**: All batch SLAs from the mainframe schedule must be met or improved in the Azure environment. See RPT-BR-008 for the SLA matrix.
4. **DORA compliance**: The basic COBOL ABEND/DISPLAY error handling must be transformed into structured DORA-compliant incident management. See RPT-BR-006 for the incident reporting framework.
5. **AML threshold configuration**: AML screening thresholds (150,000 SEK) should be configurable in the migrated system rather than hard-coded. See RPT-BR-002 for threshold details.
6. **Character encoding**: EBCDIC to UTF-8 conversion required for all text fields, with Swedish character support (å, ä, ö).
7. **Data residency**: All report data must remain within EU boundaries per GDPR requirements. Azure region selection must comply with data residency.
