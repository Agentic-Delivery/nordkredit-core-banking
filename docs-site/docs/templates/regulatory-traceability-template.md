---
id: "REG-XXX-000"
title: "[Regulation Name] Traceability Matrix"
domain: "[payments | deposits | lending | account-management]"
regulation: "[FSA FFFS 2014:5 | PSD2 | GDPR | AML/KYC | DORA | EBA Outsourcing]"
last_reviewed: "YYYY-MM-DD"
reviewed_by: "[Name and role]"
status: "draft"
---

# Regulatory Traceability Matrix: [Regulation Name]

## Regulation Overview

_Provide a summary of the applicable regulation, its scope, and why it applies to NordKredit AB's core banking system. Include the regulation's official reference and any relevant articles or sections that impact the migration._

**Regulation:** [Full regulation name and reference]
**Issuing Authority:** [e.g., European Commission, Finansinspektionen]
**Effective Date:** [Date the regulation came into effect]
**Scope:** [How this regulation applies to NordKredit AB]

## Traceability Table

| Requirement ID | Regulation Article | Business Rule ID | COBOL Source | Validation Status | Implementation Status | Test Coverage |
|---|---|---|---|---|---|---|
| REG-XXX-001 | [Article/Section ref] | [e.g., PAY-BR-042] | [e.g., PAYMNT01.cbl:450-520] | [not-validated / validated / signed-off] | [not-started / in-progress / implemented / verified] | [none / unit / integration / bdd] |
| REG-XXX-002 | [Article/Section ref] | [Business Rule ID] | [COBOL program:lines] | [Validation Status] | [Implementation Status] | [Test Coverage] |

### Column Definitions

- **Requirement ID**: Unique identifier linking to this regulation (format: `REG-[REGULATION]-[SEQ]`)
- **Regulation Article**: Specific article, section, or paragraph of the regulation
- **Business Rule ID**: Identifier from `docs/requirements/` linking to the extracted business rule
- **COBOL Source**: Original COBOL program name and line range where the rule is implemented
- **Validation Status**: Whether a domain expert has validated the extraction
  - `not-validated` — extracted but not reviewed
  - `validated` — reviewed by domain expert
  - `signed-off` — formally approved by compliance
- **Implementation Status**: Current state in the .NET 8 target system
  - `not-started` — not yet implemented
  - `in-progress` — implementation underway
  - `implemented` — code complete
  - `verified` — parallel-run output matches mainframe
- **Test Coverage**: Level of automated test coverage in the target system
  - `none` — no tests yet
  - `unit` — xUnit unit tests
  - `integration` — integration tests
  - `bdd` — SpecFlow BDD tests covering the business rule

## Gaps

_List any identified regulatory gaps that need attention. A gap exists when a regulation requirement cannot be mapped to an existing COBOL implementation, or when the migration introduces a compliance risk._

| Gap ID | Description | Risk Level | Mitigation Plan | Owner | Due Date |
|---|---|---|---|---|---|
| GAP-XXX-001 | [Description of the regulatory gap] | [critical / high / medium / low] | [Proposed mitigation] | [Responsible person/team] | [Target date] |

## Compliance Notes

_Document any compliance-related observations, decisions, or context relevant to this regulation's traceability. Include references to FSA communications, internal audit findings, or compliance team guidance._

- [Note 1]
- [Note 2]

---

**Applicable Regulations Reference:**

| Regulation | Full Name | Relevance to NordKredit |
|---|---|---|
| FSA FFFS 2014:5 | Finansinspektionen's regulations regarding information security | IT governance, risk management for banking systems |
| PSD2 | Payment Services Directive 2 (EU 2015/2366) | Payment processing, SCA, third-party access |
| GDPR | General Data Protection Regulation (EU 2016/679) | Customer data protection, data residency, right to erasure |
| AML/KYC | Anti-Money Laundering / Know Your Customer | Transaction monitoring, PEP screening, suspicious activity |
| DORA | Digital Operational Resilience Act (EU 2022/2554) | ICT risk management, incident reporting, third-party oversight |
| EBA Outsourcing | EBA Guidelines on Outsourcing Arrangements (EBA/GL/2019/02) | Cloud migration as material outsourcing, exit strategy |

---

**Template version:** 1.0
**Last updated:** 2026-02-15
