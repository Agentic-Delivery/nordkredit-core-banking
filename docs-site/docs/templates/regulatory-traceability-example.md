---
id: "REG-PSD2-001"
title: "PSD2 Strong Customer Authentication Traceability Matrix"
domain: "payments"
regulation: "PSD2"
last_reviewed: "2026-02-15"
reviewed_by: "Compliance Team (initial draft)"
status: "draft"
---

# Regulatory Traceability Matrix: PSD2 Strong Customer Authentication

## Regulation Overview

The Payment Services Directive 2 (PSD2, EU Directive 2015/2366) requires payment service providers to apply Strong Customer Authentication (SCA) when a payer initiates an electronic payment transaction. SCA requires authentication based on two or more elements from: knowledge (something the user knows), possession (something the user has), and inherence (something the user is). The European Banking Authority's Regulatory Technical Standards (EBA RTS 2018/389) provide detailed requirements for SCA implementation.

NordKredit AB, as a licensed payment service provider under Finansinspektionen supervision, must comply with PSD2 SCA for all customer-initiated electronic payments processed through the core banking system.

**Regulation:** PSD2 (EU Directive 2015/2366), EBA RTS on SCA (EU 2018/389)
**Issuing Authority:** European Commission, European Banking Authority
**Effective Date:** 14 September 2019 (SCA enforcement)
**Scope:** All customer-initiated electronic payment transactions at NordKredit AB

## Traceability Table

| Requirement ID | Regulation Article | Business Rule ID | COBOL Source | Validation Status | Implementation Status | Test Coverage |
|---|---|---|---|---|---|---|
| REG-PSD2-001 | Art. 97(1)(a) — SCA for online account access | PAY-BR-042 | AUTHNT01.cbl:120-185 | validated | not-started | none |
| REG-PSD2-002 | Art. 97(1)(b) — SCA for electronic payment initiation | PAY-BR-043 | PAYMNT01.cbl:450-520 | validated | not-started | none |
| REG-PSD2-003 | Art. 97(1)(c) — SCA for remote actions with payment fraud risk | PAY-BR-044 | PAYMNT01.cbl:525-580 | not-validated | not-started | none |
| REG-PSD2-004 | RTS Art. 4 — Authentication code requirements | PAY-BR-045 | AUTHNT01.cbl:200-260 | validated | not-started | none |
| REG-PSD2-005 | RTS Art. 5 — Dynamic linking for payment amount and payee | PAY-BR-046 | PAYMNT01.cbl:600-670 | validated | not-started | none |
| REG-PSD2-006 | RTS Art. 10 — Exemption: low-value transactions (< EUR 30) | PAY-BR-047 | PAYMNT02.cbl:100-145 | validated | not-started | none |
| REG-PSD2-007 | RTS Art. 11 — Exemption: contactless at point of sale | PAY-BR-048 | PAYMNT02.cbl:150-190 | not-validated | not-started | none |
| REG-PSD2-008 | RTS Art. 13 — Exemption: trusted beneficiaries | PAY-BR-049 | PAYMNT02.cbl:200-250 | validated | not-started | none |
| REG-PSD2-009 | RTS Art. 16 — Exemption: transaction risk analysis (TRA) | PAY-BR-050 | RSKSCR01.cbl:80-200 | not-validated | not-started | none |
| REG-PSD2-010 | Art. 74 — Liability for unauthorized transactions | PAY-BR-051 | LIABTY01.cbl:50-130 | not-validated | not-started | none |

## Gaps

| Gap ID | Description | Risk Level | Mitigation Plan | Owner | Due Date |
|---|---|---|---|---|---|
| GAP-PSD2-001 | COBOL system predates PSD2 — SCA logic was added as bolt-on patches across multiple programs. Full extraction of SCA flow requires cross-program analysis (AUTHNT01, PAYMNT01, PAYMNT02, RSKSCR01). | high | Map complete SCA call chain before implementation. Schedule dedicated sessions with retiring COBOL developers who built the SCA patches. | Migration Team Lead | 2026-04-01 |
| GAP-PSD2-002 | Transaction Risk Analysis (TRA) exemption thresholds in RSKSCR01 may not reflect current EBA fraud rate reference values. Need to verify against latest EBA published rates. | medium | Cross-reference RSKSCR01 threshold values with EBA's latest fraud rate publication. Update thresholds in target system if discrepancies found. | Compliance Team | 2026-03-15 |
| GAP-PSD2-003 | Dynamic linking implementation in PAYMNT01 uses legacy token format. Target system must use current best-practice (e.g., FIDO2/WebAuthn) while preserving the same business rule semantics. | medium | Design new authentication flow in .NET 8 that satisfies the same PSD2 dynamic linking requirements while using modern authentication protocols. | Security Architect | 2026-05-01 |

## Compliance Notes

- The existing COBOL implementation handles SCA through a combination of IMS transaction codes and CICS screens (AUTHNT01 for authentication, PAYMNT01/02 for payment validation). The migration must preserve the same authentication gates while modernizing the user-facing authentication mechanism.
- Finansinspektionen has been notified of the migration project per FSA FFFS 2014:5 requirements for significant IT system changes. Ongoing dialogue with FSA contact ensures migration approach meets supervisory expectations.
- SCA exemptions (low-value, trusted beneficiaries, TRA) must be individually validated against the current EBA RTS before implementation in the target system. The mainframe values may be outdated.
- Parallel-run validation for SCA is complex — the mainframe and Azure system must apply identical exemption logic during the transition period to avoid inconsistent customer experiences.

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
