# ADR-001: Docusaurus-based Requirements Documentation

**Date:** 2026-02-15

**Status:** Proposed

**Deciders:** NordKredit migration team, compliance and risk team, business analysts

**Tags:** architecture, documentation, compliance, knowledge-capture

---

## Context

**What is the issue we're trying to solve?**

NordKredit AB's core banking migration requires extracting ~1,500 COBOL programs' worth of business rules, data structures, and process flows. These extracted requirements must be browsable and validatable by multiple stakeholder groups: business analysts, compliance officers, C#/.NET developers, and the retiring COBOL developers who are the primary domain experts. Existing mainframe documentation cannot be trusted — all requirements must come from the COBOL source code itself.

The documentation platform must support:
- Machine-readable metadata for tracking extraction status and regulatory traceability
- Review workflows integrated with the existing PR-based development process
- Browsable output for non-technical stakeholders (compliance, business analysts)
- Audit trail for regulatory purposes (FSA, PSD2, GDPR, AML/KYC, DORA)

**What forces are at play?**

- **Time pressure:** 4 of 6 COBOL developers retiring within 18 months — knowledge capture is the hardest deadline
- **Regulatory traceability:** Every extracted business rule must link to its COBOL source AND applicable regulation (FSA FFFS 2014:5, PSD2, GDPR, AML/KYC, DORA)
- **Diverse audience:** Compliance officers need browsable views; developers need structured data; COBOL experts need to validate against source
- **Audit trail:** Finansinspektionen may require evidence of systematic knowledge capture and traceability
- **Existing workflow:** The project already uses Git, GitHub PRs, and Markdown in the `docs/` directory
- **Scale:** ~1,500 COBOL programs, ~300 copybooks, four business domains (payments, deposits, lending, account management)
- **Tooling budget:** Prefer open-source or low-cost solutions — licensing costs are already a concern (IBM z/OS contract)

---

## Decision

**What did we decide?**

We will use [Docusaurus](https://docusaurus.io/) as the documentation platform for extracted requirements. Requirements will be authored as Markdown files with YAML front matter in the existing `docs/requirements/` directory structure, and Docusaurus will render them into a browsable website.

### YAML Front Matter Strategy

Each requirement document will include machine-readable YAML front matter for metadata tracking and traceability:

```yaml
---
id: PAY-BR-042
title: Payment amount validation
domain: payments
category: business-rule
source_program: PAYMNT01.cbl
source_lines: "450-520"
regulation:
  - "PSD2 Art. 97 — SCA"
  - "FSA FFFS 2014:5 Ch. 8"
status: draft | review | validated | implemented
validated_by: ""
validated_date: ""
priority: critical | high | medium | low
tags:
  - payment-validation
  - amount-limits
---
```

**Status values:**
- `draft` — Extracted from COBOL, not yet reviewed
- `review` — Under review by domain expert or compliance
- `validated` — Signed off by domain expert (retiring COBOL developer)
- `implemented` — Corresponding C#/.NET code exists and passes tests

This front matter enables:
- Automated status dashboards (e.g., "85% of payments rules validated")
- Regulatory traceability reports (filter by regulation)
- Domain-scoped views (filter by domain)
- CI validation that every requirement has required metadata fields

### Directory Integration

Docusaurus will be configured to serve the existing `docs/` directory:

```
docs/
  requirements/
    payments/          # Payment domain requirements
    deposits/          # Deposit domain requirements
    lending/           # Lending domain requirements
    account-management/ # Account management domain requirements
  regulatory/          # Regulatory traceability matrices
  Architecture/        # ADRs (this document)
```

No restructuring of the existing directory layout is needed. Docusaurus sidebars will be auto-generated from the directory structure, with manual overrides where needed for logical grouping.

### PR Review Workflow

Because requirements are plain Markdown files in the Git repository:
- Extraction of a new business rule = a PR adding a Markdown file with front matter
- Domain expert validation = PR review and approval (captured in Git history)
- Changes to validated requirements = new PR with diff visible in review
- Every change is auditable via `git log` and GitHub PR history

---

## Alternatives Considered

### Alternative 1: Plain Markdown in GitHub

**Description:** Keep requirements as Markdown files in the `docs/` directory, browsed directly on GitHub.

**Pros:**
- Zero additional tooling — already in use
- Native PR review workflow
- No build step or hosting needed

**Cons:**
- No structured navigation (sidebar, search, cross-references)
- GitHub renders Markdown but does not support filtering by front matter metadata
- Non-technical stakeholders find GitHub's interface unintuitive for browsing documentation
- No way to build status dashboards from front matter without separate tooling

**Why rejected:** Adequate for developers but insufficient for compliance officers and business analysts who need structured browsing, search, and status tracking across ~1,500 extracted requirements.

### Alternative 2: Confluence

**Description:** Use Atlassian Confluence as the documentation platform.

**Pros:**
- Rich editing experience for non-technical users
- Built-in search, labels, and page hierarchy
- Familiar to many enterprise teams

**Cons:**
- Content lives outside the Git repository — breaks PR review workflow and audit trail
- No native YAML front matter support — metadata must be managed via Confluence labels or custom macros
- Licensing cost (~$6/user/month) adds to already escalating IBM costs
- Content drift risk — Confluence pages can be edited without review, breaking traceability
- GDPR data residency concerns with Atlassian Cloud (data processing outside EU possible)

**Why rejected:** Breaks the Git-based audit trail required for regulatory traceability. Content outside the repository cannot be reviewed via PRs, and edits without review undermine the validation workflow.

### Alternative 3: Custom-Built Documentation Tool

**Description:** Build a bespoke web application for requirements browsing, with a database backend and custom UI.

**Pros:**
- Full control over metadata schema, filtering, and dashboards
- Can integrate directly with CI/CD for status tracking
- Tailored UX for the specific stakeholder needs

**Cons:**
- Significant development effort diverts resources from the core migration
- Must be maintained alongside the migration project
- Content would likely live in a database, not in Git — losing audit trail
- Over-engineering for what is fundamentally a documentation problem

**Why rejected:** Building custom tooling contradicts the project's KISS and YAGNI principles. Docusaurus provides 90% of the needed functionality out of the box, and the remaining 10% (metadata dashboards) can be built as simple Docusaurus plugins or scripts.

---

## Consequences

### Positive
- **Regulatory audit trail:** All requirement changes tracked in Git history with PR reviews — satisfies FSA and DORA traceability requirements
- **Stakeholder accessibility:** Compliance officers and business analysts can browse requirements in a structured website without needing GitHub access
- **Machine-readable metadata:** YAML front matter enables automated status tracking, traceability reports, and CI validation
- **Zero content drift:** Requirements live in the same repository as the code — single source of truth
- **Low cost:** Docusaurus is open-source (MIT license), hosted as a static site (Azure Static Web Apps or GitHub Pages)
- **Familiar authoring:** Requirements are Markdown files — no new syntax for COBOL developers or analysts to learn

### Negative
- **Build step required:** Docusaurus must be built and deployed — adds a CI job
- **Node.js dependency:** Docusaurus requires Node.js, which is not part of the .NET toolchain
- **Learning curve:** Team must learn Docusaurus configuration (sidebars, plugins, deployment)

### Risks
- **Front matter schema evolution:** As requirements grow, the YAML schema may need changes
  - **Mitigation:** Define a minimal required schema upfront; validate via CI linting; evolve incrementally
- **Docusaurus version upgrades:** Major version upgrades may require migration effort
  - **Mitigation:** Pin to a major version; upgrade only when needed
- **Static site hosting:** Must ensure the hosted site is access-controlled (requirements may contain sensitive business logic)
  - **Mitigation:** Deploy to Azure Static Web Apps with Azure AD authentication; do not expose publicly

---

## Implementation Notes

- Initialize Docusaurus in a `docusaurus/` directory at the repository root, configured to source content from `docs/`
- Configure `docusaurus.config.js` to auto-generate sidebars from the `docs/requirements/` directory structure
- Add a CI workflow to build the Docusaurus site on PRs (validates Markdown and front matter)
- Deploy to Azure Static Web Apps with Azure AD authentication for access control
- Add a front matter linting script (CI step) to validate that every requirement file has required metadata fields (`id`, `domain`, `source_program`, `regulation`, `status`)
- Create a Docusaurus plugin or script to generate status dashboard pages from front matter metadata

---

## References

- [Docusaurus documentation](https://docusaurus.io/docs)
- [Docusaurus Markdown front matter](https://docusaurus.io/docs/api/plugins/@docusaurus/plugin-content-docs#markdown-front-matter)
- NordKredit CLAUDE.md — project structure and regulatory requirements
- FSA FFFS 2014:5 — Finansinspektionen regulations for credit institutions
- PSD2 — Payment Services Directive 2
- DORA — Digital Operational Resilience Act (ICT risk management and audit requirements)

---

**Template version:** 1.0
**Last updated:** 2026-02-15
