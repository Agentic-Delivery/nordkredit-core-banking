<!-- delivery-factory:v1.6.0 -->
# CLAUDE.md

## Regulated Financial System — READ CAREFULLY

This is a **core banking migration** for NordKredit AB, a Swedish bank regulated by Finansinspektionen (FSA).
Every business rule extracted from COBOL source must be **traceable** to the originating program AND the applicable regulation.

**Regulatory frameworks in scope**: FSA (FFFS 2014:5), PSD2, GDPR, AML/KYC, DORA, EBA Outsourcing Guidelines.

- **Never** implement a business rule without traceability (COBOL program -> requirement -> regulation)
- **Never** skip compliance validation — regulatory gaps can trigger enforcement action
- **Never** alter financial calculation logic without domain expert sign-off
- **Never** expose customer PII (GDPR) or store data outside EU (data residency)
- **Always** maintain parallel-run capability — mainframe must remain operational until cutover

---

## STOP - READ FIRST

> **These checklists apply ALWAYS, regardless of how the task was given.**

### Before git push

- [ ] **Tested locally** - no surprises in CI
- [ ] **All tests green** locally
- [ ] **Linters pass** with ZERO warnings
- [ ] **Tests included** for new/changed code

### Before claiming cause/solution

- [ ] **Read actual logs** or error messages
- [ ] **Verified with data** (not assumptions)
- [ ] **Can show evidence** for the claim

### Before implementation

- [ ] **Got explicit approval** from the user
- [ ] **Asked about uncertainties**
- [ ] **Understood existing patterns**

### After implementation is complete

**Complete the FULL flow without stopping:**

```
test locally -> commit -> push -> PR -> wait for CI green -> merge
```

- [ ] **Do NOT stop** to ask about next steps

---

## Autonomy Level

**Current level: 2 (Auto-PR)**

| Level | Description | Behavior |
|-------|-------------|----------|
| 0 | Manual | Ask before every action |
| 1 | Semi-Auto | Auto-commit, ask before push |
| **2** | **Auto-PR** | **Auto push + create PR, ask before merge** |
| 3 | Auto-Merge | Auto merge after green CI |
| 4 | Full Auto | Fully autonomous |

---

## Critical Rules

| Rule | Action |
|------|--------|
| **Never push to main** | Feature branch -> PR -> merge |
| **Never merge without green CI** | Wait for checks |
| **All changes require tests** | TDD: test first |
| **Test locally before push** | Not via CI/CD pipeline |
| **Zero linter warnings** | See Commands below |
| **Never guess** | Read logs/data first |
| **Complete full workflow** | test -> commit -> push -> PR -> CI -> merge (no pausing) |
| **Regulatory traceability** | Every business rule links to COBOL source AND regulation |
| **Domain expert review** | Business rule implementations require domain expert PR review |

### Linter Commands (ZERO warnings required)

| Technology | Command |
|------------|---------|
| C# / .NET 8 | `dotnet build /warnaserror` |
| Roslyn Analyzers | `dotnet format --verify-no-changes` |
| SonarQube | `dotnet sonarscanner begin` / `end` |
| EditorConfig | Enforced via `dotnet format` |

---

## Code Craftsmanship

| Principle | Meaning |
|-----------|---------|
| **KISS** | No unnecessary complexity |
| **YAGNI** | Only what is needed now |
| **Remove, Don't Skip** | Remove unused code entirely |
| **TDD** | RED -> GREEN -> REFACTOR |

---

## Project

**NordKredit AB — Core Banking Migration**

Migrating a 30+ year old IBM mainframe (z/OS) core banking system to C#/.NET 8 on Azure.
The system handles deposits, lending, payments, and account management across ~2 million lines
of COBOL (~1,500 programs), with batch jobs in JCL and transaction processing via IMS/CICS.

**Why**: Key COBOL developers retiring (4 of 6 within 18 months), IBM licensing costs escalating,
mainframe contract renewal in 24 months.

**Strategy**: Strangler fig pattern — migrate domain by domain with parallel-run validation.

### Migration Phases

1. **Phase 1 — Knowledge Capture (months 1-6)**: Extract business rules from COBOL source. Hardest deadline — tied to staff departures.
2. **Phase 2 — Payments Domain (months 4-12)**: First vertical slice, pilot migration approach. Parallel run with output comparison.
3. **Phase 3 — Deposits & Lending (months 10-20)**: Apply proven pattern to core domains.
4. **Phase 4 — Account Management & Cutover (months 18-28)**: Final domains, full parallel-run, mainframe decommission.

### Source System

| Component | Technology |
|-----------|-----------|
| Runtime | IBM z/OS |
| Language | COBOL 85/COBOL II |
| Database | Db2 for z/OS |
| Transactions | IMS DB/DC, CICS |
| Batch | JCL |
| Messaging | IBM MQ |
| Data structures | ~300 COBOL copybooks |
| Files | VSAM (sequential/indexed) |

### Target Architecture

| Component | Technology |
|-----------|-----------|
| Runtime | .NET 8 |
| APIs | Azure App Services (REST) |
| Batch | Azure Functions (timer triggers) |
| Database | Azure SQL Database |
| Messaging | Azure Service Bus |
| Storage | Azure Blob Storage |
| Identity | Azure AD B2C (customer), Azure AD (internal) |
| Monitoring | Application Insights, Azure Monitor |
| IaC | Bicep templates |

---

## Commands

### Build & Test

```bash
dotnet build /warnaserror
dotnet test
dotnet format --verify-no-changes
```

### Run Locally

```bash
dotnet run --project src/NordKredit.Api
```

### SpecFlow (BDD)

```bash
dotnet test --filter "Category=BDD"
```

---

## Structure

```
src/
  NordKredit.Api/              # REST API (replaces IMS transactions)
  NordKredit.Domain/           # Domain models and business rules
    Payments/                  # Payment domain
    Deposits/                  # Deposit domain
    Lending/                   # Lending domain
    AccountManagement/         # Account management domain
  NordKredit.Infrastructure/   # Azure SQL, Service Bus, external integrations
  NordKredit.Functions/        # Azure Functions (batch job replacements)
tests/
  NordKredit.UnitTests/        # xUnit unit tests
  NordKredit.BDD/              # SpecFlow BDD tests for business rules
  NordKredit.ComparisonTests/  # Output comparison tests (mainframe vs new system)
docs/
  Architecture/                # ADRs
  lessons-learned/             # Lessons learned
  regulatory/                  # Regulatory traceability matrices
docs-site/                     # Docusaurus documentation site
  docs/
    business-rules/            # Extracted business rules per domain
    requirements/              # Use cases and requirements
.factory/
  context.md                   # Shared project context
.github/
  workflows/
    ci.yml                     # PR validation CI
    deploy-docs.yml            # Docs site deployment to GitHub Pages
```

---

## Skills & Agents

### Skills (use proactively)

| Trigger | Skill |
|---------|-------|
| Next task | `/next` |
| Document lesson | `/lesson` |
| Initialize project | `/factory-init` |
| Upgrade factory | `/factory-upgrade` |
| TDD strategy | `/tdd` |
| Troubleshoot errors | `/troubleshoot` |
| Git operations | `/git-workflow` |
| Evidence package | `/evidence-package` |
| Security review | `/security-compliance` |

---

## Git Workflow

**Branch naming**: `<type>/<issue-number>-<slug>`
**Types**: feature, fix, refactor, docs, test, chore

```bash
git checkout -b feature/123-add-payment-validation
git add .
git commit -m "feat(#123): Add payment validation rules"
TESTED_LOCALLY=1 git push -u origin feature/123-add-payment-validation
gh pr create --title "feat(#123): Add payment validation rules"
```

---

## Reference

### Regulatory Traceability

Every extracted business rule must have:
1. **Source**: COBOL program name + line range (e.g., `PAYMNT01.cbl:450-520`)
2. **Requirement ID**: Structured identifier (e.g., `PAY-BR-042`)
3. **Regulation**: Applicable regulation (e.g., `PSD2 Art. 97 — SCA`)
4. **Validation**: Sign-off by domain expert (retiring COBOL developer)

### Key Integrations

| System | Protocol | Purpose |
|--------|----------|---------|
| Bankgirot | IBM MQ -> Azure Service Bus | Swedish payment clearing |
| SWIFT | IBM MQ -> Azure Service Bus | International payments |
| Online Banking | CICS -> REST API | Customer-facing operations |

### Data Migration

- Db2 -> Azure SQL schema mapping required
- EBCDIC-to-Unicode conversion for all text fields
- Incremental sync during parallel-run periods
- Referential integrity validation post-migration

### Batch SLAs

| Job | Current SLA | Validation |
|-----|------------|------------|
| Nightly interest calculation | Complete by 06:00 | Must meet same SLA on Azure |
| Monthly statements | Complete by day 3 | Must meet same SLA on Azure |
| Regulatory reporting (FSA) | Per regulatory calendar | Must meet same SLA on Azure |
| AML screening | Nightly | Must meet same SLA on Azure |
