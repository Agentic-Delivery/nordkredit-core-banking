# Project Context

> **Shared project knowledge** — committed to the repo so all team members and sessions have the same context.
>
> Updated by workers through PRs. Intake reads this file but never writes to it directly.

## Domain

### Domain Description

Core banking system for NordKredit AB, a mid-size Swedish bank. The system handles deposits, lending, payments, and account management. Currently running on IBM z/OS mainframe with ~2 million lines of COBOL (~1,500 programs), batch jobs in JCL, and transaction processing via IMS/CICS. Being migrated to C#/.NET 8 on Azure using a strangler fig pattern.

### Key Terms

| Term | Definition |
|------|-----------|
| Copybook | COBOL COPY member — shared data structure used as a contract between programs (~300 in the system) |
| IMS DB/DC | IBM Information Management System — handles hierarchical database and transaction processing |
| CICS | Customer Information Control System — online transaction processing for customer-facing operations |
| JCL | Job Control Language — batch job definitions for nightly/monthly processing |
| VSAM | Virtual Storage Access Method — file storage for batch intermediate processing |
| Strangler Fig | Migration pattern — route traffic domain-by-domain from old to new system |
| Parallel Run | Period where both mainframe and Azure process the same transactions for output comparison |
| Bankgirot | Swedish payment clearing system |
| SWIFT | International payment messaging network |
| MFS | Message Format Service — IMS terminal screen definitions |
| SCA | Strong Customer Authentication (PSD2 requirement) |
| PEP | Politically Exposed Person (AML screening requirement) |
| EBCDIC | Extended Binary Coded Decimal Interchange Code — mainframe character encoding |

### Stakeholders

| Role | Responsibility |
|------|---------------|
| Business analysts | Requirements validation and sign-off on extracted business rules |
| C#/.NET developers | Primary consumers of extracted requirements, build target system |
| Compliance and risk team | Regulatory traceability review, FSA/PSD2/AML validation |
| Operations/SRE team | Azure infrastructure, deployment, cutover execution |
| Retiring COBOL developers | Domain experts — knowledge sources during extraction phase (4 of 6 leaving within 18 months) |

## Constraints

- **Knowledge deadline**: 18 months before critical COBOL staff retire — Phase 1 (Knowledge Capture) has the hardest deadline
- **Financial deadline**: IBM z/OS mainframe contract renewal in 24 months
- **Regulatory compliance**: Must maintain FSA, PSD2, GDPR, AML/KYC, DORA compliance throughout migration — no gaps allowed
- **Parallel operation**: Mainframe cannot be shut down during migration — must run alongside new system
- **No automated tests**: Existing system has no test suite — behavior must be inferred from COBOL source and validated against production output samples
- **Batch SLAs**: Nightly and monthly batch processing windows have strict SLAs that must be met on Azure
- **Data residency**: GDPR requires customer data stored within EU
- **DORA compliance**: Azure as critical ICT third-party provider requires DORA-compliant contract clauses, audit rights, exit strategy
- **EBA outsourcing**: Cloud migration of core banking = material outsourcing — requires risk assessment, supervisory notification, documented exit plan

## Architecture Decisions

| Decision | Rationale | Date |
|----------|-----------|------|
| C#/.NET 8 as target language | Existing .NET competence in dev team, Azure ecosystem alignment, migration tooling support (SoftwareMining, Raincode) | 2026-02-15 |
| Azure as cloud platform | Strategic alignment with .NET ecosystem, enterprise banking support | 2026-02-15 |
| Modular monolith -> services | Start with modular monolith per domain, decompose into services as migration matures | 2026-02-15 |
| Strangler fig migration | Migrate domain-by-domain with parallel-run, not big-bang — reduces risk | 2026-02-15 |
| Payments as pilot domain | First vertical slice to prove migration approach and tooling | 2026-02-15 |
| xUnit + SpecFlow for testing | xUnit for unit tests, SpecFlow BDD for business rule specification and validation | 2026-02-15 |
| Azure Functions for batch | Replace JCL batch jobs with timer-triggered Azure Functions | 2026-02-15 |
| Azure Service Bus for messaging | Replace IBM MQ for async communication and integration | 2026-02-15 |

## Backlog Status

> Last updated: 2026-02-15

- **Open issues:** 0 (factory just initialized)
- **In progress:** 0
- **Completed:** 0

### Recently Completed

- Factory initialization

### Known Blockers

- Access to COBOL source code repository needed for Phase 1 knowledge capture
- Domain expert availability must be scheduled before departures

## Known Gotchas (cross-factory)

> Lessons learned across all Delivery Factory deployments. These apply to every project.

### GitHub Actions: Always set artifact retention

GitHub Actions has a storage quota for artifacts (500 MB free, 2 GB Pro). Without explicit `retention-days`, artifacts are kept for 90 days. The quota fills up silently and **all** Actions runs start failing — which breaks the factory's CI feedback loop. Workers stall or escalate because they can't read CI results.

**Fix**: Always set `retention-days` in every workflow that uploads artifacts:

```yaml
- uses: actions/upload-artifact@v4
  with:
    name: test-results
    path: results/
    retention-days: 14
```

**Detection**: If CI suddenly fails on all PRs with storage-related errors, check artifact usage at `https://github.com/<org>/<repo>/settings/actions` -> Artifact and log retention.

*Source: LESSON-016 (SmartPill Organizer) — 3-day outage masked by full artifact quota.*

## Lessons Learned

| # | Lesson | Impact |
|---|--------|--------|
| - | No lessons recorded yet | - |
