# Traceability Matrix — [Project Name]

> **Coverage: [N] of [M] user stories fully traced** ([K] have test gaps)
>
> Maps the full chain: **Requirement → Feature → Design (ADR) → Code → Test → Status**
>
> Every code change must be traceable to a documented requirement.
> This matrix is a living document — updated as new requirements and implementations are added.
>
> **This is the recommended format.** You may use any format as long as requirements are documented before code is written.

---

## The traceability chain

```
ADR (why/how)
 └── User Story (what actors need)
      └── Use Case (detailed interaction flow)
           └── Feature (capability in feature tree)
                └── Issue (implementation task)
                     └── PR (code change)
                          └── Test (proof it works)
```

| Level | Location | Example |
|-------|----------|---------|
| ADR | `docs/adr/` | [ADR-001: initial architecture decision] |
| User Story | `docs/requirements/user-stories.md` | [US-OP-01: operator provisioning] |
| Use Case | `docs/requirements/use-cases.md` | [UC-001: system onboarding] |
| Feature | `docs/requirements/feature-tree.md` | [1.1: single-command setup] |
| Issue | GitHub Issues | [#1: implement setup command] |
| PR | GitHub PRs | [#2] |
| Test | `tests/` | [tests/test-setup.sh] |

---

## How to read this matrix

| Column | Meaning |
|--------|---------|
| **Requirement** | User Story ID from `docs/requirements/user-stories.md` |
| **Feature** | Feature IDs from `docs/requirements/feature-tree.md` |
| **Design (ADR)** | Architecture Decision Record(s) from `docs/adr/` |
| **Code** | Key implementation file paths |
| **Test** | Automated test file paths |
| **Status** | **Covered** = code + tests exist, **Partial** = code exists but tests incomplete, **Missing** = not implemented |

---

## [Actor/Category] Stories

| Requirement | Feature | Design (ADR) | Code | Test | Status |
|-------------|---------|--------------|------|------|--------|
| US-[XX]-01 | [1.1–1.3] | [ADR-001] | [path/to/implementation] | [path/to/test] | Missing |
| US-[XX]-02 | [2.1] | — | — | — | Missing |

## [Actor/Category 2] Stories

| Requirement | Feature | Design (ADR) | Code | Test | Status |
|-------------|---------|--------------|------|------|--------|
| US-[YY]-01 | [3.1–3.2] | — | — | — | Missing |

---

_Add one section per actor category from your user stories document._

---

## Coverage Summary

| Category | Stories | Covered | Partial | Missing |
|----------|---------|---------|---------|---------|
| [Category 1] | [N] | 0 | 0 | [N] |
| [Category 2] | [N] | 0 | 0 | [N] |
| **Total** | **[M]** | **0** | **0** | **[M]** |

## Test Gaps

Requirements with missing or incomplete test coverage:

| Requirement | Gap | Priority |
|-------------|-----|----------|
| [US-XX-01] | [Description of what test coverage is missing] | [High/Medium/Low] |

---

**Template version:** 1.0
**Last updated:** 2026-02-18
