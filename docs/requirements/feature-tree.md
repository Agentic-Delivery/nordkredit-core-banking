# Feature Tree — [Project Name]

> Hierarchical decomposition of all system capabilities. Each leaf maps to user stories and use cases.
>
> User stories: [docs/requirements/user-stories.md](user-stories.md)
> Use cases: [docs/requirements/use-cases.md](use-cases.md)
>
> Legend: ✅ Implemented | 🔧 In progress | 📋 Planned | ❌ Not started
>
> **This is the recommended format.** You may use any format as long as requirements are documented before code is written.

---

## 1. [Feature Area Name] (UC-[NNN])

| Feature | Status | Stories |
|---------|--------|---------|
| 1.1 [Feature name] | ❌ | US-[XX]-01 |
| 1.2 [Feature name] | ❌ | US-[XX]-02 |
| 1.3 [Feature name] | ❌ | US-[XX]-01, US-[YY]-03 |

## 2. [Feature Area Name] (UC-[NNN])

| Feature | Status | Stories |
|---------|--------|---------|
| 2.1 [Feature name] | ❌ | US-[YY]-01 |
| 2.2 [Feature name] | ❌ | US-[YY]-02 |

---

_Add as many feature areas as needed. Each area typically maps to a use case._

### Feature Numbering

Use hierarchical numbering: `[area].[item]`

_Examples:_
- _1.1 — First feature in area 1_
- _3.4 — Fourth feature in area 3_

### Status Updates

Update status as implementation progresses:
- _❌ Not started → 📋 Planned (issue created)_
- _📋 Planned → 🔧 In progress (work started)_
- _🔧 In progress → ✅ Implemented (PR merged, tests pass)_

---

## Summary

| Status | Count |
|--------|-------|
| ✅ Implemented | 0 |
| 🔧 In progress | 0 |
| 📋 Planned | 0 |
| ❌ Not started | [total] |
| **Total** | **[total]** |

---

**Template version:** 1.0
**Last updated:** 2026-02-18
