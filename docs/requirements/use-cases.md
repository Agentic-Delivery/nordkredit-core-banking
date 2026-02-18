# Use Cases — [Project Name]

> Detailed interaction flows for [Project Name]. Each use case describes how an actor achieves a specific goal through the system.
>
> User stories: [docs/requirements/user-stories.md](user-stories.md)
> Actors: [docs/requirements/actors.md](actors.md)
>
> **This is the recommended format.** You may use any format as long as requirements are documented before code is written.

---

## UC-001: [Use Case Title]

| Property | Value |
|----------|-------|
| **Actor** | [Primary actor] |
| **Trigger** | [What initiates the use case — user action, system event, scheduled trigger] |
| **Precondition** | [What must be true before the use case starts] |
| **Postcondition** | [What must be true after the use case completes successfully] |
| **Satisfies** | [User story IDs — e.g., US-OP-01, US-CU-02] |

### Main Flow

1. [Actor] [action]
2. System [response]
3. [Actor] [action]
4. System [response and outcome]

### Alternative Flows

**[Alternative name]** (at step [N]):
- [Describe the alternative path]
- [How it diverges and where it rejoins or ends]

### Error Flows

**[Error name]** (at step [N]):
- [What goes wrong]
- [How the system handles it]

---

## UC-002: [Use Case Title]

| Property | Value |
|----------|-------|
| **Actor** | [Primary actor] |
| **Trigger** | [Trigger] |
| **Precondition** | [Precondition] |
| **Postcondition** | [Postcondition] |
| **Satisfies** | [Story IDs] |

### Main Flow

1. [Step]

---

_Add as many use cases as needed. Each use case should:_
- _Map to one or more user stories via the Satisfies field_
- _Be testable — the main flow describes a verifiable scenario_
- _Cover error cases — what happens when things go wrong_

### ID Convention

Use zero-padded numbering: `UC-001`, `UC-002`, etc.

Group related use cases by domain area if helpful:
- _UC-001 to UC-009: Onboarding_
- _UC-010 to UC-019: Core workflow_
- _UC-020 to UC-029: Administration_

---

**Template version:** 1.0
**Last updated:** 2026-02-18
