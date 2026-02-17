# ADR-001: Batch Pipeline Orchestrator Pattern

**Date:** 2026-02-17

**Status:** Accepted

**Deciders:** Engineering team

**Tags:** architecture, backend, batch, infrastructure

---

## Context

**What is the issue we're trying to solve?**

The nightly batch pipeline runs four sequential steps: card verification, credit/expiration validation, transaction posting, and report generation. These replace the JCL job chain (CBTRN01C → CBTRN02C → CBTRN03C). We need an orchestrator to coordinate the pipeline execution order, pass results between steps, handle errors, and monitor SLA compliance.

**What forces are at play?**

- The existing activity functions (`CardVerificationFunction`, `TransactionCreditValidationFunction`, `TransactionPostingFunction`, `TransactionReportFunction`) are plain C# classes with `RunAsync()` methods — not Azure Functions SDK classes.
- The project uses `Microsoft.NET.Sdk.Worker`, not `Microsoft.Azure.Functions.Worker`.
- The issue title mentions "Durable Functions" but the SDK is not present.
- YAGNI: Adding the Azure Functions SDK just for the orchestrator adds significant complexity and dependency without immediate deployment need.
- The pipeline must complete by 06:00 (SLA from CLAUDE.md batch SLAs).
- Failed transactions must be filtered between steps (verified-only → valid-only).
- Unrecoverable errors must halt the pipeline and raise alerts.

---

## Decision

**What did we decide?**

Implement the orchestrator as a plain C# class (`DailyBatchOrchestrator`) following the Durable Functions orchestrator *pattern* without the SDK dependency. The class:

1. Coordinates the four activity functions in sequence
2. Filters results between steps (only verified → only valid)
3. Halts on unrecoverable errors with structured logging (Application Insights)
4. Monitors SLA compliance (06:00 deadline)
5. Returns a comprehensive `DailyBatchResult` with per-step results and timing

The `Worker` (`BackgroundService`) acts as the timer trigger, invoking the orchestrator on a configurable CRON-like schedule.

When the Azure Functions SDK is later added, the orchestrator can be wrapped with `[OrchestrationTrigger]` and each step with `[ActivityTrigger]` attributes with minimal refactoring.

---

## Alternatives Considered

### Alternative 1: Full Azure Durable Functions SDK

**Description:** Add `Microsoft.Azure.Functions.Worker` and `Microsoft.Azure.Functions.Worker.Extensions.DurableTask` packages and implement with proper `[OrchestrationTrigger]`/`[ActivityTrigger]` attributes.

**Pros:**
- Real Durable Functions with replay, checkpointing, and retry policies
- Automatic state persistence

**Cons:**
- Requires rewriting all four existing activity functions to use `[ActivityTrigger]`
- Requires switching from `Microsoft.NET.Sdk.Worker` to Azure Functions SDK
- Adds deployment dependency on Azure Functions runtime
- Significant scope creep for this issue

**Why rejected:** All four activity functions are already implemented as plain classes. Adding the SDK requires rewriting them and changing the project SDK — too much scope for this issue and violates YAGNI since no Azure deployment target exists yet.

### Alternative 2: Direct orchestration in Worker.cs

**Description:** Put all pipeline logic directly in the `Worker.ExecuteAsync` method.

**Pros:**
- Simplest approach, no new classes

**Cons:**
- Untestable (BackgroundService is hard to unit test)
- Violates single responsibility
- No reuse outside the worker context

**Why rejected:** Testability and separation of concerns are critical for a regulated financial system.

---

## Consequences

### Positive
- Testable orchestrator with full unit test coverage
- Follows existing codebase patterns (plain classes, constructor injection, partial class logging)
- Ready for future Azure Functions SDK adoption with minimal refactoring
- Clear SLA monitoring with structured logging for Application Insights

### Negative
- No automatic replay/checkpointing (must be added if needed later)
- No automatic retry policies (can be added to individual steps)

### Risks
- If Azure Functions SDK is added later, some refactoring required
  - **Mitigation:** Keep step interfaces clean so wrapping with attributes is straightforward

---

## Implementation Notes

- `DailyBatchOrchestrator` in `src/NordKredit.Functions/Batch/DailyBatchOrchestrator.cs`
- `DailyBatchResult` in `src/NordKredit.Functions/Batch/DailyBatchResult.cs`
- Tests in `tests/NordKredit.UnitTests/Batch/DailyBatchOrchestratorTests.cs`
- Worker wired with configurable schedule via `IConfiguration`
- SLA deadline: 06:00 (configurable)

---

## References

- Issue #79: Batch pipeline orchestrator with Durable Functions
- COBOL source: JCL job chain CBTRN01C → CBTRN02C → CBTRN03C
- Regulations: FFFS 2014:5 Ch.4 §3 (operational risk), DORA Art.11 (ICT risk management)
- Business Rules: TRN-BR-005 through TRN-BR-009 (pipeline integration)

---

**Template version:** 1.0
**Last updated:** 2026-02-17
