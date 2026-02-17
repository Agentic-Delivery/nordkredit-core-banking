# ADR-003: Parallel-Run Orchestration Framework

**Date:** 2026-02-17

**Status:** Accepted

**Deciders:** Engineering team

**Tags:** architecture, backend, infrastructure, migration, compliance

---

## Context

**What is the issue we're trying to solve?**

The strangler fig migration strategy requires running both the mainframe (z/OS) and the new Azure system simultaneously during transition. Production traffic must be routed to both systems, outputs captured and compared automatically, and divergence tracked with metrics. This is essential for validating functional equivalence before cutover and required by DORA Art.11 (ICT system testing).

**What forces are at play?**

- Both systems must process the same requests and produce comparable outputs.
- Known intentional differences exist (card masking, date formats, EBCDIC encoding) and must not be flagged as divergence.
- Divergence tracking must include match rate, failure categories, and affected accounts.
- Alerting must flag divergence above configurable thresholds.
- Per-domain enablement is required (parallel-run one domain while others remain mainframe-only).
- Results must be logged for DORA audit trail.
- The mainframe is the system of record during parallel-run — the Azure response is for comparison only.

---

## Decision

**What did we decide?**

Implement the parallel-run orchestration as domain-layer abstractions with the following components:

1. **`ParallelRunOrchestrator`** — Coordinates execution: sends request to both systems, captures outputs, delegates comparison, tracks results. Returns the mainframe response to the caller (system of record).

2. **`IMainframeGateway`** — Interface for calling the mainframe system. Infrastructure layer will implement via HTTP/Service Bus. Domain layer only depends on the interface.

3. **`IComparisonEngine`** — Compares mainframe and Azure outputs, applying known-difference rules. Returns a `ComparisonResult` with match/mismatch details and field-level divergence.

4. **`IDivergenceStore`** — Persists comparison results for audit trail. Infrastructure implements with Azure SQL.

5. **`ParallelRunConfiguration`** — Per-domain enablement and threshold configuration.

6. **`DivergenceTracker`** — Aggregates metrics (match rate, failure categories, affected accounts) and checks thresholds for alerting.

The orchestrator lives in `NordKredit.Domain/ParallelRun/` following existing domain patterns. It uses constructor injection and async/await throughout, consistent with the rest of the codebase.

---

## Alternatives Considered

### Alternative 1: API Management traffic mirroring

**Description:** Use Azure API Management to mirror traffic to both backends.

**Pros:**
- No application code changes
- Built-in traffic splitting

**Cons:**
- No output comparison capability
- No divergence tracking or metrics
- Cannot handle batch operations
- No per-domain enablement

**Why rejected:** APIM can route traffic but cannot compare outputs or track divergence — the core requirement.

### Alternative 2: Infrastructure-only approach (reverse proxy)

**Description:** Implement comparison at the infrastructure layer only.

**Pros:**
- Transparent to application code

**Cons:**
- Cannot apply domain-specific known-difference rules
- No access to business context for divergence categorization
- Harder to test

**Why rejected:** Known-difference handling (card masking, date formats) requires domain knowledge.

---

## Consequences

### Positive
- Testable with unit tests using stub implementations
- Per-domain enablement via configuration
- Known-difference handling is extensible per domain
- Audit trail satisfies DORA Art.11 requirements
- Follows existing codebase patterns (interfaces, DI, async)

### Negative
- Application code must explicitly participate in parallel-run
- Requires mainframe gateway implementation when mainframe connectivity is available

### Risks
- Mainframe latency could impact response times during parallel-run
  - **Mitigation:** Orchestrator uses configurable timeout for mainframe calls; returns Azure result immediately if mainframe times out
- Large divergence volumes could overwhelm storage
  - **Mitigation:** Configurable sampling rate and retention policy

---

## Implementation Notes

- Domain models: `src/NordKredit.Domain/ParallelRun/`
- Unit tests: `tests/NordKredit.UnitTests/ParallelRun/`
- Builds on existing golden file patterns in `tests/NordKredit.ComparisonTests/`
- Per-domain configuration: `ParallelRunConfiguration` with domain enable flags and thresholds

---

## References

- Issue #155: Create parallel-run orchestration framework
- DORA Art.11 (ICT system testing)
- FFFS 2014:5 Ch.4 §3 (operational risk management)
- Existing comparison tests: `tests/NordKredit.ComparisonTests/`

---

**Template version:** 1.0
**Last updated:** 2026-02-17
