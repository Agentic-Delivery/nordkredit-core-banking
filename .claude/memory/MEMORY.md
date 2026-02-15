# Auto Memory

## Critical: Autonomous workflow

**NEVER stop and wait between steps.** When a task is given, complete the FULL flow:

```
implement -> test -> commit -> push -> PR -> CI green -> merge
```

## Project-specific patterns

- **Regulated financial system** — every business rule needs traceability (COBOL source -> requirement -> regulation)
- **Domain expert review required** for business rule implementations
- **Parallel-run validation** — new system output must match mainframe output before cutover
- **EBCDIC-to-Unicode** — all data from mainframe needs character encoding conversion
- **Strangler fig pattern** — migrate domain by domain, not big-bang

## Common mistakes

[To be filled from lessons learned]
