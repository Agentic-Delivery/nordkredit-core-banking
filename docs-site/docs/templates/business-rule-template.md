---
id: "{DOMAIN}-BR-{NNN}"
title: "{Short descriptive title of the business rule}"
domain: "{account-management | card-management | transactions | user-security | billing | reporting}"
cobol_source: "{PROGRAM.cbl}:{start-line}-{end-line}"
requirement_id: "{DOMAIN}-BR-{NNN}"
regulations:
  - "{Regulation reference, e.g., PSD2 Art. 97}"
status: "{extracted | validated | implemented | tested}"
validated_by: null
validated_date: null
priority: "{critical | high | medium | low}"
---

<!-- INSTRUCTIONS: Copy this template and fill in each section below.
     Replace all {placeholder} values in the YAML front matter and body.
     Remove these instruction comments before submitting for review. -->

# \{DOMAIN\}-BR-\{NNN\}: \{Title\}

## Summary

<!-- INSTRUCTIONS: Write 1-3 sentences describing what this business rule does,
     why it exists, and which business process it belongs to. This should be
     understandable by a non-technical stakeholder. -->

\{Describe the business rule in plain language. What does it enforce? Why does it exist?\}

## Business Logic

<!-- INSTRUCTIONS: Describe the rule's logic in enough detail for a developer
     to implement it. Use pseudocode, structured narrative, or decision tables.
     Include all conditions, thresholds, and outcomes. -->

### Pseudocode

```
{Write pseudocode or structured narrative describing the rule logic.}

Example format:
IF condition_a AND condition_b THEN
    perform action_x
    SET field_y = calculated_value
ELSE IF condition_c THEN
    perform action_z
    RAISE error "description"
END-IF
```

### Decision Table (if applicable)

<!-- INSTRUCTIONS: Use a decision table when the rule has multiple input
     conditions that combine to produce different outcomes. -->

| Condition A | Condition B | Outcome |
|-------------|-------------|---------|
| True        | True        | \{outcome_1\} |
| True        | False       | \{outcome_2\} |
| False       | True        | \{outcome_3\} |
| False       | False       | \{outcome_4\} |

## Source COBOL Reference

<!-- INSTRUCTIONS: Paste the relevant COBOL source code that implements this
     rule. Include enough surrounding context (5-10 lines before/after) for
     the reader to understand the flow. Always include line numbers. -->

**Program:** `{PROGRAM.cbl}`
**Lines:** \{start-line\}-\{end-line\}

```cobol
{Paste the relevant COBOL source code with line numbers.}

Example:
000120 IF WS-ACCOUNT-STATUS = 'ACTIVE'
000121    AND WS-BALANCE >= WS-WITHDRAWAL-AMT
000122    PERFORM 2100-PROCESS-WITHDRAWAL
000123 ELSE
000124    MOVE 'INSUFFICIENT-FUNDS' TO WS-ERROR-CODE
000125    PERFORM 9000-ERROR-HANDLER
000126 END-IF
```

## Acceptance Criteria

<!-- INSTRUCTIONS: Write acceptance criteria in GIVEN/WHEN/THEN format.
     Each scenario should be independently testable. Cover the happy path,
     error cases, and boundary conditions. These will be used to generate
     SpecFlow BDD tests. -->

### Scenario 1: \{Happy path description\}

```gherkin
GIVEN {precondition}
  AND {additional precondition if needed}
WHEN {action or event}
THEN {expected outcome}
  AND {additional expected outcome if needed}
```

### Scenario 2: \{Error/rejection case description\}

```gherkin
GIVEN {precondition}
WHEN {action or event that triggers the error}
THEN {expected error outcome}
  AND {error handling behavior}
```

### Scenario 3: \{Boundary/edge case description\}

```gherkin
GIVEN {boundary precondition}
WHEN {action at the boundary}
THEN {expected boundary outcome}
```

## Regulatory Mapping

<!-- INSTRUCTIONS: Map this business rule to the applicable regulations.
     Every rule must trace to at least one regulation or explicitly state
     "No direct regulatory requirement" with justification. -->

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| \{e.g., PSD2\} | \{e.g., Art. 97\} | \{Requirement summary\} | \{How the rule addresses the requirement\} |
| \{e.g., GDPR\} | \{e.g., Art. 6\} | \{Requirement summary\} | \{How the rule addresses the requirement\} |
| \{e.g., FFFS 2014:5\} | \{e.g., Ch. 4 §3\} | \{Requirement summary\} | \{How the rule addresses the requirement\} |

## Edge Cases

<!-- INSTRUCTIONS: Document known edge cases, unusual inputs, or race
     conditions that the original COBOL code handles (or fails to handle).
     These are critical for ensuring the migrated implementation is correct. -->

1. **\{Edge case title\}**: \{Description of the edge case and how the current system handles it.\}
2. **\{Edge case title\}**: \{Description of the edge case and how the current system handles it.\}
3. **\{Edge case title\}**: \{Description of the edge case and how the current system handles it.\}

## Domain Expert Notes

<!-- INSTRUCTIONS: Record any verbal explanations, clarifications, or
     institutional knowledge from domain experts (especially the retiring
     COBOL developers). Include the expert's name and date for traceability.
     This section is critical for capturing knowledge that isn't in the code. -->

- **\{Expert name\}** (\{date\}): \{Note or clarification about this rule.\}
- **\{Expert name\}** (\{date\}): \{Note or clarification about this rule.\}

---

**Template version:** 1.0
**Last updated:** 2026-02-15
