# LESSON-001: Never Reference Local MEMORY.md in Issues — Inline the Content

**Date:** 2026-02-17
**Type:** Mistake
**Tags:** intake, issues, memory, workflow

## Context

Intake was tasked with migrating shared project knowledge from the private local MEMORY.md to project files (.factory/context.md, CLAUDE.md). An issue was created to track this work.

## What Happened

The issue referenced `/home/factory/.claude/projects/.../memory/MEMORY.md` and told the worker to read it. But MEMORY.md is private to the intake session — workers run in separate sessions and cannot access it. The issue was impossible to complete as written.

The irony: the whole point of the task was to fix knowledge being trapped in a private location, and the issue itself repeated the exact same mistake.

## Lesson

MEMORY.md is local and private. Any knowledge that needs to be acted on by workers MUST be included directly in the issue body. Never reference files outside the repo in issue descriptions.

## Action

When creating issues that involve knowledge from MEMORY.md:
1. Read MEMORY.md content yourself
2. Copy the relevant content directly into the issue body
3. Never reference the file path — workers cannot access it

### Specific Actions

- [x] Updated issue #189 with inlined content
- [ ] Add to intake checklist: "Verify all referenced content is accessible to workers"

## Impact

- **Time Lost:** ~15 minutes (issue created, realized mistake, rewrote issue)

## Prevention

Intake should always ask: "Can the worker access everything referenced in this issue without my local session?"
