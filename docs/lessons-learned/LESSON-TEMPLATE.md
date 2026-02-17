# LESSON-XXX: [Title]

**Date:** YYYY-MM-DD

**Type:** Mistake | Assumption | Discovery | Pattern

**Impact:** Cost | Time | Security | Quality

**Severity:** Critical | High | Medium | Low

---

## Summary

_One sentence capturing the essence of the lesson._

_Example: A cloud messaging service with availability zones enabled cost $100/month instead of expected $5/month due to undocumented SKU pricing._

---

## Background

**Context:**

_Example: We deployed a cloud messaging service to send push notifications to mobile app users. Documentation suggested the Standard tier ($5/month) would support our expected volume (1000 users, ~4 notifications/day/user)._

**Assumptions:**

_Example:_

- _We assumed "availability zones" was a free redundancy feature_
- _We assumed Standard tier pricing was flat-rate per month_
- _We assumed IaC templates would warn about cost implications_

---

## What Happened

**Event description:**

_Example: After deploying to dev environment, we received a cloud cost alert showing $100/month projected spend for the messaging service alone. Investigation revealed that enabling zone redundancy in the IaC template changed the tier to a premium level with per-unit pricing._

**Timeline:**

- _2026-01-15 10:00 - Deployed IaC template with zone redundancy enabled_
- _2026-01-15 14:00 - Received cloud cost alert email_
- _2026-01-15 14:30 - Identified messaging service as source of cost spike_
- _2026-01-15 15:00 - Reviewed cloud pricing documentation (found buried in FAQ)_
- _2026-01-15 15:30 - Disabled zone redundancy, redeployed_
- _2026-01-15 16:00 - Verified cost dropped to $5/month_

**Evidence:**

_Example:_

- _Cloud cost management screenshot showing $100/month projection_
- _IaC diff showing zone redundancy enabled vs disabled_
- _Cloud pricing calculator showing Standard = $5, Zone-redundant = $100_

---

## Root Cause

**Why (Five Whys):**

1. _Why did we enable zone redundancy?_
   - _Because we wanted high availability for push notifications._

2. _Why didn't we know it would increase cost 20x?_
   - _Because cloud documentation doesn't clearly show pricing impact of zone redundancy._

3. _Why didn't we catch this in code review?_
   - _Because we had no checklist for reviewing cost implications of IaC changes._

4. _Why didn't we test cost before deploying?_
   - _Because we assumed dev environment was "safe" for experimentation._

5. _Why did we assume that?_
   - _Because we had no process for validating infrastructure cost before deployment._

**True root cause:**

_Example: Lack of cost-awareness in infrastructure-as-code review process. No checklist, no cost calculator step, no "test from empty environment" practice._

---

## Lesson

**What we learned:**

_Example:_

- _IaC parameters can have massive cost implications (20x in this case)_
- _"Availability zones" sounds like a best-practice feature but is expensive overkill for dev/staging_
- _Cloud documentation hides cost details in FAQs and fine print_
- _Dev environments should use cheapest possible tiers unless explicitly testing production-like load_
- _Infrastructure changes need cost review before merge_

**Key insight:**

_Example: For dev/staging, **explicitly set cheap tiers and disable premium features**. Don't rely on defaults or "sounds good" feature names. For production, justify every cost increase with business value._

---

## Actions

**Immediate fixes:**

- _[x] Disabled zone redundancy in dev/staging IaC_
- _[x] Verified cost dropped to $5/month_
- _[x] Added cost budget alert at $50/month for dev_

**Long-term changes:**

- _[x] Created `docs/checklists/cloud-resource-creation-checklist.md` with cost review steps_
- _[x] Added IaC PR template with "Cost impact reviewed?" checkbox_
- _[x] Updated CLAUDE.md with "Explicitly set cheap tiers for dev/staging" rule_
- _[x] Added deployment test skill to validate IaC from empty environment before commit_

**Documentation updates:**

- _[x] Documented cloud tier cost traps in `docs/INFRASTRUCTURE-BEST-PRACTICES.md`_
- _[x] Added this lesson to `docs/lessons-learned/`_

---

## Prevention

**How to avoid in future:**

**Before writing IaC templates:**

1. Check cloud pricing calculator for resource
2. Identify parameters that affect cost (tier, redundancy, zones)
3. Set explicit values for dev/staging (cheapest) and production (justified)

**Before committing IaC:**

1. Deploy to empty test environment
2. Check cloud cost management for projected monthly cost
3. Verify cost is within budget (dev: $25/month, staging: $100/month, prod: $500/month)
4. Delete test environment

**Before merging PR:**

1. Reviewer checks "Cost impact reviewed?" checkbox
2. Reviewer verifies explicit tier values for all resources
3. Reviewer questions any premium features (zones, redundancy, etc.) in non-prod

**Monitoring:**

1. Cloud cost alerts at 50%, 80%, 100% of budget
2. Weekly cost review dashboard
3. Anomaly detection for unexpected spikes

---

## Related Lessons

- _Example: LESSON-002: IaC deployment workflow must test from empty environment_
- _Example: LESSON-010: Cloud CLI deployment polling overhead (cost vs time tradeoff)_

---

## References

- _[Link to cloud provider pricing page]_
- _[Link to IaC diff PR #123]_
- _[Link to cloud cost alert screenshot]_
- _[Link to `docs/checklists/cloud-resource-creation-checklist.md`]_

---

> **Real-world case study:** This template was developed from a lesson where Azure Notification Hub with availability zones enabled cost 830 kr/month (~$100) instead of 30 kr/month (~$5), discovered during development of an EU MDR Class IIb medical device.

---

**Template version:** 1.0
**Last updated:** 2026-02-09
