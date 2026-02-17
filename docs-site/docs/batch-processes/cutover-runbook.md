---
title: Cutover Plan and Runbook
sidebar_position: 2
---

# Cutover Plan and Runbook

**Document ID:** CUT-RUN-001
**Classification:** Confidential — Internal Use Only
**Status:** Draft — Pending Operations/SRE Review
**Last updated:** 2026-02-17
**Owner:** NordKredit AB — Operations / SRE
**Review cycle:** Monthly during migration; quarterly post-cutover

---

## 1. Purpose and Scope

This document defines the operational plan for cutting over from the IBM z/OS mainframe to the Azure-based core banking platform. It covers pre-cutover validation, domain-by-domain traffic routing, data sync verification, rollback procedures, communication, and post-cutover monitoring.

### 1.1 Strategy

NordKredit uses the **strangler fig pattern** — each domain is migrated and cut over independently. There is no big-bang switchover. The mainframe remains operational and available for rollback throughout the process.

### 1.2 Domain Cutover Order

| Order | Domain | Rationale |
|-------|--------|-----------|
| 1 | Card Management | Lowest transaction volume; isolated domain; good pilot candidate |
| 2 | Transactions | Core payment processing; validated by Card Management pilot learnings |
| 3 | Deposits | High-value domain; benefits from proven cutover pattern |
| 4 | Lending | Complex business rules; requires most parallel-run time |
| 5 | Account Management | Cross-cutting domain; cut over last to avoid dependency conflicts |

### 1.3 Hard Deadline

IBM mainframe contract renewal in **24 months** from project start. All domains must be fully cut over and the mainframe decommissioned before contract expiry.

### 1.4 Regulatory Context

- **FSA FFFS 2014:5 Ch. 4 §3** — Operational risk management during IT changes
- **DORA Art. 11** — ICT system testing (parallel-run validation)
- **DORA Art. 17-19** — ICT-related incident reporting during cutover
- **PSD2 Art. 97** — Strong Customer Authentication must remain operational
- **GDPR Art. 5(1)(e-f)** — Data integrity and confidentiality during transition

---

## 2. Pre-Cutover Checklist

Complete **all** items before initiating cutover for any domain.

### 2.1 Technical Readiness

- [ ] Azure infrastructure deployed via Bicep templates (`infra/main.bicep`) and validated in staging
- [ ] All domain business rules extracted, implemented, and traced to COBOL source and regulation (see `docs-site/docs/regulatory-traceability/traceability-matrix.md`)
- [ ] Data migration pipeline (`MigrationPipeline`) running incremental sync with zero errors for 7+ consecutive days
- [ ] Referential integrity validation (`ReferentialIntegrityValidator`) passing on all migrated tables
- [ ] EBCDIC-to-Unicode field conversion (`FieldConverter`) validated against production samples
- [ ] Parallel-run orchestrator (`ParallelRunOrchestrator`) enabled for the target domain
- [ ] Parallel-run match rate ≥ 99.9% for the target domain over 14 consecutive days
- [ ] All divergences reviewed and categorized (known-difference vs. genuine bug)
- [ ] Zero unresolved genuine divergences
- [ ] Batch jobs (`DailyBatchOrchestrator`) meeting SLA windows on Azure for 14+ consecutive days
- [ ] Comparison tests (`NordKredit.ComparisonTests`) green for the target domain
- [ ] BDD tests (`NordKredit.BDD`) green for the target domain
- [ ] Load testing completed: Azure handles peak transaction volume with ≤ 200ms p99 latency
- [ ] Disaster recovery tested: failover to secondary Azure region validated

### 2.2 Integration Readiness

- [ ] Bankgirot integration via Azure Service Bus validated (queue: `bankgirot-payments`)
- [ ] SWIFT integration via Azure Service Bus validated (queue: `swift-payments`)
- [ ] Dead-letter queue monitoring configured (`bankgirot-payments-dlq`, `swift-payments-dlq`)
- [ ] Azure AD B2C customer authentication tested with production-like load
- [ ] Azure AD internal operator authentication validated
- [ ] Key Vault secrets rotated and current

### 2.3 Operational Readiness

- [ ] Monitoring dashboards configured in Application Insights / Azure Monitor
- [ ] Alert rules created for: error rate > 0.1%, latency p99 > 500ms, batch SLA breach, DLQ depth > 0
- [ ] On-call rotation established for cutover weekend
- [ ] Runbook reviewed by operations/SRE team
- [ ] Rollback procedure tested in staging within the last 7 days
- [ ] Communication plan approved (Section 8)

### 2.4 Regulatory Readiness

- [ ] FSA notified of planned significant IT change (FFFS 2014:5 Ch. 9)
- [ ] Regulatory traceability matrix reviewed and signed off by compliance
- [ ] DORA Art. 11 parallel-run evidence package assembled
- [ ] AML screening confirmed operational on Azure (nightly batch SLA met)
- [ ] Regulatory reporting (FSA calendar) validated on Azure

---

## 3. Timing Windows

### 3.1 Cutover Schedule

Each domain cutover is performed during a **weekend maintenance window** to minimize customer impact.

| Phase | Time (CET) | Activity |
|-------|-----------|----------|
| Friday 18:00 | T-0 | Cutover begins — enter maintenance mode |
| Friday 18:00-20:00 | T+2h | Final data sync and integrity verification |
| Friday 20:00-22:00 | T+4h | Traffic routing switch (DNS + load balancer) |
| Friday 22:00-Saturday 02:00 | T+8h | Smoke testing and initial monitoring |
| Saturday 02:00-06:00 | T+12h | Nightly batch execution on Azure (validates batch SLA) |
| Saturday 06:00-18:00 | T+24h | Extended monitoring period |
| Saturday 18:00 | T+24h | Go/no-go decision for Sunday trading |
| Sunday 06:00 | T+36h | Batch cycle 2 validation |
| Sunday 18:00 | T+48h | Final go/no-go: confirm cutover or initiate rollback |
| Monday 08:00 | T+62h | Full business hours operation on Azure |

### 3.2 Nightly Maintenance Windows (Ongoing Sync)

| Window | Time (CET) | Purpose |
|--------|-----------|---------|
| Weeknight | 01:00-05:00 | Incremental data sync, batch processing |
| Weekend | Saturday 01:00-06:00 | Extended sync window, integrity checks |

### 3.3 Blackout Periods

No cutover may be scheduled during:

| Period | Reason |
|--------|--------|
| Month-end (last 3 business days + first 3) | Statement generation, interest calculation |
| FSA regulatory reporting dates | Per FSA reporting calendar |
| Swedish public holidays | Reduced staff availability |
| December 15 – January 15 | Year-end processing freeze |

---

## 4. Domain Cutover Procedures

Each domain follows the same four-phase pattern. Domain-specific parameters are listed in the subsections below.

### 4.1 Phase 1: Parallel-Run Verification

**Duration:** 2-4 weeks per domain

1. Enable parallel-run for the domain via `ParallelRunConfiguration`
2. Monitor `DivergenceTracker` metrics daily:
   - Match rate (target: ≥ 99.9%)
   - Failure categories (must be zero unresolved genuine bugs)
   - Affected accounts (must trend to zero)
3. Review all divergence records in `IDivergenceStore`
4. Resolve any genuine divergences (known-differences are pre-configured)
5. Obtain domain expert sign-off on parallel-run results

**Exit criteria:** 14 consecutive days with ≥ 99.9% match rate and zero genuine divergences.

### 4.2 Phase 2: Traffic Routing

**Duration:** 4-8 hours (within maintenance window)

1. **Announce maintenance window** to affected users (see Section 8)
2. **Freeze writes on mainframe** for the target domain
3. **Run final incremental data sync** (`MigrationPipeline`)
   ```
   Final sync → Referential integrity check → Field conversion validation
   ```
4. **Verify data parity:**
   - Record counts match (source vs. target)
   - Checksum validation on key tables
   - Sample 1,000 random records for field-level comparison
5. **Switch traffic routing:**
   - Update Azure Front Door / Traffic Manager to route domain traffic to Azure
   - For Bankgirot/SWIFT: update Service Bus subscription routing
   - For online banking (CICS → REST): update API gateway routing
6. **Verify routing:**
   - Confirm requests reaching Azure App Service (Application Insights live metrics)
   - Confirm mainframe no longer receiving new requests for this domain
7. **Run smoke tests** against production Azure endpoints

### 4.3 Phase 3: Monitoring (48 hours)

**Duration:** 48 hours minimum

Monitor continuously using Application Insights and Azure Monitor:

| Metric | Threshold | Action if Breached |
|--------|-----------|-------------------|
| Error rate (5xx) | > 0.1% | Page on-call; investigate; prepare rollback |
| API latency (p99) | > 500ms | Alert on-call; investigate |
| Batch completion | After SLA deadline | Page on-call; prepare rollback |
| DLQ depth | > 0 messages | Alert on-call; investigate failed messages |
| Database DTU usage | > 80% | Alert on-call; scale up if needed |
| Service Bus queue depth | Growing trend | Alert on-call; investigate consumer lag |

**Escalation path:**
1. On-call engineer investigates (0-15 min)
2. Incident commander engaged (15-30 min)
3. Rollback decision by cutover lead (30-60 min)

### 4.4 Phase 4: Mainframe Domain Decommission

**Duration:** 30 days post-cutover (rollback window)

1. Keep mainframe running for the domain but with no traffic (shadow mode)
2. Continue reverse data replication (Azure → mainframe) for rollback capability
3. After 30 days with no issues:
   - Stop reverse replication
   - Disable mainframe domain programs
   - Archive mainframe domain data
   - Update parallel-run configuration to disable the domain

**Do not decommission mainframe hardware until ALL domains are cut over.**

---

## 5. Domain-Specific Cutover Parameters

### 5.1 Card Management (Domain 1 — Pilot)

| Parameter | Value |
|-----------|-------|
| COBOL programs | CBACT01C, CBACT02C, CBACT03C, CBACT04C |
| Azure service | NordKredit.Api (Card Management controllers) |
| Data tables | Card master, card status, card transactions |
| Batch jobs | Card verification (CBTRN01C step) |
| Integration points | None external (internal only) |
| Estimated data volume | ~1M card records |
| Parallel-run duration | 4 weeks (extended for pilot learning) |
| Rollback window | 30 days |

### 5.2 Transactions (Domain 2)

| Parameter | Value |
|-----------|-------|
| COBOL programs | CBTRN01C, CBTRN02C, CBTRN03C, COTRN00C |
| Azure service | NordKredit.Api + NordKredit.Functions (DailyBatchOrchestrator) |
| Data tables | Transaction ledger, posting history, daily reports |
| Batch jobs | Card verification, credit validation, posting, report generation |
| Integration points | Bankgirot (Service Bus), SWIFT (Service Bus) |
| Estimated data volume | ~50M transactions/year |
| Parallel-run duration | 3 weeks |
| Rollback window | 30 days |

**Special considerations:**
- Bankgirot and SWIFT message routing must switch atomically with API traffic
- DLQ monitoring critical — failed payment messages require immediate attention
- Nightly batch must complete by 06:00 CET

### 5.3 Deposits (Domain 3)

| Parameter | Value |
|-----------|-------|
| COBOL programs | Deposit domain programs |
| Azure service | NordKredit.Api (Deposits controllers) |
| Data tables | Deposit accounts, interest accrual, balance history |
| Batch jobs | Nightly interest calculation |
| Integration points | None external |
| Estimated data volume | ~2M deposit accounts |
| Parallel-run duration | 3 weeks |
| Rollback window | 30 days |

**Special considerations:**
- Interest calculation accuracy is critical (financial impact)
- Must validate against mainframe interest calculation output for 30 days
- Month-end cutover blackout applies (balance snapshots)

### 5.4 Lending (Domain 4)

| Parameter | Value |
|-----------|-------|
| COBOL programs | Lending domain programs |
| Azure service | NordKredit.Api (Lending controllers) |
| Data tables | Loan accounts, amortization schedules, payment plans |
| Batch jobs | Payment processing, statement generation |
| Integration points | None external |
| Estimated data volume | Variable (loan portfolio) |
| Parallel-run duration | 4 weeks (complex business rules) |
| Rollback window | 30 days |

**Special considerations:**
- EU Consumer Credit Directive (2008/48/EC) compliance — APR calculations must match exactly
- Amortization schedule rounding must be validated field-by-field
- Lending business rules are the most complex — extended parallel-run

### 5.5 Account Management (Domain 5 — Final)

| Parameter | Value |
|-----------|-------|
| COBOL programs | Account management programs |
| Azure service | NordKredit.Api (Account Management controllers) |
| Data tables | Customer master, account master, address, KYC records |
| Batch jobs | AML screening (nightly), regulatory reporting |
| Integration points | Online banking (customer-facing) |
| Estimated data volume | ~500K customers, ~2M accounts |
| Parallel-run duration | 3 weeks |
| Rollback window | 30 days |

**Special considerations:**
- Cross-cutting domain — depends on all other domains being stable on Azure
- AML screening must not be interrupted (regulatory obligation)
- GDPR data subject access requests must work throughout transition
- Online banking (customer-facing) traffic switch is highest-visibility change
- Final domain — after this, mainframe decommission can proceed

---

## 6. Rollback Procedures

### 6.1 Rollback Decision Criteria

Initiate rollback if **any** of the following occur during the 48-hour monitoring window:

| Trigger | Threshold |
|---------|-----------|
| Error rate (5xx) | > 1% sustained for 15+ minutes |
| Data integrity | Any record corruption detected |
| Batch SLA breach | Nightly batch not complete by 07:00 CET (1-hour grace) |
| Payment processing failure | Any Bankgirot/SWIFT message lost or duplicated |
| Customer impact | > 100 customer-reported issues related to the cutover |
| Regulatory function failure | AML screening or regulatory reporting fails |

**Rollback authority:** Cutover lead (with CTO escalation for business hours rollback).

### 6.2 Rollback Procedure (Per Domain)

**Target: Complete rollback within 30 minutes.**

1. **Announce rollback** to war room and stakeholders
2. **Switch traffic routing back to mainframe:**
   - Revert Azure Front Door / Traffic Manager routing
   - Revert Service Bus subscription routing (Bankgirot/SWIFT)
   - Revert API gateway routing (online banking)
3. **Verify mainframe receiving traffic:**
   - Confirm transactions processing on mainframe
   - Confirm batch jobs executing on mainframe
4. **Resume mainframe write operations** for the domain
5. **Initiate reverse data sync** (Azure → mainframe) for any transactions processed during the cutover window:
   - Run `MigrationPipeline` in reverse mode
   - Validate record counts and checksums
6. **Verify data consistency** on mainframe
7. **Notify stakeholders** of rollback completion
8. **Post-mortem** within 24 hours

### 6.3 Rollback Limitations

| Scenario | Limitation | Mitigation |
|----------|-----------|------------|
| > 30 days post-cutover | Reverse replication stopped; full rollback not possible | Maintain rollback capability; extend if uncertain |
| Payment messages already cleared | Bankgirot/SWIFT settlements are irreversible | Reconciliation process; manual correction if needed |
| Customer data changed on Azure | Must be synced back to mainframe | Reverse sync pipeline handles this |
| Batch outputs diverged | Reports generated on Azure differ from mainframe | Re-run batch on mainframe post-rollback |

---

## 7. Data Sync Verification

### 7.1 Pre-Cutover Verification

Run before every domain cutover:

| Check | Method | Pass Criteria |
|-------|--------|--------------|
| Record count parity | `SELECT COUNT(*) FROM [table]` on both systems | Exact match |
| Checksum validation | SHA-256 hash of key columns per table | Exact match |
| Random sample comparison | 1,000 random records, field-by-field | 100% match |
| Referential integrity | `ReferentialIntegrityValidator.ValidateAsync()` | Zero violations |
| EBCDIC conversion | `FieldConverter` validation on Swedish character set | Zero conversion errors |
| Timestamp currency | `MigrationSyncState.LastSyncTimestamp` | Within 5 minutes of cutover start |

### 7.2 Post-Cutover Verification

Run after traffic routing switch:

| Check | Frequency | Pass Criteria |
|-------|-----------|--------------|
| Transaction reconciliation | Hourly for first 24h, then daily | Zero unreconciled transactions |
| Balance verification | After each batch cycle | Balances match expected values |
| Audit log completeness | Daily | All transactions have audit entries (DORA Art. 11) |
| DLQ monitoring | Continuous | Zero messages in DLQ |
| Data migration audit log | Daily | `IMigrationAuditLog` entries complete |

### 7.3 Data Freeze Protocol

During the traffic routing window (Phase 2, ~4-8 hours):

1. **T+0:** Disable new write operations on mainframe for target domain
2. **T+5min:** Run final incremental sync
3. **T+30min:** Verify sync completion and data parity
4. **T+45min:** Switch traffic to Azure
5. **T+60min:** Azure processing new transactions; mainframe in read-only mode

---

## 8. Communication Plan

### 8.1 Internal Communication

| Audience | Channel | Timing | Message |
|----------|---------|--------|---------|
| Executive team | Email + Teams | T-7 days | Cutover schedule, risk summary, go/no-go criteria |
| Engineering team | Teams war room | T-24h | Detailed runbook review, on-call assignments |
| Operations/SRE | Teams war room | T-2h | Final checklist review, monitoring dashboard links |
| Customer support | Email + briefing | T-3 days | FAQ document, escalation procedures, expected behaviour |
| All staff | Email | T-1 day | Maintenance window notification |

### 8.2 Customer Communication

| Audience | Channel | Timing | Message |
|----------|---------|--------|---------|
| All customers | Online banking notification | T-7 days | Planned maintenance window; limited service availability |
| All customers | SMS (opt-in) | T-24h | Reminder of maintenance window |
| All customers | Online banking banner | T-2h | Maintenance in progress; estimated completion time |
| All customers | Online banking notification | T+48h (post-cutover) | Maintenance complete; normal operations resumed |
| Affected customers (if issues) | Direct contact | As needed | Specific issue notification and resolution timeline |

### 8.3 Regulatory Notification

| Regulator | Requirement | Timing |
|-----------|-------------|--------|
| FSA (Finansinspektionen) | Notification of significant IT change (FFFS 2014:5 Ch. 9) | 30 days before first domain cutover |
| FSA | Notification of outsourcing arrangement (EBA/GL/2019/02 §76) | 30 days before first domain cutover |
| FSA | Post-cutover confirmation | Within 5 business days of final domain cutover |
| FSA | Incident notification (if cutover triggers incident) | Per DORA Art. 19 timelines (initial: 4 hours, intermediate: 72 hours, final: 1 month) |

---

## 9. Success Criteria

### 9.1 Per-Domain Cutover Success

A domain cutover is considered successful when **all** of the following are met:

| Criterion | Measurement | Target |
|-----------|------------|--------|
| Error rate | Application Insights error rate | `< 0.1%` for 48 hours |
| Latency | Application Insights p99 | `< 500ms` for 48 hours |
| Batch SLA | Batch completion time | Within SLA window for 2 consecutive cycles |
| Data integrity | Reconciliation checks | Zero discrepancies |
| Payment processing | Bankgirot/SWIFT message delivery | Zero lost or duplicated messages |
| Customer impact | Support ticket volume | No increase attributable to cutover |
| Regulatory functions | AML screening, FSA reporting | Operating normally |

### 9.2 Overall Migration Success

The full migration is considered complete when:

- [ ] All 5 domains successfully cut over and stable for 30 days each
- [ ] Mainframe receiving zero production traffic across all domains
- [ ] All batch jobs running on Azure within SLA windows
- [ ] Parallel-run framework disabled (no longer needed)
- [ ] Data migration pipeline stopped (incremental sync no longer needed)
- [ ] Regulatory traceability matrix fully validated and signed off
- [ ] FSA notified of completed migration
- [ ] Mainframe decommission plan activated (separate from this document)

---

## 10. Post-Cutover Monitoring Plan

### 10.1 Monitoring Duration

| Period | Monitoring Level | On-Call |
|--------|-----------------|---------|
| Days 1-7 | 24/7 active monitoring | Dedicated cutover team |
| Days 8-14 | Enhanced monitoring (business hours active, after-hours on-call) | Rotating on-call |
| Days 15-30 | Standard monitoring with cutover-specific alerts | Standard on-call |
| Day 30+ | Standard production monitoring | Standard on-call |

### 10.2 Key Metrics Dashboard

Configure an Application Insights dashboard with:

| Panel | Metric | Alert Threshold |
|-------|--------|----------------|
| API Health | Request success rate | `< 99.9%` |
| API Latency | p50, p95, p99 response time | p99 > 500ms |
| Error Breakdown | 4xx and 5xx by endpoint | 5xx > 0.1% |
| Batch Status | Last batch completion time | Past SLA deadline |
| Service Bus | Queue depth, DLQ depth | DLQ > 0 |
| Database | DTU %, connections, deadlocks | DTU > 80% |
| Azure Functions | Execution count, failures, duration | Failure rate > 0% |
| Data Sync | Last sync timestamp, records synced | Sync lag > 5 min (during parallel-run) |

### 10.3 Post-Cutover Review Schedule

| Review | Timing | Participants | Purpose |
|--------|--------|-------------|---------|
| Daily standup | Days 1-14 | Engineering + Ops | Status, issues, metrics review |
| Weekly review | Weeks 1-4 | Engineering + Ops + Management | Trend analysis, risk assessment |
| 30-day retrospective | Day 30 | All stakeholders | Lessons learned, confirm stability |
| Domain sign-off | Day 30 | CTO + Compliance + Ops | Formal approval to end rollback window |

---

## 11. Roles and Responsibilities

| Role | Responsibility | Authority |
|------|---------------|-----------|
| **Cutover Lead** | Overall cutover coordination; go/no-go decisions | Rollback authority; escalation to CTO |
| **Database Lead** | Data sync verification; integrity checks | Data freeze/unfreeze |
| **Infrastructure Lead** | Traffic routing; Azure resource management | DNS and load balancer changes |
| **Application Lead** | Application health monitoring; bug triage | Emergency hotfix deployment |
| **Integration Lead** | Bankgirot/SWIFT/Service Bus verification | Message routing changes |
| **On-Call Engineer** | 24/7 monitoring during cutover window | First-responder; escalation |
| **Compliance Officer** | Regulatory notification; audit trail verification | Regulatory hold on cutover |
| **Communications Lead** | Internal and customer communications | Customer notification timing |
| **CTO** | Executive escalation point | Override cutover lead decisions |

---

## 12. Mainframe Decommission Timeline

After all domains are successfully cut over and stable:

```
Month 0     All domains cut over and stable (30-day windows elapsed)
            ├── Final reconciliation across all domains
            ├── Archive all mainframe data to Azure Blob Storage
            └── FSA notification of completed migration

Month 1     Mainframe in read-only archive mode
            ├── No active processing
            ├── Historical data access only
            └── Reverse replication stopped

Month 2     Mainframe data export and verification
            ├── Full mainframe data export to provider-independent format
            ├── Verification against Azure SQL records
            └── Compliance team sign-off on data completeness

Month 3     Mainframe decommission
            ├── IBM z/OS shutdown
            ├── IBM contract termination (or non-renewal)
            ├── Hardware return or disposal
            └── Final FSA notification
```

---

## Appendix A — Cutover Checklist Template

Use this checklist for each domain cutover. Copy and fill in domain-specific values.

```
Domain: _______________
Cutover Date: _______________
Cutover Lead: _______________

PRE-CUTOVER (T-7 days)
[ ] Parallel-run match rate ≥ 99.9% for 14 days
[ ] Zero unresolved genuine divergences
[ ] Data sync running with zero errors for 7+ days
[ ] Load test completed
[ ] Rollback tested in staging
[ ] FSA notification sent (first domain only)
[ ] Communication plan executed (T-7 day notices)

PRE-CUTOVER (T-24h)
[ ] Final go/no-go meeting held
[ ] On-call rotation confirmed
[ ] Monitoring dashboards verified
[ ] Customer SMS notification sent
[ ] War room channel created

CUTOVER (T-0)
[ ] Maintenance mode announced
[ ] Mainframe write freeze for domain
[ ] Final incremental data sync completed
[ ] Data parity verified (counts + checksums + samples)
[ ] Traffic routing switched to Azure
[ ] Routing verified (requests reaching Azure)
[ ] Smoke tests passed

MONITORING (T+0 to T+48h)
[ ] Error rate < 0.1% sustained
[ ] Latency p99 < 500ms sustained
[ ] First batch cycle completed within SLA
[ ] Second batch cycle completed within SLA
[ ] DLQ depth = 0
[ ] No customer-reported issues

POST-CUTOVER (T+48h)
[ ] Go/no-go: CONFIRMED / ROLLBACK
[ ] Stakeholders notified of outcome
[ ] Post-cutover monitoring level set
[ ] 30-day rollback window started
```

---

## Appendix B — Rollback Checklist

```
Domain: _______________
Rollback Initiated: _______________
Rollback Lead: _______________
Reason: _______________

ROLLBACK EXECUTION
[ ] War room notified
[ ] Traffic routing reverted to mainframe
[ ] Mainframe receiving traffic confirmed
[ ] Mainframe write operations resumed
[ ] Reverse data sync initiated (Azure → mainframe)
[ ] Reverse sync completed and verified
[ ] Data consistency confirmed on mainframe
[ ] Stakeholders notified

POST-ROLLBACK
[ ] Customer communication sent (if customer-facing impact)
[ ] Post-mortem scheduled (within 24h)
[ ] Root cause identified
[ ] Fix plan created
[ ] Re-cutover date proposed
```

---

## Appendix C — Regulatory Reference

| Regulation | Article/Section | Relevance to Cutover |
|-----------|----------------|---------------------|
| FSA FFFS 2014:5 | Ch. 4 §3 | Operational risk management during IT changes |
| FSA FFFS 2014:5 | Ch. 9 | Outsourcing notification requirements |
| DORA (EU) 2022/2554 | Art. 11 | ICT system testing — parallel-run evidence |
| DORA (EU) 2022/2554 | Art. 17-19 | ICT-related incident reporting |
| DORA (EU) 2022/2554 | Art. 28 | ICT third-party risk management |
| PSD2 | Art. 97 | SCA must remain operational throughout cutover |
| GDPR (EU) 2016/679 | Art. 5(1)(e-f) | Data integrity and confidentiality during transition |
| GDPR (EU) 2016/679 | Art. 17 | Right to erasure must remain operational |
| EU Consumer Credit Directive | 2008/48/EC | Lending calculations must remain accurate |
| Swedish AML Act | 2017:630 | AML screening must not be interrupted |
| EBA/GL/2019/02 | §76-78 | Supervisory notification of outsourcing |
