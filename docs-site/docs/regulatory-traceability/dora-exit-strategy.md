---
title: DORA Exit Strategy for Azure
sidebar_position: 3
---

# DORA Exit Strategy for Azure

**Document ID:** DORA-EXIT-001
**Classification:** Confidential — Internal Use Only
**Status:** Draft — Pending Compliance Team Review
**Last updated:** 2026-02-17
**Owner:** NordKredit AB — IT Risk & Compliance
**Review cycle:** Annual (or upon material change to Azure dependency)

---

## 1. Purpose and Scope

This document fulfils the exit strategy requirements mandated by:

- **DORA Art. 28** — Management of ICT third-party risk, requiring financial entities to have exit plans for critical ICT service providers
- **DORA Art. 29** — Preliminary assessment of ICT concentration risk at entity and group level
- **DORA Art. 30(2)(f)** — Key contractual provisions requiring documented exit strategies, including termination rights, transition periods, and data return obligations
- **EBA/GL/2019/02** — Guidelines on outsourcing arrangements, requiring exit plans for material outsourcing of critical or important functions

Microsoft Azure is classified as a **critical ICT third-party service provider** for NordKredit AB's core banking platform. This exit strategy covers all Azure services consumed by the migrated core banking system.

### 1.1 Azure Services in Scope

| Azure Service | Function | Criticality |
|---------------|----------|-------------|
| Azure App Services | REST API hosting (replaces IMS/CICS transactions) | Critical |
| Azure SQL Database | Primary data store (replaces Db2 for z/OS) | Critical |
| Azure Functions | Batch processing (replaces JCL batch jobs) | Critical |
| Azure Service Bus | Messaging (replaces IBM MQ for Bankgirot/SWIFT) | Critical |
| Azure AD B2C | Customer authentication | Critical |
| Azure AD | Internal operator authentication | High |
| Azure Blob Storage | Document and file storage | High |
| Application Insights | Monitoring and observability | Medium |
| Azure Monitor | Infrastructure monitoring and alerting | Medium |
| Azure Key Vault | Secret and certificate management | Critical |

### 1.2 Regulatory Context

NordKredit AB is supervised by **Finansinspektionen (FSA)** and subject to:

- **DORA** (Regulation (EU) 2022/2554) — applicable from 17 January 2025
- **EBA/GL/2019/02** — Guidelines on outsourcing arrangements
- **FSA FFFS 2014:5** — Regulations and general guidelines regarding governance, risk management, and control (Ch. 9 on outsourcing)
- **GDPR** (Regulation (EU) 2016/679) — data portability and deletion requirements during exit

---

## 2. Concentration Risk Assessment (DORA Art. 29)

### 2.1 Current Azure Dependency Profile

| Dimension | Assessment | Risk Level |
|-----------|------------|------------|
| Compute | All production workloads hosted on Azure App Services and Azure Functions | High |
| Data storage | All customer and transaction data in Azure SQL and Blob Storage | High |
| Identity | Customer auth (B2C) and operator auth (AD) both on Azure | High |
| Messaging | Bankgirot and SWIFT integration via Azure Service Bus | High |
| Monitoring | All observability via Application Insights / Azure Monitor | Medium |
| IaC | Bicep templates (Azure-specific) | Medium |
| Geographic | Sweden Central / North Europe Azure regions (EU data residency) | Low |

### 2.2 Concentration Risk Factors

| Factor | Description | Mitigation |
|--------|-------------|------------|
| Single-cloud dependency | All core banking functions on one provider | Exit strategy; IaC abstraction roadmap |
| Azure-specific PaaS coupling | Service Bus, B2C, Functions use Azure-native APIs | Abstraction layers in NordKredit.Infrastructure |
| Contractual lock-in | Enterprise Agreement terms, reserved instances | Minimum 12-month notice in contract |
| Staff skills concentration | .NET/Azure skills dominant in team | Cross-training plan for alternative platforms |
| Data gravity | Large volumes of customer and transaction data in Azure SQL | Incremental data export capability maintained |

### 2.3 Substitutability Assessment

Azure services can be replaced, though with varying effort:

| Azure Service | Substitute Options | Migration Effort |
|---------------|-------------------|-----------------|
| Azure App Services | AWS ECS/EKS, GCP Cloud Run, on-premises Kubernetes | Medium — .NET 8 runs on any container platform |
| Azure SQL Database | AWS RDS (SQL Server/PostgreSQL), GCP Cloud SQL, on-premises SQL Server | Medium — standard SQL; EF Core supports multiple providers |
| Azure Functions | AWS Lambda, GCP Cloud Functions, Kubernetes CronJobs | Medium — timer-triggered functions are portable patterns |
| Azure Service Bus | AWS SQS/SNS, GCP Pub/Sub, RabbitMQ, Apache Kafka | Medium — messaging abstracted via `IMessagePublisher`/`IMessageConsumer` interfaces |
| Azure AD B2C | AWS Cognito, Auth0, Keycloak, on-premises ADFS | High — customer identity migration requires careful planning |
| Azure AD | Any OIDC/SAML provider, on-premises ADFS | Medium — standard OIDC protocols used |
| Azure Blob Storage | AWS S3, GCP Cloud Storage, MinIO | Low — standard blob/object storage APIs |
| Application Insights | Datadog, Grafana/Prometheus, Elastic APM | Low — OpenTelemetry instrumentation planned |
| Azure Key Vault | AWS Secrets Manager, HashiCorp Vault | Medium — secrets abstracted via configuration providers |

---

## 3. Data Portability Plan (DORA Art. 30(2)(f))

### 3.1 Data Classification

| Data Category | Volume (Est.) | Format | Sensitivity | Retention Requirement |
|---------------|---------------|--------|-------------|----------------------|
| Customer master data | ~500K records | Azure SQL (relational) | PII — GDPR Art. 5 | Active + 10 years post-closure |
| Account data | ~2M accounts | Azure SQL (relational) | PII — GDPR Art. 5 | Active + 10 years post-closure |
| Transaction history | ~50M records/year | Azure SQL (relational) | PII — FSA Ch. 7 | 7 years minimum |
| Card data | ~1M records | Azure SQL (relational, encrypted) | PCI-DSS, PII | Active + 3 years post-expiry |
| Batch job outputs | Variable | Azure Blob Storage (files) | Mixed | Per regulatory calendar |
| Audit logs | Continuous | Application Insights / Azure Monitor | Internal | 5 years |
| Identity data (B2C) | ~500K customer profiles | Azure AD B2C (directory) | PII — GDPR Art. 5 | Active + account closure period |
| Messaging history | Variable | Azure Service Bus (transient) | Transaction data | 30 days (queue TTL) |

### 3.2 Export Capabilities

| Data Store | Export Method | Format | Frequency |
|------------|-------------|--------|-----------|
| Azure SQL Database | BACPAC export, `bcp` utility, EF Core data extraction | SQL Server native format, CSV, JSON | On-demand; daily backup retained 35 days |
| Azure Blob Storage | AzCopy, Azure Storage REST API | Original file formats | On-demand |
| Azure AD B2C | MS Graph API bulk export | JSON (SCIM-compatible) | On-demand |
| Application Insights | Log Analytics export, REST API | JSON, CSV | Continuous export configured |
| Azure Key Vault | Key/secret export (where policy allows) | PFX (certificates), plaintext (secrets) | On-demand — requires privileged access |

### 3.3 Data Portability Procedures

1. **Regular export testing**: Quarterly test of full data export from Azure SQL to validate export pipeline integrity
2. **Backup independence**: Database backups stored in geo-redundant storage; export copies maintained in a provider-independent format (CSV/Parquet) on a quarterly basis
3. **Schema documentation**: Complete Entity Framework Core model documentation maintained in source control, independent of Azure
4. **EBCDIC/Unicode mapping**: Character encoding conversion documentation preserved from original Db2 migration, enabling reconversion if needed

---

## 4. Service Continuity During Transition

### 4.1 Transition Architecture

During an exit from Azure, NordKredit will maintain service continuity through a phased approach:

| Phase | Duration | Approach | Service Level |
|-------|----------|----------|---------------|
| Phase 1 — Preparation | 0-3 months | Establish target environment; begin data replication | Full service on Azure |
| Phase 2 — Parallel Run | 3-9 months | Run both Azure and target in parallel; compare outputs | Full service on Azure; validation on target |
| Phase 3 — Incremental Cutover | 9-15 months | Migrate domain-by-domain (same strangler fig pattern) | Split traffic; primary shifts to target |
| Phase 4 — Decommission | 15-18 months | Shut down Azure services; verify data deletion | Full service on target |

### 4.2 Minimum Service Levels During Transition

| Service | SLA During Transition | Degradation Tolerance |
|---------|----------------------|----------------------|
| Customer-facing API | 99.9% availability | Read-only mode acceptable for up to 4 hours during cutover |
| Batch processing | Complete within existing SLA windows | One-time SLA extension of 2 hours during migration weekends |
| Payment clearing (Bankgirot/SWIFT) | No disruption | Zero tolerance — parallel messaging maintained |
| Regulatory reporting | Per FSA calendar | No delay permitted |
| AML screening | Nightly completion | No delay permitted |

### 4.3 Rollback Capability

Each domain cutover maintains rollback capability for 30 days:
- Azure resources kept running but traffic-drained
- Database replication continues in reverse (target to Azure)
- DNS and routing changes reversible within 15 minutes

---

## 5. Alternative Provider Assessment

### 5.1 Evaluation Criteria

Alternative providers must satisfy:

| Criterion | Requirement | Weight |
|-----------|-------------|--------|
| EU data residency | Data centres in EU/EEA; Swedish or Nordic location preferred | Mandatory |
| .NET 8 runtime support | Container or PaaS hosting for .NET 8 applications | Mandatory |
| SQL Server or PostgreSQL | Managed relational database with high availability | Mandatory |
| Messaging service | Queues/topics supporting at least-once delivery | Mandatory |
| Identity provider | OIDC/SAML compliant; supports customer-facing B2C flows | Mandatory |
| FSA/DORA compliance | Provider's own DORA compliance posture documented | Mandatory |
| ISAE 3402 / SOC 2 | Third-party audit reports available | Mandatory |
| Financial viability | Provider operational stability over 5+ year horizon | High |
| Migration tooling | Tooling or professional services for migration assistance | Medium |

### 5.2 Pre-Assessed Alternatives

| Provider | Strengths | Weaknesses | Readiness |
|----------|-----------|------------|-----------|
| **AWS** | Mature .NET support (ECS/EKS), RDS SQL Server, SQS/SNS, Cognito | B2C migration effort; Bicep-to-CloudFormation conversion | High |
| **GCP** | Cloud Run (.NET containers), Cloud SQL, Pub/Sub | Smaller Nordic presence; less .NET-specific tooling | Medium |
| **On-premises / co-location** | Full control; no third-party dependency | Capex cost; operational burden; recruitment challenge | Low |
| **Nordic cloud (e.g., Safespring, Binero)** | Swedish data residency; regulatory familiarity | Limited PaaS; smaller service catalogue; scale concerns | Low-Medium |

### 5.3 Recommended Primary Alternative

**AWS (EU regions — Stockholm `eu-north-1`)** is the recommended primary alternative based on:
- .NET 8 container support via ECS/Fargate
- SQL Server on RDS with Multi-AZ
- SQS/SNS as Service Bus substitute (messaging interfaces already abstracted)
- Cognito for customer identity (B2C equivalent)
- Swedish data centre presence (Stockholm region)
- ISAE 3402 Type II and SOC 2 reports available
- FSA has accepted AWS for other Swedish financial institutions

---

## 6. Contractual Provisions and Audit Rights (DORA Art. 30)

### 6.1 Required Contractual Clauses

The Microsoft Enterprise Agreement and Azure service terms must include (per DORA Art. 30):

| Provision | DORA Article | Status |
|-----------|-------------|--------|
| Right to terminate with reasonable notice (minimum 12 months) | Art. 30(2)(f) | To be verified in EA |
| Data return in commonly used, machine-readable format | Art. 30(2)(f) | Covered by Azure export tools |
| Data deletion confirmation post-exit | Art. 30(2)(f) | To be negotiated |
| Audit rights for NordKredit and FSA | Art. 30(2)(e) | Standard in Microsoft EA for financial services |
| Sub-processor notification and approval | Art. 30(2)(a) | Covered by Microsoft DPA |
| Incident notification within agreed timeframes | Art. 30(2)(g) | Covered by Microsoft security incident notification |
| Data processing locations (EU only) | Art. 30(2)(b) | Enforced via Azure Policy (EU regions only) |
| Business continuity arrangements | Art. 30(2)(d) | Covered by Azure SLA and geo-redundancy |

### 6.2 Audit Rights

- **NordKredit audit right**: Right to audit Azure infrastructure and controls, exercisable annually or upon material incident
- **FSA supervisory access**: Finansinspektionen has direct access rights to Microsoft/Azure for supervisory purposes per DORA Art. 30(2)(e)
- **Third-party audit reliance**: Microsoft provides ISAE 3402 Type II, SOC 1/2/3, ISO 27001/27017/27018 reports that may be used to satisfy ongoing monitoring obligations
- **Pooled audit arrangements**: NordKredit may participate in joint audits organized through the European Supervisory Authorities' oversight framework for critical ICT providers (DORA Art. 31-36)

---

## 7. Data Deletion Verification Post-Exit (DORA Art. 30(2)(f))

### 7.1 Deletion Scope

Upon completion of exit, the following data must be verifiably deleted from Azure:

| Data Category | Azure Service | Deletion Method | Verification |
|---------------|---------------|----------------|-------------|
| Customer and account data | Azure SQL Database | Database deletion + storage wipe | Microsoft deletion certificate |
| Identity profiles | Azure AD B2C | Tenant deletion | Microsoft deletion certificate |
| Documents and files | Azure Blob Storage | Storage account deletion | Microsoft deletion certificate |
| Secrets and certificates | Azure Key Vault | Vault soft-delete purge | Key Vault audit log |
| Monitoring data | Application Insights | Workspace deletion | Microsoft deletion certificate |
| Backups | Azure SQL geo-redundant backups | Automatic expiry after retention period | Backup retention policy confirmation |
| Messaging data | Azure Service Bus | Namespace deletion | Service Bus audit log |

### 7.2 Deletion Verification Process

1. **Pre-deletion inventory**: Complete asset inventory of all Azure resources via Azure Resource Graph
2. **Data export confirmation**: Verify all data has been successfully exported and validated in the target environment
3. **Deletion execution**: Delete all Azure resources, resource groups, and subscriptions
4. **Deletion certificate request**: Request formal data deletion certificate from Microsoft, per the Data Protection Addendum
5. **Backup expiry monitoring**: Monitor and confirm that geo-redundant backups expire per retention policy (maximum 35 days for Azure SQL)
6. **Audit log retention**: Retain Azure activity logs and deletion evidence for 5 years for regulatory audit purposes
7. **GDPR Art. 17 alignment**: Confirm deletion satisfies right-to-erasure obligations — no customer PII remains on Azure

---

## 8. Exit Timeline

### 8.1 Planned Exit Timeline (18 months)

```
Month 0-1    Decision and notification
             ├── Board decision to exit Azure
             ├── FSA supervisory notification (EBA/GL/2019/02 §76)
             └── Microsoft notification per EA terms

Month 1-3    Preparation
             ├── Target environment procurement and setup
             ├── IaC conversion (Bicep → target platform IaC)
             ├── CI/CD pipeline adaptation
             └── Data replication pipeline establishment

Month 3-6    Infrastructure migration
             ├── Deploy application stack on target platform
             ├── Establish database replication (Azure SQL → target)
             ├── Configure messaging bridge (Service Bus → target)
             └── Identity provider migration planning (B2C → target)

Month 6-9    Parallel run
             ├── Both platforms processing identical workloads
             ├── Output comparison testing (reuse parallel-run framework)
             ├── Batch SLA validation on target platform
             └── Customer identity migration (phased, with dual-write)

Month 9-15   Domain-by-domain cutover
             ├── Payments domain cutover
             ├── Deposits and lending cutover
             ├── Account management cutover
             └── Regulatory reporting cutover

Month 15-18  Decommission
             ├── Azure traffic drained; monitoring only
             ├── Final data export and reconciliation
             ├── Azure resource deletion
             ├── Deletion verification and certification
             └── FSA notification of completed exit
```

### 8.2 Emergency Exit Timeline (6 months)

In the event of a critical failure, regulatory order, or provider insolvency:

| Phase | Duration | Actions |
|-------|----------|---------|
| Emergency declaration | Week 1 | Board decision; FSA notification; invoke EA termination clause |
| Rapid deployment | Weeks 1-4 | Deploy to pre-assessed alternative (AWS eu-north-1) using containerised workloads |
| Data migration | Weeks 2-8 | Azure SQL export via BACPAC; restore on target; validate integrity |
| Service cutover | Weeks 8-16 | Domain-by-domain cutover with reduced parallel-run period |
| Stabilisation | Weeks 16-26 | Performance tuning; SLA validation; regulatory reporting confirmation |

**Pre-requisites for emergency exit readiness:**
- Quarterly export test to validate data portability
- Annual deployment test to alternative provider (dry run)
- Containerised application images maintained in provider-independent registry
- Messaging interface abstraction validated against alternative broker

---

## 9. EBA Outsourcing Guidelines Compliance

### 9.1 Supervisory Notification (EBA/GL/2019/02 §76-78)

| Requirement | Status | Evidence |
|-------------|--------|----------|
| Notification to FSA before outsourcing core banking to Azure | Required | To be submitted as part of migration Phase 2 |
| Notification of material changes to outsourcing arrangement | Required | Process defined — triggered by EA renewal or service changes |
| Notification of exit from outsourcing arrangement | Required | Included in exit timeline (Month 0-1) |
| Outsourcing register maintained and available to FSA | Required | Maintained in NordKredit compliance system |

### 9.2 Risk Assessment (EBA/GL/2019/02 §38-43)

| Risk Factor | Assessment | Mitigation |
|-------------|------------|------------|
| Operational risk | Azure outage could halt core banking | Multi-region deployment; Azure SLA 99.99%; exit strategy |
| Concentration risk | Single provider for all core banking ICT | This exit strategy; annual alternative readiness test |
| Data risk | Customer PII in third-party cloud | EU-only regions; encryption at rest and in transit; GDPR DPA |
| Compliance risk | Provider non-compliance with DORA | Contractual audit rights; Microsoft DORA compliance programme |
| Vendor lock-in risk | Azure-specific PaaS services | Infrastructure abstraction layers; IaC portability plan |
| Country/political risk | Microsoft US-headquartered; EU data transfer | EU Data Boundary commitment; Schrems II safeguards |

### 9.3 Concentration Risk Mitigation (DORA Art. 29)

| Mitigation Measure | Implementation | Frequency |
|---------------------|----------------|-----------|
| Exit strategy maintenance | This document, reviewed and tested | Annual |
| Alternative provider assessment | Section 5 of this document | Annual |
| Data export test | Full database export to provider-independent format | Quarterly |
| Emergency deployment dry run | Deploy to AWS eu-north-1 from container images | Annual |
| Messaging abstraction validation | Test NordKredit.Infrastructure messaging against RabbitMQ | Annual |
| IaC portability review | Assess Bicep-to-Terraform conversion feasibility | Annual |

---

## 10. Technical Portability Measures

### 10.1 Architecture Decisions Supporting Portability

The NordKredit core banking architecture incorporates portability by design:

| Decision | Portability Benefit |
|----------|-------------------|
| .NET 8 on containers | Runs on any OCI-compliant container runtime |
| Entity Framework Core | Database provider can be swapped (SQL Server, PostgreSQL, etc.) |
| `IMessagePublisher` / `IMessageConsumer` interfaces | Messaging provider abstracted in `NordKredit.Infrastructure.Messaging` |
| OpenTelemetry (planned) | Vendor-neutral observability instrumentation |
| Standard OIDC/OAuth 2.0 | Identity provider can be replaced without application code changes |
| Bicep IaC | Convertible to Terraform or Pulumi for multi-cloud support |

### 10.2 Current Azure-Specific Dependencies

| Dependency | Location in Codebase | Portability Risk | Mitigation |
|------------|---------------------|-----------------|------------|
| Azure Service Bus SDK | `NordKredit.Infrastructure.Messaging` | Medium | Abstracted behind `IMessagePublisher`/`IMessageConsumer` |
| Azure SQL connection strings | Configuration / Key Vault | Low | Standard SQL Server connection string format |
| Azure AD B2C | API authentication middleware | High | Migrate to alternative OIDC provider; update token validation |
| Application Insights SDK | Telemetry initialisation | Low | Replace with OpenTelemetry exporter |
| Bicep templates | `infrastructure/` | Medium | Convert to Terraform modules |
| Azure Blob Storage SDK | File storage operations | Low | Replace with S3-compatible SDK or MinIO |

---

## 11. Governance and Review

### 11.1 Review Schedule

| Activity | Frequency | Responsible |
|----------|-----------|-------------|
| Full exit strategy review | Annual | IT Risk & Compliance |
| Alternative provider reassessment | Annual | CTO Office |
| Data export test execution | Quarterly | Operations / SRE |
| Emergency deployment dry run | Annual | Operations / SRE |
| FSA outsourcing register update | Upon material change | Compliance |
| Board reporting on cloud concentration risk | Annual | CRO |

### 11.2 Trigger Events for Exit Activation

The exit plan must be activated if any of the following occur:

1. **Regulatory order**: FSA or other authority orders cessation of Azure use
2. **Provider failure**: Microsoft/Azure material service degradation exceeding SLA for 30+ consecutive days
3. **Contract termination**: Failure to agree acceptable EA renewal terms
4. **Compliance breach**: Azure found non-compliant with DORA, GDPR, or other applicable regulation
5. **Strategic decision**: Board decision to change cloud strategy
6. **Insolvency event**: Microsoft enters insolvency or restructuring proceedings

### 11.3 Approval and Sign-Off

| Role | Name | Date | Signature |
|------|------|------|-----------|
| Chief Technology Officer | ___________________ | ____/____/________ | ___________________ |
| Chief Risk Officer | ___________________ | ____/____/________ | ___________________ |
| Head of Compliance | ___________________ | ____/____/________ | ___________________ |
| Data Protection Officer | ___________________ | ____/____/________ | ___________________ |

---

## Appendix A — Regulatory Reference

| Regulation | Article/Section | Requirement |
|-----------|----------------|-------------|
| DORA (EU) 2022/2554 | Art. 28 | General principles on management of ICT third-party risk |
| DORA (EU) 2022/2554 | Art. 29 | Preliminary assessment of ICT concentration risk |
| DORA (EU) 2022/2554 | Art. 30(2)(f) | Key contractual provisions: exit strategies, termination, transition |
| DORA (EU) 2022/2554 | Art. 30(2)(e) | Audit and access rights for financial entity and competent authority |
| DORA (EU) 2022/2554 | Art. 31-36 | Oversight framework for critical ICT third-party service providers |
| EBA/GL/2019/02 | §38-43 | Risk assessment for outsourcing arrangements |
| EBA/GL/2019/02 | §53-60 | Contractual requirements for outsourcing |
| EBA/GL/2019/02 | §76-78 | Supervisory notification requirements |
| EBA/GL/2019/02 | §108-110 | Exit strategies for outsourcing arrangements |
| FSA FFFS 2014:5 | Ch. 9 | Outsourcing of operational activities |
| GDPR (EU) 2016/679 | Art. 17 | Right to erasure (relevant to post-exit data deletion) |
| GDPR (EU) 2016/679 | Art. 20 | Right to data portability |
| GDPR (EU) 2016/679 | Art. 28 | Processor obligations (data return and deletion) |

## Appendix B — Azure Service Inventory

A complete inventory of Azure resources consumed by the NordKredit core banking platform is maintained in the Azure Resource Graph and exported quarterly as part of the data portability testing process. The inventory includes:

- Resource group names and regions
- Service types and SKUs
- Data volumes and growth rates
- Cost allocation per service
- Network dependencies and peering configurations

This inventory is the starting point for any exit execution and must be current at all times.
