# ADR-005: Bicep IaC Templates for Azure Deployment

**Date:** 2026-02-17

**Status:** Accepted

**Deciders:** Platform team

**Tags:** infrastructure, compliance, security

---

## Context

**What is the issue we're trying to solve?**

NordKredit's target architecture runs on Azure (App Service, Azure SQL, Azure Functions, Service Bus, Key Vault, Application Insights) but no infrastructure-as-code exists. Manual provisioning is error-prone and non-auditable, which conflicts with DORA requirements for documented ICT infrastructure and GDPR data residency enforcement.

**What forces are at play?**

- **Regulatory:** GDPR requires data residency in EU (Sweden Central). DORA requires documented ICT third-party arrangements.
- **Repeatability:** dev/staging/production environments must be consistent.
- **Security:** Secrets must be stored in Key Vault, not in application configuration.
- **Auditability:** Infrastructure changes must be version-controlled and reviewable.
- **Existing patterns:** The project already uses Azure-native services (Service Bus, App Insights, Azure AD).

---

## Decision

**What did we decide?**

Use Azure Bicep with a modular structure:

1. **One module per resource type** in `infra/modules/` — App Service, Azure SQL, Azure Functions, Service Bus, Key Vault, Application Insights.
2. **One orchestration file** `infra/main.bicep` that composes all modules.
3. **Per-environment parameter files** `infra/parameters/` for dev, staging, and production.
4. **All resources deployed to Sweden Central** (swedencentral) for GDPR compliance.
5. **Key Vault** stores all connection strings and secrets; applications reference Key Vault via managed identity.
6. **Service Bus** defines queues for Bankgirot and SWIFT integrations.

---

## Alternatives Considered

### Alternative 1: Terraform
**Description:** Use HashiCorp Terraform with azurerm provider.

**Pros:**
- Multi-cloud support
- Large community and ecosystem

**Cons:**
- Requires separate state management (remote backend)
- Additional tool dependency (Terraform CLI)
- Not Azure-native — slight lag in new resource support

**Why rejected:** Project is Azure-only; Bicep is first-class Azure IaC with native validation and no state management overhead.

### Alternative 2: ARM Templates (raw JSON)
**Description:** Use raw ARM JSON templates directly.

**Pros:**
- No additional tooling needed
- Direct Azure Resource Manager format

**Cons:**
- Verbose and hard to read/maintain
- No native modularity or type safety
- Poor developer experience

**Why rejected:** Bicep compiles to ARM but provides superior authoring experience, modularity, and type safety.

---

## Consequences

### Positive
- Infrastructure is version-controlled and auditable (DORA compliance)
- Consistent environments across dev/staging/production
- GDPR data residency enforced at the template level (swedencentral)
- Secrets managed securely via Key Vault
- Validates with `az bicep build` before deployment

### Negative
- Requires Azure CLI with Bicep extension for local development
- Azure AD B2C tenant cannot be provisioned via Bicep (manual step)

### Risks
- Azure AD B2C tenant provisioning is a manual step
  - **Mitigation:** Document as manual prerequisite; reference tenant IDs via parameters
- SKU/pricing changes between environments need careful parameter management
  - **Mitigation:** Environment-specific parameter files with appropriate tier selections

---

## Implementation Notes

- Module structure: `infra/modules/{resource}.bicep`
- Parameter files: `infra/parameters/{env}.bicepparam`
- All modules accept `location` parameter defaulting to `swedencentral`
- Key Vault access policies use managed identities from App Service and Functions
- Service Bus queues: `bankgirot-payments`, `swift-payments` with dead-letter enabled

---

## References

- [Issue #142](../../..) — Create Bicep IaC templates
- [ADR-002](ADR-002-azure-service-bus-messaging.md) — Service Bus messaging design
- [CLAUDE.md](../../CLAUDE.md) — Target architecture specification

---

**Template version:** 1.0
**Last updated:** 2026-02-17
