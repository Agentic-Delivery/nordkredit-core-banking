# ADR-002: Azure Service Bus for Payment Clearing Messaging

**Date:** 2026-02-17

**Status:** Accepted

**Deciders:** Engineering team

**Tags:** architecture, backend, infrastructure, payments

---

## Context

**What is the issue we're trying to solve?**

The mainframe uses IBM MQ for integration with Bankgirot (Swedish domestic payment clearing) and SWIFT (international payments via MT103). As part of the core banking migration, we need a cloud-native messaging replacement that supports reliable, asynchronous message delivery with dead-letter handling.

**What forces are at play?**

- **Reliability:** Payment clearing messages must not be lost — financial data integrity is critical.
- **Regulatory:** PSD2 Art. 97 (SCA), FFFS 2014:5 (accurate records), AML/KYC screening requirements.
- **Format compatibility:** Bankgirot uses ISO 20022 (pain.001), SWIFT uses MT103 format.
- **Retry semantics:** Failed messages need exponential backoff and dead-letter queue handling.
- **Observability:** Full correlation ID tracing for audit trails.
- **Cost:** Azure Service Bus Premium for production (VNET integration, message sessions).

---

## Decision

**What did we decide?**

Use Azure Service Bus with abstracted `IMessagePublisher` / `IMessageConsumer` interfaces in the Domain layer, implemented by `ServiceBusMessagePublisher` / `ServiceBusMessageConsumer` in the Infrastructure layer.

Key design points:
1. Domain layer defines interfaces and message DTOs — no Azure SDK dependency in Domain.
2. Infrastructure layer implements the interfaces using `Azure.Messaging.ServiceBus` SDK.
3. Publisher supports named queues/topics with correlation IDs for traceability.
4. Consumer supports dead-letter queue handling for failed messages.
5. Retry policy: exponential backoff, max 3 retries, configurable via `ServiceBusClientOptions`.
6. Message DTOs: `BankgirotPaymentMessage` (ISO 20022) and `SwiftPaymentMessage` (MT103).

---

## Alternatives Considered

### Alternative 1: Azure Event Hubs
**Description:** Use Event Hubs for high-throughput event streaming.

**Pros:**
- Higher throughput (millions of events/second)
- Built-in partitioning

**Cons:**
- No dead-letter queue support
- No per-message acknowledgment
- Event streaming model, not request-response

**Why rejected:** Payment clearing requires guaranteed delivery with dead-letter handling, not event streaming.

### Alternative 2: Azure Queue Storage
**Description:** Use Azure Queue Storage for simple queuing.

**Pros:**
- Low cost
- Simple API

**Cons:**
- No topics/subscriptions
- No dead-letter queue
- Limited message size (64 KB)
- No sessions or ordering guarantees

**Why rejected:** Lacks enterprise messaging features needed for payment clearing (dead-letter, topics, ordering).

---

## Consequences

### Positive
- Clean separation: Domain defines contracts, Infrastructure implements with Azure SDK.
- Reliable delivery with dead-letter queue handling for failed messages.
- Correlation ID support for regulatory audit trails.
- Exponential backoff retry prevents overwhelming downstream systems.
- Testable: interfaces allow stub implementations in unit tests.

### Negative
- Azure Service Bus Premium required for production (VNET integration).
- Additional infrastructure cost vs. IBM MQ (offset by mainframe savings).
- Azure SDK dependency in Infrastructure layer.

### Risks
- Service Bus outage impacts payment clearing.
  - **Mitigation:** Dead-letter queue, retry policies, Application Insights alerts.
- Message format incompatibility with Bankgirot/SWIFT gateways.
  - **Mitigation:** Strict DTO validation, comparison tests against mainframe output.

---

## Implementation Notes

- NuGet package: `Azure.Messaging.ServiceBus`
- Interfaces in `NordKredit.Domain/Payments/Messaging/`
- Implementations in `NordKredit.Infrastructure/Messaging/`
- DI registration in `NordKredit.Api/Program.cs`
- Unit tests for serialization/deserialization in `NordKredit.UnitTests/Messaging/`

---

## References

- [Azure Service Bus documentation](https://learn.microsoft.com/en-us/azure/service-bus-messaging/)
- [ISO 20022 pain.001 format](https://www.iso20022.org/iso-20022-message-definitions)
- [SWIFT MT103 specification](https://www.swift.com/standards/data-standards/mt-mx)
- PSD2 Art. 97 — Strong Customer Authentication
- FFFS 2014:5 Ch. 8 — Accurate records

---

**Template version:** 1.0
**Last updated:** 2026-02-17
