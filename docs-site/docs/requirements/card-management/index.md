---
title: Card Management
sidebar_position: 1
---

# Card Management Use Cases

Use cases for the card management domain, documenting operator workflows for card listing, viewing, and updating.

## Source System

The card management subsystem consists of three CICS online programs operating in a hub-and-spoke pattern centered on the card list screen:

| Program | Function | Use Cases |
|---------|----------|-----------|
| `COCRDLIC.cbl` | Card list with pagination, filtering, and selection | UC-CARD-01 |
| `COCRDSLC.cbl` | Card detail view/select | (Planned) |
| `COCRDUPC.cbl` | Card update | (Planned) |

## Use Cases

| Use Case ID | Title | User Stories | Status |
|-------------|-------|-------------|--------|
| [UC-CARD-01](./uc-card-01) | List Cards | US-CARD-01.1 through US-CARD-01.9 | Documented |
