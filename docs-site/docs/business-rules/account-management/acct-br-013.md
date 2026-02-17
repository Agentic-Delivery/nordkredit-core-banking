---
id: "acct-br-013"
title: "Account dormancy detection and management"
domain: "account-management"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "ACCT-BR-013"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "GDPR Art. 5(1)(e)"
  - "AML 2017:11 Art. 25"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-013: Account dormancy detection and management

## Summary

Accounts that have had no customer-initiated activity for an extended period must be identified as dormant (vilande konton) and managed according to Swedish regulatory requirements. Dormancy management involves periodic scanning of account activity, applying state transitions based on inactivity duration, restricting certain operations, attempting customer contact, and eventually escheating unclaimed funds to the state (Kammarkollegiet). The dedicated dormancy detection batch program is not yet available in the repository.

This rule is inferred from the account active status field (`ACCT-ACTIVE-STATUS` in CVACT01Y.cpy), the expiration enforcement mechanism in CBTRN02C (ACCT-BR-006), Swedish banking regulations, and the dormancy patterns documented in the deposits domain (DEP-BR-009). The account management domain requires its own dormancy rules because credit card and general ledger accounts may have different inactivity thresholds and treatment from deposit accounts.

## Business Logic

### Pseudocode

```
PERFORM DORMANCY-DETECTION-BATCH:
    OPEN ACCOUNT-FILE (I-O)
    OPEN TRANSACT-FILE (INPUT, browse)

    PERFORM UNTIL END-OF-ACCOUNT-FILE
        READ next ACCOUNT-RECORD

        IF ACCT-ACTIVE-STATUS = 'Y'
            PERFORM CHECK-ACCOUNT-ACTIVITY
        END-IF
    END-PERFORM

PERFORM CHECK-ACCOUNT-ACTIVITY:
    -- Step 1: Find last customer-initiated transaction
    SEARCH TRANSACT-FILE for account
    IDENTIFY most recent customer-initiated transaction date
    SET last-activity-date

    -- Step 2: Compute inactivity period
    COMPUTE months-inactive = current-date - last-activity-date (in months)

    -- Step 3: Apply dormancy state transitions
    EVALUATE TRUE
        WHEN months-inactive >= 120 (10 years)
            -- Escheatment: transfer unclaimed funds to state
            IF ACCT-CURR-BAL > 0
                PERFORM ESCHEAT-FUNDS-TO-STATE
            END-IF
            MOVE 'N' TO ACCT-ACTIVE-STATUS
            LOG 'ACCOUNT ESCHEATED'

        WHEN months-inactive >= 24 (2 years)
            -- Full dormancy: restrict operations
            SET dormancy-flag = 'DORMANT'
            FLAG for annual customer contact attempt
            RESTRICT online and card transactions

        WHEN months-inactive >= 12 (1 year)
            -- Pre-dormancy: send notification
            SET dormancy-flag = 'PRE-DORMANT'
            SEND inactivity notification to customer

        WHEN OTHER
            -- Active: no action required
            CONTINUE
    END-EVALUATE

    REWRITE ACCOUNT-RECORD

PERFORM REACTIVATE-DORMANT-ACCOUNT:
    -- Triggered by customer-initiated activity on dormant account
    IF customer-provides-identification AND identity-verified
        SET dormancy-flag = 'ACTIVE'
        RESTORE normal operations
        LOG 'ACCOUNT REACTIVATED'
    END-IF
```

### Dormancy State Machine

| State | Inactivity Period | Restrictions | Customer Actions | System Actions |
|---|---|---|---|---|
| Active | &lt; 12 months | None | All operations available | Normal processing |
| Pre-Dormant | 12-24 months | None (monitoring only) | All operations available | Send inactivity notification |
| Dormant | 24 months - 10 years | Card transactions blocked, online access restricted | In-branch only with ID verification | Annual contact attempts, regulatory reporting |
| Escheated | &gt; 10 years | All operations blocked | Claim through Kammarkollegiet | Funds transferred to state, account closed |

### Activity Classification

| Transaction Type | Counts as Activity? | Reason |
|---|---|---|
| Customer purchase | Yes | Customer-initiated |
| Customer payment | Yes | Customer-initiated |
| Customer transfer | Yes | Customer-initiated |
| Interest posting | No | System-generated |
| Fee charge | No | System-generated |
| Refund/chargeback | No | Bank-initiated |
| Automatic payment | Depends | Customer-configured but auto-executed — may or may not reset dormancy clock depending on policy |

## Source COBOL Reference

**Program:** Dedicated dormancy management program — not yet in repository.

The account status and expiration mechanisms from existing programs inform dormancy handling:

**Account active status (from CVACT01Y.cpy, reconstructed):**

```cobol
    ACCT-ACTIVE-STATUS        PIC X(01)      -- 'Y' = active, 'N' = inactive
```
*(CVACT01Y.cpy — the current Y/N model does not distinguish between dormant and closed. The migrated system will need an extended status model or a separate dormancy flag.)*

**Expiration date enforcement (CBTRN02C.cbl lines 414-418):**

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                 CONTINUE
000416               ELSE
000417                 MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                 MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
```
*(CBTRN02C.cbl lines 414-418 — a similar mechanism could enforce transaction restrictions on dormant accounts by checking a dormancy flag or last-activity date before processing.)*

**Account file random access (CBTRN01C.cbl lines 52-56):**

```cobol
000052            SELECT ACCOUNT-FILE ASSIGN TO   ACCTFILE
000053                   ORGANIZATION IS INDEXED
000054                   ACCESS MODE  IS RANDOM
000055                   RECORD KEY   IS FD-ACCT-ID
000056                   FILE STATUS  IS ACCTFILE-STATUS.
```
*(CBTRN01C.cbl lines 52-56 — the dormancy batch would use sequential access mode to scan all accounts.)*

## Acceptance Criteria

### Scenario 1: Account enters pre-dormancy after 12 months inactivity

```gherkin
GIVEN an active account with:
  | ACCT-ID | 41000000001 |
  | LastCustomerActivityDate | 2025-01-15 |
  | DormancyStatus | ACTIVE |
WHEN the dormancy detection batch runs on 2026-02-17
  AND 13 months have elapsed since last customer-initiated activity
THEN the account DormancyStatus is set to PRE-DORMANT
  AND an inactivity notification is sent to the customer's registered address
```

### Scenario 2: Account becomes dormant after 24 months inactivity

```gherkin
GIVEN an account with:
  | ACCT-ID | 41000000001 |
  | LastCustomerActivityDate | 2024-01-01 |
  | DormancyStatus | PRE-DORMANT |
WHEN the dormancy detection batch runs on 2026-02-17
  AND 25 months have elapsed since last customer-initiated activity
THEN the account DormancyStatus is set to DORMANT
  AND card transactions are blocked
  AND online banking access is restricted
  AND the account is flagged for annual contact attempts
```

### Scenario 3: Customer activity reactivates dormant account

```gherkin
GIVEN a dormant account with:
  | ACCT-ID | 41000000001 |
  | DormancyStatus | DORMANT |
WHEN the customer visits a branch and provides valid identification
  AND the customer initiates a transaction
THEN the account DormancyStatus is set to ACTIVE
  AND card transactions are re-enabled
  AND online banking access is restored
  AND the reactivation is logged for audit
```

### Scenario 4: Escheatment after 10 years of inactivity

```gherkin
GIVEN a dormant account with:
  | ACCT-CURR-BAL | 2500.00 |
  | LastCustomerActivityDate | 2016-01-01 |
  | DormancyStatus | DORMANT |
WHEN the dormancy detection batch runs on 2026-02-17
  AND 10+ years have elapsed since last activity
  AND all customer contact attempts have been exhausted
THEN the balance of 2500.00 is transferred to the state (Kammarkollegiet)
  AND ACCT-ACTIVE-STATUS is set to 'N'
  AND the escheatment is logged for regulatory audit
```

### Scenario 5: System-generated transactions do not prevent dormancy

```gherkin
GIVEN an account with:
  | LastCustomerActivityDate | 2024-01-01 |
  | LastTransactionDate | 2026-01-31 (interest posting) |
WHEN the dormancy detection batch runs
THEN the account is evaluated based on LastCustomerActivityDate
  AND system-generated transactions (interest, fees) do NOT reset the dormancy clock
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Financial institutions must identify and manage dormant accounts | Periodic batch scanning identifies inactive accounts and applies graduated state transitions with full audit logging |
| GDPR | Art. 5(1)(e) | Storage limitation — personal data should not be retained longer than necessary | Dormancy management and eventual escheatment ensure account data is not retained indefinitely for inactive relationships |
| AML 2017:11 | Art. 25 | Record retention after business relationship ends | Dormant accounts are monitored and records retained per AML requirements; escheatment triggers the end-of-relationship retention clock |

## Edge Cases

1. **System-generated vs. customer-initiated transactions**: The dormancy detection must distinguish transaction sources. Interest postings, fees, and bank-initiated adjustments should NOT reset the dormancy clock. Only customer-initiated transactions (purchases, payments, transfers) count as activity. The transaction record must carry a source indicator.

2. **Dormancy flag storage**: The current CVACT01Y.cpy record has only `ACCT-ACTIVE-STATUS` (Y/N). A dormancy state requires either a new field in the account record or a separate dormancy tracking table. The 300-byte record has FILLER space that may already contain a dormancy indicator.

3. **Multiple cards per account**: If an account has multiple linked cards (via XREF), activity on ANY card should reset the dormancy clock for the shared account. The detection batch must scan all XREF entries for the account.

4. **Negative balance dormancy**: An account with a negative balance (credit to the customer) should still be subject to dormancy rules. The escheatment process for credit balances requires refund to the customer before state transfer.

5. **Regulatory reporting**: Dormant accounts must be reported to FSA as part of periodic regulatory returns. The migrated system should flag dormant accounts for inclusion in regulatory submissions.

6. **Customer notification delivery**: If pre-dormancy or dormancy notifications are returned as undeliverable, the bank must attempt alternative contact methods (email, phone, registered mail) before proceeding to the next dormancy stage.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated dormancy management program must be obtained from the mainframe team. Key questions: (1) What is the current dormancy detection period — 12, 24, or 36 months? (2) Is there a dormancy flag in the account record FILLER area, or is dormancy inferred from transaction history? (3) How does the existing system distinguish customer-initiated from system-generated transactions? (4) What is the escheatment timeline — 10 years as per Swedish law? (5) Are there product-specific dormancy rules for different account types? (6) How are customer contact attempts tracked and recorded?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
