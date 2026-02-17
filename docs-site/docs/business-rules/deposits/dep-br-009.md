---
id: "dep-br-009"
title: "Account dormancy detection and management"
domain: "deposits"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "DEP-BR-009"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "GDPR Art. 5(1)(e)"
  - "EU Deposit Guarantee Directive 2014/49/EU"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# DEP-BR-009: Account dormancy detection and management

## Summary

Deposit accounts that have had no customer-initiated transactions for an extended period must be identified as dormant (vilande konton) and managed according to regulatory requirements. Swedish banking regulations require institutions to monitor for account inactivity, attempt to contact account holders, restrict certain operations on dormant accounts, and eventually escheat unclaimed funds to the state. The dormancy detection batch runs periodically (monthly or quarterly) to scan all deposit accounts for inactivity and apply the appropriate dormancy state transitions.

The dedicated COBOL program for dormancy management is not yet available in the repository. This rule is inferred from the account active status field (ACCT-ACTIVE-STATUS in CVACT01Y.cpy), Swedish regulatory requirements, and common banking practices for dormant account handling.

## Business Logic

### Pseudocode

```
PERFORM DORMANCY-DETECTION:
    OPEN ACCOUNT-FILE (I-O)
    OPEN TRANSACT-FILE (INPUT, browse)

    PERFORM UNTIL END-OF-ACCOUNT-FILE
        READ next ACCOUNT-RECORD

        IF ACCT-ACTIVE-STATUS = 'Y'
          AND account-type = DEPOSIT
            PERFORM CHECK-ACCOUNT-ACTIVITY
        END-IF
    END-PERFORM

PERFORM CHECK-ACCOUNT-ACTIVITY:
    -- Step 1: Determine last customer-initiated transaction date
    STARTBR TRANSACT-FILE for account (reverse chronological)
    SEARCH for most recent customer-initiated transaction
    SET last-activity-date

    -- Step 2: Compute inactivity period
    COMPUTE months-inactive = current-date - last-activity-date (in months)

    -- Step 3: Apply dormancy state transitions
    EVALUATE TRUE
        WHEN months-inactive >= 120 (10 years)
            -- Escheatment: transfer to state
            PERFORM ESCHEAT-UNCLAIMED-FUNDS
            SET ACCT-ACTIVE-STATUS = 'N'

        WHEN months-inactive >= 24 (2 years)
            -- Full dormancy: restrict all operations
            SET account-dormancy-flag = 'DORMANT'
            FLAG for annual customer contact attempt
            RESTRICT online banking access

        WHEN months-inactive >= 12 (1 year)
            -- Pre-dormancy warning
            SET account-dormancy-flag = 'PRE-DORMANT'
            SEND inactivity notification to customer

        WHEN OTHER
            -- Active: no action needed
            CONTINUE
    END-EVALUATE

    REWRITE ACCOUNT-RECORD
```

### Dormancy State Machine

| State | Inactivity Period | Actions | Next State |
|-------|------------------|---------|------------|
| Active | < 12 months | Normal operations | Active |
| Pre-Dormant | 12-24 months | Customer notification, monitoring | Dormant (if no activity) or Active (if activity) |
| Dormant | 24 months - 10 years | Restricted operations, annual contact attempts, regulatory reporting | Escheatment or Active (if reactivated) |
| Escheated | > 10 years | Funds transferred to state, account closed | Closed |

### Reactivation

```
PERFORM REACTIVATE-DORMANT-ACCOUNT:
    -- Customer-initiated activity reactivates the account
    IF customer-provides-identification
      AND identity-verified
        SET account-dormancy-flag = 'ACTIVE'
        RESTORE normal operations
        LOG reactivation event
    END-IF
```

## Source COBOL Reference

**Program:** Dedicated dormancy management program — not yet in repository.

The account active status field is used in existing programs:

```cobol
    ACCT-ACTIVE-STATUS        PIC X(01)      -- 'Y' = active, 'N' = inactive
```
*(CVACT01Y.cpy — the active status field. The current implementation supports only Y/N values. For dormancy management, additional states or a separate dormancy flag field may be needed.)*

The account expiration check in batch processing:

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
```
*(CBTRN02C.cbl line 414 — while this checks expiration dates, a similar mechanism could enforce transaction restrictions on dormant accounts by checking a dormancy flag before processing.)*

## Acceptance Criteria

### Scenario 1: Account enters pre-dormancy after 12 months inactivity

```gherkin
GIVEN a deposit account with:
  | CurrentBalance       | 25000.00 |
  | LastActivityDate     | 2025-02-01 |
  | DormancyStatus       | ACTIVE |
WHEN the dormancy detection runs on 2026-02-17
  AND 12+ months have elapsed since last activity
THEN the account DormancyStatus is set to PRE-DORMANT
  AND an inactivity notification is sent to the customer's registered address
```

### Scenario 2: Account becomes dormant after 24 months inactivity

```gherkin
GIVEN a deposit account with:
  | CurrentBalance       | 25000.00 |
  | LastActivityDate     | 2024-01-15 |
  | DormancyStatus       | PRE-DORMANT |
WHEN the dormancy detection runs on 2026-02-17
  AND 24+ months have elapsed since last activity
THEN the account DormancyStatus is set to DORMANT
  AND online banking access is restricted
  AND the account is flagged for annual contact attempts
```

### Scenario 3: Dormant account reactivation

```gherkin
GIVEN a dormant deposit account with:
  | CurrentBalance       | 25000.00 |
  | DormancyStatus       | DORMANT |
WHEN the customer visits a branch and provides valid identification
  AND the customer initiates a transaction
THEN the account DormancyStatus is set to ACTIVE
  AND normal operations are restored
  AND the reactivation is logged for audit
```

### Scenario 4: Escheatment after 10 years

```gherkin
GIVEN a dormant deposit account with:
  | CurrentBalance       | 15000.00 |
  | LastActivityDate     | 2016-01-01 |
  | DormancyStatus       | DORMANT |
WHEN the dormancy detection runs on 2026-02-17
  AND 10+ years have elapsed since last activity
  AND all contact attempts have been exhausted
THEN the balance of 15000.00 is transferred to the state (Kammarkollegiet)
  AND the account is closed (ActiveStatus = 'N')
  AND the escheatment is logged for regulatory audit
```

### Scenario 5: Interest-only transactions do not prevent dormancy

```gherkin
GIVEN a deposit account with:
  | LastCustomerActivityDate | 2024-01-01 |
  | LastTransactionDate      | 2026-01-31 (interest posting) |
WHEN the dormancy detection runs
THEN the account is evaluated based on LastCustomerActivityDate, not LastTransactionDate
  AND system-generated transactions (interest, fees) do NOT reset the dormancy clock
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — institutions must identify and manage dormant accounts | The periodic dormancy scan identifies inactive accounts and applies state transitions with full audit logging |
| GDPR | Art. 5(1)(e) | Storage limitation — personal data should not be kept longer than necessary for the purpose | Dormancy management and eventual escheatment ensure that account data is not retained indefinitely for accounts with no ongoing relationship |
| Deposit Guarantee Directive | 2014/49/EU Art. 6 | Eligible deposits — dormant accounts with balances remain covered by the guarantee scheme until escheatment | Dormant accounts are tracked for deposit guarantee reporting, ensuring continued coverage until the account is formally closed |

## Edge Cases

1. **System-generated vs. customer-initiated transactions**: Interest postings, fee charges, and other system-generated transactions should NOT reset the dormancy clock. Only customer-initiated transactions (deposits, withdrawals, transfers) count as activity. The system must distinguish transaction sources.

2. **Joint account dormancy**: If a joint account has multiple owners, activity by any owner should reset the dormancy clock. The system must check activity across all account holders.

3. **Linked accounts**: If a dormant account is linked as a payout destination for a term deposit (DEP-BR-005), the maturity payout should trigger a dormancy review but may not fully reactivate the account without customer identification.

4. **Undeliverable notifications**: If pre-dormancy notifications are returned as undeliverable (invalid address), the bank must attempt alternative contact methods before proceeding to full dormancy.

5. **Escheatment record keeping**: Swedish law requires banks to retain records of escheated accounts for a specified period after escheatment, enabling rightful owners to reclaim funds through the state.

6. **Foreign currency deposits**: If NordKredit holds foreign currency deposit accounts, dormancy rules and escheatment procedures may differ based on the currency jurisdiction.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated dormancy management program must be obtained from the mainframe team. Key questions: (1) What is the current COBOL system's dormancy detection period — 12, 24, or 36 months? (2) Is there a dormancy flag in the account record, or is dormancy inferred from transaction history? (3) How are customer contact attempts tracked — in the account record or a separate log? (4) What is the escheatment timeline — 10 years as per Swedish law, or a different period? (5) Are there product-specific dormancy rules (e.g., term deposits exempt until maturity)? (6) How does the existing system distinguish customer-initiated transactions from system-generated ones?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
