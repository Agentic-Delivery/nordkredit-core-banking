---
id: "acct-br-012"
title: "Account closing and termination"
domain: "account-management"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "ACCT-BR-012"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "GDPR Art. 17"
  - "GDPR Art. 5(1)(e)"
  - "PSD2 Art. 55"
  - "AML 2017:11 Art. 25"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-012: Account closing and termination

## Summary

Account closing is the process of terminating an active account, settling outstanding balances, disabling transaction processing, and managing the retention of account data per regulatory requirements. The dedicated account closing program is not yet available in the repository. This rule is inferred from the account active status field (`ACCT-ACTIVE-STATUS` in CVACT01Y.cpy, used in ACCT-BR-005), the expiration date enforcement in CBTRN02C (ACCT-BR-006), and Swedish regulatory requirements for account termination.

Swedish banking law and GDPR require that closed accounts have their data retained for a minimum period (typically 5-7 years for financial records under Swedish bookkeeping law, Bokföringslagen 1999:1078) but that personal data is not retained indefinitely. GDPR Art. 17 (right to erasure) must be balanced against regulatory record-keeping obligations.

## Business Logic

### Pseudocode

```
PERFORM CLOSE-ACCOUNT:
    -- Step 1: Validate account exists and is closeable
    READ ACCOUNT-FILE by ACCT-ID
    IF account not found
        DISPLAY 'ACCOUNT NOT FOUND'
        EXIT
    END-IF

    IF ACCT-ACTIVE-STATUS = 'N'
        DISPLAY 'ACCOUNT ALREADY CLOSED'
        EXIT
    END-IF

    -- Step 2: Verify zero balance
    IF ACCT-CURR-BAL NOT = 0
        DISPLAY 'ACCOUNT HAS OUTSTANDING BALANCE'
        EXIT (cannot close with balance)
    END-IF

    -- Step 3: Check for pending transactions
    SEARCH DALYTRAN-FILE for ACCT-ID
    IF pending transactions exist
        DISPLAY 'PENDING TRANSACTIONS EXIST'
        EXIT (must process pending first)
    END-IF

    -- Step 4: Close the account
    MOVE 'N' TO ACCT-ACTIVE-STATUS
    MOVE current-date TO ACCT-CLOSE-DATE  (inferred field)
    REWRITE ACCOUNT-RECORD

    -- Step 5: Invalidate card linkages
    FOR EACH XREF record where XREF-ACCT-ID = closing ACCT-ID
        FLAG card as inactive
    END-FOR

    -- Step 6: Generate final statement
    PERFORM generate-final-statement

    -- Step 7: Start data retention timer
    SET retention-expiry = current-date + retention-period (5-7 years)
    LOG closure event for audit

PERFORM CUSTOMER-INITIATED-CLOSE:
    -- PSD2 Art. 55: Customer has right to close at any time
    -- Must settle balance first, then follow CLOSE-ACCOUNT
    IF outstanding-balance > 0
        PRESENT final settlement options
    END-IF
    PERFORM CLOSE-ACCOUNT
```

### Account Closing State Machine

| Current State | Event | Conditions | Next State | Actions |
|---|---|---|---|---|
| Active (Y) | Customer request | Balance = 0, no pending txn | Closed (N) | Set status, log, generate final statement |
| Active (Y) | Customer request | Balance > 0 | Active (Y) | Reject — settlement required first |
| Active (Y) | Bank-initiated | Fraud, regulatory, or inactivity | Closed (N) | Set status, notify customer, log reason |
| Active (Y) | Expiration | ACCT-EXPIRAION-DATE passed | Expired/Closed (N) | Already enforced by ACCT-BR-006 |
| Closed (N) | Data retention expiry | Retention period elapsed | Purged | Anonymize or delete personal data per GDPR |
| Closed (N) | Reopen request | Within grace period | Active (Y) | Revalidate KYC, reactivate |

### Data Retention Requirements

| Data Category | Retention Period | Regulation | Action After Expiry |
|---|---|---|---|
| Financial records (transactions, balances) | 7 years | Bokföringslagen 1999:1078 Ch. 7 | Archive then purge |
| AML records (KYC, screening results) | 5 years after relationship end | AML 2017:11 Art. 25 | Purge |
| Customer personal data | Until retention expiry | GDPR Art. 5(1)(e) | Anonymize or delete |
| Audit trail | 7 years | FSA FFFS 2014:5 | Archive then purge |

## Source COBOL Reference

**Program:** Dedicated account closing program — not yet in repository.

The account status field used for closure is referenced in existing programs:

**Account active status (from CVACT01Y.cpy, reconstructed):**

```cobol
    ACCT-ACTIVE-STATUS        PIC X(01)      -- 'Y' = active, 'N' = inactive/closed
```
*(CVACT01Y.cpy — the Y/N flag is the only status indicator in the available source. Closing sets this to 'N'.)*

**Expiration enforcement in batch (CBTRN02C.cbl lines 414-418):**

```cobol
000414               IF ACCT-EXPIRAION-DATE >= DALYTRAN-ORIG-TS (1:10)
000415                 CONTINUE
000416               ELSE
000417                 MOVE 103 TO WS-VALIDATION-FAIL-REASON
000418                 MOVE 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
```
*(CBTRN02C.cbl lines 414-418 — expired accounts effectively behave as closed for transaction posting. The closing program would explicitly set ACCT-ACTIVE-STATUS = 'N'.)*

**Account update via REWRITE (COCRDUPC.cbl, CBTRN02C.cbl):**

```cobol
           REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD
              INVALID KEY
                MOVE 109 TO WS-VALIDATION-FAIL-REASON
```
*(CBTRN02C.cbl line 554 — the REWRITE mechanism used by existing programs would also be used by the closing program to update the status field.)*

## Acceptance Criteria

### Scenario 1: Successful account closure with zero balance

```gherkin
GIVEN an active account with:
  | Field | Value |
  | ACCT-ID | 41000000001 |
  | ACCT-ACTIVE-STATUS | Y |
  | ACCT-CURR-BAL | 0.00 |
WHEN a closure request is processed
THEN ACCT-ACTIVE-STATUS is set to 'N'
  AND a final statement is generated
  AND all linked cards are flagged as inactive
  AND the closure is logged for audit
```

### Scenario 2: Closure rejected due to outstanding balance

```gherkin
GIVEN an active account with:
  | Field | Value |
  | ACCT-CURR-BAL | 500.00 |
WHEN a closure request is submitted
THEN the closure is rejected
  AND a message indicates the balance must be settled first
  AND the account remains active
```

### Scenario 3: Customer-initiated closure (PSD2 right)

```gherkin
GIVEN a customer requests account closure under PSD2 Art. 55
  AND the account has a zero balance
WHEN the closure is processed
THEN the account is closed within the regulatory timeframe
  AND no closure fee is charged (PSD2 Art. 55(3))
  AND the customer receives confirmation
```

### Scenario 4: Post-closure transaction rejection

```gherkin
GIVEN a closed account with ACCT-ACTIVE-STATUS = 'N'
WHEN a transaction is submitted against this account
THEN the transaction is rejected during batch validation
  AND the rejection reason indicates the account is closed
```

### Scenario 5: Data retention after closure

```gherkin
GIVEN an account closed on 2026-02-17
WHEN 7 years have elapsed (2033-02-17)
THEN financial records may be purged per Bokföringslagen
  AND personal data is anonymized or deleted per GDPR Art. 17
  AND the purge action is logged
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Financial institutions must maintain records of account lifecycle events including closure | Closure event is logged with date, reason, and final state for regulatory audit |
| GDPR | Art. 17 | Right to erasure — data subjects can request deletion of personal data | After regulatory retention period, personal data associated with closed accounts is purged or anonymized |
| GDPR | Art. 5(1)(e) | Storage limitation — data not kept longer than necessary | Data retention timers ensure closed account data is purged after the mandated retention period |
| PSD2 | Art. 55 | Payment service users have the right to terminate at any time without charge after 12 months | Customer-initiated closure is processed without penalties beyond the initial contract period |
| AML 2017:11 | Art. 25 | AML records must be retained for 5 years after the business relationship ends | KYC and screening records are retained for 5 years post-closure before purge |

## Edge Cases

1. **Credit balance at closure**: If the account has a negative balance (credit/overpayment), the bank must refund the customer before final closure. The COBOL system may require a forced payment transaction to zero the balance before allowing closure.

2. **Recurring payment linkages**: If the account is linked to recurring payments or direct debits, these must be cancelled or redirected before closure. The migrated system should check for active mandates.

3. **Joint account closure**: If the account has multiple holders, all holders may need to consent to closure. Swedish law may require all parties to agree, or allow any party to initiate closure.

4. **Disputed transactions**: If there are active disputes on the account, closure may need to be deferred until dispute resolution. The COBOL system likely has a dispute flag or pending investigation status.

5. **Regulatory hold**: Law enforcement or tax authority holds prevent account closure. The migrated system must check for legal holds before processing closure.

6. **Final statement generation**: The closing process should trigger a final statement covering any activity since the last regular statement through the closure date, using the same logic as ACCT-BR-010.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated account closing program must be obtained from the mainframe team. Key questions: (1) What is the COBOL program name for account closing — CICS transaction or batch? (2) Does the system support customer-initiated closure via online banking, or only branch-initiated? (3) Is there a grace period or cooling-off period after closure request? (4) How are credit balances refunded at closure — automatic payment or manual check? (5) What happens to linked cards at account closure — are they physically destroyed or just logically deactivated? (6) Is there a separate data retention/purge batch job?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
