---
id: "lnd-br-008"
title: "Delinquency management and collections"
domain: "lending"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "LND-BR-008"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Consumer Credit Directive 2008/48/EC Art. 14"
  - "GDPR Art. 5(1)(a)"
  - "Swedish Debt Recovery Act (Inkassolagen 1974:182)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# LND-BR-008: Delinquency management and collections

## Summary

Delinquency management tracks overdue loan payments and initiates the collections process when borrowers fail to meet their repayment obligations. While no dedicated collections COBOL program has been obtained from the mainframe, the regulatory framework and account structure reveal the required business logic. The system must detect missed payments, classify accounts into delinquency stages (30/60/90+ days past due), calculate late fees, send dunning notices, and escalate to external collections or legal action when necessary.

Under FSA FFFS 2014:5 Chapter 6, credit institutions must have robust processes for managing non-performing exposures. The Swedish Debt Recovery Act (Inkassolagen) regulates the collections process, including requirements for written notices, cooling-off periods, and consumer protection. GDPR Art. 5(1)(a) requires that all processing (including collections communications) is lawful, fair, and transparent.

## Business Logic

### Pseudocode

```
PERFORM DELINQUENCY-DETECTION (nightly batch):
    FOR EACH active account with ACCT-CURR-BAL > 0:
        -- Step 1: Check if minimum payment is due
        IF current-date > payment-due-date
           AND minimum-payment-not-received
            SET days-past-due = current-date - payment-due-date

            -- Step 2: Classify delinquency stage
            EVALUATE TRUE
                WHEN days-past-due >= 1 AND < 30
                    SET delinquency-stage = 'EARLY'
                    -- Send payment reminder
                WHEN days-past-due >= 30 AND < 60
                    SET delinquency-stage = 'STAGE-1'
                    -- First formal dunning notice
                    PERFORM CALCULATE-LATE-FEE
                WHEN days-past-due >= 60 AND < 90
                    SET delinquency-stage = 'STAGE-2'
                    -- Second dunning notice with escalation warning
                    PERFORM CALCULATE-LATE-FEE
                WHEN days-past-due >= 90
                    SET delinquency-stage = 'STAGE-3'
                    -- Account flagged for collections/write-off review
                    PERFORM CALCULATE-LATE-FEE
                    IF days-past-due >= 90
                        SET account-classification = 'NON-PERFORMING'
                    END-IF
            END-EVALUATE

            -- Step 3: Apply late fee (if applicable)
            IF late-fee > 0
                ADD late-fee TO ACCT-CURR-BAL
                WRITE late-fee transaction
            END-IF

            -- Step 4: Update delinquency record
            WRITE DELINQUENCY-RECORD
                (acct-id, days-past-due, stage, late-fee, action-taken)
        END-IF
    END-FOR

PERFORM COLLECTIONS-ESCALATION:
    FOR EACH account in STAGE-3 (90+ days):
        IF not already referred to external collections
            -- Inkassolagen requirements:
            -- 1. Send written demand (kravbrev) with 8-day response period
            -- 2. Include total amount owed, breakdown, and payment instructions
            -- 3. Inform of right to dispute
            GENERATE KRAVBREV (demand letter)
            SET escalation-date = current-date
            SET response-deadline = current-date + 8 days
        END-IF
        IF response-deadline has passed AND no payment received
            REFER to external collections agency
            SET account-status = 'COLLECTIONS'
        END-IF
    END-FOR
```

### Decision Table

| Days Past Due | Stage | Action | Late Fee | Regulatory Notice |
|---------------|-------|--------|----------|-------------------|
| 1-29 | Early | Payment reminder (SMS/email) | No | No |
| 30-59 | Stage 1 | First dunning notice (letter) | Yes | Inkassolagen §5 |
| 60-89 | Stage 2 | Second dunning + escalation warning | Yes | Inkassolagen §5 |
| 90+ | Stage 3 | Collections referral / write-off review | Yes | Inkassolagen §8, FSA NPE |
| 365+ | Write-off | Recommend write-off to credit committee | Final | FSA FFFS 2014:5 Ch. 6 |

## Source COBOL Reference

**Program:** Dedicated delinquency management and collections program not yet available in repository.
**Inferred from:** Account structure and regulatory requirements.

The account balance field used for delinquency assessment:

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
```
*(Line 547, CBTRN02C.cbl — ACCT-CURR-BAL represents the outstanding amount that must be repaid; delinquency is assessed when scheduled payments are not received to reduce this balance)*

The account status field used for delinquency flagging:

```cobol
      * ACCT-ACTIVE-STATUS        PIC X(01)    -- 'Y' = active, 'N' = inactive
```
*(Reconstructed from CVACT01Y.cpy — the active status field may need extension to support delinquency states in the migrated system)*

The reject processing pattern shows how the system handles exceptions:

```cobol
000214                       ADD 1 TO WS-REJECT-COUNT
000215                       PERFORM 2500-WRITE-REJECT-REC
```
*(Lines 214-215, CBTRN02C.cbl — the reject pattern for failed validations could be analogous to how delinquent accounts are flagged and tracked)*

## Acceptance Criteria

### Scenario 1: Early delinquency detection (1-29 days)

```gherkin
GIVEN a loan account with a minimum payment due on 2026-01-15
  AND no payment has been received
WHEN the delinquency detection batch runs on 2026-01-25
THEN the account is classified as "EARLY" delinquency (10 days past due)
  AND a payment reminder is generated
  AND no late fee is applied
```

### Scenario 2: Stage 1 delinquency with late fee (30-59 days)

```gherkin
GIVEN a loan account with a minimum payment due on 2026-01-15
  AND no payment has been received
WHEN the delinquency detection batch runs on 2026-02-15
THEN the account is classified as "STAGE-1" delinquency (31 days past due)
  AND a formal dunning notice is generated per Inkassolagen §5
  AND a late fee is calculated and added to the balance
```

### Scenario 3: Non-performing loan classification (90+ days)

```gherkin
GIVEN a loan account with a minimum payment due on 2025-10-15
  AND no payment has been received
WHEN the delinquency detection batch runs on 2026-01-15
THEN the account is classified as "STAGE-3" delinquency (92 days past due)
  AND the account is flagged as "NON-PERFORMING" per FSA guidelines
  AND the account is queued for collections escalation
```

### Scenario 4: Collections demand letter (Kravbrev)

```gherkin
GIVEN an account in Stage 3 delinquency
  AND not previously referred to external collections
WHEN the collections escalation process runs
THEN a written demand letter (kravbrev) is generated containing:
  | Total Amount Owed      | (balance + accrued interest + fees) |
  | Payment Deadline       | 8 days from letter date             |
  | Dispute Instructions   | Contact details for disputes        |
  AND the response deadline is tracked
```

### Scenario 5: Partial payment reduces delinquency

```gherkin
GIVEN a loan account classified as "STAGE-1" (35 days past due)
  AND the minimum payment due is 5000.00 SEK
WHEN a partial payment of 3000.00 SEK is received
THEN the balance is reduced by 3000.00
  AND the delinquency classification is reassessed
  AND if minimum payment threshold is now met, the account returns to current status
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Non-performing exposure management — institutions must identify, measure, and manage non-performing loans | The delinquency staging system classifies accounts by days past due and triggers appropriate management actions at each stage |
| Consumer Credit Directive | Art. 14 | Right of withdrawal — consumer has 14 days to withdraw from credit agreement | Delinquency detection must not classify an account as delinquent within the withdrawal period of a new loan |
| Swedish Inkassolagen | §5, §8 | Debt recovery requirements — written notice with specified content, response period, and consumer rights | The kravbrev generation includes all legally required elements (amount, deadline, dispute rights) |
| GDPR | Art. 5(1)(a) | Lawfulness, fairness, and transparency — processing must be transparent to the data subject | Dunning notices clearly explain the amount owed, basis for the claim, and the borrower's rights |

## Edge Cases

1. **Grace period handling**: Some loan products include a grace period (e.g., 3-5 days after due date) before delinquency is triggered. The system must support product-specific grace periods.

2. **Multiple missed payments**: If a borrower misses multiple consecutive payments, the delinquency should track cumulative missed amount, not just the most recent missed payment. The days-past-due calculation should reference the first missed payment date.

3. **Partial payment ambiguity**: When a partial payment is received, the system must decide whether to apply it to the oldest due amount first (FIFO) or to the most recent. Swedish convention typically applies to the oldest debt first.

4. **Restructured loans**: If a delinquent loan is restructured (new terms agreed), the delinquency counter should be reset. However, FSA requires that restructured loans remain flagged as "forborne" for regulatory reporting.

5. **Bankruptcy notification**: When a borrower files for personal bankruptcy (skuldsanering), collections activity must cease immediately per Swedish law. The system must support a "bankruptcy hold" status.

6. **Joint account delinquency**: For joint liability loans, delinquency notices must be sent to all liable parties. GDPR requires careful handling of personal data when communicating with multiple borrowers.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The delinquency management and collections COBOL programs must be obtained from the mainframe team. Key questions: (1) What is the batch job name for delinquency detection? (2) What are the configured late fee amounts and calculation methods? (3) Is there a minimum payment calculation program? (4) How are grace periods configured per product? (5) What external collections agencies are used and how are referrals transmitted (file transfer, API)? (6) Is there a write-off committee approval workflow in the COBOL system?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
