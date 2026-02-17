---
id: "lnd-br-007"
title: "Loan disbursement and fund transfer"
domain: "lending"
cobol_source: "Dedicated program not yet in repository (inferred from CBTRN02C.cbl transaction posting pattern)"
requirement_id: "LND-BR-007"
regulations:
  - "PSD2 Art. 83"
  - "PSD2 Art. 89"
  - "FSA FFFS 2014:5 Ch. 3"
  - "AML 2017:11"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# LND-BR-007: Loan disbursement and fund transfer

## Summary

Loan disbursement is the process of transferring approved loan funds to the borrower's account. After successful origination (LND-BR-003) and collateral registration (LND-BR-006 for secured loans), the approved credit amount is made available through either a lump-sum disbursement (term loans) or credit line activation (revolving credit). While no dedicated disbursement COBOL program has been obtained from the mainframe, the transaction posting pattern in CBTRN02C.cbl shows how funds flow through the system.

For term loans, disbursement creates a positive transaction that increases the account balance to the approved amount. For revolving credit (the primary model visible in the COBOL source), activation sets the credit limit and the borrower draws funds through individual transactions that are validated against the limit (LND-BR-002). Under PSD2, fund transfers must be executed within the agreed timeframe, and under AML regulations, disbursements above threshold amounts require enhanced due diligence.

## Business Logic

### Pseudocode

```
PERFORM LOAN-DISBURSEMENT:
    -- Step 1: Validate disbursement prerequisites
    READ ACCOUNT-RECORD using loan-acct-id
    IF ACCT-ACTIVE-STATUS NOT = 'Y'
        REJECT with 'ACCOUNT NOT ACTIVE'
        EXIT
    END-IF
    IF disbursement-amount > ACCT-CREDIT-LIMIT
        REJECT with 'DISBURSEMENT EXCEEDS CREDIT LIMIT'
        EXIT
    END-IF

    -- Step 2: AML check for large disbursements
    IF disbursement-amount >= 150000.00  (SEK threshold)
        PERFORM ENHANCED-DUE-DILIGENCE
        IF EDD-check fails
            REJECT with 'AML/EDD CHECK FAILED'
            EXIT
        END-IF
    END-IF

    -- Step 3: For term loan — full disbursement
    IF loan-type = 'TERM'
        MOVE disbursement-amount TO DALYTRAN-AMT
        SET DALYTRAN-TYPE-CD = 'DB'  (disbursement)
        PERFORM 2000-POST-TRANSACTION
        -- Balance increases by disbursement amount

    -- Step 4: For revolving credit — activate credit line
    ELSE IF loan-type = 'REVOLVING'
        -- No balance change at activation
        -- Borrower draws funds through future transactions
        -- Credit limit already set during origination
        SET disbursement-status = 'CREDIT LINE ACTIVE'
    END-IF

    -- Step 5: Generate disbursement confirmation
    WRITE DISBURSEMENT-CONFIRMATION-RECORD
    -- Trigger fund transfer via Bankgirot/SWIFT if external account
```

### Decision Table

| Loan Type | Account Active | Within Limit | AML Passed | Outcome |
|-----------|---------------|-------------|-----------|---------|
| Term | No | N/A | N/A | Rejected (account inactive) |
| Term | Yes | No | N/A | Rejected (exceeds limit) |
| Term | Yes | Yes | No | Rejected (AML failure) |
| Term | Yes | Yes | Yes | Funds disbursed, balance updated |
| Revolving | No | N/A | N/A | Rejected (account inactive) |
| Revolving | Yes | N/A | N/A | Credit line activated |

## Source COBOL Reference

**Program:** Dedicated disbursement program not yet available in repository.
**Inferred from:** `CBTRN02C.cbl` (transaction posting pattern for balance updates)

The disbursement would follow the same posting pattern as regular transactions:

```cobol
000424 2000-POST-TRANSACTION.
000425           MOVE  DALYTRAN-ID            TO    TRAN-ID
000426           MOVE  DALYTRAN-TYPE-CD       TO    TRAN-TYPE-CD
000427           MOVE  DALYTRAN-CAT-CD        TO    TRAN-CAT-CD
000428           MOVE  DALYTRAN-SOURCE        TO    TRAN-SOURCE
000429           MOVE  DALYTRAN-DESC          TO    TRAN-DESC
000430           MOVE  DALYTRAN-AMT           TO    TRAN-AMT
             ...
000440           PERFORM 2700-UPDATE-TCATBAL
000441           PERFORM 2800-UPDATE-ACCOUNT-REC
000442           PERFORM 2900-WRITE-TRANSACTION-FILE
```
*(Lines 424-442, CBTRN02C.cbl — standard transaction posting: a disbursement would use this same flow with a positive amount and disbursement type code)*

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
```
*(Lines 547-549, CBTRN02C.cbl — a disbursement (positive amount) increases the current balance and accumulates in cycle credit)*

## Acceptance Criteria

### Scenario 1: Term loan full disbursement

```gherkin
GIVEN a newly originated term loan account with:
  | Account ID    | 12345678901 |
  | Active Status | Y           |
  | Credit Limit  | 500000.00   |
  | Balance       | 0.00        |
WHEN a disbursement of 500000.00 is processed
THEN the account balance is updated to 500000.00
  AND the cycle credit is updated to 500000.00
  AND a disbursement transaction is recorded in the transaction file
  AND a disbursement confirmation is generated
```

### Scenario 2: Revolving credit line activation

```gherkin
GIVEN a newly originated revolving credit account with:
  | Account ID    | 98765432100 |
  | Active Status | Y           |
  | Credit Limit  | 100000.00   |
  | Balance       | 0.00        |
WHEN the credit line is activated
THEN the balance remains 0.00 (no immediate disbursement)
  AND the credit limit of 100000.00 is available for drawdown
  AND the account status confirms credit line is active
```

### Scenario 3: Disbursement rejected for inactive account

```gherkin
GIVEN a loan account with Active Status = 'N'
WHEN a disbursement is attempted
THEN the disbursement is rejected with reason "ACCOUNT NOT ACTIVE"
  AND no balance change occurs
```

### Scenario 4: Large disbursement triggers enhanced due diligence

```gherkin
GIVEN a term loan account approved for 200000.00 SEK
WHEN the disbursement is processed
THEN enhanced due diligence is performed (amount >= 150000.00 SEK threshold)
  AND the disbursement proceeds only if EDD check passes
  AND the EDD check result is recorded for AML audit trail
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 83 | Execution time for payment transactions — funds must be available within agreed timeframe | Disbursement processing ensures funds are credited to the borrower's account within the batch processing cycle (same business day) |
| PSD2 | Art. 89 | Value dating — credit value date must be no later than the business day on which the amount is received | The batch posting mechanism credits the disbursement on the processing date |
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — all financial transactions must be accurately recorded | The disbursement is posted as a transaction with full audit trail (transaction ID, type, amount, timestamp) |
| AML 2017:11 | §4 | Enhanced due diligence for transactions above thresholds | Disbursements above the SEK threshold trigger enhanced due diligence checks before fund release |

## Edge Cases

1. **Partial disbursement**: Some term loans may support phased disbursement (e.g., construction loans where funds are released in stages). The system must track cumulative disbursements against the approved amount.

2. **External account disbursement**: If funds are disbursed to an external bank account (not held at NordKredit), the transfer flows through Bankgirot (domestic) or SWIFT (international). These external transfers have different settlement times and fee structures.

3. **Disbursement reversal**: If a disbursement needs to be reversed (e.g., origination fraud discovered), the system must support a reversal transaction. The COBOL system's sequential processing means reversals would be a separate negative transaction.

4. **Concurrent disbursement prevention**: For term loans, only one full disbursement should be allowed. A guard must prevent duplicate disbursements to the same account. The COBOL system may use a disbursement flag in the account record.

5. **Weekend/holiday disbursement**: Batch processing runs daily but banking settlement only occurs on business days. A Friday disbursement may not settle until Monday. The value date must reflect the actual settlement date per PSD2 requirements.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The disbursement COBOL program must be obtained from the mainframe team. Key questions: (1) What is the program name for loan disbursement? (2) Is there a distinction between term loan disbursement and revolving credit activation? (3) What type codes are used for disbursement transactions? (4) How are external transfers (Bankgirot/SWIFT) triggered from the disbursement program? (5) Is there a multi-stage disbursement mechanism for construction or project loans?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
