---
id: "lnd-br-006"
title: "Collateral management and valuation"
domain: "lending"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "LND-BR-006"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "FSA FFFS 2014:5 Ch. 8"
  - "EU Capital Requirements Regulation (CRR) Art. 194-217"
  - "GDPR Art. 5(1)(e)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# LND-BR-006: Collateral management and valuation

## Summary

Collateral management encompasses the registration, valuation, monitoring, and release of assets pledged as security for lending facilities. While no dedicated collateral management COBOL program has been obtained from the mainframe, the regulatory requirements under FSA FFFS 2014:5 (Chapter 6 — Credit Risk, Chapter 8 — Assets) and the EU Capital Requirements Regulation mandate that the bank maintain accurate records of all collateral, perform periodic revaluations, and ensure proper loan-to-value (LTV) ratio monitoring.

NordKredit AB's lending portfolio includes secured lending products (mortgages, vehicle financing, business loans) where collateral is a prerequisite for credit approval. The collateral management system must track collateral type, current valuation, valuation date, LTV ratio, and linkage to the secured loan account. This data feeds into the bank's capital adequacy calculations (CRR risk-weighted assets) and is subject to regulatory reporting requirements.

## Business Logic

### Pseudocode

```
PERFORM COLLATERAL-REGISTRATION:
    -- Step 1: Validate collateral data
    VALIDATE collateral-type (REAL_ESTATE, VEHICLE, SECURITIES, GUARANTEE, OTHER)
    VALIDATE collateral-value > 0
    VALIDATE valuation-date is not future
    VALIDATE loan-account-id exists in ACCTFILE

    -- Step 2: Calculate loan-to-value ratio
    COMPUTE LTV-ratio = ACCT-CURR-BAL / collateral-value × 100

    -- Step 3: Check LTV against product limits
    IF collateral-type = 'REAL_ESTATE'
        IF LTV-ratio > 85   -- Swedish mortgage cap (Finansinspektionen)
            REJECT with 'LTV EXCEEDS REGULATORY LIMIT'
        END-IF
    ELSE IF collateral-type = 'VEHICLE'
        IF LTV-ratio > 80
            FLAG for manual review
        END-IF
    END-IF

    -- Step 4: Register collateral record
    WRITE COLLATERAL-RECORD
        (collateral-id, loan-acct-id, type, value, valuation-date, LTV)

PERFORM PERIODIC-REVALUATION (batch):
    FOR EACH active collateral record:
        IF months-since-last-valuation > revaluation-frequency
            -- Real estate: revalue annually (FSA requirement)
            -- Securities: revalue daily (mark-to-market)
            -- Vehicles: revalue annually (depreciation model)
            PERFORM REVALUATION using collateral-type, asset-reference
            UPDATE collateral-value and valuation-date
            RECOMPUTE LTV-ratio
            IF LTV-ratio > warning-threshold
                GENERATE LTV-BREACH-ALERT
            END-IF
        END-IF
    END-FOR
```

### Decision Table

| Collateral Type | Revaluation Frequency | LTV Limit | Source of Valuation |
|----------------|----------------------|-----------|-------------------|
| Real Estate | Annual (FSA FFFS 2014:5) | 85% (Finansinspektionen mortgage cap) | Independent valuation or statistical model |
| Vehicle | Annual | 80% (institution policy) | Depreciation model or market comparison |
| Securities | Daily (mark-to-market) | 70% (haircut for volatility) | Market data feed |
| Guarantee | At issuance + annual review | N/A (binary: valid/expired) | Guarantor creditworthiness assessment |

## Source COBOL Reference

**Program:** Dedicated collateral management program not yet available in repository.
**Inferred from:** Regulatory requirements and account structure.

The account master record does not contain embedded collateral fields. Collateral data is expected to reside in a separate VSAM file with the loan account ID as a foreign key:

```cobol
      * Inferred COLLATERAL record structure:
      * COLLATERAL-ID             PIC X(12)    -- Unique collateral identifier
      * COLLATERAL-ACCT-ID        PIC 9(11)    -- FK to ACCTFILE
      * COLLATERAL-TYPE           PIC X(02)    -- RE=Real Estate, VH=Vehicle, SC=Securities, GR=Guarantee
      * COLLATERAL-VALUE          PIC S9(12)V99 -- Current assessed value (SEK)
      * COLLATERAL-VALUATION-DATE PIC X(10)    -- Date of last valuation (YYYY-MM-DD)
      * COLLATERAL-LTV-RATIO      PIC 9(03)V99 -- Loan-to-value percentage
      * COLLATERAL-DESCRIPTION    PIC X(80)    -- Free-text asset description
      * COLLATERAL-STATUS         PIC X(01)    -- A=Active, R=Released, E=Expired
```

The loan account balance used for LTV calculation is the same field used in credit limit enforcement:

```cobol
000547           ADD DALYTRAN-AMT  TO ACCT-CURR-BAL
```
*(Line 547, CBTRN02C.cbl — ACCT-CURR-BAL is the outstanding balance against which LTV is calculated)*

## Acceptance Criteria

### Scenario 1: Register collateral for a new secured loan

```gherkin
GIVEN a loan account "12345678901" with a current balance of 2000000.00 SEK
  AND a property valuation of 3000000.00 SEK
WHEN collateral of type "Real Estate" is registered
THEN a collateral record is created with:
  | Linked Account  | 12345678901 |
  | Type            | REAL_ESTATE |
  | Value           | 3000000.00  |
  | LTV Ratio       | 66.67%      |
  AND the LTV is within the 85% regulatory limit
```

### Scenario 2: Reject registration when LTV exceeds regulatory limit

```gherkin
GIVEN a loan account with current balance of 900000.00 SEK
  AND a property valuation of 1000000.00 SEK
WHEN collateral registration is attempted
THEN the LTV ratio is calculated as 90.00%
  AND the registration is rejected with "LTV EXCEEDS REGULATORY LIMIT"
  AND the rejection is logged for regulatory audit
```

### Scenario 3: Periodic revaluation triggers LTV breach alert

```gherkin
GIVEN an active collateral record with:
  | Collateral Value | 3000000.00 |
  | Loan Balance     | 2400000.00 |
  | LTV Ratio        | 80.00%     |
WHEN the annual revaluation determines the property value is now 2700000.00
THEN the collateral value is updated to 2700000.00
  AND the LTV ratio is recalculated as 88.89%
  AND an LTV breach alert is generated (exceeds 85% threshold)
  AND the loan officer is notified for remediation action
```

### Scenario 4: Collateral release upon loan payoff

```gherkin
GIVEN a loan account with current balance of 0.00
  AND an active collateral record linked to the account
WHEN a collateral release request is processed
THEN the collateral status is changed to "Released"
  AND the release date is recorded
  AND the collateral is no longer counted in regulatory capital calculations
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Credit risk management — collateral must be properly valued and monitored | Periodic revaluation ensures collateral values are current; LTV monitoring detects deterioration |
| FSA FFFS 2014:5 | Ch. 8 | Assets — institutions must maintain accurate records of all assets, including collateral | Collateral records maintain type, value, valuation date, and linkage to the secured loan account |
| CRR | Art. 194-217 | Credit risk mitigation — conditions for using collateral to reduce risk-weighted assets | Collateral registration and valuation provides the data needed for CRR risk-weight calculations |
| GDPR | Art. 5(1)(e) | Storage limitation — data kept no longer than necessary | Collateral records for released loans should be retained only for the regulatory retention period (10 years for Swedish financial records) then purged |

## Edge Cases

1. **Multiple collateral per loan**: A single loan may be secured by multiple collateral items (e.g., property + guarantee). The LTV calculation should use the aggregate collateral value against the loan balance. The collateral data model must support one-to-many relationship.

2. **Cross-collateralization**: A single collateral asset may secure multiple loans. The available collateral value for LTV calculation must account for all loans against the same asset. The migrated system needs a many-to-many relationship model.

3. **Currency mismatch**: If collateral is denominated in a currency different from the loan (e.g., EUR property securing a SEK loan), the LTV calculation must use the current exchange rate. This introduces FX risk in the LTV monitoring.

4. **Valuation disputes**: Independent valuations may differ from the bank's internal model. The system should support multiple valuation records per collateral with a designated "official" valuation for LTV purposes.

5. **Collateral depreciation**: Vehicle and equipment collateral depreciates over time. The revaluation mechanism should apply appropriate depreciation models between formal revaluations.

6. **Finansinspektionen mortgage cap**: Swedish regulations impose an 85% LTV cap for residential mortgages. Existing loans that exceed this threshold due to property value decline are subject to amortization requirements, not forced liquidation. The system must distinguish between new origination limits and existing portfolio monitoring.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: Collateral management COBOL programs must be obtained from the mainframe team. Key questions: (1) What is the VSAM file structure for collateral records? (2) Is there a collateral valuation batch job — what is its frequency and methodology? (3) How is cross-collateralization tracked? (4) What is the LTV monitoring frequency for different collateral types? (5) Are there automated escalation procedures when LTV thresholds are breached? (6) How is the Finansinspektionen mortgage cap enforced in the origination process?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
