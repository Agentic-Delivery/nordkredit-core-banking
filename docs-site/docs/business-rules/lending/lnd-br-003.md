---
id: "lnd-br-003"
title: "Loan origination and credit assessment"
domain: "lending"
cobol_source: "Dedicated program not yet in repository (referenced via CVACT01Y.cpy field structure)"
requirement_id: "LND-BR-003"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "EU Consumer Credit Directive 2008/48/EC Art. 8"
  - "AML 2017:11"
  - "GDPR Art. 6(1)(b)"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# LND-BR-003: Loan origination and credit assessment

## Summary

Loan origination is the process of creating a new lending account with an authorized credit limit. While no dedicated loan origination COBOL program has been obtained from the mainframe, the account master record structure (CVACT01Y.cpy) and the credit limit enforcement logic in CBTRN02C.cbl reveal the data requirements for origination. A new loan account must be initialized with an ACCT-CREDIT-LIMIT, ACCT-CASH-CREDIT-LIMIT, and ACCT-GROUP-ID (linking to the applicable interest rate schedule). The ACCT-ACTIVE-STATUS must be set to 'Y' and ACCT-EXPIRAION-DATE set to the loan maturity date.

Under Swedish regulatory requirements (FSA FFFS 2014:5 Chapter 6) and the EU Consumer Credit Directive (Art. 8), creditworthiness assessment is mandatory before granting credit. The origination program must validate the borrower's ability to repay, apply risk-based pricing, and ensure AML/KYC checks are completed before account activation. The specific COBOL programs implementing these checks must be obtained from the mainframe team.

## Business Logic

### Pseudocode

```
PERFORM LOAN-ORIGINATION:
    -- Step 1: Validate customer identity (AML/KYC)
    PERFORM AML-KYC-CHECK using customer-id
    IF AML-KYC-CHECK fails
        REJECT origination with 'AML/KYC VERIFICATION FAILED'
        EXIT
    END-IF

    -- Step 2: Assess creditworthiness (FSA requirement)
    PERFORM CREDITWORTHINESS-ASSESSMENT using customer-id, requested-amount
    IF creditworthiness-score < minimum-threshold
        REJECT origination with 'CREDIT ASSESSMENT FAILED'
        EXIT
    END-IF

    -- Step 3: Determine credit limit and terms
    SET ACCT-CREDIT-LIMIT = approved-credit-amount
    SET ACCT-CASH-CREDIT-LIMIT = cash-advance-limit (typically % of credit limit)
    SET ACCT-GROUP-ID = applicable-interest-rate-group

    -- Step 4: Initialize account record
    GENERATE new ACCT-ID (11-digit sequential)
    SET ACCT-ACTIVE-STATUS = 'Y'
    SET ACCT-CURR-BAL = 0.00
    SET ACCT-CURR-CYC-CREDIT = 0.00
    SET ACCT-CURR-CYC-DEBIT = 0.00
    SET ACCT-EXPIRAION-DATE = loan-maturity-date

    -- Step 5: Write account record
    WRITE ACCOUNT-RECORD to ACCTFILE
    IF write fails
        ABEND with error
    END-IF

    -- Step 6: Create card cross-reference (if applicable)
    WRITE XREF-RECORD linking card to new account
```

### Decision Table

| AML/KYC Passed | Credit Score ≥ Threshold | Requested Amount ≤ Max Limit | Outcome |
|----------------|--------------------------|------------------------------|---------|
| No | N/A | N/A | Origination rejected (AML/KYC) |
| Yes | No | N/A | Origination rejected (creditworthiness) |
| Yes | Yes | No | Origination rejected (exceeds max limit) |
| Yes | Yes | Yes | Account created, credit limit set |

## Source COBOL Reference

**Program:** Dedicated loan origination program not yet available in repository.
**Inferred from:** `CVACT01Y.cpy` (account record structure), `CBTRN02C.cbl` (credit limit field usage)

The account initialization fields are inferred from the account record layout referenced across programs:

```cobol
      *ACCOUNT RECORD LAYOUT
      *COPY CVACT01Y.
```
*(Lines 230-231, COCRDSLC.cbl — copybook reference showing CVACT01Y defines the account master used in origination)*

```cobol
000088       01  FD-ACCTFILE-REC.
000089           05 FD-ACCT-ID                        PIC 9(11).
000090           05 FD-ACCT-DATA                      PIC X(289).
```
*(Lines 88-90, CBTRN02C.cbl — account file record layout showing 300-byte structure: 11-digit key + 289-byte data area)*

The credit limit and balance fields are set during origination and later validated during transaction posting:

```cobol
000407               IF ACCT-CREDIT-LIMIT >= WS-TEMP-BAL
000408                  CONTINUE
000409                ELSE
000410                  MOVE 102 TO WS-VALIDATION-FAIL-REASON
000411                  MOVE 'OVERLIMIT TRANSACTION'
000412                    TO WS-VALIDATION-FAIL-REASON-DESC
000413                END-IF
```
*(Lines 407-413, CBTRN02C.cbl — credit limit enforcement depends on the limit value set during origination)*

## Acceptance Criteria

### Scenario 1: Successful loan origination

```gherkin
GIVEN a customer has passed AML/KYC verification
  AND the creditworthiness assessment approves a credit limit of 50000.00
WHEN a loan origination request is processed
THEN a new account record is created with:
  | Field              | Value      |
  | AccountId          | (11-digit) |
  | ActiveStatus       | Y          |
  | CurrentBalance     | 0.00       |
  | CreditLimit        | 50000.00   |
  | CycleCredit        | 0.00       |
  | CycleDebit         | 0.00       |
  AND the account is available for transactions
```

### Scenario 2: Origination rejected due to AML/KYC failure

```gherkin
GIVEN a customer has NOT passed AML/KYC verification
WHEN a loan origination request is submitted
THEN the origination is rejected
  AND no account record is created
  AND the rejection reason includes 'AML/KYC VERIFICATION FAILED'
  AND the rejection is logged for regulatory audit
```

### Scenario 3: Origination rejected due to insufficient creditworthiness

```gherkin
GIVEN a customer has passed AML/KYC verification
  AND the creditworthiness assessment score is below the minimum threshold
WHEN a loan origination request is submitted
THEN the origination is rejected
  AND no account record is created
  AND the rejection reason includes 'CREDIT ASSESSMENT FAILED'
  AND the customer is informed of the adverse decision per Consumer Credit Directive Art. 9
```

### Scenario 4: Account initialized with zero balances

```gherkin
GIVEN a new loan account is originated
WHEN the account record is written to the ACCTFILE
THEN ACCT-CURR-BAL = 0.00
  AND ACCT-CURR-CYC-CREDIT = 0.00
  AND ACCT-CURR-CYC-DEBIT = 0.00
  AND all financial fields start at zero
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Creditworthiness assessment before granting credit — institutions must assess the borrower's ability to fulfil obligations | The origination process includes a mandatory creditworthiness assessment step before setting the credit limit |
| Consumer Credit Directive | Art. 8 | Obligation to assess the creditworthiness of the consumer before concluding a credit agreement | Credit assessment is a prerequisite for account creation; failed assessments prevent origination |
| Consumer Credit Directive | Art. 9 | Database access and adverse action notification — consumer must be informed of adverse credit decisions | Origination rejections generate a rejection reason that can be communicated to the applicant |
| AML 2017:11 | §3 | Customer due diligence — identity verification before establishing business relationship | AML/KYC check is the first step in origination, blocking account creation for unverified customers |
| GDPR | Art. 6(1)(b) | Processing necessary for performance of a contract — personal data processing is lawful when necessary to enter into a contract | Customer data collected during origination is limited to what is necessary for the credit assessment and account creation |

## Edge Cases

1. **Duplicate account prevention**: The COBOL ACCTFILE uses ACCT-ID as a KSDS primary key. Attempting to write a duplicate key would trigger an INVALID KEY condition. The migrated system must enforce unique account IDs and handle collisions gracefully.

2. **Maximum credit limit**: ACCT-CREDIT-LIMIT is PIC S9(10)V99, allowing up to ±9,999,999,999.99. The migrated system should enforce institution-specific maximum credit limits well below this technical maximum, per risk appetite and regulatory requirements.

3. **Cash advance limit relationship**: The relationship between ACCT-CREDIT-LIMIT and ACCT-CASH-CREDIT-LIMIT during origination is unknown. Typically the cash advance limit is a percentage (e.g., 20-50%) of the total credit limit. The mainframe team must confirm the business rule.

4. **Interest rate group assignment**: The ACCT-GROUP-ID field links the account to a disclosure group for interest rate determination. The origination program must assign the correct group based on the product type and risk tier. The disclosure group master file programs have not yet been obtained.

5. **Concurrent origination**: If two operators attempt to create accounts simultaneously, the sequential ACCT-ID generation could create duplicates. The COBOL system may use a counter file or sequence program. The migrated system should use database sequences or GUID-based IDs.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The loan origination COBOL program must be obtained from the mainframe team. Key questions: (1) What is the program name for loan/account origination? (2) What creditworthiness assessment criteria are used (income verification, existing debt ratio, credit bureau check)? (3) How are credit limits determined — manual underwriting, automated scoring, or both? (4) What is the ACCT-ID generation mechanism — sequential counter, check-digit algorithm, or external sequence? (5) Is there a separate program for credit line increases vs. new origination?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
