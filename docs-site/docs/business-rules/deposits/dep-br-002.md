---
id: "dep-br-002"
title: "Deposit account opening and KYC verification"
domain: "deposits"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "DEP-BR-002"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "AML 2017:11"
  - "GDPR Art. 5(1)(a)"
  - "GDPR Art. 6(1)(b)"
  - "EU Deposit Guarantee Directive 2014/49/EU"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# DEP-BR-002: Deposit account opening and KYC verification

## Summary

Deposit account opening is the entry point for the deposits domain, requiring KYC (Know Your Customer) verification before an account can be created. Under Swedish AML regulations (FFFS 2017:11), the bank must verify the customer's identity, assess the purpose of the account, and determine the risk level before opening any deposit account. The process involves validating the customer's personnummer (Swedish national ID), verifying identity documents, performing sanctions screening, and creating the account record with an initial deposit.

The dedicated COBOL program for deposit account opening is not yet available in the repository. This rule is inferred from regulatory requirements and the account data structure observed in CVACT01Y.cpy. The mainframe team must provide the deposit account opening program(s) for complete extraction.

## Business Logic

### Pseudocode

```
PERFORM OPEN-DEPOSIT-ACCOUNT:
    -- Step 1: Validate customer identity
    READ CUSTOMER-FILE using CUST-PERSONNUMMER
    IF customer not found
        PERFORM CREATE-NEW-CUSTOMER
    END-IF

    -- Step 2: KYC verification
    PERFORM VERIFY-IDENTITY-DOCUMENTS
    PERFORM SANCTIONS-SCREENING
    PERFORM RISK-ASSESSMENT
    IF KYC-FAILED
        SET account-status = 'REJECTED'
        LOG rejection reason
        EXIT
    END-IF

    -- Step 3: Assign account parameters
    GENERATE ACCT-ID (11-digit unique identifier)
    SET ACCT-ACTIVE-STATUS = 'Y'
    SET ACCT-CURR-BAL = initial-deposit-amount
    SET ACCT-CREDIT-LIMIT = 0 (deposit account, no credit)
    SET ACCT-CASH-CREDIT-LIMIT = 0
    SET ACCT-CURR-CYC-CREDIT = initial-deposit-amount
    SET ACCT-CURR-CYC-DEBIT = 0
    ASSIGN ACCT-GROUP-ID based on product-type
    SET ACCT-EXPIRAION-DATE based on product-type:
        - Demand deposit: null or far-future
        - Term deposit: maturity date

    -- Step 4: Create account record
    WRITE ACCOUNT-FILE
    WRITE XREF-FILE (card/account cross-reference)

    -- Step 5: Deposit guarantee registration
    PERFORM REGISTER-DEPOSIT-GUARANTEE
    COMPUTE total-covered = aggregate customer deposits
    IF total-covered > 1050000.00 (SEK guarantee limit)
        FLAG excess-deposit notification
    END-IF
```

### Decision Table

| Condition | Identity Valid | KYC Passed | Sanctions Clear | Risk Acceptable | Product Available | Outcome |
|-----------|--------------|------------|----------------|----------------|-------------------|---------|
| All valid | Yes | Yes | Yes | Yes | Yes | Account opened |
| Invalid ID | No | N/A | N/A | N/A | N/A | Rejected — ID verification failed |
| KYC failed | Yes | No | N/A | N/A | N/A | Rejected — KYC requirements not met |
| Sanctioned | Yes | Yes | No | N/A | N/A | Rejected — sanctions match |
| High risk | Yes | Yes | Yes | No | N/A | Rejected or escalated to compliance |
| Product unavailable | Yes | Yes | Yes | Yes | No | Rejected — product not offered |

## Source COBOL Reference

**Program:** Dedicated deposit account opening program — not yet in repository.

The account record structure is inferred from CVACT01Y.cpy references in existing programs:

```cobol
000089       COPY CVACT01Y.
```
*(Line 89, COTRN02C.cbl — CVACT01Y copybook inclusion showing the account master structure is shared across programs. The deposit account opening program would use the same copybook to create new account records.)*

```cobol
000054           MOVE 8 TO APPL-RESULT.
000055           OPEN I-O  ACCOUNT-FILE
```
*(Lines 309-311, CBTRN02C.cbl — ACCOUNT-FILE opened in I-O mode, demonstrating the account master is an indexed VSAM file supporting both read and write operations. The account opening program would WRITE new records to this file.)*

## Acceptance Criteria

### Scenario 1: Successful deposit account opening

```gherkin
GIVEN a customer with valid personnummer "199001011234"
  AND identity documents are verified
  AND sanctions screening is clear
  AND risk assessment is acceptable
WHEN a demand deposit account is opened with initial deposit of 10000.00 SEK
THEN a new account record is created with:
  | AccountId       | (11-digit generated)  |
  | ActiveStatus    | Y                     |
  | CurrentBalance  | 10000.00              |
  | CreditLimit     | 0.00                  |
  | CycleCredit     | 10000.00              |
  | CycleDebit      | 0.00                  |
  | GroupId         | (assigned per product) |
  AND the account is registered with the deposit guarantee scheme
```

### Scenario 2: Account opening rejected due to KYC failure

```gherkin
GIVEN a customer whose identity cannot be verified
WHEN a deposit account opening is attempted
THEN the account is NOT created
  AND the rejection reason is logged for audit
  AND the customer is notified of the rejection
```

### Scenario 3: Account opening triggers deposit guarantee notification

```gherkin
GIVEN a customer with existing deposits totaling 1,000,000.00 SEK
WHEN a new deposit account is opened with 100,000.00 SEK
THEN the total deposits become 1,100,000.00 SEK
  AND the system flags that deposits exceed the guarantee limit of 1,050,000.00 SEK
  AND the customer is notified of the partial coverage
```

### Scenario 4: Term deposit account opening with maturity date

```gherkin
GIVEN a customer opens a 12-month term deposit
  AND the opening date is 2026-03-01
WHEN the term deposit account is created
THEN ExpirationDate is set to 2027-03-01
  AND the disclosure group is assigned for the term deposit product rate
  AND withdrawal restrictions are applied until maturity
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records — institutions must create and maintain accurate records for all new accounts | The account opening process creates a complete account record with all required fields in the account master file |
| AML 2017:11 | Ch. 3 | Customer due diligence — identity verification, purpose of account, risk assessment before establishing business relationship | KYC verification steps (identity, sanctions, risk) are mandatory prerequisites before account creation |
| GDPR | Art. 5(1)(a) | Lawfulness, fairness, transparency — personal data must be processed lawfully with clear purpose | Customer data collected during account opening is limited to what is required by AML regulations, with clear legal basis |
| GDPR | Art. 6(1)(b) | Contract performance — processing necessary for performance of a contract | Account creation requires processing personal data (personnummer, identity) to establish the deposit contract |
| Deposit Guarantee Directive | 2014/49/EU Art. 5 | Eligibility of deposits — credit institutions must identify eligible depositors at point of account opening | Deposit guarantee registration occurs during account opening to ensure coverage tracking from day one |

## Edge Cases

1. **Duplicate account prevention**: The system must prevent duplicate accounts for the same customer and product type. The 11-digit ACCT-ID generation must guarantee uniqueness across the VSAM file.

2. **Minor accounts**: Swedish law allows deposit accounts for minors (under 18), but with restrictions on withdrawals. The account opening process must flag minor accounts and enforce guardian requirements.

3. **Non-resident accounts**: Non-Swedish residents may open deposit accounts but with enhanced KYC requirements under AML regulations. The system must support different KYC levels based on residency status.

4. **Joint accounts**: The COBOL account structure appears to be single-owner. If joint deposit accounts exist, the customer-to-account relationship must support multiple owners per account for deposit guarantee calculations.

5. **Initial deposit requirements**: Different deposit products may have minimum initial deposit amounts. The system must validate the initial deposit against the product's minimum requirement.

6. **PEP screening**: Politically Exposed Persons require enhanced due diligence under AML regulations. The account opening process must include PEP screening as part of the risk assessment.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated deposit account opening program must be obtained from the mainframe team. Key questions: (1) What is the account ID generation algorithm — sequential, random, or checksum-based? (2) How are deposit products differentiated (product code field, separate files, or disclosure group alone)? (3) What KYC data is stored in the account or customer record? (4) Is there a separate CICS online program for account opening, or is it a batch process? (5) How are joint accounts represented — multiple customer-account cross-references? (6) What is the minimum initial deposit for each product type?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
