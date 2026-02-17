---
id: "acct-br-011"
title: "Account opening and initial setup"
domain: "account-management"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "ACCT-BR-011"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 4 §3"
  - "AML 2017:11 Art. 11"
  - "GDPR Art. 6"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# ACCT-BR-011: Account opening and initial setup

## Summary

Account opening is the process of creating a new account record in the ACCTFILE, establishing the customer-account-card relationships in the XREF file, and initializing all balance and status fields. The dedicated account opening program is not yet available in the repository. This rule is inferred from the account master record structure (CVACT01Y.cpy, reconstructed in ACCT-BR-001), the XREF linkage structure (CVACT03Y.cpy), the validation rules observed in existing programs (ACCT-BR-002 through ACCT-BR-007), and Swedish regulatory requirements for new account onboarding.

Swedish banking regulations (FSA FFFS 2014:5) and AML requirements (2017:11) mandate that account opening includes customer identity verification (KYC), source of funds documentation, and regulatory risk assessment before the account can be activated. The COBOL system likely implements these as a CICS online transaction or a combination of online entry and batch processing.

## Business Logic

### Pseudocode

```
PERFORM OPEN-NEW-ACCOUNT:
    -- Step 1: Validate customer identity (KYC/AML)
    READ CUSTOMER-FILE by CUST-ID
    IF customer not found OR customer not verified
        REJECT account opening
        EXIT
    END-IF

    -- Step 2: Generate new account ID
    GENERATE next ACCT-ID (11-digit numeric, unique)

    -- Step 3: Initialize account record
    INITIALIZE ACCOUNT-RECORD
    MOVE new-ACCT-ID         TO ACCT-ID
    MOVE 'Y'                 TO ACCT-ACTIVE-STATUS
    MOVE current-date        TO ACCT-OPEN-DATE
    MOVE calculated-expiry   TO ACCT-EXPIRAION-DATE
    MOVE requested-limit     TO ACCT-CREDIT-LIMIT
    MOVE 0                   TO ACCT-CURR-BAL
    MOVE 0                   TO ACCT-CURR-CYC-CREDIT
    MOVE 0                   TO ACCT-CURR-CYC-DEBIT
    MOVE 0                   TO ACCT-CASH-CREDIT-LIMIT

    -- Step 4: Write account record
    WRITE ACCOUNT-RECORD TO ACCTFILE
    IF write fails (duplicate key or I/O error)
        DISPLAY error
        ROLLBACK
        EXIT
    END-IF

    -- Step 5: Create XREF linkage (card-account-customer)
    INITIALIZE CARD-XREF-RECORD
    MOVE card-number    TO XREF-CARD-NUM
    MOVE CUST-ID        TO XREF-CUST-ID
    MOVE new-ACCT-ID    TO XREF-ACCT-ID
    WRITE CARD-XREF-RECORD TO XREF-FILE

    -- Step 6: Log account opening for audit
    WRITE audit record with opening details
```

### Account Initialization Fields

| Field | Initial Value | Source | Notes |
|---|---|---|---|
| ACCT-ID | Generated (11 digits) | System-assigned | Unique key for ACCTFILE KSDS |
| ACCT-ACTIVE-STATUS | 'Y' | Default | New accounts start as active |
| ACCT-OPEN-DATE | Current date | System clock | Inferred field — likely in FILLER area of CVACT01Y.cpy |
| ACCT-EXPIRAION-DATE | Open date + product term | Business rule | E.g., 3-5 years for credit card accounts |
| ACCT-CREDIT-LIMIT | Per credit assessment | Underwriting | Subject to credit risk evaluation |
| ACCT-CURR-BAL | 0.00 | Default | No balance at opening |
| ACCT-CURR-CYC-CREDIT | 0.00 | Default | No cycle activity at opening |
| ACCT-CURR-CYC-DEBIT | 0.00 | Default | No cycle activity at opening |

### KYC/AML Requirements for Account Opening

| Check | Regulation | Requirement |
|---|---|---|
| Customer identification | AML 2017:11 Art. 11 | Verify identity via government-issued ID (Swedish personnummer or equivalent) |
| Beneficial ownership | AML 2017:11 Art. 13 | Identify and verify beneficial owners for corporate accounts |
| Source of funds | AML 2017:11 Art. 14 | Document source of funds for accounts above threshold |
| PEP screening | AML 2017:11 Art. 16 | Screen for politically exposed persons |
| Risk classification | AML 2017:11 Art. 12 | Assign risk category (low/medium/high) |

## Source COBOL Reference

**Program:** Dedicated account opening program — not yet in repository.

The account record structure and validation rules are inferred from existing programs:

**Account record initialization (from CVACT01Y.cpy, reconstructed):**

```cobol
    01 ACCOUNT-RECORD.
       05 ACCT-ID                    PIC 9(11).
       05 ACCT-ACTIVE-STATUS         PIC X(01).
       05 ACCT-CURR-BAL              PIC S9(10)V99.
       05 ACCT-CREDIT-LIMIT          PIC S9(10)V99.
       05 ACCT-CASH-CREDIT-LIMIT     PIC S9(10)V99.
       05 ACCT-OPEN-DATE             PIC X(10).    (inferred)
       05 ACCT-EXPIRAION-DATE        PIC X(10).
       05 ACCT-CURR-CYC-CREDIT       PIC S9(10)V99.
       05 ACCT-CURR-CYC-DEBIT        PIC S9(10)V99.
```
*(CVACT01Y.cpy — reconstructed from field references across CBTRN02C, COCRDUPC, COCRDSLC. The 300-byte record likely contains additional fields in the FILLER area.)*

**XREF record for linking (from CVACT03Y.cpy lines 4-8):**

```cobol
    01 CARD-XREF-RECORD.
       05  XREF-CARD-NUM             PIC X(16).
       05  XREF-CUST-ID              PIC 9(09).
       05  XREF-ACCT-ID              PIC 9(11).
       05  FILLER                     PIC X(14).
```
*(CVACT03Y.cpy — the XREF record establishes the card-customer-account linkage that must be created at account opening.)*

**Account file definition (from CBTRN01C.cbl lines 52-56):**

```cobol
000052            SELECT ACCOUNT-FILE ASSIGN TO   ACCTFILE
000053                   ORGANIZATION IS INDEXED
000054                   ACCESS MODE  IS RANDOM
000055                   RECORD KEY   IS FD-ACCT-ID
000056                   FILE STATUS  IS ACCTFILE-STATUS.
```
*(CBTRN01C.cbl lines 52-56 — ACCTFILE is an indexed VSAM KSDS with 11-digit ACCT-ID as primary key.)*

## Acceptance Criteria

### Scenario 1: Successful account opening with KYC verification

```gherkin
GIVEN a verified customer with CUST-ID 123456789
  AND valid identification documents have been submitted
  AND AML screening has been completed with no adverse findings
WHEN a new account opening is requested
THEN a new account record is created with:
  | Field | Value |
  | ACCT-ID | (system-generated 11-digit ID) |
  | ACCT-ACTIVE-STATUS | Y |
  | ACCT-CURR-BAL | 0.00 |
  | ACCT-CURR-CYC-CREDIT | 0.00 |
  | ACCT-CURR-CYC-DEBIT | 0.00 |
  AND a XREF record links the card to the customer and account
  AND an audit trail records the opening event
```

### Scenario 2: Account opening rejected for unverified customer

```gherkin
GIVEN a customer who has not completed KYC verification
WHEN an account opening is requested
THEN the account is NOT created
  AND an error is returned indicating KYC verification required
  AND the rejection is logged for compliance audit
```

### Scenario 3: Account opening rejected for PEP without enhanced due diligence

```gherkin
GIVEN a customer identified as a Politically Exposed Person (PEP)
  AND enhanced due diligence has NOT been completed
WHEN an account opening is requested
THEN the account is NOT created
  AND the request is flagged for compliance review
```

### Scenario 4: Duplicate account ID prevention

```gherkin
GIVEN an account with ACCT-ID 41000000001 already exists in ACCTFILE
WHEN an attempt is made to create another account with the same ID
THEN the WRITE fails with INVALID KEY (duplicate)
  AND no partial record is persisted
```

### Scenario 5: XREF linkage creation

```gherkin
GIVEN a new account 41000000099 for customer 123456789 with card 4000000000000099
WHEN the account is successfully created
THEN a XREF record is written:
  | Field | Value |
  | XREF-CARD-NUM | 4000000000000099 |
  | XREF-CUST-ID | 123456789 |
  | XREF-ACCT-ID | 41000000099 |
  AND the card can be resolved to the account via XREF lookup
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 3 | Financial institutions must maintain complete records of account creation | Account opening creates a full record with all mandatory fields initialized and an audit trail |
| FSA FFFS 2014:5 | Ch. 4 §3 | Operational risk — account creation must follow established procedures | Structured initialization ensures all fields are set to known values, preventing undefined state |
| AML 2017:11 | Art. 11 | Customer identity verification required before establishing business relationship | KYC checks must complete before account record is created in the system |
| GDPR | Art. 6 | Lawful basis for processing personal data | Account opening establishes the contractual basis for processing the customer's financial data |
| PSD2 | Art. 97 | Strong customer authentication for account access | The account-card-customer linkage via XREF enables authentication path from card to customer identity |

## Edge Cases

1. **Account ID generation**: The 11-digit account ID must be unique within the ACCTFILE. The generation mechanism (sequential, random, check-digit) is not visible in available source. The migrated system should use a sequence generator with gap-free allocation for audit purposes.

2. **Partial creation failure**: If the account record is written successfully but the XREF write fails, the system has an orphaned account with no card linkage. The COBOL system likely handles this via program ABEND (rolling back the CICS unit of work). The migrated system must use database transactions to ensure atomicity.

3. **Credit limit assignment**: The credit limit is set at account opening based on credit assessment. The available source shows credit limit enforcement (ACCT-BR-007) but not assignment. The migrated system needs a credit decision engine or integration with underwriting.

4. **Expiration date calculation**: Account expiration dates are enforced by ACCT-BR-006 and CBTRN02C, but the initial calculation method is not visible. Typical credit card accounts expire in 3-5 years from opening.

5. **Multiple cards per account**: The XREF structure allows multiple cards to link to the same account (many-to-one). Account opening may create one initial card, with additional cards added later. The migrated system should support this relationship model.

6. **Corporate vs. personal accounts**: Swedish AML regulations have different KYC requirements for corporate accounts (beneficial ownership) vs. personal accounts (individual ID). The account opening process must distinguish account types.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated account opening program must be obtained from the mainframe team. Key questions: (1) What is the COBOL program name for account opening — is it a CICS online transaction or batch process? (2) How are account IDs generated — sequential, algorithmic, or with a check digit? (3) What KYC/AML checks are performed within the COBOL program vs. external systems? (4) Is the credit limit assigned during account opening or in a separate underwriting step? (5) What is the standard expiration period for new accounts? (6) Are there different account opening flows for different product types (credit card, deposit, loan)?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
