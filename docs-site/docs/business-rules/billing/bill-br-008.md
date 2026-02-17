---
id: "bill-br-008"
title: "Fee calculation and schedule management"
domain: "billing"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "BILL-BR-008"
regulations:
  - "FSA FFFS 2014:5 Ch. 6"
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 45"
  - "EU Consumer Credit Directive 2008/48/EC Art. 19"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# BILL-BR-008: Fee calculation and schedule management

## Summary

Fee calculation is a core billing function that determines and applies various fees to customer accounts based on account activity, product type, and fee schedules. The COBOL system maintains fee schedules that define the fee type, amount or calculation method, and conditions under which each fee is assessed. Typical billing fees include annual fees, transaction fees, cash advance fees, foreign transaction fees, and service charges.

The dedicated COBOL program for fee assessment is not yet available in the repository. This rule is inferred from the account data structure (CVACT01Y.cpy), the transaction posting logic in CBTRN02C.cbl (which posts fee transactions the same way as regular transactions), the disclosure group mechanism (CVTRA02Y.cpy), and Swedish/EU regulatory requirements for fee transparency.

Under PSD2 Art. 45, payment service providers must inform users of all charges before a transaction is executed. The EU Consumer Credit Directive requires that all fees be included in the APR calculation. FSA FFFS 2014:5 Ch. 6-7 requires transparent fee disclosure and accurate financial record-keeping.

## Business Logic

### Pseudocode

```
PERFORM FEE-ASSESSMENT (nightly/monthly batch):
    OPEN ACCOUNT-FILE (I-O)
    OPEN FEE-SCHEDULE-FILE (INPUT)
    OPEN TRANSACT-FILE (OUTPUT)

    PERFORM UNTIL end-of-accounts
        READ next ACCOUNT-RECORD from ACCOUNT-FILE

        -- Step 1: Skip inactive accounts
        IF ACCT-ACTIVE-STATUS NOT = 'Y'
            CONTINUE
        END-IF

        -- Step 2: Look up fee schedule for account product type
        READ FEE-SCHEDULE-FILE using ACCT-GROUP-ID
        SET fee-schedule = applicable fees for this group

        -- Step 3: Assess annual fee (if anniversary date)
        IF today = account-anniversary-date
            SET annual-fee = fee-schedule.ANNUAL-FEE-AMT
            IF annual-fee > 0
                ADD annual-fee TO ACCT-CURR-BAL
                ADD annual-fee TO ACCT-CURR-CYC-DEBIT
                WRITE fee-transaction (type='AF', amount=annual-fee)
            END-IF
        END-IF

        -- Step 4: Assess cash advance fees (per transaction)
        FOR EACH unprocessed cash-advance transaction:
            COMPUTE cash-advance-fee =
                MAX(fee-schedule.CASH-ADV-MIN-FEE,
                    transaction-amount * fee-schedule.CASH-ADV-PCT / 100)
            ADD cash-advance-fee TO ACCT-CURR-BAL
            WRITE fee-transaction (type='CA', amount=cash-advance-fee)
        END-FOR

        -- Step 5: Assess foreign transaction fees
        FOR EACH unprocessed foreign transaction:
            COMPUTE foreign-fee =
                transaction-amount * fee-schedule.FOREIGN-TXN-PCT / 100
            ROUND foreign-fee to 2 decimal places
            ADD foreign-fee TO ACCT-CURR-BAL
            WRITE fee-transaction (type='FT', amount=foreign-fee)
        END-FOR

        -- Step 6: Assess overlimit fee (if applicable)
        IF ACCT-CURR-BAL > ACCT-CREDIT-LIMIT
           AND overlimit-fee-not-already-assessed-this-cycle
            SET overlimit-fee = fee-schedule.OVERLIMIT-FEE-AMT
            ADD overlimit-fee TO ACCT-CURR-BAL
            WRITE fee-transaction (type='OL', amount=overlimit-fee)
        END-IF

        REWRITE ACCOUNT-RECORD
    END-PERFORM
```

### Decision Table

| Fee Type | Trigger | Calculation Method | Frequency | Outcome |
|----------|---------|-------------------|-----------|---------|
| Annual Fee | Account anniversary date | Fixed amount from schedule | Annually | Fee posted to account |
| Cash Advance Fee | Cash advance transaction | Greater of minimum fee or percentage of amount | Per transaction | Fee posted with transaction |
| Foreign Transaction Fee | Non-domestic transaction | Percentage of transaction amount | Per transaction | Fee posted with transaction |
| Overlimit Fee | Balance exceeds credit limit | Fixed amount from schedule | Once per billing cycle | Fee posted to account |
| Late Payment Fee | See BILL-BR-011 | See BILL-BR-011 | Per missed payment | See BILL-BR-011 |
| Returned Payment Fee | Payment returned/bounced | Fixed amount from schedule | Per occurrence | Fee posted to account |

### Fee Schedule Data Structure (Inferred)

| Field | Type | Length | Description |
|-------|------|--------|-------------|
| Fee Schedule ID | X(10) | 10 | Links to ACCT-GROUP-ID (disclosure group) |
| Annual Fee Amount | S9(05)V99 | 7 | Annual fee in SEK |
| Cash Advance Min Fee | S9(05)V99 | 7 | Minimum cash advance fee |
| Cash Advance Percentage | S9(02)V99 | 4 | Cash advance fee as % of amount |
| Foreign Transaction Pct | S9(02)V99 | 4 | Foreign transaction fee percentage |
| Overlimit Fee Amount | S9(05)V99 | 7 | Fee for exceeding credit limit |
| Late Payment Fee Amount | S9(05)V99 | 7 | Fee for missed minimum payment |
| Returned Payment Fee | S9(05)V99 | 7 | Fee for returned/bounced payment |

## Source COBOL Reference

**Program:** Dedicated fee assessment program not yet available in repository.
**Inferred from:** `CVACT01Y.cpy` (ACCT-GROUP-ID links to fee schedule), `CBTRN02C.cbl` (transaction posting pattern), `CVTRA02Y.cpy` (disclosure group structure)

The fee posting would follow the same transaction posting pattern as CBTRN02C:

```cobol
000424       2000-POST-TRANSACTION.
000425           MOVE  DALYTRAN-ID            TO    TRAN-ID
             ...
000440           PERFORM 2700-UPDATE-TCATBAL
000441           PERFORM 2800-UPDATE-ACCOUNT-REC
000442           PERFORM 2900-WRITE-TRANSACTION-FILE
```
*(Lines 424-442, CBTRN02C.cbl — fee transactions would be posted using the same mechanism: update category balance, update account balance, write transaction record)*

The account balance update for fees follows the debit pattern:

```cobol
000548           IF DALYTRAN-AMT >= 0
000549              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
000550           ELSE
000551              ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
000552           END-IF
```
*(Lines 548-552, CBTRN02C.cbl — fees are positive amounts that increase the balance owed, tracked in the cycle credit accumulator)*

## Acceptance Criteria

### Scenario 1: Annual fee assessment on anniversary date

```gherkin
GIVEN an active account with:
  | AccountGroupId    | PREMIUM01 |
  | AnniversaryDate   | 2026-03-15 |
  AND the fee schedule for PREMIUM01 has annual fee = 595.00 SEK
WHEN the fee assessment batch runs on 2026-03-15
THEN an annual fee of 595.00 is posted to the account
  AND the account balance increases by 595.00
  AND a fee transaction record is written with type 'AF'
```

### Scenario 2: Cash advance fee (percentage exceeds minimum)

```gherkin
GIVEN a cash advance transaction of 10000.00 SEK
  AND the fee schedule specifies:
  | CashAdvanceMinFee    | 75.00  |
  | CashAdvancePercent   | 3.00   |
WHEN the cash advance fee is calculated
THEN the percentage-based fee = 10000.00 * 3.00% = 300.00
  AND the minimum fee = 75.00
  AND the applied fee = MAX(300.00, 75.00) = 300.00
  AND 300.00 is posted to the account balance
```

### Scenario 3: Cash advance fee (minimum exceeds percentage)

```gherkin
GIVEN a cash advance transaction of 1000.00 SEK
  AND the fee schedule specifies:
  | CashAdvanceMinFee    | 75.00  |
  | CashAdvancePercent   | 3.00   |
WHEN the cash advance fee is calculated
THEN the percentage-based fee = 1000.00 * 3.00% = 30.00
  AND the minimum fee = 75.00
  AND the applied fee = MAX(30.00, 75.00) = 75.00
  AND 75.00 is posted to the account balance
```

### Scenario 4: Foreign transaction fee

```gherkin
GIVEN a foreign transaction of 5000.00 SEK equivalent
  AND the fee schedule specifies foreign transaction percentage = 1.50%
WHEN the foreign transaction fee is calculated
THEN the fee = 5000.00 * 1.50% = 75.00
  AND 75.00 is posted to the account balance
  AND a fee transaction record is written with type 'FT'
```

### Scenario 5: No fees on inactive account

```gherkin
GIVEN an account with ActiveStatus = 'N'
WHEN the fee assessment batch runs
THEN no fees are assessed or posted to the account
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 6 | Risk management — fee income must be accurately calculated and recorded per product terms | The fee schedule mechanism ensures consistent, auditable fee application per account group and product type |
| FSA FFFS 2014:5 | Ch. 7 | Financial systems — accurate bookkeeping of all fee transactions | Each fee generates a separate transaction record with type code, amount, and timestamp for audit trail |
| PSD2 | Art. 45 | Information on charges — payment service users must be informed of all applicable charges | The fee schedule provides a transparent, per-product definition of all fees. Fee amounts are disclosed at account opening and available on request |
| Consumer Credit Directive | Art. 19 | APR calculation must include all mandatory fees | All fees defined in the schedule (annual, transaction, overlimit) are inputs to the APR calculation |

## Edge Cases

1. **Fee waiver for premium accounts**: Some account products may waive certain fees (e.g., annual fee waiver for first year, foreign transaction fee waiver for premium cards). The fee schedule must support zero-amount or waived-flag entries. The migrated system should track both the notional fee and the waiver reason for regulatory reporting.

2. **Fee cap regulations**: Swedish consumer protection may impose maximum fee amounts or require fees to be proportionate to the actual cost. The migrated system should validate fee amounts against regulatory caps before posting.

3. **Multiple fees per transaction**: A single transaction could trigger multiple fees (e.g., a foreign cash advance triggers both cash advance fee and foreign transaction fee). The fee assessment must process all applicable fee types independently.

4. **Fee refund**: If a fee is disputed or a billing error is identified, the system must support fee reversal. The COBOL system likely handles this as a negative fee transaction or a separate credit adjustment.

5. **Currency conversion for foreign fees**: Foreign transaction fees are calculated on the SEK equivalent amount after currency conversion. The conversion rate and fee calculation order (convert then fee, or fee then convert) must be confirmed by the mainframe team.

6. **Prorated annual fees**: If an account is opened mid-year, the first annual fee may be prorated. The COBOL system's handling of prorated fees is unknown.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL: The dedicated fee assessment program must be obtained from the mainframe team. Key questions: (1) What is the batch job name for fee assessment? (2) Is there a separate fee schedule file, or are fees embedded in the disclosure group? (3) What are the current fee amounts per product type? (4) Are any fees capped by regulation or internal policy? (5) How are fee waivers and reversals processed? (6) Is the overlimit fee assessed once per cycle or each time the limit is exceeded?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
