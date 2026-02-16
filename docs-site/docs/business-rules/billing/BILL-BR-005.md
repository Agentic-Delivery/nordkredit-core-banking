---
id: "BILL-BR-005"
title: "Fee computation scheduled in interest calculation batch but not yet implemented"
domain: "billing"
cobol_source: "CBACT04C.cbl:518-520"
requirement_id: "BILL-BR-005"
regulations:
  - "FFFS 2014:5 Ch. 4 §2 — Fee Disclosure Requirements"
  - "PSD2 Art. 45 — Information on Charges"
  - "PSD2 Art. 62 — Charges for the Use of a Payment Instrument"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-005: Fee computation scheduled in interest calculation batch but not yet implemented

## Summary

The interest calculation batch program (CBACT04C) contains a placeholder paragraph `1400-COMPUTE-FEES` that is invoked during the monthly billing cycle but contains no implementation — only an `EXIT` statement. The JCL job `INTCALC.jcl` is titled "Process transaction balance file and compute interest **and fees**", confirming that fee computation was planned as part of the same batch pipeline as interest calculation (BILL-BR-002). This rule documents the fee computation gap and the architectural slot reserved for it in the existing COBOL system.

## Business Logic

### Pseudocode (Current — Stub)

```
1400-COMPUTE-FEES:
    EXIT.
```

### Intended Architecture (Inferred from Program Structure)

```
FOR EACH transaction-category-balance-record:
    PERFORM 1200-GET-INTEREST-RATE
    IF interest-rate <> 0 THEN
        PERFORM 1300-COMPUTE-INTEREST
    END-IF
    PERFORM 1400-COMPUTE-FEES        <-- Invoked here, but no-op
END-FOR

PERFORM 1050-UPDATE-ACCOUNT:
    ADD total-interest TO account-balance
    (fees would also be added here if implemented)
    RESET cycle counters
    REWRITE account record
```

### Fee Types Expected (Inferred from Industry and Regulation)

Based on the system's credit card billing model, Swedish FSA regulations (FFFS 2014:5), and PSD2, the following fee categories would typically be computed during a monthly billing cycle:

| Fee Type | Trigger | Regulatory Basis |
|----------|---------|-----------------|
| Annual card fee | Card anniversary date within billing cycle | FFFS 2014:5 Ch. 4 §2 |
| Late payment fee | Payment not received by due date | FFFS 2014:5 Ch. 4 §2 |
| Cash advance fee | Category balance for cash advance transactions > 0 | PSD2 Art. 62(3) |
| Foreign transaction fee | Transactions with non-SEK currency | PSD2 Art. 62(2) |
| Over-limit fee | Account balance exceeds credit limit | FFFS 2014:5 Ch. 4 §2 |
| Returned payment fee | Payment reversal or bounce | National banking practice |

### Decision Table

| Fee Computation Invoked | Implementation Present | Outcome |
|------------------------|----------------------|---------|
| Yes (called in main loop) | No (EXIT only) | No fees calculated, no impact on balance |
| — | If implemented | Fees would be computed per category, accumulated, and added to account balance alongside interest |

## Source COBOL Reference

**Program:** `CBACT04C.cbl`
**Lines:** 518-520 (Fee computation stub)

```cobol
       1400-COMPUTE-FEES.

           EXIT.
```

**Lines:** 300-320 (Main processing loop showing where fees are invoked)

```cobol
       1000-PROCESS-RECORDS.

           READ TCATBAL-FILE INTO TRAN-CAT-BAL-RECORD
                AT END SET WS-TCATBAL-EOF TO TRUE
           END-READ.

           IF NOT WS-TCATBAL-EOF
               ...
               PERFORM 1200-GET-INTEREST-RATE
               IF DIS-INT-RATE NOT = ZERO
                   PERFORM 1300-COMPUTE-INTEREST
               END-IF
               PERFORM 1400-COMPUTE-FEES
           END-IF.
```

**JCL:** `INTCALC.jcl`
**Comment line 21:** `Process transaction balance file and compute interest and fees.`

**Data structures that would be used by fee computation:**
- `CVTRA01Y.cpy` — Transaction Category Balance (category-level balance available for fee triggers)
- `CVACT01Y.cpy` — Account Record (credit limit, account status, due dates)
- `CVTRA02Y.cpy` — Disclosure Group (could be extended with fee rate fields)

## Acceptance Criteria

### Scenario 1: Fee computation stub has no effect

```gherkin
GIVEN the interest calculation batch runs for account "00000012345"
  AND the account has category balances with non-zero interest
WHEN the 1400-COMPUTE-FEES paragraph is executed
THEN no fee amount is computed
  AND no fee transaction record is created
  AND the account balance is only affected by interest charges
```

### Scenario 2: Fee computation position in processing pipeline

```gherkin
GIVEN the batch processes transaction category balance records sequentially
WHEN each category record is processed
THEN 1200-GET-INTEREST-RATE executes first (rate lookup)
  AND 1300-COMPUTE-INTEREST executes second (if rate > 0)
  AND 1400-COMPUTE-FEES executes third (currently no-op)
  AND this order is preserved for every category record
```

### Scenario 3: Annual card fee (migration requirement)

```gherkin
GIVEN account "00000012345" has card anniversary date within the current billing cycle
  AND the annual fee for this card type is 500.00 SEK
WHEN the fee computation runs in the migrated system
THEN a fee transaction record is created with:
  | TRAN-TYPE-CD | '03' (fee) |
  | TRAN-SOURCE | 'System' |
  | TRAN-DESC | 'Annual card fee for a/c 00000012345' |
  | TRAN-AMT | 500.00 |
  AND the fee amount is added to the account balance
```

### Scenario 4: Late payment fee (migration requirement)

```gherkin
GIVEN account "00000012345" has minimum payment due of 1,000.00 SEK
  AND the payment due date has passed
  AND no payment of at least 1,000.00 SEK has been received
WHEN the fee computation runs in the migrated system
THEN a late payment fee transaction is created
  AND the fee amount follows the schedule defined in the fee table
  AND the fee is disclosed per FFFS 2014:5 Ch. 4 §2
```

### Scenario 5: Cash advance fee (migration requirement)

```gherkin
GIVEN account "00000012345" has cash advance category balance of 2,000.00 SEK
  AND the cash advance fee rate is 3% (minimum 75.00 SEK)
WHEN the fee computation runs in the migrated system
THEN a cash advance fee = MAX(2000.00 * 0.03, 75.00) = 75.00 SEK is charged
  AND the fee transaction is recorded with description indicating cash advance fee
```

### Scenario 6: Fee disclosure on statement

```gherkin
GIVEN fees have been computed for account "00000012345"
WHEN the monthly statement is generated (BILL-BR-003)
THEN each fee appears as a separate transaction line
  AND the fee type and amount are clearly identified
  AND the total fees are included in the statement total
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §2 | All fees must be disclosed to the customer before agreement and on each statement | The fee computation creates individual transaction records that appear on statements. The migration must ensure fee disclosure compliance. |
| PSD2 | Art. 45(1) | Payment service provider must inform user of all charges | Fee transactions are recorded individually, enabling itemized disclosure. |
| PSD2 | Art. 62(2-3) | Charges for payment instruments must be reasonable and disclosed | Cash advance and foreign transaction fees must be within regulatory limits and pre-disclosed. |

## Edge Cases

1. **No-op in current system**: The fee computation is a placeholder. Any fees currently charged to NordKredit customers must be applied through a separate mechanism not captured in the CardDemo COBOL source. The domain expert must confirm how fees are currently charged.

2. **Fee accumulation**: If fees are implemented, they would logically follow the same pattern as interest — accumulated in a working storage variable and added to `ACCT-CURR-BAL` in `1050-UPDATE-ACCOUNT`. A new `WS-TOTAL-FEES` variable would be needed.

3. **Fee transaction type code**: Interest uses `TRAN-TYPE-CD = '01'` and `TRAN-CAT-CD = '05'`. A new type/category code pair would be needed for fees (e.g., `'03'`/`'06'`). This must be coordinated with the transaction type reference data (CVTRA03Y).

4. **Regulatory fee caps**: Swedish consumer credit regulations (FFFS 2014:5) impose limits on certain fees. The migrated system must implement fee cap validation.

5. **Fee waiver logic**: Many card programs waive certain fees (e.g., annual fee waived first year, late fee waived for first occurrence). The current stub has no waiver mechanism; this would need to be designed as a new requirement.

6. **Double-charging prevention**: If the batch runs multiple times in the same cycle (e.g., recovery after failure), fees must not be charged again. The transaction ID generation scheme (date + sequential suffix) provides some protection, but explicit idempotency checks should be implemented.

## Domain Expert Notes

- **CRITICAL — Domain expert input required**: The fee computation stub indicates that fees were planned but never implemented in the CardDemo source. The domain expert must clarify:
  1. How are fees currently charged in the production mainframe system? Is there a separate program or manual process?
  2. What fee types and amounts are applicable to NordKredit credit card accounts?
  3. Are there fee schedules that vary by account group (similar to disclosure groups for interest)?
  4. What are the Swedish regulatory caps on specific fee types under FFFS 2014:5?
  5. Should the migration implement fee computation as a new feature, or replicate the current state (no automated fees)?
- The `1400-COMPUTE-FEES` paragraph is called unconditionally for every category record, suggesting the original design intended per-category fee computation (e.g., different fees for purchases vs. cash advances).
- Fee computation is the most significant functional gap in the billing domain and must be resolved before Phase 2 (Payments Domain) migration can be considered complete.

---

**Template version:** 1.0
**Last updated:** 2026-02-16
