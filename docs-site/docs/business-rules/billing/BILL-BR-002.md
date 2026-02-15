---
id: "BILL-BR-002"
title: "Monthly interest calculated per transaction category using disclosure group rates"
domain: "billing"
cobol_source: "CBACT04C.cbl:180-520"
requirement_id: "BILL-BR-002"
regulations:
  - "FFFS 2014:5 Ch. 4 — Interest Rate Disclosure"
  - "PSD2 Art. 45 — Information on Charges and Interest"
  - "EBA GL 2020/06 — Interest Rate Risk"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# BILL-BR-002: Monthly interest calculated per transaction category using disclosure group rates

## Summary

The interest calculation batch job (CBACT04C, invoked by INTCALC.jcl) processes transaction category balance records, looks up applicable interest rates from a disclosure group file, computes monthly interest per category, accumulates the total interest per account, and updates the account balance. Interest is calculated using the formula: `monthly_interest = (category_balance * annual_interest_rate) / 1200`. A fee computation step exists but is not yet implemented in the source.

## Business Logic

### Interest Calculation Formula

```
monthly_interest = (transaction_category_balance * annual_interest_rate) / 1200
```

Where:
- `transaction_category_balance` (`TRAN-CAT-BAL`): The balance for a specific transaction category (e.g., purchases, cash advances)
- `annual_interest_rate` (`DIS-INT-RATE`): The annual percentage rate from the disclosure group file
- `1200`: Divisor to convert annual rate to monthly (annual_rate / 12, with the rate expressed as a percentage, so effectively annual_rate_percent / 12 / 100 = annual_rate / 1200)

### Precision and Data Types

| Variable | COBOL Picture | Type | Precision |
|----------|--------------|------|-----------|
| `TRAN-CAT-BAL` | (from CVTRA01Y copybook) | Signed decimal | Depends on copybook definition |
| `DIS-INT-RATE` | (from CVTRA02Y copybook) | Decimal | Depends on copybook definition |
| `WS-MONTHLY-INT` | `PIC S9(09)V99` | Signed, 2 decimal places | 9 digits + 2 decimal |
| `WS-TOTAL-INT` | `PIC S9(09)V99` | Signed, 2 decimal places | 9 digits + 2 decimal |
| `ACCT-CURR-BAL` | (from CVACT01Y copybook) | Signed decimal | Depends on copybook definition |

**Critical**: COBOL `COMPUTE` with `PIC S9(09)V99` truncates to 2 decimal places without explicit rounding. The migration must replicate this truncation behavior exactly to match mainframe output during parallel run.

### Pseudocode

```
PROCEDURE DIVISION USING EXTERNAL-PARMS:
    OPEN files: TCATBALF (input), XREFFILE (input), DISCGRP (input),
                ACCTFILE (I-O), TRANSACT (output)

    SET first-time = 'Y'
    SET total-interest = 0

    READ EACH record FROM transaction-category-balance-file:
        IF account-id <> last-account-id THEN
            IF NOT first-time THEN
                PERFORM UPDATE-ACCOUNT:
                    ADD total-interest TO account-current-balance
                    RESET cycle-credit = 0
                    RESET cycle-debit = 0
                    REWRITE account record
            ELSE
                SET first-time = 'N'
            END-IF

            RESET total-interest = 0
            SAVE current account-id as last-account-id
            READ account master record (ACCTFILE) by account-id
            READ cross-reference record (XREFFILE) by account-id
        END-IF

        LOOK UP interest rate:
            SET disclosure-group-key = (account-group-id, tran-type-cd, tran-cat-cd)
            READ disclosure-group-file (DISCGRP) by key
            IF NOT FOUND (status '23') THEN
                SET disclosure-group-key = ('DEFAULT', tran-type-cd, tran-cat-cd)
                READ disclosure-group-file with DEFAULT group
            END-IF

        IF interest-rate <> 0 THEN
            COMPUTE monthly-interest = (category-balance * interest-rate) / 1200
            ADD monthly-interest TO total-interest
            WRITE interest transaction record to TRANSACT file:
                TRAN-TYPE-CD  = '01'
                TRAN-CAT-CD   = '05'
                TRAN-SOURCE   = 'System'
                TRAN-DESC     = 'Int. for a/c <account-id>'
                TRAN-AMT      = monthly-interest
                TRAN-CARD-NUM = card number from xref
                TRAN-ORIG-TS  = current timestamp
                TRAN-PROC-TS  = current timestamp
        END-IF

    END-READ

    PERFORM UPDATE-ACCOUNT for final account
    CLOSE all files
```

### Decision Table

| Category Balance Found | Group Rate Found | Default Rate Found | Rate = 0 | Outcome |
|-----------------------|-----------------|-------------------|----------|---------|
| Yes | Yes | — | No | Calculate interest, write transaction, accumulate |
| Yes | Yes | — | Yes | Skip interest (no charge for this category) |
| Yes | No (status '23') | Yes | No | Calculate using DEFAULT rate |
| Yes | No (status '23') | Yes | Yes | Skip interest |
| Yes | No (status '23') | No | — | ABEND: error reading default disclosure group |
| Yes | Error (other) | — | — | ABEND: error reading disclosure group file |
| No (EOF) | — | — | — | Update final account, close files, end |
| Error reading | — | — | — | ABEND: error reading transaction category file |

## Source COBOL Reference

**Program:** `CBACT04C.cbl`
**Lines:** 462-470 (Core interest calculation)

```cobol
       1300-COMPUTE-INTEREST.

           COMPUTE WS-MONTHLY-INT
            = ( TRAN-CAT-BAL * DIS-INT-RATE) / 1200

           ADD WS-MONTHLY-INT  TO WS-TOTAL-INT
           PERFORM 1300-B-WRITE-TX.

           EXIT.
```

**Lines:** 350-370 (Account balance update)

```cobol
       1050-UPDATE-ACCOUNT.
      * Update the balances in account record to reflect posted trans.
           ADD WS-TOTAL-INT  TO ACCT-CURR-BAL
           MOVE 0 TO ACCT-CURR-CYC-CREDIT
           MOVE 0 TO ACCT-CURR-CYC-DEBIT

           REWRITE FD-ACCTFILE-REC FROM  ACCOUNT-RECORD
```

**Lines:** 415-460 (Disclosure group lookup with DEFAULT fallback)

```cobol
       1200-GET-INTEREST-RATE.
           READ DISCGRP-FILE INTO DIS-GROUP-RECORD
                INVALID KEY
                   DISPLAY 'DISCLOSURE GROUP RECORD MISSING'
                   DISPLAY 'TRY WITH DEFAULT GROUP CODE'
           END-READ.

           IF  DISCGRP-STATUS  = '00'  OR '23'
               MOVE 0 TO APPL-RESULT
           ELSE
               MOVE 12 TO APPL-RESULT
           END-IF

           IF  DISCGRP-STATUS  = '23'
               MOVE 'DEFAULT' TO FD-DIS-ACCT-GROUP-ID
               PERFORM 1200-A-GET-DEFAULT-INT-RATE
           END-IF
           EXIT.
```

**Lines:** 473-515 (Interest transaction record creation)

```cobol
       1300-B-WRITE-TX.
           ADD 1 TO WS-TRANID-SUFFIX

           STRING PARM-DATE,
                  WS-TRANID-SUFFIX
             DELIMITED BY SIZE
             INTO TRAN-ID
           END-STRING.

           MOVE '01'                 TO TRAN-TYPE-CD
           MOVE '05'                 TO TRAN-CAT-CD
           MOVE 'System'             TO TRAN-SOURCE
           STRING 'Int. for a/c ' ,
                  ACCT-ID
                  DELIMITED BY SIZE
            INTO TRAN-DESC
           END-STRING
           MOVE WS-MONTHLY-INT       TO TRAN-AMT
```

**JCL:** `INTCALC.jcl`
**Input Files:** TCATBALF (transaction category balances), XREFFILE (card cross-reference), ACCTFILE (account master), DISCGRP (disclosure group rates)
**Output Files:** TRANSACT (interest transaction records), ACCTFILE (updated balances)

**Copybooks:** `CVTRA01Y.cpy` (Transaction Category Balance), `CVACT03Y.cpy` (Card Cross-Reference), `CVTRA02Y.cpy` (Disclosure Group), `CVACT01Y.cpy` (Account Record), `CVTRA05Y.cpy` (Transaction Record)

## Acceptance Criteria

### Scenario 1: Standard monthly interest calculation

```gherkin
GIVEN a transaction category balance record for account "00000012345":
  | Field | Value |
  | TRANCAT-ACCT-ID | 00000012345 |
  | TRANCAT-TYPE-CD | 01 |
  | TRANCAT-CD | 0001 |
  | TRAN-CAT-BAL | 5000.00 |
  AND the disclosure group for account group "GRP001", type "01", category "0001" has:
  | DIS-INT-RATE | 18.00 |
WHEN the interest calculation batch runs
THEN the monthly interest = (5000.00 * 18.00) / 1200 = 75.00
  AND a transaction record is written with TRAN-AMT = 75.00
  AND the account balance is increased by 75.00
```

### Scenario 2: Multiple categories for same account

```gherkin
GIVEN account "00000012345" has two transaction category balances:
  | Category | Balance | Rate |
  | Purchases (type 01, cat 0001) | 3000.00 | 18.00 |
  | Cash Advances (type 02, cat 0002) | 1000.00 | 24.00 |
WHEN the interest calculation batch runs
THEN two interest transactions are created:
  | Category | Monthly Interest |
  | Purchases | (3000.00 * 18.00) / 1200 = 45.00 |
  | Cash Advances | (1000.00 * 24.00) / 1200 = 20.00 |
  AND the total interest added to account balance = 65.00
```

### Scenario 3: DEFAULT rate fallback

```gherkin
GIVEN a transaction category balance for account "00000012345" with group "NEWGRP"
  AND no disclosure group record exists for group "NEWGRP", type "01", cat "0001"
  AND a DEFAULT disclosure group record exists for type "01", cat "0001" with rate 15.00
WHEN the interest calculation batch runs
THEN the DEFAULT rate of 15.00 is used for the calculation
  AND interest is computed normally using the default rate
```

### Scenario 4: Zero interest rate — no charge

```gherkin
GIVEN a transaction category balance with a balance of 2000.00
  AND the applicable disclosure group has DIS-INT-RATE = 0
WHEN the interest calculation batch runs
THEN no interest transaction is created for that category
  AND the account balance is not affected by that category
```

### Scenario 5: Account cycle counters reset

```gherkin
GIVEN account "00000012345" has:
  | ACCT-CURR-BAL | 10000.00 |
  | ACCT-CURR-CYC-CREDIT | 500.00 |
  | ACCT-CURR-CYC-DEBIT | 300.00 |
  AND the total accumulated interest for the account is 125.00
WHEN the account balance is updated by the interest batch
THEN ACCT-CURR-BAL = 10125.00
  AND ACCT-CURR-CYC-CREDIT = 0
  AND ACCT-CURR-CYC-DEBIT = 0
```

### Scenario 6: Fractional interest truncation

```gherkin
GIVEN a transaction category balance of 1234.56 with rate 17.50
WHEN monthly interest is calculated
THEN interest = (1234.56 * 17.50) / 1200 = 18.00 (truncated from 18.0040...)
  AND the stored value in WS-MONTHLY-INT (PIC S9(09)V99) is 18.00
```

### Scenario 7: Batch parameter provides processing date

```gherkin
GIVEN the JCL passes parameter '2022071800' (date 2022-07-18)
WHEN interest transactions are created
THEN each transaction ID is formed as: '<parm-date><sequential-suffix>'
  AND the suffix increments from 000001 for each transaction in the batch run
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §1-4 | Credit interest rates must be disclosed and calculated transparently | Interest rates are stored in a disclosure group file keyed by account group and transaction category, enabling auditable rate application. |
| PSD2 | Art. 45(1) | Charges and interest applied must be disclosed | Each interest charge creates an individual transaction record with description 'Int. for a/c <id>', providing a clear audit trail. |
| EBA GL 2020/06 | §4.3 | Interest rate risk in the banking book must be managed | The separation of rates by disclosure group and transaction category allows differential rate management. |

## Edge Cases

1. **COBOL truncation vs. rounding**: The COMPUTE statement with `PIC S9(09)V99` truncates to 2 decimal places. The migration must use truncation (not rounding) to match mainframe output exactly during parallel run. Example: `(1234.56 * 17.50) / 1200 = 18.004...` truncates to `18.00`, not rounds to `18.00`.

2. **Fees not implemented**: The `1400-COMPUTE-FEES` paragraph (line 518) is a stub with only `EXIT`. The migration should note this as a gap — fee computation may need to be implemented as a new business requirement.

3. **Transaction ID format**: Transaction IDs are composed by STRING concatenation of `PARM-DATE` (10 chars from JCL) + `WS-TRANID-SUFFIX` (6 digits). The suffix increments per transaction within a single batch run, starting from 000001. The ID format is `<date><suffix>`, giving a 16-character key.

4. **Account processing order**: Records are read sequentially from TCATBALF, which is an indexed VSAM KSDS file. Records are ordered by the composite key (account-id, type-cd, cat-cd). When the account ID changes, the previous account is updated. This means the final account requires an extra update after the read loop ends (line 220-221).

5. **DEFAULT rate fallback**: If the specific disclosure group record is not found (file status '23'), the system falls back to a group ID of 'DEFAULT'. If the DEFAULT record is also missing, the program ABENDs. The migration must implement this fallback chain.

6. **Single-threaded batch**: The batch reads sequentially and updates accounts one at a time. In the migration, parallel processing could be considered but must ensure the same deterministic results (especially for the final account balance).

7. **Cycle counter reset**: When updating the account, `ACCT-CURR-CYC-CREDIT` and `ACCT-CURR-CYC-DEBIT` are both set to 0 (lines 353-354). This implies the interest calculation is also a billing cycle boundary. The migration must preserve this cycle-close behavior.

8. **Negative balances**: The formula makes no check for negative category balances. If a category has a negative balance (credit), the computed interest would also be negative (a credit to the customer). The migration should verify this is the intended behavior.

## Domain Expert Notes

- **Pending validation**: This rule is HIGH RISK and requires review by a domain expert, particularly for: (1) the truncation vs. rounding behavior, (2) the exact data types in the copybooks CVTRA01Y and CVTRA02Y, and (3) the fee computation gap.
- The division by 1200 (not 12 * 100) is the standard COBOL pattern for converting annual percentage rate to monthly decimal. For a rate of 18%, the monthly rate is 18/1200 = 0.015 = 1.5%.
- Interest is added to the account balance (debit), which is correct for a credit card account where balance represents amount owed by the customer.
- The `TRAN-TYPE-CD = '01'` and `TRAN-CAT-CD = '05'` classify these as system-generated interest transactions, distinct from `'02'`/`2` used for bill payments.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
