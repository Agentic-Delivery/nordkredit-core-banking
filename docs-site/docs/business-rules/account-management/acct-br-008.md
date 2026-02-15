---
id: "acct-br-008"
title: "Batch interest calculation and account balance update"
domain: "account-management"
cobol_source: "CBACT04C.cbl:1-653"
requirement_id: "ACCT-BR-008"
regulations:
  - "FSA FFFS 2014:5 Ch. 4 S10"
  - "EBA Guidelines on Interest Rate Risk"
  - "PSD2 Art. 45"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-008: Batch interest calculation and account balance update

## Summary

CBACT04C is a batch COBOL program that calculates monthly interest on account balances by transaction category and updates account records with the accumulated interest. This is one of the most critical financial calculation programs in the CardDemo system. The program sequentially reads the transaction category balance file (TCATBAL), looks up interest rates from a disclosure group file (DISCGRP) based on account group, transaction type, and category, computes monthly interest using the formula `(balance x annual_rate) / 1200`, accumulates interest across categories for each account, and updates the account master record (ACCTFILE) by adding total interest to the current balance and resetting cycle accumulators. For each interest amount calculated, an interest transaction record is generated and written to the transaction output file (TRANSACT). The program processes accounts in sequence, using account-break logic to detect when the account ID changes and trigger the update of the previous account. Interest rate lookup includes a fallback mechanism: if the rate is not found for the account's group, the program retries with a 'DEFAULT' group before ABENDing.

## Business Logic

### Pseudocode

```
PROGRAM CBACT04C:

    ACCEPT PARM-DATE FROM JCL PARM    -- PIC X(10), e.g., "2025-01-31"

    OPEN INPUT  TCATBAL-FILE           -- Transaction category balance
    OPEN I-O    ACCTFILE               -- Account master (read + update)
    OPEN INPUT  XREFFILE               -- Cross-reference (card number lookup)
    OPEN INPUT  DISCGRP-FILE           -- Disclosure group (interest rates)
    OPEN OUTPUT TRANSACT-FILE          -- Interest transactions output

    SET WS-FIRST-RECORD = TRUE
    SET WS-TOTAL-INT = 0
    PERFORM 1000-READ-TCATBAL

    PERFORM UNTIL end-of-file
        -- Account break detection
        IF TRANCAT-ACCT-ID NOT = WS-PREV-ACCT-ID
            -- New account encountered
            IF NOT WS-FIRST-RECORD
                -- Update PREVIOUS account with accumulated interest
                PERFORM 1050-UPDATE-ACCOUNT
            END-IF
            SET WS-FIRST-RECORD = FALSE
            SET WS-TOTAL-INT = 0
            MOVE TRANCAT-ACCT-ID TO WS-PREV-ACCT-ID
            PERFORM 1100-GET-ACCT-DATA     -- Read account record
            PERFORM 1110-GET-XREF-DATA     -- Read cross-reference for card number
        END-IF

        -- Look up interest rate for this category
        PERFORM 1200-LOOKUP-INTEREST-RATE
        IF WS-INT-RATE NOT = ZERO
            PERFORM 1300-COMPUTE-INTEREST
        END-IF

        PERFORM 1000-READ-TCATBAL
    END-PERFORM

    -- Update the LAST account (no more account breaks to trigger it)
    IF NOT WS-FIRST-RECORD
        PERFORM 1050-UPDATE-ACCOUNT
    END-IF

    CLOSE all files
    STOP RUN.

1050-UPDATE-ACCOUNT:
    ADD WS-TOTAL-INT TO ACCT-CURR-BAL
    MOVE 0 TO ACCT-CURR-CYC-CREDIT
    MOVE 0 TO ACCT-CURR-CYC-DEBIT
    REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD

1100-GET-ACCT-DATA:
    MOVE TRANCAT-ACCT-ID TO ACCT-ID
    READ ACCTFILE INTO ACCOUNT-RECORD
    IF file-status NOT = '00'
        ABEND with code 999
    END-IF

1110-GET-XREF-DATA:
    MOVE TRANCAT-ACCT-ID TO XREF-ACCT-ID
    READ XREFFILE INTO CARD-XREF-RECORD
        KEY IS XREF-ACCT-ID (alternate key)
    IF file-status NOT = '00'
        ABEND with code 999
    END-IF

1200-LOOKUP-INTEREST-RATE:
    -- Build lookup key: ACCT-GROUP-ID + TRANCAT-TYPE-CD + TRANCAT-CD
    MOVE ACCT-GROUP-ID  TO DIS-GROUP-ID
    MOVE TRANCAT-TYPE-CD TO DIS-TYPE-CD
    MOVE TRANCAT-CD     TO DIS-CAT-CD
    READ DISCGRP-FILE INTO DISCLOSURE-RECORD
    IF file-status = '23'    (record not found)
        -- Fallback to DEFAULT group
        MOVE 'DEFAULT   ' TO DIS-GROUP-ID
        READ DISCGRP-FILE INTO DISCLOSURE-RECORD
        IF file-status = '23'
            ABEND with code 999   (no default rate either)
        END-IF
    END-IF
    MOVE DIS-INT-RATE TO WS-INT-RATE

1300-COMPUTE-INTEREST:
    -- Monthly interest formula
    COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200

    ADD WS-MONTHLY-INT TO WS-TOTAL-INT

    -- Generate interest transaction record
    PERFORM 1300-B-WRITE-TX

1300-B-WRITE-TX:
    -- Build transaction ID: PARM-DATE + sequential suffix
    MOVE PARM-DATE TO TX-ID-DATE-PART
    ADD 1 TO WS-TX-SEQ-NUM
    MOVE WS-TX-SEQ-NUM TO TX-ID-SEQ-PART
    MOVE '01'          TO TX-TYPE-CD        (transaction type)
    MOVE '05'          TO TX-CAT-CD         (interest category)
    MOVE 'System'      TO TX-SOURCE
    STRING 'Int. for a/c ' TRANCAT-ACCT-ID
        INTO TX-DESCRIPTION
    MOVE WS-MONTHLY-INT TO TX-AMOUNT
    -- Timestamp in DB2 format: YYYY-MM-DD-HH.MM.SS.HH0000
    ACCEPT current-date-time
    FORMAT AS 'YYYY-MM-DD-HH.MM.SS.HH0000'
    MOVE formatted-timestamp TO TX-TIMESTAMP
    MOVE XREF-CARD-NUM TO TX-CARD-NUM
    WRITE TRANSACT-REC

1400-COMPUTE-FEES:
    -- STUB: "To be implemented"
    CONTINUE.
```

### Interest Calculation Formula

```
monthly_interest = (category_balance * annual_interest_rate) / 1200
```

Where:
- `category_balance` = TRAN-CAT-BAL from the transaction category balance file
- `annual_interest_rate` = DIS-INT-RATE from the disclosure group file (percentage, e.g., 18.0 for 18%)
- `1200` = 12 months x 100 (to convert percentage to decimal and divide by 12)

### Decision Table: Interest Rate Lookup

| Account Group ID | Lookup Result | Fallback to DEFAULT | DEFAULT Result | Outcome |
|-----------------|---------------|--------------------:|----------------|---------|
| Specific group  | Rate found    | N/A                 | N/A            | Use found rate |
| Specific group  | Not found ('23') | Yes              | Rate found     | Use DEFAULT rate |
| Specific group  | Not found ('23') | Yes              | Not found ('23') | ABEND code 999 |

### Decision Table: Interest Computation

| Interest Rate | Category Balance | Action |
|--------------|-----------------|--------|
| Zero (0.00)  | Any             | Skip computation, no transaction generated |
| Non-zero     | Positive        | Compute positive interest (charge), write transaction |
| Non-zero     | Negative        | Compute negative interest (credit), write transaction |
| Non-zero     | Zero            | Compute zero interest, write transaction (amount = 0) |

## Source COBOL Reference

**Program:** `CBACT04C.cbl`
**Lines:** 1-653 (complete program)

**Key sections:**
- File definitions and LINKAGE SECTION: lines 28-178
- Main processing loop with account break logic: lines 188-222
- Account update (1050-UPDATE-ACCOUNT): lines 350-370
- Account data read (1100-GET-ACCT-DATA): lines 375-395
- Cross-reference read (1110-GET-XREF-DATA): lines 398-415
- Interest rate lookup with fallback (1200-LOOKUP-INTEREST-RATE): lines 415-460
- Interest computation (1300-COMPUTE-INTEREST): lines 462-470
- Transaction record write (1300-B-WRITE-TX): lines 473-515
- Fee computation stub (1400-COMPUTE-FEES): lines 518-520

```cobol
000175 LINKAGE SECTION.
000176 01  PARM-RECORD.
000177     05  PARM-LENGTH         PIC S9(04) COMP.
000178     05  PARM-DATE           PIC X(10).
```

```cobol
000188     PERFORM 1000-READ-TCATBAL.
000189     PERFORM UNTIL TCATBAL-EOF
000190         IF TRANCAT-ACCT-ID NOT = WS-PREV-ACCT-ID
000191             IF NOT WS-FIRST-RECORD
000192                 PERFORM 1050-UPDATE-ACCOUNT
000193             END-IF
000194             SET WS-FIRST-RECORD TO FALSE
000195             MOVE 0 TO WS-TOTAL-INT
000196             MOVE TRANCAT-ACCT-ID TO WS-PREV-ACCT-ID
000197             PERFORM 1100-GET-ACCT-DATA
000198             PERFORM 1110-GET-XREF-DATA
000199         END-IF
000200         PERFORM 1200-LOOKUP-INTEREST-RATE
000201         IF WS-INT-RATE NOT = ZERO
000202             PERFORM 1300-COMPUTE-INTEREST
000203         END-IF
000204         PERFORM 1000-READ-TCATBAL
000222     END-PERFORM.
```

```cobol
000350 1050-UPDATE-ACCOUNT.
000351     ADD WS-TOTAL-INT TO ACCT-CURR-BAL.
000352     MOVE 0 TO ACCT-CURR-CYC-CREDIT.
000353     MOVE 0 TO ACCT-CURR-CYC-DEBIT.
000354     REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD.
```

```cobol
000437         MOVE 'DEFAULT   ' TO DIS-GROUP-ID.
000438         READ DISCGRP-FILE INTO DISCLOSURE-RECORD.
```

```cobol
000464     COMPUTE WS-MONTHLY-INT =
000465         (TRAN-CAT-BAL * DIS-INT-RATE) / 1200.
```

```cobol
000473 1300-B-WRITE-TX.
000474     STRING PARM-DATE WS-TX-SEQ-SUFFIX
000475         DELIMITED BY SIZE INTO TX-ID.
000476     MOVE '01'          TO TX-TYPE-CD.
000477     MOVE '05'          TO TX-CAT-CD.
000478     MOVE 'System'      TO TX-SOURCE.
000479     STRING 'Int. for a/c ' TRANCAT-ACCT-ID
000480         DELIMITED BY SIZE INTO TX-DESCRIPTION.
000481     MOVE WS-MONTHLY-INT TO TX-AMOUNT.
```

```cobol
000518 1400-COMPUTE-FEES.
000519 *   To be implemented
000520     CONTINUE.
```

**Input/Output files:**

| File | DD Name | Access Mode | Key | Description |
|------|---------|-------------|-----|-------------|
| TCATBAL | TCATBAL-FILE | Sequential read | ACCT-ID + TYPE-CD + CAT-CD | Transaction category balances |
| ACCTFILE | ACCTFILE | Random I-O | ACCT-ID | Account master (read + rewrite) |
| XREFFILE | XREFFILE | Random read | XREF-ACCT-ID (alternate key) | Card cross-reference |
| DISCGRP | DISCGRP-FILE | Random read | GROUP-ID + TYPE-CD + CAT-CD | Disclosure group interest rates |
| TRANSACT | TRANSACT-FILE | Sequential write | N/A | Generated interest transactions |

## Acceptance Criteria

### Scenario 1: Monthly interest calculated correctly for a single category

```gherkin
GIVEN a transaction category balance record with TRAN-CAT-BAL = 10000.00
  AND the disclosure group file contains an interest rate of 18.0% for the matching group, type, and category
WHEN the interest computation is performed
THEN the monthly interest is calculated as (10000.00 * 18.0) / 1200 = 150.00
  AND an interest transaction record is written with amount 150.00
```

### Scenario 2: Interest accumulated across multiple categories for one account

```gherkin
GIVEN account "00012345678" has three transaction category balance records
  AND category 1 produces monthly interest of 150.00
  AND category 2 produces monthly interest of 25.50
  AND category 3 produces monthly interest of 0.00 (zero rate, skipped)
WHEN the account break is detected (next account encountered)
THEN WS-TOTAL-INT equals 175.50
  AND ACCT-CURR-BAL is increased by 175.50
  AND ACCT-CURR-CYC-CREDIT is reset to 0
  AND ACCT-CURR-CYC-DEBIT is reset to 0
  AND the account record is rewritten to ACCTFILE
```

### Scenario 3: Interest rate fallback to DEFAULT group

```gherkin
GIVEN an account with ACCT-GROUP-ID = "PREMIUM"
  AND the disclosure group file has no rate for group "PREMIUM", type "01", category "03"
  AND the disclosure group file has a rate of 12.5% for group "DEFAULT", type "01", category "03"
WHEN the interest rate lookup is performed
THEN the DEFAULT group rate of 12.5% is used for the calculation
```

### Scenario 4: ABEND when no rate found for DEFAULT group

```gherkin
GIVEN an account with ACCT-GROUP-ID = "UNKNOWN"
  AND the disclosure group file has no rate for group "UNKNOWN", type "01", category "03"
  AND the disclosure group file has no rate for group "DEFAULT", type "01", category "03"
WHEN the interest rate lookup is performed
THEN the program ABENDs with code 999
```

### Scenario 5: Zero interest rate skips computation

```gherkin
GIVEN a transaction category balance record with a looked-up interest rate of 0.00
WHEN the interest rate is checked
THEN no interest computation is performed for this category
  AND no interest transaction record is written for this category
  AND WS-TOTAL-INT is not modified
```

### Scenario 6: Interest transaction record contains correct fields

```gherkin
GIVEN PARM-DATE = "2025-01-31"
  AND the interest for account "00012345678", category balance produces monthly interest of 75.25
  AND the cross-reference file maps the account to card number "4000123456789012"
WHEN the interest transaction is written
THEN the transaction type code is '01'
  AND the transaction category code is '05'
  AND the source is 'System'
  AND the description contains 'Int. for a/c 00012345678'
  AND the amount is 75.25
  AND the card number is '4000123456789012'
  AND the transaction ID starts with '2025-01-31'
```

### Scenario 7: Last account in file is updated correctly

```gherkin
GIVEN the transaction category balance file contains records for accounts A, B, and C (in sorted order)
WHEN the end-of-file is reached after processing account C's categories
THEN account C's accumulated interest is applied to its balance
  AND account C's cycle accumulators are reset to zero
  AND account C's record is rewritten to ACCTFILE
```

### Scenario 8: Cycle accumulators reset after interest posting

```gherkin
GIVEN account "00012345678" has ACCT-CURR-CYC-CREDIT = 5000.00 and ACCT-CURR-CYC-DEBIT = 3200.00
WHEN the account update is performed after interest calculation
THEN ACCT-CURR-CYC-CREDIT is set to 0
  AND ACCT-CURR-CYC-DEBIT is set to 0
  AND ACCT-CURR-BAL reflects the addition of accumulated interest
```

### Scenario 9: Fee computation stub has no effect

```gherkin
GIVEN the 1400-COMPUTE-FEES paragraph exists in the program
WHEN processing reaches the fee computation step
THEN no fee is calculated or applied
  AND no fee transaction records are generated
  AND account balances are not affected by fees
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 4 S10 | Credit institutions must calculate and apply interest in accordance with agreed terms and conditions, with transparent methodology | The interest calculation uses a documented formula (balance x rate / 1200) with rates sourced from the disclosure group file; the migrated system must produce identical interest amounts to the penny for parallel-run validation |
| EBA Guidelines on Interest Rate Risk | General | Institutions must have reliable systems for interest rate computation with auditable methodology | The batch program provides a clear, deterministic interest calculation flow: rate lookup by group/type/category, monthly accrual formula, and transaction-level audit trail via TRANSACT file; the migrated system must preserve this auditability |
| PSD2 | Art. 45 | Payment service providers must provide clear information on interest rates and charges applied to payment accounts | Interest transactions are recorded with descriptive text ('Int. for a/c' + account ID), type code '01', and category '05', enabling downstream statement generation; the migrated system must maintain equivalent transaction detail for customer-facing statements |

## Edge Cases

1. **Account with no transaction category balances**: If an account exists in ACCTFILE but has no records in TCATBAL, the program never processes it. No interest is calculated and no balance update occurs. The migrated system must handle this equivalently -- accounts not present in the category balance input are untouched.

2. **Negative category balances producing credit interest**: The formula does not distinguish between positive and negative balances. A negative TRAN-CAT-BAL with a positive rate produces negative interest (a credit to the customer). The migrated system must preserve this behavior and not clamp interest to zero.

3. **Rounding of interest amounts**: The COMPUTE statement in COBOL uses the default rounding behavior (truncation toward zero) unless ROUNDED is specified. The COBOL code at line 464-465 does not use the ROUNDED keyword. The migrated C# implementation must use equivalent truncation behavior (`decimal` arithmetic with explicit truncation) rather than banker's rounding (`MidpointRounding.ToEven`). A discrepancy of even one cent per account across millions of accounts produces material differences.

4. **Transaction category balance file must be sorted by account ID**: The account-break logic assumes TCATBAL records are sorted by TRANCAT-ACCT-ID. If the file is unsorted, the program will repeatedly open and close accounts, producing incorrect accumulated interest totals. The migrated system should validate input ordering or sort before processing.

5. **Fee computation stub (1400-COMPUTE-FEES)**: The fee calculation paragraph is explicitly marked as "To be implemented." If fees are later added to the COBOL program before migration is complete, the migrated system must incorporate that logic. The migration team should monitor this paragraph for changes in subsequent COBOL releases.

6. **PARM-DATE dependency for transaction IDs**: The program receives its processing date via JCL PARM, not from the system clock. This allows reprocessing for a prior date. The migrated system must accept an equivalent parameter and not assume the processing date is always today.

7. **Cross-reference file lookup by alternate key**: The XREFFILE is read by XREF-ACCT-ID (an alternate key), not by the primary key (card number). If an account has multiple cards, the VSAM alternate key read returns the first matching record. The migrated system must replicate this behavior -- use the first card number associated with the account for interest transaction records.

8. **Disclosure group key composition**: The interest rate lookup uses a composite key of GROUP-ID + TYPE-CD + CAT-CD. If any component is spaces or invalid, the lookup may return an incorrect rate or trigger the DEFAULT fallback. The migrated system should add validation for the lookup key components.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Critical questions for the retiring COBOL developers: (1) Is the interest calculation formula (balance x rate / 1200) the contractually agreed methodology, or does it vary by product type? (2) Are there any accounts that should be excluded from interest calculation (e.g., dormant, frozen, or closed accounts)? (3) Does the cycle accumulator reset (credit and debit set to zero) happen only during interest posting, or also at other points in the monthly cycle? (4) What is the expected behavior if the REWRITE of the account record fails? (5) Is the fee computation stub (1400-COMPUTE-FEES) planned for a specific timeline, and what fee types are expected? (6) What is the expected rounding behavior -- truncation or commercial rounding? This is critical for regulatory compliance.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
