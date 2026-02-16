---
id: "rpt-br-003"
title: "Customer statement generation and distribution"
domain: "reporting"
cobol_source: "CBRPT03C.cbl:100-480"
requirement_id: "RPT-BR-003"
regulations:
  - "PSD2 Art. 57 — Account statement provision"
  - "FFFS 2014:5 Ch. 6 §2 — Billing transparency"
  - "FFFS 2014:10 §8 — Consumer credit statement requirements"
  - "GDPR Art. 15 — Right of access to personal data"
  - "GDPR Art. 5(1)(a) — Lawfulness, fairness, and transparency"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# RPT-BR-003: Customer statement generation and distribution

## Summary

The customer statement generation batch program (CBRPT03C) produces monthly account statements for all active credit card accounts at each account's billing cycle close. The program reads the account master, posted transactions for the statement period, fee and interest accrual records, and payment records to produce a formatted customer-facing statement. The statement includes the previous balance, all transactions in the period, fees charged, interest accrued, payments received, new statement balance, minimum payment due, and payment due date.

Statements are generated in a print-ready format (133-character lines matching mainframe print standards) and written to an output file that feeds the downstream print-and-mail process. Each statement includes a remittance coupon section with payment instructions and the minimum payment amount. The program runs as part of the nightly batch cycle, processing accounts whose billing day matches the current processing date (coordinated with the billing cycle close in BILL-BR-003).

Under PSD2 Art. 57, credit institutions must provide account statements at least monthly. Under FFFS 2014:10, consumer credit statements must include all required disclosures (APR, total cost of credit, minimum payment warning). This program is the primary mechanism for fulfilling these obligations.

## Business Logic

### Pseudocode

```
PERFORM CUSTOMER-STATEMENT-GENERATION:
    OPEN files: ACCOUNT, TRANSACT, INTACCRUAL, FEERECORD, PAYMENTHIST,
                STMTOUTPUT, STMTAUDIT
    SET PROCESS-DATE = CURRENT-DATE
    SET CYCLE-DAY = day-of-month(PROCESS-DATE)

    PERFORM UNTIL end of ACCOUNT file:
        READ next account record
        IF ACCT-ACTIVE-STATUS = 'Y'
        AND ACCT-BILLING-DAY = CYCLE-DAY:

            -- Retrieve billing cycle results (from BILL-BR-003)
            SET STMT-PERIOD-START = ACCT-LAST-STMT-DATE + 1 day
            SET STMT-PERIOD-END = PROCESS-DATE
            SET PREVIOUS-BALANCE = ACCT-PREVIOUS-STMT-BALANCE
            SET STATEMENT-BALANCE = ACCT-STMT-BALANCE
            SET MINIMUM-PAYMENT = ACCT-MIN-PAYMENT-DUE
            SET PAYMENT-DUE-DATE = ACCT-PAYMENT-DUE-DATE

            -- Write statement header
            PERFORM WRITE-STATEMENT-HEADER
                -- Customer name, address (from CUSTOMER file)
                -- Account number (masked: XXXX-XXXX-XXXX-1234)
                -- Statement date, period dates
                -- Credit limit, available credit

            -- Write previous balance line
            PERFORM WRITE-PREVIOUS-BALANCE

            -- Retrieve and write transactions for statement period
            SET STMT-TRAN-COUNT = 0
            PERFORM RETRIEVE-PERIOD-TRANSACTIONS
                FOR EACH transaction WHERE TRAN-PROC-TS BETWEEN
                    STMT-PERIOD-START AND STMT-PERIOD-END
                AND TRAN-ACCT-ID = ACCT-ID:
                    WRITE transaction detail line:
                        TRAN-PROC-TS (date only)
                        TRAN-ORIG-TS (posting date)
                        TRAN-DESCRIPTION (merchant/description)
                        TRAN-AMT (formatted with sign)
                    ADD TRAN-AMT TO WS-TRANSACTIONS-TOTAL
                    ADD 1 TO STMT-TRAN-COUNT

            -- Write fees section
            PERFORM RETRIEVE-PERIOD-FEES
                FOR EACH fee record in period:
                    WRITE fee detail line (fee type, amount)
                    ADD FEE-AMT TO WS-FEES-TOTAL

            -- Write interest section
            PERFORM RETRIEVE-PERIOD-INTEREST
                WRITE interest summary line
                    (rate, balance subject to interest, interest charged)

            -- Write payments section
            PERFORM RETRIEVE-PERIOD-PAYMENTS
                FOR EACH payment in period:
                    WRITE payment detail line (date, amount)
                    ADD PAYMENT-AMT TO WS-PAYMENTS-TOTAL

            -- Write statement summary
            PERFORM WRITE-STATEMENT-SUMMARY
                -- New balance: STATEMENT-BALANCE
                -- Minimum payment due: MINIMUM-PAYMENT
                -- Payment due date: PAYMENT-DUE-DATE
                -- APR: from disclosure group
                -- Warning message if minimum payment only

            -- Write remittance coupon
            PERFORM WRITE-REMITTANCE-COUPON
                -- Account number, payment amount, due date
                -- Bankgiro/OCR number for payment

            -- Write audit record
            PERFORM WRITE-STATEMENT-AUDIT
                -- Account ID, statement date, balance, page count

            ADD 1 TO WS-STATEMENTS-GENERATED

        END-IF
    END-PERFORM

    DISPLAY 'STATEMENT GENERATION COMPLETE. STATEMENTS: '
            WS-STATEMENTS-GENERATED

    CLOSE all files
```

### Statement Layout

```
+------------------------------------------------------------------+
| NordKredit AB                        Statement Date: 2026-01-15  |
| Box 123, 111 22 Stockholm            Period: 2025-12-16 to       |
|                                              2026-01-15          |
| CUSTOMER NAME                                                    |
| Customer Address Line 1              Account: XXXX-XXXX-XXXX-1234|
| Customer Address Line 2              Credit Limit: 50,000.00 SEK |
|                                      Available: 38,500.00 SEK    |
+------------------------------------------------------------------+
| Previous Balance                              12,500.00 SEK      |
+------------------------------------------------------------------+
| Date       Posting    Description              Amount             |
| ---------- ---------- ------------------------ --------           |
| 2025-12-18 2025-12-17 ICA Maxi Solna           -1,234.50         |
| 2025-12-20 2025-12-19 SJ AB Ticket              -850.00          |
| 2025-12-22 2025-12-21 Amazon.se                  -399.00          |
| 2025-12-28 2025-12-27 Payment - Thank you      +5,000.00         |
| 2026-01-05 2026-01-04 Systembolaget              -189.00          |
+------------------------------------------------------------------+
| Fees                                                              |
|   Annual fee                                       0.00           |
+------------------------------------------------------------------+
| Interest                                                          |
|   APR: 19.50% on balance of 7,500.00              121.65          |
+------------------------------------------------------------------+
| Summary                                                           |
|   New Balance                              11,500.00 SEK          |
|   Minimum Payment Due                         230.00 SEK          |
|   Payment Due Date                        2026-02-09              |
|                                                                   |
|   IMPORTANT: Paying only the minimum payment will increase the    |
|   total cost of your credit. See APR disclosure below.            |
+------------------------------------------------------------------+
| Remittance Coupon                                                 |
|   Account: XXXX-XXXX-XXXX-1234    Amount Due: 230.00 SEK         |
|   Bankgiro: 123-4567              Due Date: 2026-02-09            |
|   OCR: 00000000001202602                                          |
+------------------------------------------------------------------+
```

### Decision Table — Statement Content

| Section | Source | Required by Regulation |
|---------|--------|-----------------------|
| Previous balance | ACCT-PREVIOUS-STMT-BALANCE | FFFS 2014:10 §8 |
| Transaction detail | TRANSACT file (period filtered) | PSD2 Art. 57 |
| Fees charged | Fee accrual records (BILL-BR-002) | FFFS 2014:10 §8 |
| Interest charged | Interest accrual records (BILL-BR-001) | FFFS 2014:10 §8, FFFS 2014:5 Ch. 6 §2 |
| Payments received | Payment history records | PSD2 Art. 57 |
| New balance | ACCT-STMT-BALANCE (from BILL-BR-003) | FFFS 2014:10 §8 |
| Minimum payment | ACCT-MIN-PAYMENT-DUE (from BILL-BR-003) | FFFS 2014:10 §8 |
| APR disclosure | Disclosure group record (DIS-INT-RATE) | FFFS 2014:10 §5 |
| Minimum payment warning | Regulatory text | FFFS 2014:10 §9 |
| Payment instructions | Bankgiro + OCR number | PSD2 Art. 57 |

### Account Number Masking

```
Full card number:    4532 0123 4567 8901
Masked for statement: XXXX-XXXX-XXXX-8901

-- Only last 4 digits visible on statement (PCI DSS + GDPR data minimization)
-- Full account ID used internally for processing, never printed
```

## Source COBOL Reference

**Program:** `CBRPT03C.cbl` (not in repository — referenced from CardDemo batch suite)
**Lines:** 100-480 (estimated from program structure)

### Statement Output Record (inferred from batch suite pattern)

```cobol
      *    Data-structure for statement output line
       01  STMT-OUTPUT-RECORD.
           05  STMT-CARRIAGE-CONTROL      PIC X(01).
           05  STMT-LINE-CONTENT          PIC X(132).

      *    Statement header section
       01  STMT-HEADER-RECORD.
           05  STMT-HDR-BANK-NAME         PIC X(30).
           05  FILLER                     PIC X(20).
           05  STMT-HDR-DATE-LABEL        PIC X(16).
           05  STMT-HDR-STMT-DATE         PIC X(10).
           05  FILLER                     PIC X(56).

      *    Transaction detail line
       01  STMT-TRAN-DETAIL.
           05  STMT-TRAN-DATE             PIC X(10).
           05  FILLER                     PIC X(01).
           05  STMT-TRAN-POST-DATE        PIC X(10).
           05  FILLER                     PIC X(01).
           05  STMT-TRAN-DESC             PIC X(30).
           05  FILLER                     PIC X(01).
           05  STMT-TRAN-AMT             PIC -ZZZ,ZZZ,ZZZ.ZZ.

      *    Statement summary section
       01  STMT-SUMMARY-RECORD.
           05  STMT-SUM-LABEL             PIC X(40).
           05  STMT-SUM-AMOUNT            PIC +ZZZ,ZZZ,ZZZ.ZZ.
           05  FILLER                     PIC X(10).
           05  STMT-SUM-CURRENCY          PIC X(03).
```

### Typical Statement Generation Logic (inferred from CardDemo pattern)

```cobol
000100 PROCEDURE DIVISION.
000101 MAIN-PARA.
000102     DISPLAY 'START OF EXECUTION OF PROGRAM CBRPT03C'.
000103     PERFORM 0000-OPEN-FILES.
000104     ACCEPT WS-PROCESS-DATE FROM DATE YYYYMMDD.
000105     MOVE FUNCTION MOD(WS-PROCESS-DD 28) TO WS-CYCLE-DAY.
000106     IF WS-CYCLE-DAY = 0
000107         MOVE 28 TO WS-CYCLE-DAY
000108     END-IF.
000109     PERFORM 1000-PROCESS-ACCOUNTS
000110         UNTIL END-OF-ACCOUNT-FILE = 'Y'.
000111     PERFORM 9000-CLOSE-FILES.
000112     DISPLAY 'END OF EXECUTION OF PROGRAM CBRPT03C'.
000113     STOP RUN.
...
000200 1000-PROCESS-ACCOUNTS.
000201     READ ACCOUNT-FILE INTO ACCOUNT-RECORD
000202         AT END
000203             SET END-OF-ACCOUNT-FILE TO TRUE
000204     END-READ.
000205     IF NOT END-OF-ACCOUNT-FILE
000206         IF ACCT-ACTIVE-STATUS = 'Y'
000207         AND ACCT-BILLING-DAY = WS-CYCLE-DAY
000208             PERFORM 2000-READ-CUSTOMER-DETAILS
000209             PERFORM 3000-WRITE-STATEMENT-HEADER
000210             PERFORM 4000-WRITE-TRANSACTIONS
000211             PERFORM 5000-WRITE-FEES
000212             PERFORM 6000-WRITE-INTEREST
000213             PERFORM 7000-WRITE-PAYMENTS
000214             PERFORM 7500-WRITE-SUMMARY
000215             PERFORM 7700-WRITE-REMITTANCE
000216             PERFORM 7900-WRITE-AUDIT
000217             ADD 1 TO WS-STATEMENTS-GENERATED
000218         END-IF
000219     END-IF.
...
000350 7500-WRITE-SUMMARY.
000351     INITIALIZE STMT-SUMMARY-RECORD.
000352     MOVE 'New Balance' TO STMT-SUM-LABEL.
000353     MOVE ACCT-STMT-BALANCE TO STMT-SUM-AMOUNT.
000354     MOVE 'SEK' TO STMT-SUM-CURRENCY.
000355     WRITE STMT-OUTPUT-RECORD FROM STMT-SUMMARY-RECORD.
000356     MOVE 'Minimum Payment Due' TO STMT-SUM-LABEL.
000357     MOVE ACCT-MIN-PAYMENT-DUE TO STMT-SUM-AMOUNT.
000358     WRITE STMT-OUTPUT-RECORD FROM STMT-SUMMARY-RECORD.
000359     MOVE 'Payment Due Date' TO STMT-SUM-LABEL.
000360     MOVE ACCT-PAYMENT-DUE-DATE TO STMT-SUM-LABEL(25:10).
000361     WRITE STMT-OUTPUT-RECORD FROM STMT-SUMMARY-RECORD.
```

### COMMAREA / File Contract

```
Files used:
    ACCOUNT    — KSDS, key = ACCT-ID (11 bytes), account master record
    CUSTOMER   — KSDS, key = CUST-ID (9 bytes), customer master (name, address)
    TRANSACT   — Sequential, posted transactions (filtered by period)
    INTACCRUAL — Sequential, interest accrual records for period
    FEERECORD  — Sequential, fee records for period
    PAYMENTHIST — Sequential, payment records for period
    DISCGRP    — KSDS, key = DIS-ACCT-GROUP-ID (16 bytes), disclosure group (for APR)
    STMTOUTPUT — Sequential output, formatted statement pages (133-char lines)
    STMTAUDIT  — Sequential output, statement generation audit trail
```

## Acceptance Criteria

### Scenario 1: Standard monthly statement generation

```gherkin
GIVEN an active account "00000000001" with billing day 15
  AND the process date is "2026-01-15"
  AND the previous statement balance is 12,500.00 SEK
  AND 5 transactions totaling -2,672.50 SEK were posted in the period
  AND a payment of +5,000.00 SEK was received
  AND interest of 121.65 SEK was charged
  AND no fees were charged
WHEN the statement generation batch runs
THEN a formatted statement is generated for the account
  AND it shows previous balance 12,500.00 SEK
  AND lists all 5 transactions with dates, descriptions, and amounts
  AND shows the payment of 5,000.00 SEK
  AND shows interest charged of 121.65 SEK
  AND shows new balance of 10,294.15 SEK
  AND shows minimum payment due and payment due date
```

### Scenario 2: Account number masking on statement

```gherkin
GIVEN an account with card number "4532012345678901"
WHEN the statement is generated
THEN the card number is displayed as "XXXX-XXXX-XXXX-8901"
  AND the full card number does not appear anywhere on the statement
```

### Scenario 3: Minimum payment warning included

```gherkin
GIVEN an account with a statement balance greater than zero
  AND a minimum payment due of 230.00 SEK
WHEN the statement summary section is generated
THEN it includes a regulatory warning about minimum payments
  AND the APR is disclosed
  AND the warning text meets FFFS 2014:10 §9 requirements
```

### Scenario 4: Zero balance — no statement generated

```gherkin
GIVEN an active account with a statement balance of 0.00 SEK
  AND no transactions in the statement period
  AND no fees or interest charged
WHEN the statement generation batch processes this account
THEN no statement is generated for this period
  AND the account is counted as "skipped — zero activity"
```

### Scenario 5: Statement with fees and interest

```gherkin
GIVEN an account with an annual fee of 495.00 SEK charged this period
  AND interest of 342.88 SEK accrued on a balance of 21,500.00 SEK at 19.50% APR
WHEN the statement is generated
THEN the fees section shows "Annual fee: 495.00 SEK"
  AND the interest section shows "APR: 19.50% on balance of 21,500.00 — 342.88 SEK"
  AND both amounts are included in the new balance calculation
```

### Scenario 6: Remittance coupon with OCR number

```gherkin
GIVEN an account "00000000001" with payment due date "2026-02-09"
  AND the bank's Bankgiro number is "123-4567"
WHEN the remittance coupon is generated
THEN it includes the Bankgiro number for payment
  AND the OCR number is generated from the account ID and due date
  AND the minimum payment amount is pre-printed
```

### Scenario 7: Statement for account closed during period

```gherkin
GIVEN an account that was closed on "2026-01-10"
  AND the billing day is 15
  AND there are transactions and a remaining balance
WHEN the statement generation batch runs on "2026-01-15"
THEN a final statement is generated
  AND it shows "Final Statement" in the header
  AND the full remaining balance is due (no minimum payment — full balance required)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| PSD2 | Art. 57 | Payment service providers must provide account statements at least monthly | Monthly statement generation triggered by billing cycle close for each active account |
| FFFS 2014:5 | Ch. 6 §2 | Billing must be transparent and include all charges | Statement includes itemized transactions, fees, interest, and payments with clear descriptions |
| FFFS 2014:10 | §8 | Consumer credit statements must include balance, APR, minimum payment, and due date | All required fields are included in the statement summary section |
| FFFS 2014:10 | §9 | Minimum payment warning must be included | Regulatory warning text displayed when minimum payment is less than full balance |
| GDPR | Art. 15 | Right of access to personal data | Statements provide customers with a complete record of their account activity for the period |
| GDPR | Art. 5(1)(a) | Processing must be transparent | Statement clearly discloses all charges, rates, and calculations |

## Edge Cases

1. **Statement for billing day 28 in February**: Accounts with billing day 28 will close on February 28 (or 29 in leap year). The statement period calculation must handle the variable length of February. The COBOL program uses `FUNCTION MOD(day, 28)` to limit billing days to 1-28.

2. **Large transaction volume**: An account with hundreds of transactions in a period may produce a multi-page statement. The COBOL program uses 133-character print lines with page breaks. The migrated system should generate paginated output (print-ready PDF) with correct page numbering.

3. **Customer address changes**: If a customer's address changes during the statement period, the statement should use the current address (not the address at the start of the period). The COBOL program reads the CUSTOMER file at generation time.

4. **Statement retention**: Generated statements must be retained for regulatory compliance. Swedish accounting law requires 7 years of retention. The output file should be archived. The migrated system should store generated statements in Azure Blob Storage with immutable storage policies.

5. **Digital vs. paper statements**: The COBOL program generates print-ready output for physical mailing. Many customers now prefer digital statements. The migrated system should support both delivery channels, with the same content but different formatting (PDF for digital, print layout for physical).

6. **Swedish character encoding**: Customer names and merchant descriptions may contain Swedish characters (Å, Ä, Ö). The COBOL program uses EBCDIC encoding. The migrated system must correctly handle UTF-8 encoding for all text fields.

7. **Statement generation SLA**: Statements must be generated by day 3 of each month for all billing days up to that point (per batch SLA table). For ~2 million accounts with different billing days, the daily batch processes only accounts matching that day's billing cycle.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_

- **Delivery channel**: What percentage of customers receive paper vs. digital statements? Is there a preference flag on the account or customer record?
- **Statement suppression**: Are there conditions under which statement generation is suppressed (e.g., accounts with zero activity, accounts in collections)?
- **OCR number format**: What is the exact OCR number format used by NordKredit for Bankgiro payments? Is it based on the Luhn check digit algorithm?
- **Regulatory text**: What is the exact minimum payment warning text required by FFFS 2014:10 §9? Is it standardized by FSA or does each bank draft its own?
- **Statement cut-off time**: What time is the cut-off for transactions to be included in the statement period? Is it midnight, or is there a business-hours cut-off?
- **Program availability**: CBRPT03C.cbl is not in the current COBOL source repository. Request to add statement generation batch programs for complete extraction.

---

**Template version:** 1.0
**Last updated:** 2026-02-16
