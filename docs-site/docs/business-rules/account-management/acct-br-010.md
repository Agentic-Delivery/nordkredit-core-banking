---
id: "acct-br-010"
title: "Account statement cycle aggregation and reporting"
domain: "account-management"
cobol_source: "CBTRN03C.cbl:127-322"
requirement_id: "ACCT-BR-010"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 64"
  - "EU Consumer Credit Directive 2008/48/EC Art. 6"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# ACCT-BR-010: Account statement cycle aggregation and reporting

## Summary

The transaction detail report batch program (CBTRN03C) generates periodic account statements by aggregating transactions within a configurable date range. The program reads all posted transactions sequentially, filters by processing date, and produces a report with three levels of totaling: page totals, account totals, and a grand total. Account boundaries are detected by card number changes in the transaction stream, triggering account-level subtotals. The report includes transaction type and category descriptions resolved via reference file lookups (TRANTYPE-FILE and TRANCATG-FILE) and cross-references card numbers to account/customer identifiers via the XREF file.

This report serves as the foundation for customer statements and regulatory disclosure. The date range is parameterized via an external date parameter file (DATEPARM), allowing the same program to generate daily, weekly, monthly, or custom-period reports.

## Business Logic

### Pseudocode

```
PERFORM MAIN:
    OPEN all files (TRANSACT, XREF, TRANTYPE, TRANCATG, REPORT, DATEPARM)
    READ date parameters (WS-START-DATE, WS-END-DATE)

    PERFORM UNTIL END-OF-FILE
        READ next TRAN-RECORD from TRANSACT-FILE

        -- Date range filter
        IF TRAN-PROC-TS(1:10) >= WS-START-DATE
          AND TRAN-PROC-TS(1:10) <= WS-END-DATE
            CONTINUE (process this transaction)
        ELSE
            SKIP (outside date range)
        END-IF

        -- Account break detection (card number change)
        IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM
            IF NOT first-time
                PERFORM WRITE-ACCOUNT-TOTALS   -- flush previous account
            END-IF
            MOVE TRAN-CARD-NUM TO WS-CURR-CARD-NUM
            LOOKUP XREF by card number → get account/customer ID
        END-IF

        -- Resolve transaction descriptions
        LOOKUP TRANTYPE-FILE by TRAN-TYPE-CD
        LOOKUP TRANCATG-FILE by TRAN-TYPE-CD + TRAN-CAT-CD

        -- Accumulate and write detail line
        ADD TRAN-AMT TO WS-PAGE-TOTAL, WS-ACCOUNT-TOTAL
        WRITE transaction detail line to report

        -- Page break handling
        IF line-counter MOD page-size = 0
            PERFORM WRITE-PAGE-TOTALS
            WRITE headers for new page
        END-IF
    END-PERFORM

    -- Final totals
    PERFORM WRITE-PAGE-TOTALS
    PERFORM WRITE-GRAND-TOTALS

PERFORM WRITE-ACCOUNT-TOTALS:
    WRITE account total line (WS-ACCOUNT-TOTAL)
    RESET WS-ACCOUNT-TOTAL to 0

PERFORM WRITE-PAGE-TOTALS:
    WRITE page total line (WS-PAGE-TOTAL)
    ADD WS-PAGE-TOTAL TO WS-GRAND-TOTAL
    RESET WS-PAGE-TOTAL to 0

PERFORM WRITE-GRAND-TOTALS:
    WRITE grand total line (WS-GRAND-TOTAL)
```

### Three-Level Totaling Structure

| Level | Variable | Reset Trigger | Purpose |
|---|---|---|---|
| Page | WS-PAGE-TOTAL (S9(09)V99) | Every WS-PAGE-SIZE (20) lines | Running page subtotal for readability |
| Account | WS-ACCOUNT-TOTAL (S9(09)V99) | Card number change (new account) | Total per account for statement disclosure |
| Grand | WS-GRAND-TOTAL (S9(09)V99) | End of report | Overall total across all accounts in date range |

### Date Range Filtering

| Field | Source | Format | Purpose |
|---|---|---|---|
| WS-START-DATE | DATEPARM file position 1-10 | YYYY-MM-DD (X(10)) | Inclusive start of reporting period |
| WS-END-DATE | DATEPARM file position 12-21 | YYYY-MM-DD (X(10)) | Inclusive end of reporting period |
| TRAN-PROC-TS(1:10) | Transaction record | YYYY-MM-DD (first 10 chars of 26-char timestamp) | Transaction processing date for comparison |

## Source COBOL Reference

**Program:** `CBTRN03C.cbl`
**Lines:** 122-137 (variables), 158-206 (main loop), 274-322 (totaling paragraphs)

**Date parameter and report variables (lines 122-137):**

```cobol
000122        01 WS-DATEPARM-RECORD.
000123            05 WS-START-DATE      PIC X(10).
000124            05 FILLER             PIC X(01).
000125            05 WS-END-DATE        PIC X(10).
000127        01 WS-REPORT-VARS.
000128            05 WS-FIRST-TIME      PIC X      VALUE 'Y'.
000129            05 WS-LINE-COUNTER    PIC 9(09) COMP-3 VALUE 0.
000131            05 WS-PAGE-SIZE       PIC 9(03) COMP-3 VALUE 20.
000133            05 WS-BLANK-LINE      PIC X(133) VALUE SPACES.
000134            05 WS-PAGE-TOTAL      PIC S9(09)V99 VALUE 0.
000135            05 WS-ACCOUNT-TOTAL   PIC S9(09)V99 VALUE 0.
000136            05 WS-GRAND-TOTAL     PIC S9(09)V99 VALUE 0.
000137            05 WS-CURR-CARD-NUM   PIC X(16) VALUE SPACES.
```
*(Lines 122-137 — date parameters and three-level totaling accumulators.)*

**Main processing loop with date filter and account break (lines 170-206):**

```cobol
000170            PERFORM UNTIL END-OF-FILE = 'Y'
000171              IF END-OF-FILE = 'N'
000172                 PERFORM 1000-TRANFILE-GET-NEXT
000173                 IF TRAN-PROC-TS (1:10) >= WS-START-DATE
000174                    AND TRAN-PROC-TS (1:10) <= WS-END-DATE
000175                    CONTINUE
000176                 ELSE
000177                    NEXT SENTENCE
000178                 END-IF
000179                 IF END-OF-FILE = 'N'
000181                    IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM
000182                      IF WS-FIRST-TIME = 'N'
000183                        PERFORM 1120-WRITE-ACCOUNT-TOTALS
000184                      END-IF
000185                      MOVE TRAN-CARD-NUM TO WS-CURR-CARD-NUM
000186                      MOVE TRAN-CARD-NUM TO FD-XREF-CARD-NUM
000187                      PERFORM 1500-A-LOOKUP-XREF
000188                    END-IF
```
*(Lines 170-188 — date filtering and account break detection by card number change.)*

**Transaction detail with page break (lines 274-290):**

```cobol
000274        1100-WRITE-TRANSACTION-REPORT.
000282            IF FUNCTION MOD(WS-LINE-COUNTER, WS-PAGE-SIZE) = 0
000283               PERFORM 1110-WRITE-PAGE-TOTALS
000284               PERFORM 1120-WRITE-HEADERS
000285            END-IF
000287            ADD TRAN-AMT TO WS-PAGE-TOTAL
000288                            WS-ACCOUNT-TOTAL
000289            PERFORM 1120-WRITE-DETAIL
```
*(Lines 274-289 — each transaction adds to both page and account totals simultaneously.)*

**Account totals (lines 306-316):**

```cobol
000306        1120-WRITE-ACCOUNT-TOTALS.
000307            MOVE WS-ACCOUNT-TOTAL   TO REPT-ACCOUNT-TOTAL
000308            MOVE REPORT-ACCOUNT-TOTALS TO FD-REPTFILE-REC
000309            PERFORM 1111-WRITE-REPORT-REC
000310            MOVE 0 TO WS-ACCOUNT-TOTAL
000311            ADD 1 TO WS-LINE-COUNTER
```
*(Lines 306-311 — writes account subtotal and resets accumulator to zero.)*

**Page totals (lines 293-304):**

```cobol
000293        1110-WRITE-PAGE-TOTALS.
000294            MOVE WS-PAGE-TOTAL TO REPT-PAGE-TOTAL
000295            MOVE REPORT-PAGE-TOTALS TO FD-REPTFILE-REC
000296            PERFORM 1111-WRITE-REPORT-REC
000297            ADD WS-PAGE-TOTAL TO WS-GRAND-TOTAL
000298            MOVE 0 TO WS-PAGE-TOTAL
```
*(Lines 293-298 — page total rolls up into grand total before reset.)*

**Grand totals (lines 318-322):**

```cobol
000318        1110-WRITE-GRAND-TOTALS.
000319            MOVE WS-GRAND-TOTAL TO REPT-GRAND-TOTAL
000320            MOVE REPORT-GRAND-TOTALS TO FD-REPTFILE-REC
000321            PERFORM 1111-WRITE-REPORT-REC
```
*(Lines 318-321 — final grand total written at end of report.)*

## Acceptance Criteria

### Scenario 1: Single account within date range produces account total

```gherkin
GIVEN a transaction file with 3 transactions for card 4000000000000001:
  | TRAN-PROC-TS | TRAN-AMT |
  | 2026-01-15 | +100.00 |
  | 2026-01-20 | +250.00 |
  | 2026-01-25 | -50.00 |
  AND the date range is 2026-01-01 to 2026-01-31
WHEN the report is generated
THEN the account total for card 4000000000000001 is 300.00
  AND the grand total is 300.00
```

### Scenario 2: Multiple accounts produce separate account totals

```gherkin
GIVEN transactions for two different cards:
  | Card | TRAN-AMT |
  | 4000000000000001 | +200.00 |
  | 4000000000000002 | +150.00 |
WHEN the card number changes from 4000000000000001 to 4000000000000002
THEN account totals for card 4000000000000001 (200.00) are written
  AND a new accumulation begins for card 4000000000000002
  AND the grand total is 350.00
```

### Scenario 3: Transactions outside date range are excluded

```gherkin
GIVEN the date range is 2026-01-01 to 2026-01-31
  AND a transaction with TRAN-PROC-TS = 2026-02-05 exists
WHEN the report processes this transaction
THEN the transaction is skipped (not included in any totals)
```

### Scenario 4: Page breaks trigger page totals

```gherkin
GIVEN WS-PAGE-SIZE = 20
  AND 25 transactions exist for one account
WHEN the 21st transaction is processed
THEN a page total is written for the first 20 transactions
  AND report headers are reprinted for the new page
  AND the page total is added to the grand total
```

### Scenario 5: XREF lookup resolves card to account

```gherkin
GIVEN a transaction with TRAN-CARD-NUM = 4000000000000001
WHEN the card number changes (first occurrence or new card)
THEN the XREF file is looked up to resolve the account ID and customer ID
  AND the account context is available for the report header
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 7 | Financial system accuracy and completeness of reporting | Three-level totaling ensures mathematical accuracy — page totals roll up to grand total, with account-level subtotals for disclosure |
| PSD2 | Art. 64 | Transaction information must be available and accurately reported | The report includes all transactions within the date range with type/category descriptions, amounts, and account context |
| EU Consumer Credit Directive | Art. 6 (2008/48/EC) | Periodic statements must disclose transaction activity by account | Account-level totaling provides the per-account summary required for customer statement disclosure |

## Edge Cases

1. **Transaction file ordering assumption**: The program assumes transactions are ordered by card number within the sequential file. If transactions are not sorted, account totals would be incorrect — a card number could reappear after a different card. The mainframe likely sorts the TRANSACT-FILE before running CBTRN03C. The migrated system must ensure transactions are ordered by account before aggregation.

2. **Date comparison as string**: The date filter compares `TRAN-PROC-TS(1:10)` as a character string against `WS-START-DATE` and `WS-END-DATE`. This works because the YYYY-MM-DD format sorts lexicographically. The migrated system should use proper date comparison types.

3. **Page total vs. grand total accumulation**: Page totals are added to the grand total at page breaks. If the final page does not fill completely, the remaining page total is still rolled up at end-of-file (line 200-203). This ensures the grand total always equals the sum of all transaction amounts.

4. **Account total reset timing**: The account total is reset to zero only when a new card number is detected (line 310). If the same card appears in non-contiguous records (unsorted file), the total would be split across multiple subtotals, producing incorrect per-account figures.

5. **133-character report line**: The report file uses 133-character records (`PIC X(133)`), a standard mainframe print line width (1 control character + 132 print positions). The migrated system will likely produce PDF or HTML statements instead.

6. **Empty date range**: If no transactions fall within the date range, the program produces only headers and a grand total of zero. The migrated system should handle empty statement periods gracefully.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Is CBTRN03C used for customer-facing statements, internal reporting, or both? (2) Is the TRANSACT-FILE pre-sorted by card number before this program runs — is there a JCL SORT step? (3) What is the standard reporting period — monthly billing cycle, or configurable? (4) Are there additional report programs for different report formats (e.g., summary vs. detail)? (5) How does this report relate to the billing cycle reset of ACCT-CURR-CYC-CREDIT and ACCT-CURR-CYC-DEBIT in ACCT-BR-004? (6) Should the migrated system generate PDF statements, or are electronic formats (CSV, JSON) acceptable?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
