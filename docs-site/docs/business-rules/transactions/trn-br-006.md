---
id: "trn-br-006"
title: "Daily transaction detail report generation"
domain: "transactions"
cobol_source: "CBTRN03C.cbl:158-374"
requirement_id: "TRN-BR-006"
regulations:
  - "FSA FFFS 2014:5"
  - "AML 2017:11"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-006: Daily transaction detail report generation

## Summary

CBTRN03C is a batch program that generates the Daily Transaction Detail Report. It reads posted transactions from the TRANSACT file, filters them by a date range specified in a parameter file, enriches each transaction with type and category descriptions from lookup files, and produces a formatted report with page totals, account totals, and a grand total. The report groups transactions by card number (account) and paginates at 20 lines per page. Extracted from `CBTRN03C.cbl`.

## Business Logic

### Pseudocode

```
PERFORM MAIN:
    Open all files:
        TRANSACT (input, sequential) - posted transactions
        REPORT (output, sequential) - report output
        CARDXREF (input, indexed) - card cross-reference
        TRANTYPE (input, indexed) - transaction type descriptions
        TRANCATG (input, indexed) - transaction category descriptions
        DATEPARM (input, sequential) - date range parameters

    PERFORM 0550-DATEPARM-READ
        Read start and end dates from parameter file
        Format: "YYYY-MM-DD YYYY-MM-DD" (10 + 1 + 10 chars)

    PERFORM UNTIL end-of-file = 'Y'
        READ next transaction record

        Filter by date range:
        IF TRAN-PROC-TS(1:10) >= WS-START-DATE
           AND TRAN-PROC-TS(1:10) <= WS-END-DATE
            Continue processing
        ELSE
            Skip to next record
        END-IF

        IF card number changed from previous record
            IF not first record
                Write account totals for previous card
            END-IF
            Look up new card in XREF to get account ID
        END-IF

        Look up transaction type description
        Look up transaction category description

        PERFORM 1100-WRITE-TRANSACTION-REPORT:
            IF first time: write report header with date range
            IF line counter is multiple of page size (20):
                Write page totals
                Write new page header
            ADD TRAN-AMT TO WS-PAGE-TOTAL and WS-ACCOUNT-TOTAL
            Write detail line
    END-PERFORM

    At end of file:
        Write final page totals
        Write grand total (sum of all page totals)

    Close all files
    GOBACK
```

### Report Structure

```
DALYREPT            Daily Transaction Report    Date Range: 2026-01-01 to 2026-01-31

Transaction ID  Account ID  Transaction Type   Tran Category                  Tran Source     Amount
---------------------------------------------------------------------...----------------------
0000000000000001 00012345678 01-Purchase        0001-Retail                    POS              -125.50
0000000000000002 00012345678 02-Payment         0002-Online                    WEB            +1000.00
...
Page Total.......................................................................       +874.50

Account Total....................................................................       +874.50

---------------------------------------------------------------------...----------------------
[next account's transactions]
...
Grand Total......................................................................     +12345.67
```

### Report Line Formats

| Line Type | Format | Content |
|---|---|---|
| Header | REPORT-NAME-HEADER | Short name, long name, date range |
| Column Headers | TRANSACTION-HEADER-1 | Column labels |
| Separator | TRANSACTION-HEADER-2 | 133 dashes |
| Detail | TRANSACTION-DETAIL-REPORT | ID, Account, Type, Category, Source, Amount |
| Page Total | REPORT-PAGE-TOTALS | "Page Total" + dotted leader + amount |
| Account Total | REPORT-ACCOUNT-TOTALS | "Account Total" + dotted leader + amount |
| Grand Total | REPORT-GRAND-TOTALS | "Grand Total" + dotted leader + amount |

### Amount Formatting

| Context | COBOL PIC | Example |
|---|---|---|
| Detail line amount | -ZZZ,ZZZ,ZZZ.ZZ | -125.50 |
| Page total | +ZZZ,ZZZ,ZZZ.ZZ | +12,345.67 |
| Account total | +ZZZ,ZZZ,ZZZ.ZZ | +50,000.00 |
| Grand total | +ZZZ,ZZZ,ZZZ.ZZ | +1,234,567.89 |

### Totaling Hierarchy

```
Grand Total = Sum of all Page Totals
Page Total  = Sum of Detail Amounts on current page (resets every 20 lines)
Account Total = Sum of Detail Amounts for current card/account (resets on card change)
```

**Critical precision notes:**
- WS-PAGE-TOTAL: `S9(09)V99` — signed with 2 decimal places
- WS-ACCOUNT-TOTAL: `S9(09)V99` — signed with 2 decimal places
- WS-GRAND-TOTAL: `S9(09)V99` — signed with 2 decimal places
- All totals use ADD (COBOL fixed-point addition, no rounding)

## Source COBOL Reference

**Program:** `CBTRN03C.cbl`
**Lines:** 158-217 (main loop), 220-243 (date parameter read), 248-272 (transaction read), 274-290 (write report entry), 293-304 (page totals), 306-316 (account totals), 318-322 (grand totals), 324-341 (headers), 361-374 (detail line)

```cobol
000173                IF TRAN-PROC-TS (1:10) >= WS-START-DATE
000174                   AND TRAN-PROC-TS (1:10) <= WS-END-DATE
000175                   CONTINUE
000176                ELSE
000177                   NEXT SENTENCE
000178                END-IF
```

```cobol
000287           ADD TRAN-AMT TO WS-PAGE-TOTAL
000288                           WS-ACCOUNT-TOTAL
000289           PERFORM 1120-WRITE-DETAIL
```

```cobol
000361       1120-WRITE-DETAIL.
000362           INITIALIZE TRANSACTION-DETAIL-REPORT
000363           MOVE TRAN-ID TO TRAN-REPORT-TRANS-ID
000364           MOVE XREF-ACCT-ID TO TRAN-REPORT-ACCOUNT-ID
000365           MOVE TRAN-TYPE-CD OF TRAN-RECORD TO TRAN-REPORT-TYPE-CD
000366           MOVE TRAN-TYPE-DESC TO TRAN-REPORT-TYPE-DESC
000367           MOVE TRAN-CAT-CD OF TRAN-RECORD  TO TRAN-REPORT-CAT-CD
000368           MOVE TRAN-CAT-TYPE-DESC TO TRAN-REPORT-CAT-DESC
000369           MOVE TRAN-SOURCE TO TRAN-REPORT-SOURCE
000370           MOVE TRAN-AMT TO TRAN-REPORT-AMT
```

### File Dependencies

| File | DD Name | Organization | Access | Key | Purpose |
|---|---|---|---|---|---|
| TRANSACT | TRANFILE | Sequential | Sequential | N/A | Posted transactions (input) |
| REPORT | TRANREPT | Sequential | Sequential | N/A | Report output file |
| CARDXREF | CARDXREF | Indexed | Random | FD-XREF-CARD-NUM (X(16)) | Card-to-account cross-reference |
| TRANTYPE | TRANTYPE | Indexed | Random | FD-TRAN-TYPE (X(02)) | Transaction type descriptions |
| TRANCATG | TRANCATG | Indexed | Random | FD-TRAN-CAT-KEY (X(02)+9(04)) | Transaction category descriptions |
| DATEPARM | DATEPARM | Sequential | Sequential | N/A | Date range parameters |

## Acceptance Criteria

### Scenario 1: Report generation within date range

GIVEN a date parameter file with start date 2026-01-01 and end date 2026-01-31
  AND the TRANSACT file contains transactions with processing dates in January 2026
WHEN the batch report program runs
THEN only transactions with TRAN-PROC-TS between the start and end dates are included
  AND each transaction line shows ID, account, type description, category description, source, and amount

### Scenario 2: Transaction type and category enrichment

GIVEN a transaction with type code "01" and category code "0001"
WHEN the report detail line is generated
THEN the type description is looked up from TRANTYPE file
  AND the category description is looked up from TRANCATG file
  AND both are displayed alongside the codes

### Scenario 3: Page totaling every 20 lines

GIVEN the report has generated 20 detail lines on a page
WHEN the 21st line would be written
THEN a page total line is written with the sum of the 20 transaction amounts
  AND new page headers are printed
  AND the page total counter is reset to 0

### Scenario 4: Account total on card number change

GIVEN transactions are grouped by card number
WHEN the card number changes between consecutive records
THEN an account total line is written for the previous card's transactions
  AND the account total counter is reset to 0

### Scenario 5: Grand total at end of report

GIVEN all transactions have been processed
WHEN the end of the TRANSACT file is reached
THEN a grand total line is written
  AND the grand total equals the sum of all page totals

### Scenario 6: Date range filtering

GIVEN transactions exist outside the specified date range
WHEN the batch program reads those transactions
THEN they are skipped (not included in the report)
  AND no amounts are accumulated for skipped transactions

### Scenario 7: Invalid card number in XREF

GIVEN a transaction references a card number not in the CARDXREF file
WHEN the report program looks up the card
THEN the program ABENDs with an error message
  AND the report generation stops

### Scenario 8: Amount formatting precision

GIVEN transaction amounts use S9(09)V99 precision
WHEN amounts are formatted for the report
THEN detail amounts use format -ZZZ,ZZZ,ZZZ.ZZ (with comma grouping)
  AND totals use format +ZZZ,ZZZ,ZZZ.ZZ (with sign and comma grouping)
  AND no rounding occurs during accumulation

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 7 | Financial reporting and audit trail | Daily transaction report provides complete audit trail with totals at page, account, and grand level |
| AML 2017:11 | Para. 3 | Transaction monitoring and reporting | Date-range filtered reports support regulatory transaction review and suspicious activity analysis |
| DORA | Art. 11 | ICT risk management reporting | Structured report generation with error handling supports operational resilience requirements |

## Edge Cases

1. **NEXT SENTENCE for date filtering**: The COBOL code uses `NEXT SENTENCE` (line 177) to skip records outside the date range. This falls through to the `IF END-OF-FILE = 'N'` check below. The migrated system should use a simple `CONTINUE` equivalent — just skip the record.

2. **Account totals at end-of-file**: When EOF is reached, the code at lines 198-204 adds the last transaction's amount and writes page and grand totals. However, the account total for the last card group may not be written if the card-change detection doesn't trigger. The migrated system must ensure the final account total is always written.

3. **Report line width**: The report file uses 133-character records (FD-REPTFILE-REC PIC X(133)), matching standard mainframe 132-column print plus 1 carriage control. The migrated system should use equivalent formatting.

4. **Page size**: The page size is set to 20 lines (WS-PAGE-SIZE PIC 9(03) COMP-3 VALUE 20). This includes header lines in the count, so the effective detail lines per page may be fewer. The migrated system should confirm whether this is configurable.

5. **ABEND on lookup failures**: Invalid card numbers, transaction types, or category codes cause an immediate ABEND. This means data quality issues in the posted transaction file stop the entire report. The migrated system should consider logging errors and continuing.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Questions: (1) Is the 20-line page size a business requirement or a printer constraint? (2) Should the report be generated as PDF or another format in the migrated system? (3) Should lookup failures for type/category be fatal or should unknown codes be reported with a placeholder? (4) The date filter uses TRAN-PROC-TS (processing date) not TRAN-ORIG-TS (origination date) — is this the correct business intent?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
