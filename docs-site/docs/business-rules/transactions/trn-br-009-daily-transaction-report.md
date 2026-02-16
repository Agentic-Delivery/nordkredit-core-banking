---
id: "TRN-BR-009"
title: "Daily transaction detail report generation"
domain: "transactions"
cobol_source: "CBTRN03C.cbl:159-373"
requirement_id: "TRN-BR-009"
regulations:
  - "FFFS 2014:5 Ch. 16 — Financial reporting requirements"
  - "PSD2 Art. 94 — Transaction record accessibility"
  - "AML Directive — Transaction monitoring and reporting"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# TRN-BR-009: Daily transaction detail report generation

## Summary

The CBTRN03C batch program generates a detailed transaction report for a configurable date range. It reads the posted TRANSACT file sequentially, filters transactions by processing timestamp against a date range parameter file, and produces a formatted report with transaction details, page totals, account totals, and a grand total. The report is organized by card number (account), with account-level subtotals. Transaction type and category descriptions are resolved through lookup files.

## Business Logic

### Pseudocode

```
PERFORM MAIN-REPORT-GENERATION:
    OPEN files: TRANSACT (input), REPORT (output), XREF, TRANTYPE, TRANCATG, DATEPARM
    READ date parameters (start-date, end-date from DATEPARM file)

    PERFORM UNTIL end of TRANSACT file:
        READ next transaction record
        IF TRAN-PROC-TS(1:10) >= start-date AND <= end-date:
            ** Transaction is in date range **

            IF card number changed from previous record:
                IF not first record:
                    WRITE account totals for previous card
                END-IF
                SET current card number
                LOOKUP card in XREF to get Account ID

            LOOKUP transaction type description
            LOOKUP transaction category description

            ADD TRAN-AMT to page-total and account-total
            WRITE detail line to report

            IF page is full (every 20 lines):
                WRITE page totals
                WRITE new page headers
        ELSE:
            ** Transaction outside date range — skip **
        END-IF
    END-PERFORM

    ** End of file processing **
    WRITE final page totals
    WRITE grand total (sum of all page totals)
    CLOSE all files
```

### Report Layout (133 columns)

| Column | Width | Content |
|--------|-------|---------|
| 1-16 | 16 | Transaction ID |
| 18-28 | 11 | Account ID |
| 30-31 | 2 | Transaction Type Code |
| 32 | 1 | "-" separator |
| 33-47 | 15 | Type Description |
| 49-52 | 4 | Category Code |
| 53 | 1 | "-" separator |
| 54-82 | 29 | Category Description |
| 84-93 | 10 | Transaction Source |
| 98-113 | 16 | Amount (formatted -ZZZ,ZZZ,ZZZ.ZZ) |

### Report Totals

| Total Type | Scope | Format |
|-----------|-------|--------|
| Page Total | Every 20 lines | +ZZZ,ZZZ,ZZZ.ZZ |
| Account Total | When card number changes | +ZZZ,ZZZ,ZZZ.ZZ |
| Grand Total | End of report | +ZZZ,ZZZ,ZZZ.ZZ |

## Source COBOL Reference

**Program:** `CBTRN03C.cbl`
**Lines:** 159-373

```cobol
000159 PROCEDURE DIVISION.
000160     DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN03C'.
...
000168     PERFORM 0550-DATEPARM-READ.
000170     PERFORM UNTIL END-OF-FILE = 'Y'
000171       IF END-OF-FILE = 'N'
000172          PERFORM 1000-TRANFILE-GET-NEXT
000173          IF TRAN-PROC-TS (1:10) >= WS-START-DATE
000174             AND TRAN-PROC-TS (1:10) <= WS-END-DATE
000175             CONTINUE
000176          ELSE
000177             NEXT SENTENCE
000178          END-IF
000179          IF END-OF-FILE = 'N'
000181             IF WS-CURR-CARD-NUM NOT= TRAN-CARD-NUM
000182               IF WS-FIRST-TIME = 'N'
000183                 PERFORM 1120-WRITE-ACCOUNT-TOTALS
000184               END-IF
000185               MOVE TRAN-CARD-NUM TO WS-CURR-CARD-NUM
000186               MOVE TRAN-CARD-NUM TO FD-XREF-CARD-NUM
000187               PERFORM 1500-A-LOOKUP-XREF
000188             END-IF
000189             MOVE TRAN-TYPE-CD OF TRAN-RECORD TO FD-TRAN-TYPE
000190             PERFORM 1500-B-LOOKUP-TRANTYPE
000193             PERFORM 1500-C-LOOKUP-TRANCATG
000196             PERFORM 1100-WRITE-TRANSACTION-REPORT
...
000274 1100-WRITE-TRANSACTION-REPORT.
000275     IF WS-FIRST-TIME = 'Y'
000276        MOVE 'N' TO WS-FIRST-TIME
000277        MOVE WS-START-DATE TO REPT-START-DATE
000278        MOVE WS-END-DATE TO REPT-END-DATE
000279        PERFORM 1120-WRITE-HEADERS
000280     END-IF
000282     IF FUNCTION MOD(WS-LINE-COUNTER, WS-PAGE-SIZE) = 0
000283        PERFORM 1110-WRITE-PAGE-TOTALS
000284        PERFORM 1120-WRITE-HEADERS
000285     END-IF
000287     ADD TRAN-AMT TO WS-PAGE-TOTAL
000288                     WS-ACCOUNT-TOTAL
000289     PERFORM 1120-WRITE-DETAIL
...
000361 1120-WRITE-DETAIL.
000362     INITIALIZE TRANSACTION-DETAIL-REPORT
000363     MOVE TRAN-ID TO TRAN-REPORT-TRANS-ID
000364     MOVE XREF-ACCT-ID TO TRAN-REPORT-ACCOUNT-ID
000365     MOVE TRAN-TYPE-CD OF TRAN-RECORD TO TRAN-REPORT-TYPE-CD
000366     MOVE TRAN-TYPE-DESC TO TRAN-REPORT-TYPE-DESC
000367     MOVE TRAN-CAT-CD OF TRAN-RECORD TO TRAN-REPORT-CAT-CD
000368     MOVE TRAN-CAT-TYPE-DESC TO TRAN-REPORT-CAT-DESC
000369     MOVE TRAN-SOURCE TO TRAN-REPORT-SOURCE
000370     MOVE TRAN-AMT TO TRAN-REPORT-AMT
```

## Acceptance Criteria

### Scenario 1: Generate report for date range

```gherkin
GIVEN the date parameter file contains "2026-01-01 2026-01-31"
  AND the TRANSACT file contains transactions with various processing dates
WHEN the report program runs
THEN only transactions with TRAN-PROC-TS between "2026-01-01" and "2026-01-31" are included
  AND the report header shows "Date Range: 2026-01-01 to 2026-01-31"
```

### Scenario 2: Page break at 20 lines

```gherkin
GIVEN 25 transactions are being reported
WHEN the 21st transaction is processed
THEN a page total line is written for the first 20 transactions
  AND new column headers are printed
  AND the remaining 5 transactions follow
```

### Scenario 3: Account totals on card number change

```gherkin
GIVEN transactions for card "4000000000000001" totaling 1500.00
  AND the next transaction is for card "4000000000000002"
WHEN the card number changes
THEN an account total line of 1500.00 is written
  AND a new account section begins for the second card
```

### Scenario 4: Grand total at end of report

```gherkin
GIVEN all transactions have been processed
  AND page totals sum to 25000.00
WHEN the report completes
THEN a grand total line of 25000.00 is written as the last line
```

### Scenario 5: Transaction type and category resolution

```gherkin
GIVEN a transaction with type code "01" and category code "0001"
  AND the TRANTYPE file maps "01" to "Purchase"
  AND the TRANCATG file maps "01/0001" to "Groceries"
WHEN the detail line is written
THEN the type column shows "01-Purchase"
  AND the category column shows "0001-Groceries"
```

### Scenario 6: Invalid card number in XREF causes abend

```gherkin
GIVEN a transaction references a card number not in the XREF file
WHEN the XREF lookup fails
THEN the program abends with an error message
  AND the report is incomplete
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 16 | Financial reporting must be detailed and timely | Daily report provides transaction-level detail with totals |
| PSD2 | Art. 94 | Transaction records must be accessible for reference | Report provides a date-filtered view of all posted transactions |
| AML Directive | Art. 11 | Transaction monitoring for suspicious activity | Detailed report enables review of transaction patterns by account |

## Edge Cases

1. **Date filtering with NEXT SENTENCE**: The COBOL uses `NEXT SENTENCE` (line 177) for date range filtering, which skips to the period after the current sentence. This is a legacy COBOL construct that can behave unexpectedly with nested IF statements. The date filtering logic should be carefully tested in migration.

2. **Transactions not sorted by card**: The TRANSACT file is keyed by Transaction ID, not card number. If transactions for the same card are interleaved with other cards, the account totals will be fragmented (separate subtotals for each contiguous group of the same card). The report assumes sequential ordering by card.

3. **Page size of 20**: The WS-PAGE-SIZE is hardcoded to 20 lines per page (PIC 9(03) COMP-3 VALUE 20). The migrated system could make this configurable.

4. **Amount format**: Report amounts use PIC -ZZZ,ZZZ,ZZZ.ZZ, showing negative values with a leading minus and suppressing leading zeros with commas. The migrated system should use locale-appropriate formatting (Swedish: 1 234 567,89 kr).

5. **ABEND on lookup failure**: Unlike CBTRN02C which rejects transactions with invalid cards, CBTRN03C ABENDs on any XREF, TRANTYPE, or TRANCATG lookup failure. This is because the report is reading already-posted transactions — a missing reference record indicates a data integrity issue.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_
- Is this report generated daily as part of the batch job chain?
- Who are the consumers of this report (operations, audit, management)?
- Should the migrated system produce a PDF/Excel instead of a fixed-width text file?
- Is the date parameter file generated by another process or manually maintained?

---

**Template version:** 1.0
**Last updated:** 2026-02-15
