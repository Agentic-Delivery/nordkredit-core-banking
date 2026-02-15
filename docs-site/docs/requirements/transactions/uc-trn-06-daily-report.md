---
id: uc-trn-06
title: "UC-TRN-06: Generate Daily Transaction Report"
sidebar_label: "UC-TRN-06: Daily Report"
---

# UC-TRN-06: Generate Daily Transaction Report

## Use Case Overview

| Field | Value |
|---|---|
| **Use Case ID** | UC-TRN-06 |
| **Title** | Generate Daily Transaction Report |
| **Domain** | Transactions |
| **Primary Actor** | Batch Processing System |
| **Secondary Actor** | Compliance Officer (report consumer) |
| **Trigger** | Nightly batch schedule — step 3 (after posting completes) |
| **Source Program** | `CBTRN03C.cbl` |
| **Priority** | High |

## Preconditions

1. The TRANSACT file contains posted transactions (step 2 — CBTRN02C — has completed successfully).
2. The DATEPARM file contains a valid date range in the format `YYYY-MM-DD YYYY-MM-DD`.
3. Lookup files are accessible: CARDXREF (card cross-reference), TRANTYPE (transaction type descriptions), TRANCATG (transaction category descriptions).

## Postconditions

1. A formatted daily transaction report has been written to the TRANREPT file.
2. The report contains only transactions within the specified date range.
3. All page totals, account totals, and the grand total are mathematically consistent.

## Main Flow

1. The Batch Processing System opens all required files (TRANSACT, REPORT, CARDXREF, TRANTYPE, TRANCATG, DATEPARM).
2. The system reads the start and end dates from the DATEPARM file (`YYYY-MM-DD YYYY-MM-DD`).
3. The system writes the report header including the date range.
4. For each transaction record in the TRANSACT file:
   a. The system checks the processing timestamp (`TRAN-PROC-TS`) against the date range.
   b. If outside the date range, the transaction is skipped.
   c. If the card number has changed from the previous record (and this is not the first record), the system writes an account total for the previous card.
   d. The system looks up the card number in CARDXREF to resolve the account ID.
   e. The system looks up the transaction type description from TRANTYPE.
   f. The system looks up the transaction category description from TRANCATG.
   g. If the line count has reached 20, the system writes a page total and new page headers, then resets the page total.
   h. The system accumulates the transaction amount into the page total and account total.
   i. The system writes the formatted detail line.
5. After all records are processed, the system writes the final page total, the final account total, and the grand total.
6. The system closes all files.

## Alternative Flows

### AF-1: Invalid card number in XREF

- **At step:** 4d
- **Condition:** The card number is not found in the CARDXREF file.
- **Response:** The program ABENDs with an error message. Report generation stops.
- **Business rule:** [TRN-BR-006 Scenario 7](/docs/business-rules/transactions/trn-br-006#scenario-7-invalid-card-number-in-xref)
- **Migration note:** Domain expert question — should the migrated system log the error and continue instead of halting?

### AF-2: Lookup failure for transaction type

- **At step:** 4e
- **Condition:** The transaction type code is not found in the TRANTYPE file.
- **Response:** The program ABENDs. Report generation stops.
- **Migration note:** Same question as AF-1 regarding error tolerance.

### AF-3: Lookup failure for transaction category

- **At step:** 4f
- **Condition:** The transaction category code is not found in the TRANCATG file.
- **Response:** The program ABENDs. Report generation stops.
- **Migration note:** Same question as AF-1 regarding error tolerance.

### AF-4: Missing or invalid date parameters

- **At step:** 2
- **Condition:** The DATEPARM file cannot be read or is empty.
- **Response:** The program ABENDs with APPL-RESULT 12 or 16.
- **Business rule:** [TRN-BR-009](/docs/business-rules/transactions/trn-br-009) — pipeline error handling

### AF-5: Empty transaction file (no records in date range)

- **At step:** 4
- **Condition:** No transactions fall within the specified date range.
- **Response:** The report is generated with headers and a grand total of zero.

## Business Rules Traceability

| Business Rule | Description | Source |
|---|---|---|
| [TRN-BR-006](/docs/business-rules/transactions/trn-br-006) | Daily transaction detail report generation | `CBTRN03C.cbl:158-374` |
| [TRN-BR-007](/docs/business-rules/transactions/trn-br-007) | Transaction record data structures | `CVTRA05Y.cpy:1-21`, `CVTRA07Y.cpy` |
| [TRN-BR-009](/docs/business-rules/transactions/trn-br-009) | Daily batch transaction processing pipeline (step 3) | `CBTRN03C.cbl:1-650` |

## Regulatory Traceability

| Regulation | Article/Section | Requirement | How This Use Case Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 7 | Financial reporting and audit trail | Daily transaction report provides a complete audit trail with totals at page, account, and grand level |
| AML 2017:11 | Para. 3 | Transaction monitoring and reporting | Date-range filtered reports support regulatory transaction review and suspicious activity analysis |
| DORA | Art. 11 | ICT risk management reporting | Structured report generation with deterministic error handling supports operational resilience requirements |

---

## User Stories

### US-TRN-06.1: Filter transactions by date range

**As a** compliance officer,
**I want** the report to include only transactions within a specified date range (from the DATEPARM file),
**So that** the report covers exactly the intended period.

#### Acceptance Criteria

```gherkin
Scenario: Report generation within date range
  Given a date parameter file with start date "2026-01-01" and end date "2026-01-31"
    And the TRANSACT file contains transactions with processing dates in January 2026
    And the TRANSACT file contains transactions with processing dates in December 2025
  When the batch report program runs
  Then only transactions with TRAN-PROC-TS between "2026-01-01" and "2026-01-31" are included
    And transactions outside the date range are not included
    And no amounts are accumulated for skipped transactions

Scenario: Date range filtering uses processing date not origination date
  Given a transaction with origination date "2025-12-31" and processing date "2026-01-02"
    And the date range is "2026-01-01" to "2026-01-31"
  When the batch report program runs
  Then the transaction is included (processing date is within range)
```

#### Traceability

- **Business rule:** [TRN-BR-006 Scenario 1](/docs/business-rules/transactions/trn-br-006#scenario-1-report-generation-within-date-range), [Scenario 6](/docs/business-rules/transactions/trn-br-006#scenario-6-date-range-filtering)
- **COBOL source:** `CBTRN03C.cbl:173-178` — date comparison using `TRAN-PROC-TS(1:10)`
- **Regulations:** FSA FFFS 2014:5 Ch. 7, AML 2017:11 Para. 3

---

### US-TRN-06.2: Enrich transactions with descriptions

**As a** compliance officer,
**I want** each transaction line enriched with type description (from TRANTYPE) and category description (from TRANCATG),
**So that** I can understand transaction classifications without looking up codes.

#### Acceptance Criteria

```gherkin
Scenario: Transaction type and category enrichment
  Given a transaction with type code "01" and category code "0001"
    And the TRANTYPE file maps "01" to "Purchase"
    And the TRANCATG file maps "01"+"0001" to "Retail"
  When the report detail line is generated
  Then the type description "Purchase" is displayed alongside the code "01"
    And the category description "Retail" is displayed alongside the code "0001"

Scenario: Lookup failure abends the program
  Given a transaction with type code "99" that does not exist in the TRANTYPE file
  When the report program looks up the type
  Then the program ABENDs with an error message
    And report generation stops
```

#### Traceability

- **Business rule:** [TRN-BR-006 Scenario 2](/docs/business-rules/transactions/trn-br-006#scenario-2-transaction-type-and-category-enrichment), [Scenario 7](/docs/business-rules/transactions/trn-br-006#scenario-7-invalid-card-number-in-xref)
- **COBOL source:** `CBTRN03C.cbl:361-370` — MOVE of type/category descriptions to report line
- **Data structures:** [TRN-BR-007](/docs/business-rules/transactions/trn-br-007) — TRAN-TYPE-RECORD (`CVTRA03Y.cpy`), TRAN-CAT-RECORD (`CVTRA04Y.cpy`)
- **Regulations:** FSA FFFS 2014:5 Ch. 7

---

### US-TRN-06.3: Group transactions by card/account

**As a** compliance officer,
**I want** transactions grouped by card number with account totals on card number change,
**So that** I can see per-account summaries.

#### Acceptance Criteria

```gherkin
Scenario: Account total on card number change
  Given transaction records sorted by card number in the TRANSACT file
    And card "4000000000000001" has 3 transactions totaling +1500.00
    And card "4000000000000002" has 2 transactions totaling +800.00
  When the card number changes between consecutive records
  Then an account total line is written for card "4000000000000001" showing +1500.00
    And the account total counter resets to zero
    And processing continues with card "4000000000000002"

Scenario: Account total for the last card group at end of file
  Given the last transactions in the file belong to card "4000000000000002"
  When the end of the TRANSACT file is reached
  Then the account total for card "4000000000000002" is written
    And the account total reflects all transactions for that card
```

#### Traceability

- **Business rule:** [TRN-BR-006 Scenario 4](/docs/business-rules/transactions/trn-br-006#scenario-4-account-total-on-card-number-change)
- **COBOL source:** `CBTRN03C.cbl:158-217` — main loop with card change detection
- **Edge case:** TRN-BR-006 Edge Case 2 — final account total may not trigger if card-change detection doesn't fire at EOF; migrated system must ensure the final account total is always written
- **Regulations:** FSA FFFS 2014:5 Ch. 7

---

### US-TRN-06.4: Page totals every 20 lines

**As a** compliance officer,
**I want** page totals printed every 20 detail lines with new page headers,
**So that** the report is easy to read and verify.

#### Acceptance Criteria

```gherkin
Scenario: Page totaling every 20 lines
  Given the report has generated 20 detail lines on a page
  When the 21st detail line would be written
  Then a page total line is written with the sum of the 20 transaction amounts
    And new page headers are printed
    And the page total counter resets to zero

Scenario: Partial page at end of report
  Given the final page has fewer than 20 detail lines
  When the end of the TRANSACT file is reached
  Then a page total is written for the partial page
```

#### Traceability

- **Business rule:** [TRN-BR-006 Scenario 3](/docs/business-rules/transactions/trn-br-006#scenario-3-page-totaling-every-20-lines)
- **COBOL source:** `CBTRN03C.cbl:274-290` — write report entry with page size check; `WS-PAGE-SIZE PIC 9(03) COMP-3 VALUE 20`
- **Edge case:** TRN-BR-006 Edge Case 4 — page size of 20 may be a printer constraint rather than a business requirement; domain expert review needed
- **Regulations:** FSA FFFS 2014:5 Ch. 7

---

### US-TRN-06.5: Grand total at end of report

**As a** compliance officer,
**I want** a grand total at the end of the report (sum of all page totals),
**So that** I can verify the total daily transaction volume.

#### Acceptance Criteria

```gherkin
Scenario: Grand total at end of report
  Given the report has processed all transactions within the date range
    And page totals of +5000.00, +3000.00, and +2000.00 were accumulated
  When the end of the TRANSACT file is reached
  Then a grand total line is written showing +10000.00
    And the grand total equals the sum of all page totals

Scenario: Grand total consistency
  Given the report contains multiple pages and multiple accounts
  When the grand total is calculated
  Then Grand Total = Sum of all Page Totals (not sum of account totals)
    And the calculation uses S9(09)V99 fixed-point addition with no rounding
```

#### Traceability

- **Business rule:** [TRN-BR-006 Scenario 5](/docs/business-rules/transactions/trn-br-006#scenario-5-grand-total-at-end-of-report)
- **COBOL source:** `CBTRN03C.cbl:318-322` — grand total write
- **Totaling hierarchy:** Grand Total = Σ Page Totals; Page Total = Σ Detail Amounts (per page); Account Total = Σ Detail Amounts (per card)
- **Regulations:** FSA FFFS 2014:5 Ch. 7, AML 2017:11 Para. 3

---

### US-TRN-06.6: Proper amount formatting

**As a** compliance officer,
**I want** detail amounts formatted as `-ZZZ,ZZZ,ZZZ.ZZ` and totals as `+ZZZ,ZZZ,ZZZ.ZZ` (with comma grouping),
**So that** large amounts are readable.

#### Acceptance Criteria

```gherkin
Scenario: Amount formatting precision
  Given a transaction with amount -125.50
  When the detail line is formatted
  Then the amount is displayed as "-125.50" using format -ZZZ,ZZZ,ZZZ.ZZ

Scenario: Total amount formatting
  Given a page total of +12345.67
  When the page total line is formatted
  Then the amount is displayed as "+12,345.67" using format +ZZZ,ZZZ,ZZZ.ZZ

Scenario: No rounding during accumulation
  Given transaction amounts use S9(09)V99 precision (signed, 9 integer digits, 2 decimal)
  When amounts are accumulated into page totals, account totals, and grand total
  Then fixed-point addition is used
    And no rounding occurs at any stage
```

#### Traceability

- **Business rule:** [TRN-BR-006 Scenario 8](/docs/business-rules/transactions/trn-br-006#scenario-8-amount-formatting-precision)
- **COBOL source:** `CBTRN03C.cbl:287-289` — `ADD TRAN-AMT TO WS-PAGE-TOTAL, WS-ACCOUNT-TOTAL`
- **Precision:** `WS-PAGE-TOTAL S9(09)V99`, `WS-ACCOUNT-TOTAL S9(09)V99`, `WS-GRAND-TOTAL S9(09)V99`
- **Regulations:** FSA FFFS 2014:5 Ch. 7

---

## Report Structure

The report uses 133-character wide lines (mainframe standard: 132 columns + 1 carriage control).

```
DALYREPT    Daily Transaction Report    Date Range: YYYY-MM-DD to YYYY-MM-DD

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

### Report Line Types

| Line Type | COBOL Structure | Content |
|---|---|---|
| Header | `REPORT-NAME-HEADER` | Short name, long name, date range (115 chars) |
| Column Headers | `TRANSACTION-HEADER-1` | Column labels (114 chars) |
| Separator | `TRANSACTION-HEADER-2` | 133 dashes |
| Detail | `TRANSACTION-DETAIL-REPORT` | ID, Account, Type, Category, Source, Amount (133 chars) |
| Page Total | `REPORT-PAGE-TOTALS` | "Page Total" + dotted leader + amount (111 chars) |
| Account Total | `REPORT-ACCOUNT-TOTALS` | "Account Total" + dotted leader + amount (111 chars) |
| Grand Total | `REPORT-GRAND-TOTALS` | "Grand Total" + dotted leader + amount (111 chars) |

## File Dependencies

| File | Organization | Access | Key | Purpose |
|---|---|---|---|---|
| TRANSACT | Sequential | Sequential read | N/A | Posted transactions (input) |
| TRANREPT | Sequential | Sequential write | N/A | Report output file |
| CARDXREF | Indexed (KSDS) | Random read | Card number — `X(16)` | Card-to-account cross-reference |
| TRANTYPE | Indexed (KSDS) | Random read | Type code — `X(02)` | Transaction type descriptions |
| TRANCATG | Indexed (KSDS) | Random read | Type code + Category code — `X(02)+9(04)` | Transaction category descriptions |
| DATEPARM | Sequential | Sequential read | N/A | Date range parameters |

## Pipeline Context

This use case represents **step 3** of the nightly batch transaction processing pipeline ([TRN-BR-009](/docs/business-rules/transactions/trn-br-009)):

```
Step 1: CBTRN01C (Verify)  →  Step 2: CBTRN02C (Post)  →  Step 3: CBTRN03C (Report) ← this use case
```

The report step only executes if the posting step (CBTRN02C) completed with RC ≤ 4 (JCL `COND=(4,LT,POST)`). An ABEND in step 1 or 2 prevents report generation entirely.

### SLA Requirements

| Requirement | Target |
|---|---|
| Nightly processing complete | By 06:00 |
| Report available | Before business hours |

## Migration Considerations

### Open Questions for Domain Expert

1. **Page size**: Is the 20-line page size a business requirement or a printer constraint? Should it be configurable in the migrated system?
2. **Output format**: Should the report be generated as PDF, CSV, or another format in addition to the fixed-width text?
3. **Error tolerance**: Should lookup failures (type, category, card XREF) be fatal or should unknown codes be reported with a placeholder description and processing continue?
4. **Date filter field**: The date filter uses `TRAN-PROC-TS` (processing date), not `TRAN-ORIG-TS` (origination date). Is this the correct business intent?

### Technical Migration Notes

- **Report line width**: 133 characters — matches standard mainframe 132-column print + 1 carriage control character.
- **Amount precision**: All totals use `S9(09)V99` (signed, 9 integer + 2 decimal). Map to `decimal(11,2)` in .NET.
- **Final account total edge case**: The COBOL card-change detection may not trigger for the last card group at EOF. The migrated system must explicitly write the final account total.
- **EBCDIC to Unicode**: All text fields (merchant names, descriptions) require EBCDIC-to-UTF-8 conversion with Swedish character support (å, ä, ö).
- **Azure target**: This batch step maps to an Azure Function (timer trigger) as part of the batch pipeline orchestration. See [TRN-BR-009](/docs/business-rules/transactions/trn-br-009) for pipeline architecture decisions.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
