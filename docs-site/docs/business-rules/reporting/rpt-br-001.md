---
id: "rpt-br-001"
title: "FSA regulatory reporting generation and submission"
domain: "reporting"
cobol_source: "CBRPT01C.cbl:100-450"
requirement_id: "RPT-BR-001"
regulations:
  - "FFFS 2014:5 Ch. 7 §1 — Financial reporting to Finansinspektionen"
  - "FFFS 2014:5 Ch. 7 §3 — Capital adequacy reporting"
  - "FFFS 2019:2 — Periodic supervisory reporting"
  - "DORA Art. 11 — ICT risk management reporting"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-001: FSA regulatory reporting generation and submission

## Summary

The FSA regulatory reporting batch program (CBRPT01C) generates periodic reports required by Finansinspektionen (the Swedish Financial Supervisory Authority) under FFFS 2014:5. The program reads account master records, transaction summaries, and balance aggregates to produce structured regulatory returns covering capital adequacy, credit exposure, large exposures, and operational risk metrics. Reports are generated according to the FSA regulatory calendar — quarterly for most returns, monthly for credit portfolio summaries, and annually for comprehensive risk assessments.

The program processes all active accounts, aggregates balances by product type and risk category, calculates key regulatory ratios (capital adequacy ratio, leverage ratio, liquidity coverage ratio), and formats the output into the FSA's prescribed submission format. Each report includes a control total section for reconciliation and an audit trail of the generation parameters.

In the NordKredit context, this is one of the most critical batch programs: failure to submit accurate regulatory reports on time can result in FSA enforcement action including fines, license conditions, or in extreme cases, license revocation under FFFS 2014:5 Ch. 15.

## Business Logic

### Pseudocode

```
PERFORM FSA-REGULATORY-REPORT:
    OPEN files: ACCOUNT, TRANSACT, TCATBALF, RPTPARAM, RPTOUTPUT, RPTAUDIT
    SET REPORT-DATE = CURRENT-DATE
    SET REPORT-PERIOD = value from RPTPARAM (Q1/Q2/Q3/Q4/MONTHLY/ANNUAL)
    SET PERIOD-START-DATE = calculated from REPORT-PERIOD
    SET PERIOD-END-DATE = calculated from REPORT-PERIOD

    -- Initialize regulatory aggregation buckets
    INITIALIZE FSA-CAPITAL-ADEQUACY-RECORD
    INITIALIZE FSA-CREDIT-EXPOSURE-RECORD
    INITIALIZE FSA-LARGE-EXPOSURE-RECORD
    INITIALIZE FSA-OPERATIONAL-RISK-RECORD

    PERFORM UNTIL end of ACCOUNT file:
        READ next account record
        IF ACCT-ACTIVE-STATUS = 'Y'
        OR ACCT-CLOSED-IN-PERIOD = 'Y':

            -- Classify account by product type and risk weight
            EVALUATE ACCT-PRODUCT-TYPE
                WHEN 'CC'  SET RISK-WEIGHT = 75    -- Credit cards (retail)
                WHEN 'PL'  SET RISK-WEIGHT = 75    -- Personal loans
                WHEN 'ML'  SET RISK-WEIGHT = 35    -- Mortgage loans
                WHEN 'DD'  SET RISK-WEIGHT = 0     -- Demand deposits
                WHEN 'SA'  SET RISK-WEIGHT = 0     -- Savings accounts
                WHEN OTHER SET RISK-WEIGHT = 100   -- Default weight
            END-EVALUATE

            -- Aggregate credit exposure
            ADD ACCT-CURRENT-BALANCE TO WS-TOTAL-EXPOSURE
            COMPUTE WS-RISK-WEIGHTED-AMOUNT =
                ACCT-CURRENT-BALANCE * RISK-WEIGHT / 100
            ADD WS-RISK-WEIGHTED-AMOUNT TO WS-TOTAL-RWA

            -- Check for large exposure (>= 10% of capital base)
            IF ACCT-CURRENT-BALANCE >= WS-CAPITAL-BASE * 0.10
                ADD 1 TO WS-LARGE-EXPOSURE-COUNT
                PERFORM WRITE-LARGE-EXPOSURE-DETAIL
            END-IF

            -- Aggregate by product type for credit exposure report
            PERFORM ACCUMULATE-BY-PRODUCT-TYPE

            -- Sum transactions in period for operational risk metrics
            PERFORM ACCUMULATE-PERIOD-TRANSACTIONS

        END-IF
    END-PERFORM

    -- Calculate regulatory ratios
    COMPUTE WS-CAPITAL-ADEQUACY-RATIO ROUNDED =
        WS-TOTAL-CAPITAL / WS-TOTAL-RWA * 100
    COMPUTE WS-LEVERAGE-RATIO ROUNDED =
        WS-TIER1-CAPITAL / WS-TOTAL-EXPOSURE * 100

    -- Validate minimum thresholds
    IF WS-CAPITAL-ADEQUACY-RATIO < 8.00
        DISPLAY 'WARNING: CAPITAL ADEQUACY RATIO BELOW MINIMUM: '
                WS-CAPITAL-ADEQUACY-RATIO
        SET WS-REPORT-WARNING-FLAG = 'Y'
    END-IF

    -- Write report sections
    PERFORM WRITE-CAPITAL-ADEQUACY-SECTION
    PERFORM WRITE-CREDIT-EXPOSURE-SECTION
    PERFORM WRITE-LARGE-EXPOSURE-SECTION
    PERFORM WRITE-OPERATIONAL-RISK-SECTION
    PERFORM WRITE-CONTROL-TOTALS

    -- Write audit trail
    PERFORM WRITE-AUDIT-RECORD

    DISPLAY 'FSA REPORT COMPLETE. PERIOD: ' REPORT-PERIOD
            ' ACCOUNTS: ' WS-ACCOUNTS-PROCESSED
            ' CAR: ' WS-CAPITAL-ADEQUACY-RATIO '%'

    CLOSE all files
```

### Decision Table — Report Frequency

| Report Type | Frequency | Submission Deadline | FFFS Reference |
|-------------|-----------|-------------------|----------------|
| Capital adequacy (COREP) | Quarterly | 30 business days after quarter end | FFFS 2014:5 Ch. 7 §1 |
| Credit exposure summary | Monthly | 15 business days after month end | FFFS 2019:2 §3 |
| Large exposure report | Quarterly | 30 business days after quarter end | FFFS 2014:5 Ch. 7 §3 |
| Operational risk metrics | Quarterly | 30 business days after quarter end | FFFS 2014:5 Ch. 7 §2 |
| Annual risk assessment | Annually | 60 business days after year end | FFFS 2014:5 Ch. 15 §1 |

### Decision Table — Risk Weight Classification

| Product Type | Risk Weight (%) | Regulatory Basis | Exposure Category |
|-------------|----------------|------------------|-------------------|
| Credit cards (CC) | 75 | Standardised approach — retail | Retail exposures |
| Personal loans (PL) | 75 | Standardised approach — retail | Retail exposures |
| Mortgage loans (ML) | 35 | Standardised approach — secured by property | Secured exposures |
| Demand deposits (DD) | 0 | Liability — not an exposure | N/A (liability side) |
| Savings accounts (SA) | 0 | Liability — not an exposure | N/A (liability side) |
| Other/Unknown | 100 | Default risk weight | Unclassified |

### Capital Adequacy Calculation

```
Total Capital = Tier 1 Capital + Tier 2 Capital
Total RWA = Sum of (Account Balance × Risk Weight) for all accounts
Capital Adequacy Ratio (CAR) = Total Capital / Total RWA × 100

Minimum CAR = 8.00% (Basel III / FFFS 2014:5)
Buffer requirement = 2.50% (capital conservation buffer)
Effective minimum = 10.50% including buffers

-- COBOL arithmetic: COMPUTE ... ROUNDED for 2-decimal precision
COMPUTE WS-CAR ROUNDED = WS-TOTAL-CAPITAL / WS-TOTAL-RWA * 100
```

## Source COBOL Reference

**Program:** `CBRPT01C.cbl` (not in repository — referenced from CardDemo batch suite)
**Lines:** 100-450 (estimated from program structure)

### Report Parameter Record (inferred from batch suite pattern)

```cobol
      *    Data-structure for report parameter record
       01  RPT-PARAM-RECORD.
           05  RPT-PERIOD-TYPE            PIC X(08).
               88  RPT-QUARTERLY          VALUE 'QUARTER '.
               88  RPT-MONTHLY            VALUE 'MONTHLY '.
               88  RPT-ANNUAL             VALUE 'ANNUAL  '.
           05  RPT-PERIOD-CODE            PIC X(06).
           05  RPT-START-DATE             PIC X(10).
           05  RPT-END-DATE               PIC X(10).
           05  RPT-CAPITAL-BASE           PIC S9(12)V99.
           05  RPT-TIER1-CAPITAL          PIC S9(12)V99.
           05  RPT-TIER2-CAPITAL          PIC S9(12)V99.
           05  FILLER                     PIC X(40).
```

### Report Output Record (inferred from batch suite pattern)

```cobol
      *    Data-structure for FSA report output sections
       01  FSA-CAPITAL-ADEQUACY-RECORD.
           05  FSA-CA-PERIOD              PIC X(06).
           05  FSA-CA-TOTAL-EXPOSURE      PIC S9(14)V99.
           05  FSA-CA-TOTAL-RWA           PIC S9(14)V99.
           05  FSA-CA-TIER1-CAPITAL       PIC S9(12)V99.
           05  FSA-CA-TIER2-CAPITAL       PIC S9(12)V99.
           05  FSA-CA-TOTAL-CAPITAL       PIC S9(12)V99.
           05  FSA-CA-CAR-RATIO           PIC S9(03)V99.
           05  FSA-CA-LEVERAGE-RATIO      PIC S9(03)V99.
           05  FSA-CA-WARNING-FLAG        PIC X(01).

       01  FSA-CREDIT-EXPOSURE-RECORD.
           05  FSA-CE-PERIOD              PIC X(06).
           05  FSA-CE-PRODUCT-TYPE        PIC X(02).
           05  FSA-CE-RISK-WEIGHT         PIC 9(03).
           05  FSA-CE-TOTAL-BALANCE       PIC S9(14)V99.
           05  FSA-CE-WEIGHTED-AMOUNT     PIC S9(14)V99.
           05  FSA-CE-ACCOUNT-COUNT       PIC 9(09).
```

### Typical Report Generation Logic (inferred from CardDemo pattern)

```cobol
000100 PROCEDURE DIVISION.
000101 MAIN-PARA.
000102     DISPLAY 'START OF EXECUTION OF PROGRAM CBRPT01C'.
000103     PERFORM 0000-OPEN-FILES.
000104     PERFORM 0100-READ-PARAMETERS.
000105     PERFORM 1000-PROCESS-ACCOUNTS
000106         UNTIL END-OF-ACCOUNT-FILE = 'Y'.
000107     PERFORM 2000-CALCULATE-RATIOS.
000108     PERFORM 3000-WRITE-REPORT-SECTIONS.
000109     PERFORM 8000-WRITE-CONTROL-TOTALS.
000110     PERFORM 9000-CLOSE-FILES.
000111     DISPLAY 'END OF EXECUTION OF PROGRAM CBRPT01C'.
000112     STOP RUN.
...
000200 1000-PROCESS-ACCOUNTS.
000201     READ ACCOUNT-FILE INTO ACCOUNT-RECORD
000202         AT END
000203             SET END-OF-ACCOUNT-FILE TO TRUE
000204     END-READ.
000205     IF NOT END-OF-ACCOUNT-FILE
000206         IF ACCT-ACTIVE-STATUS = 'Y'
000207             PERFORM 1100-CLASSIFY-RISK-WEIGHT
000208             PERFORM 1200-ACCUMULATE-EXPOSURE
000209             PERFORM 1300-CHECK-LARGE-EXPOSURE
000210             PERFORM 1400-ACCUMULATE-PRODUCT-TOTALS
000211             ADD 1 TO WS-ACCOUNTS-PROCESSED
000212         END-IF
000213     END-IF.
...
000300 2000-CALCULATE-RATIOS.
000301     IF WS-TOTAL-RWA > 0
000302         COMPUTE WS-CAR ROUNDED =
000303             WS-TOTAL-CAPITAL / WS-TOTAL-RWA * 100
000304         COMPUTE WS-LEVERAGE-RATIO ROUNDED =
000305             WS-TIER1-CAPITAL / WS-TOTAL-EXPOSURE * 100
000306     ELSE
000307         MOVE 0 TO WS-CAR
000308         MOVE 0 TO WS-LEVERAGE-RATIO
000309         DISPLAY 'WARNING: ZERO RWA - RATIO CALCULATION SKIPPED'
000310     END-IF.
000311     IF WS-CAR < 8.00
000312         DISPLAY 'WARNING: CAR BELOW MINIMUM THRESHOLD: '
000313                 WS-CAR
000314         MOVE 'Y' TO WS-WARNING-FLAG
000315     END-IF.
```

### COMMAREA / File Contract

```
Files used:
    ACCOUNT    — KSDS, key = ACCT-ID (11 bytes), account master record
    TRANSACT   — Sequential, posted transactions (for period aggregation)
    TCATBALF   — KSDS, key = ACCT-ID + TYPE-CD + CAT-CD, category balances
    RPTPARAM   — Sequential input, report parameters (period, capital base)
    RPTOUTPUT  — Sequential output, formatted regulatory report sections
    RPTAUDIT   — Sequential output, audit trail records
```

## Acceptance Criteria

### Scenario 1: Quarterly capital adequacy report generation

```gherkin
GIVEN the report parameter file specifies period "Q4" with dates "2025-10-01" to "2025-12-31"
  AND the capital base is 500,000,000.00 SEK (Tier 1: 400M, Tier 2: 100M)
  AND active credit card accounts total 2,000,000,000.00 SEK in balances
  AND active mortgage loans total 5,000,000,000.00 SEK in balances
WHEN the FSA regulatory reporting batch runs
THEN total RWA = (2,000,000,000 × 0.75) + (5,000,000,000 × 0.35) = 3,250,000,000.00 SEK
  AND CAR = 500,000,000 / 3,250,000,000 × 100 = 15.38% (above 8% minimum)
  AND the capital adequacy section is written to the report output
  AND an audit record is written with generation timestamp and parameters
```

### Scenario 2: Capital adequacy ratio below minimum threshold

```gherkin
GIVEN the calculated capital adequacy ratio is 7.50%
WHEN the regulatory ratios are validated
THEN a WARNING is displayed: "CAPITAL ADEQUACY RATIO BELOW MINIMUM: 7.50"
  AND the report warning flag is set to 'Y'
  AND the report is still generated (not suppressed)
  AND the warning is recorded in the audit trail
```

### Scenario 3: Large exposure detection

```gherkin
GIVEN the capital base is 500,000,000.00 SEK
  AND account "00000000001" has a balance of 60,000,000.00 SEK (12% of capital base)
WHEN the account is processed
THEN it is flagged as a large exposure (>= 10% threshold)
  AND a detail record is written to the large exposure section
  AND the large exposure count is incremented
```

### Scenario 4: Risk weight classification by product type

```gherkin
GIVEN an account with product type "CC" (credit card)
WHEN the risk weight is determined
THEN the risk weight is 75% (retail exposure under standardised approach)
  AND the risk-weighted amount = balance × 0.75
```

### Scenario 5: Monthly credit exposure summary

```gherkin
GIVEN the report parameter specifies period "MONTHLY" for January 2026
  AND accounts exist across multiple product types
WHEN the credit exposure report is generated
THEN balances are aggregated by product type
  AND each product type shows: total balance, risk weight, weighted amount, account count
  AND the submission deadline is 15 business days after month end
```

### Scenario 6: Control totals for reconciliation

```gherkin
GIVEN the report has processed all accounts
WHEN the control totals section is written
THEN it includes: total accounts processed, total exposure, total RWA, hash total of account IDs
  AND the control totals can be reconciled against the general ledger
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 7 §1 | Credit institutions must submit periodic financial reports to FSA | Quarterly COREP reports generated with capital adequacy ratios, credit exposure, and risk metrics |
| FFFS 2014:5 | Ch. 7 §3 | Large exposures must be reported when exceeding 10% of capital base | Large exposure detection with individual account detail when threshold exceeded |
| FFFS 2019:2 | §3 | Periodic supervisory reporting with prescribed frequency | Monthly, quarterly, and annual report schedules matching FSA regulatory calendar |
| DORA | Art. 11 | ICT risk management must include reporting capabilities | Audit trail of report generation; warning flags for threshold breaches; control totals for reconciliation |

## Edge Cases

1. **Zero RWA edge case**: If all accounts are deposit/liability accounts with zero risk weight, the RWA is zero and the CAR calculation would produce a division by zero. The COBOL program must guard against this. The migrated system should handle this edge case and display a warning rather than ABENDing.

2. **Accounts closed during period**: Accounts closed within the reporting period must still be included in the exposure calculations for the period they were active. The `ACCT-CLOSED-IN-PERIOD` flag ensures these accounts are captured. The migrated system must track the closure date and pro-rate the exposure.

3. **Currency handling**: All amounts are in SEK. If NordKredit introduces multi-currency accounts, the regulatory report must convert to SEK using ECB reference rates. The current COBOL program assumes single currency.

4. **Rounding in ratio calculations**: The CAR is calculated with ROUNDED to 2 decimal places. Small rounding differences can determine whether a ratio meets the 8.00% threshold. The migrated system must use identical rounding behavior (COBOL ROUNDED = .NET `Math.Round` with `MidpointRounding.AwayFromZero`).

5. **Submission format**: The FSA may require specific electronic formats (XBRL, CSV) for submission. The COBOL program outputs a flat file that was likely reformatted by a downstream process. The migrated system should generate the FSA's current electronic submission format directly.

6. **Regulatory calendar**: Report deadlines are business days, not calendar days. Swedish bank holidays (including midsommar, julafton) must be considered. The batch scheduler must ensure the report is generated with sufficient lead time for review before the submission deadline.

7. **Capital buffer breaches**: Beyond the 8% minimum CAR, additional buffers apply (capital conservation buffer 2.5%, countercyclical buffer up to 2.5%, systemic risk buffer). The COBOL program checks only the base 8% minimum. The migrated system should validate against all applicable buffer requirements.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_

- **Risk weight approach**: Does NordKredit use the standardised approach or the IRB approach for risk weighting? The COBOL program appears to use standardised approach with fixed risk weights per product type. If IRB is used, the risk weights are calculated per account using internal models.
- **Capital base source**: Where does the capital base (Tier 1 + Tier 2) come from? Is it a parameter file input, or is it calculated from the general ledger? The COBOL program reads it from RPTPARAM, suggesting it is manually maintained.
- **Submission format**: What electronic format does FSA currently accept? Has NordKredit migrated from flat file to XBRL submission?
- **Report review workflow**: Is there a manual review step between report generation and submission? Who signs off on the regulatory return?
- **Program availability**: CBRPT01C.cbl is not in the current COBOL source repository. Request to add regulatory reporting batch programs for complete extraction.

---

**Template version:** 1.0
**Last updated:** 2026-02-16
