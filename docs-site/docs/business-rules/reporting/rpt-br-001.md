---
id: "rpt-br-001"
title: "FSA regulatory reporting structure and calendar compliance"
domain: "reporting"
cobol_source: "CBTRN03C.cbl:1-650 (report generation framework)"
requirement_id: "RPT-BR-001"
regulations:
  - "FSA FFFS 2014:5 Ch. 3"
  - "FSA FFFS 2014:5 Ch. 7"
  - "EBA Outsourcing Guidelines"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-001: FSA regulatory reporting structure and calendar compliance

## Summary

NordKredit AB is supervised by the Swedish Financial Supervisory Authority (Finansinspektionen, FSA) under FFFS 2014:5 (the FSA's regulations on governance, risk management, and control). The bank must produce and submit regulatory reports on a defined calendar — annual reports, quarterly prudential filings, monthly liquidity reports, and ad-hoc filings triggered by material events. The current mainframe system generates the underlying data extracts for these reports via batch programs (including CBTRN03C for transaction-level detail). The migrated system must preserve the ability to generate FSA-compliant data extracts on the regulatory calendar while adding structured metadata for traceability. Extracted from the CBTRN03C.cbl report generation framework and FSA FFFS 2014:5 requirements.

## Business Logic

### Pseudocode

```
FSA REGULATORY REPORTING CYCLE:

    MONTHLY (by day 15 of following month):
        Generate liquidity coverage ratio (LCR) report
        Generate large exposure report (if thresholds exceeded)
        Input: Account balances, transaction summaries, position data
        Output: Structured data extract per FSA template

    QUARTERLY (by day 30 of following quarter):
        Generate capital adequacy report
        Generate profit and loss summary
        Generate risk exposure report
        Input: End-of-quarter balance snapshots, cumulative transaction data
        Output: FSA-formatted regulatory filings

    ANNUALLY (by March 31 of following year):
        Generate annual financial statements extract
        Generate governance and risk management report data
        Generate outsourcing register report (EBA Guidelines)
        Input: Full fiscal year data, audit trail from daily reports
        Output: Complete annual regulatory package

    AD-HOC (within FSA-specified deadlines):
        Material event notifications
        Threshold breach reports
        Input: Event-triggered data
        Output: Immediate filing to FSA

DATA TRACEABILITY:
    FOR EACH regulatory report:
        Record report-id, generation-timestamp, reporting-period
        Record source-data-hash for reproducibility
        Record generator-program-version
        Retain report output for minimum 7 years (Ch. 3 record retention)
    END-FOR
```

### Decision Table

| Report Type | Frequency | Deadline | Source Data | FSA Section |
|---|---|---|---|---|
| Liquidity coverage (LCR) | Monthly | Day 15 of M+1 | Account balances, cash flows | Ch. 7 |
| Large exposures | Monthly (if triggered) | Day 15 of M+1 | Counterparty positions | Ch. 7 |
| Capital adequacy | Quarterly | Day 30 of Q+1 | Risk-weighted assets, capital | Ch. 7 |
| Profit and loss | Quarterly | Day 30 of Q+1 | Transaction summaries | Ch. 3 |
| Annual financial statements | Annually | March 31 of Y+1 | Full fiscal year data | Ch. 3 |
| Governance report | Annually | March 31 of Y+1 | Risk management data | Ch. 7 |
| Outsourcing register | Annually | March 31 of Y+1 | Service provider data | EBA Guidelines |
| Material event | Ad-hoc | Per FSA instruction | Event-specific | Ch. 7 |

## Source COBOL Reference

**Program:** `CBTRN03C.cbl` (650 lines)
**Context:** CBTRN03C is the batch report generation program that produces the daily transaction detail report — the foundational data extract from which regulatory summaries are derived. The program demonstrates the mainframe reporting pattern: sequential file reads, date-range filtering, lookup enrichment, and formatted output with multi-level totals.

```cobol
000155       1000-DAILYTRAN-OPEN.
000156           OPEN INPUT  TRANSACT-FILE
000157                OUTPUT REPORT-FILE
000158                INPUT  CARD-XREF-FILE
000159                INPUT  TRAN-TYPE-FILE
000160                INPUT  TRAN-CAT-FILE
000161                INPUT  DATE-PARMS-FILE
```

The regulatory reporting framework in the mainframe system follows the same pattern as CBTRN03C:
- Open source files and output file
- Read parameters (reporting period, entity, regulatory template ID)
- Sequential processing of source records
- Lookup enrichment from reference data files
- Formatted output with control break totals
- Deterministic error handling (ABEND on data quality issues)

**Note:** Dedicated FSA reporting COBOL programs are not yet available in the repository. The reporting pattern is inferred from CBTRN03C and the regulatory framework requirements. The mainframe team must provide any FSA-specific batch programs for complete extraction.

## Acceptance Criteria

### Scenario 1: Monthly liquidity report generation

```gherkin
GIVEN the regulatory calendar requires a monthly LCR report
  AND the reporting period is January 2026
WHEN the batch reporting program runs on or before February 15, 2026
THEN a liquidity coverage ratio data extract is generated
  AND the extract covers all account balances as of January 31, 2026
  AND the report includes a generation timestamp and reporting period metadata
  AND the report is retained for a minimum of 7 years
```

### Scenario 2: Quarterly capital adequacy report

```gherkin
GIVEN the regulatory calendar requires a quarterly capital report
  AND the reporting period is Q4 2025
WHEN the batch reporting program runs on or before January 30, 2026
THEN a capital adequacy data extract is generated
  AND risk-weighted assets are calculated from end-of-quarter positions
  AND the report is formatted per FSA template requirements
```

### Scenario 3: Annual financial statements data extract

```gherkin
GIVEN the fiscal year 2025 has ended
WHEN the annual reporting batch runs before March 31, 2026
THEN a complete financial statements data extract is generated
  AND the extract includes full-year transaction summaries from daily reports
  AND the extract is reconcilable to the sum of monthly/quarterly reports
  AND an audit trail links each line item to source daily reports
```

### Scenario 4: Report reproducibility

```gherkin
GIVEN a regulatory report was generated for a specific period
WHEN the same report is regenerated with the same parameters
THEN the output is identical to the original
  AND the source data hash matches the original generation
```

### Scenario 5: Regulatory calendar deadline enforcement

```gherkin
GIVEN a report is due on a specific date per the FSA calendar
WHEN the deadline date arrives
  AND the report has not been generated
THEN the system raises an alert to the compliance team
  AND the overdue report is flagged in the reporting dashboard
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 3 | Accounting records must be accurate, complete, and retained for minimum periods | Report generation includes metadata, source traceability, and 7-year retention requirement |
| FSA FFFS 2014:5 | Ch. 7 | Institutions must have adequate systems for financial reporting and regulatory filings | Structured batch reporting with calendar-driven scheduling ensures timely FSA submissions |
| EBA Outsourcing Guidelines | Section 13 | Outsourcing register must be maintained and reported to supervisory authorities | Annual outsourcing register report included in regulatory reporting calendar |

## Edge Cases

1. **Regulatory calendar changes**: The FSA may change reporting deadlines or introduce new report types. The migrated system must support configurable reporting calendars without code changes. The current mainframe system handles this via JCL schedule changes — the migrated system should use Azure configuration or a regulatory calendar table.

2. **Restatement of prior periods**: If errors are discovered in a previously submitted report, a corrected version must be generated and submitted. The system must support regeneration of historical reports with corrected data while preserving the original for audit purposes.

3. **Entity-level reporting**: NordKredit may have subsidiary entities that require separate or consolidated FSA reporting. The current mainframe processes all accounts as a single entity. The migrated system should support entity-level filtering.

4. **FSA format changes**: The FSA periodically updates reporting templates (XBRL, CSV, XML formats). The mainframe generates fixed-width text extracts that are manually reformatted. The migrated system should support configurable output formats.

5. **Cross-period transactions**: Transactions posted near period boundaries (e.g., December 31 vs. January 1) must be consistently assigned to the correct reporting period. The COBOL date filter uses processing timestamp (TRAN-PROC-TS), not origination date — this convention must be preserved.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Which specific FSA report templates does NordKredit currently submit? (2) Are there dedicated COBOL programs for each FSA report, or are they derived from the daily transaction report (CBTRN03C) output? (3) What is the current process for reformatting mainframe extracts into FSA submission format? (4) Does NordKredit report as a single entity or are there subsidiary entities requiring separate filings? (5) What is the retention period enforced in the current system — is it 7 years per Ch. 3 or longer?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
