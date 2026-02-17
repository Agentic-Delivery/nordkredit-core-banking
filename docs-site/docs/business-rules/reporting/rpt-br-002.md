---
id: "rpt-br-002"
title: "AML/KYC screening report generation"
domain: "reporting"
cobol_source: "CBTRN03C.cbl:158-374 (report generation pattern), CBTRN02C.cbl:1-723 (transaction posting source)"
requirement_id: "RPT-BR-002"
regulations:
  - "AML 2017:11 Para. 3"
  - "AML 2017:11 Para. 4"
  - "FSA FFFS 2014:5 Ch. 7"
  - "GDPR Art. 5"
  - "PSD2 Art. 97"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-002: AML/KYC screening report generation

## Summary

NordKredit AB must generate nightly AML (Anti-Money Laundering) and KYC (Know Your Customer) screening reports as required by Swedish AML law (Lag 2017:630, implementing the EU 4th and 5th Anti-Money Laundering Directives) and FSA regulations. The nightly batch window includes AML screening that processes the day's posted transactions (output of CBTRN02C) against risk rules and watchlists. The screening report identifies transactions that exceed threshold amounts, match suspicious patterns, or involve flagged counterparties. The daily transaction detail report (CBTRN03C) provides the foundational transaction data, while the AML screening applies additional risk-based filters and generates Suspicious Activity Report (SAR) candidates. Extracted from the batch pipeline framework (CBTRN02C/CBTRN03C) and Swedish AML regulatory requirements.

## Business Logic

### Pseudocode

```
AML/KYC NIGHTLY SCREENING BATCH:

    PRECONDITION: Daily transaction posting (CBTRN02C) has completed successfully

    INPUT FILES:
        TRANSACT    - Posted transactions (output of CBTRN02C)
        ACCTFILE    - Account master (customer risk rating, KYC status)
        CUSTFILE    - Customer master (PEP flag, sanctions list match)
        WATCHLIST   - External sanctions/watchlist reference file
        DATEPARM    - Date range for screening period

    OUTPUT FILES:
        AMLREPT     - AML screening report (structured output)
        SARALERT    - SAR candidate alerts for compliance review

    PERFORM SCREENING:
        Read date parameters (same pattern as CBTRN03C 0550-DATEPARM-READ)

        FOR EACH posted transaction in date range:
            1. THRESHOLD CHECK:
               IF TRAN-AMT >= 150000 SEK (single transaction threshold)
                   Flag as reportable (Penningtvattslag 3 kap. 4 §)
               END-IF

            2. CUMULATIVE CHECK:
               Accumulate daily total per account
               IF daily-account-total >= 150000 SEK
                   Flag account for structuring review
               END-IF

            3. PATTERN CHECK:
               IF transaction matches suspicious pattern rules:
                   - Multiple transactions just below threshold
                   - Unusual transaction frequency for account profile
                   - Cross-border transactions to high-risk jurisdictions
                   Flag for SAR review
               END-IF

            4. WATCHLIST CHECK:
               Look up counterparty against WATCHLIST file
               IF match found:
                   Flag as sanctions match (immediate escalation)
               END-IF

            5. CUSTOMER RISK CHECK:
               Look up account in ACCTFILE for risk rating
               IF risk-rating = 'HIGH' AND TRAN-AMT >= 50000 SEK
                   Flag for enhanced monitoring
               END-IF
        END-FOR

        GENERATE REPORT:
            Write header with screening date and parameter summary
            FOR EACH flagged transaction:
                Write detail line with flag reason, amount, account, customer
            END-FOR
            Write summary totals:
                - Total transactions screened
                - Total flagged for threshold
                - Total flagged for patterns
                - Total watchlist matches
                - Total SAR candidates

    SLA: Screening must complete by 06:00 (same batch window as TRN-BR-009)
```

### Decision Table

| Transaction Amount (SEK) | Customer Risk | Watchlist Match | Pattern Flag | Action |
|---|---|---|---|---|
| >= 150,000 | Any | No | No | Threshold report (mandatory) |
| >= 150,000 | Any | Yes | Any | Sanctions alert + threshold report |
| < 150,000 | High | No | No | Enhanced monitoring report |
| < 150,000 | Any | Yes | Any | Sanctions alert (immediate) |
| Any | Any | No | Yes | SAR candidate for review |
| Cumulative daily >= 150,000 | Any | No | No | Structuring review flag |
| < 50,000 | Low/Medium | No | No | No action (standard processing) |

### AML Threshold Values

| Threshold | Amount (SEK) | Regulation | Purpose |
|---|---|---|---|
| Single transaction reporting | 150,000 | Penningtvattslag 3 kap. 4 § | Mandatory report to Finanspolisen |
| Enhanced monitoring trigger (high-risk) | 50,000 | AML 2017:11 Para. 3 | Enhanced due diligence threshold |
| Structuring detection (cumulative) | 150,000 | AML 2017:11 Para. 4 | Detect split transactions |

## Source COBOL Reference

**Programs:** `CBTRN02C.cbl` (723 lines — transaction posting, provides input data), `CBTRN03C.cbl` (650 lines — report generation pattern)

The AML screening batch follows the same file I/O and reporting patterns established in CBTRN03C:

```cobol
000173                IF TRAN-PROC-TS (1:10) >= WS-START-DATE
000174                   AND TRAN-PROC-TS (1:10) <= WS-END-DATE
000175                   CONTINUE
000176                ELSE
000177                   NEXT SENTENCE
000178                END-IF
```

Date-range filtering pattern (CBTRN03C:173-178) is reused for AML screening period selection.

```cobol
000287           ADD TRAN-AMT TO WS-PAGE-TOTAL
000288                           WS-ACCOUNT-TOTAL
```

Amount accumulation pattern (CBTRN03C:287-289) is reused for threshold accumulation per account.

**Note:** The dedicated AML screening COBOL programs are not yet available in the repository. The screening logic is inferred from the batch pipeline framework (TRN-BR-009) and Swedish AML regulatory requirements. The mainframe team must provide the AML-specific batch programs for complete extraction.

## Acceptance Criteria

### Scenario 1: Single transaction threshold detection

```gherkin
GIVEN a posted transaction with amount 200,000 SEK
  AND the screening batch runs for the current date
WHEN the transaction is evaluated against AML thresholds
THEN the transaction is flagged as a threshold report candidate
  AND the flag reason is "Single transaction >= 150,000 SEK"
  AND the transaction appears in the AMLREPT output file
```

### Scenario 2: Cumulative daily structuring detection

```gherkin
GIVEN account "12345678901" has 4 posted transactions today
  AND each transaction is 40,000 SEK (total 160,000 SEK)
  AND no single transaction exceeds 150,000 SEK
WHEN the screening batch accumulates daily totals per account
THEN account "12345678901" is flagged for structuring review
  AND the flag reason is "Cumulative daily total >= 150,000 SEK"
  AND all 4 transactions appear in the screening report
```

### Scenario 3: Watchlist match escalation

```gherkin
GIVEN a posted transaction involves counterparty "EXAMPLE CORP"
  AND "EXAMPLE CORP" appears on the sanctions watchlist file
WHEN the screening batch checks the watchlist
THEN the transaction is flagged as a sanctions match
  AND an immediate alert record is written to SARALERT
  AND the compliance team is notified for urgent review
```

### Scenario 4: Enhanced monitoring for high-risk customers

```gherkin
GIVEN account "98765432109" has customer risk rating "HIGH"
  AND a transaction of 75,000 SEK is posted to this account
WHEN the screening batch evaluates risk-based thresholds
THEN the transaction is flagged for enhanced monitoring
  AND the flag reason is "High-risk customer, amount >= 50,000 SEK"
```

### Scenario 5: AML screening report summary

```gherkin
GIVEN the nightly screening batch has processed all transactions for 2026-01-15
WHEN the screening report is finalized
THEN the report summary shows:
  | Metric | Value |
  | Total transactions screened | 15,234 |
  | Threshold flags | 12 |
  | Pattern flags | 45 |
  | Watchlist matches | 0 |
  | SAR candidates | 8 |
  AND the report is available before 06:00
```

### Scenario 6: GDPR-compliant data handling in AML reports

```gherkin
GIVEN the AML screening report contains customer PII (names, account numbers)
WHEN the report is generated and stored
THEN access to the report is restricted to authorized compliance personnel
  AND the report is stored within EU data residency boundaries
  AND the retention period follows AML record-keeping requirements (5 years minimum)
  AND customer data is not exposed beyond what is necessary for AML purposes
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| AML 2017:11 | Para. 3 | Ongoing transaction monitoring and suspicious activity reporting | Nightly batch screening of all posted transactions against risk rules and thresholds |
| AML 2017:11 | Para. 4 | Detection of structuring (split transactions to avoid thresholds) | Cumulative daily amount tracking per account detects transactions split below the 150,000 SEK threshold |
| FSA FFFS 2014:5 | Ch. 7 | Adequate systems for risk monitoring and regulatory reporting | Structured AML screening integrated into the nightly batch pipeline with defined SLAs |
| GDPR | Art. 5 | Data minimization and purpose limitation | AML reports contain only data necessary for compliance purposes; access restricted to authorized personnel |
| PSD2 | Art. 97 | Transaction security and monitoring | AML screening provides an additional layer of transaction integrity verification |

## Edge Cases

1. **Threshold currency conversion**: The 150,000 SEK threshold applies to SEK amounts. For transactions in foreign currencies (EUR, USD, etc.), the screening must apply the equivalent threshold using the daily exchange rate. The current mainframe system processes all amounts in SEK. The migrated system must handle multi-currency threshold conversion.

2. **Cross-border transaction identification**: The COBOL transaction record (TRAN-SOURCE field in CBTRN03C) identifies the transaction source but not the geographic origin. High-risk jurisdiction detection requires additional data enrichment not present in the current batch pipeline. The migrated system should integrate with a country risk classification service.

3. **False positive management**: The screening may generate high volumes of false positives, especially for pattern-based rules. The current mainframe outputs all flags for manual review. The migrated system should support risk scoring and prioritization of alerts.

4. **Retrospective screening**: Regulatory investigations may require re-screening historical transactions with updated watchlists. The system must support parameterized re-runs for arbitrary date ranges without affecting current-day screening.

5. **SAR filing workflow**: Flagged transactions are candidates for Suspicious Activity Reports filed with Finanspolisen (Swedish Financial Intelligence Unit). The current system generates the data extract; the actual filing is manual. The migrated system should consider workflow automation for SAR preparation and submission.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Is there a dedicated AML screening COBOL batch program, or is the screening performed externally using CBTRN03C output? (2) What are the exact pattern rules used for suspicious activity detection? (3) How is the watchlist file updated — daily download from a sanctions provider? (4) What is the current SAR filing process with Finanspolisen? (5) Are threshold amounts hard-coded or configurable in the current system? (6) How does the current system handle multi-currency transactions for threshold comparison?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
