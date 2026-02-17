---
id: "rpt-br-005"
title: "Internal management reporting and analytics"
domain: "reporting"
cobol_source: "CBTRN03C.cbl:1-650 (report generation framework), CBTRN02C.cbl:1-723 (transaction and balance data)"
requirement_id: "RPT-BR-005"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "FSA FFFS 2014:5 Ch. 3"
  - "DORA Art. 11"
status: "extracted"
validated_by: null
validated_date: null
priority: "medium"
---

# RPT-BR-005: Internal management reporting and analytics

## Summary

NordKredit AB generates internal management reports for business decision-making, portfolio monitoring, and operational oversight. These reports aggregate data from the daily transaction processing pipeline (CBTRN02C/CBTRN03C) and account master files to provide management with key performance indicators, portfolio health metrics, and operational statistics. The mainframe batch system produces several management report extracts during the nightly batch window, following the same file processing patterns as CBTRN03C. Management reports are not directly submitted to regulators but support FSA governance requirements (FFFS 2014:5 Ch. 7) for adequate management information systems. Extracted from the batch pipeline framework and FSA governance requirements.

## Business Logic

### Pseudocode

```
INTERNAL MANAGEMENT REPORTING BATCH:

    PRECONDITION: Daily transaction posting (CBTRN02C) has completed

    REPORT 1 — DAILY OPERATIONS SUMMARY:
        Input: TRANSACT, DALYREJS (rejects from CBTRN02C)
        FOR the current processing date:
            Count total transactions processed
            Count total transactions rejected (from DALYREJS)
            Calculate acceptance rate (processed / total)
            Sum total transaction volume (SEK)
            Calculate average transaction amount
            Count unique accounts with activity
        END-FOR
        Output: Operations summary with metrics

    REPORT 2 — PORTFOLIO BALANCE REPORT:
        Input: ACCTFILE (account master)
        FOR EACH account:
            Read current balance, credit limit, status
            Accumulate totals by account status (active/suspended/closed)
            Calculate portfolio utilization (total balance / total credit limit)
            Identify accounts over credit limit
            Identify accounts with zero balance
        END-FOR
        Output: Portfolio summary with balance distribution

    REPORT 3 — DELINQUENCY AND AGING REPORT:
        Input: ACCTFILE, BILLFILE (billing records)
        FOR EACH account with balance > 0:
            Calculate days past due (current date - last payment date)
            Classify into aging buckets:
                Current (0 days)
                30 days past due
                60 days past due
                90 days past due
                120+ days past due
            Accumulate balance per bucket
        END-FOR
        Output: Aging distribution with balances and counts

    REPORT 4 — TRANSACTION CATEGORY ANALYSIS:
        Input: TRANSACT, TCATBALF (category balances from CBTRN02C)
        FOR EACH transaction category:
            Sum transaction count and volume
            Calculate percentage of total
            Compare to previous period
        END-FOR
        Output: Category breakdown with trends

    SLA: Management reports generated after daily report (CBTRN03C)
         Available before 08:00 for morning management review
```

### Management Report Metrics

| Report | Key Metrics | Frequency | Audience |
|---|---|---|---|
| Daily Operations | Transaction count, reject rate, volume, average amount | Daily | Operations manager |
| Portfolio Balance | Total balance, utilization, overlimit count | Daily | Risk manager |
| Delinquency Aging | Aging buckets (current/30/60/90/120+), balance per bucket | Daily | Credit risk, Collections |
| Category Analysis | Volume by category, trends, top categories | Weekly | Product management |
| Monthly Executive | All above aggregated, month-over-month comparison | Monthly | Executive committee |

### Decision Table

| Metric | Threshold | Action |
|---|---|---|
| Daily reject rate > 5% | Warning | Operations alert for investigation |
| Portfolio utilization > 80% | Warning | Risk management review |
| Accounts overlimit > 100 | Critical | Credit policy review escalation |
| 90+ days past due balance > 5M SEK | Critical | Board-level reporting trigger |
| Daily volume deviation > 20% from average | Warning | Anomaly investigation |

## Source COBOL Reference

**Programs:** `CBTRN02C.cbl` (723 lines — transaction posting, source of operational data), `CBTRN03C.cbl` (650 lines — report generation pattern)

Management reports follow the same batch processing patterns:

**Transaction counting and accumulation** (CBTRN02C:230-232):
```cobol
000230           IF WS-REJECT-COUNT > 0
000231              MOVE 4 TO RETURN-CODE
000232           END-IF
```

The reject count tracked by CBTRN02C feeds directly into the daily operations summary reject rate calculation.

**Balance accumulation pattern** (from account file reads):
```cobol
         READ ACCOUNT-FILE INTO ACCT-RECORD
         ADD ACCT-CURR-BAL TO WS-PORTFOLIO-TOTAL
         IF ACCT-CURR-BAL > ACCT-CREDIT-LIMIT
             ADD 1 TO WS-OVERLIMIT-COUNT
         END-IF
```

**Aging bucket classification** (conceptual, from account dates):
```cobol
         COMPUTE WS-DAYS-PAST-DUE =
             FUNCTION INTEGER-OF-DATE(WS-CURRENT-DATE) -
             FUNCTION INTEGER-OF-DATE(ACCT-LAST-PMT-DATE)
         EVALUATE TRUE
             WHEN WS-DAYS-PAST-DUE = 0
                 ADD ACCT-CURR-BAL TO WS-BUCKET-CURRENT
             WHEN WS-DAYS-PAST-DUE <= 30
                 ADD ACCT-CURR-BAL TO WS-BUCKET-30
             WHEN WS-DAYS-PAST-DUE <= 60
                 ADD ACCT-CURR-BAL TO WS-BUCKET-60
             WHEN WS-DAYS-PAST-DUE <= 90
                 ADD ACCT-CURR-BAL TO WS-BUCKET-90
             WHEN OTHER
                 ADD ACCT-CURR-BAL TO WS-BUCKET-120-PLUS
         END-EVALUATE
```

**Note:** Dedicated management reporting COBOL programs are not yet available in the repository. The logic is inferred from the batch pipeline framework, account master structure (ACCT-BR-001/004/007), and FSA governance requirements. The mainframe team must provide any management-specific batch programs for complete extraction.

## Acceptance Criteria

### Scenario 1: Daily operations summary generation

```gherkin
GIVEN the daily transaction posting (CBTRN02C) has completed for 2026-01-15
  AND 15,234 transactions were processed successfully
  AND 127 transactions were rejected (written to DALYREJS)
WHEN the daily operations summary report runs
THEN the report shows:
  | Metric | Value |
  | Transactions processed | 15,234 |
  | Transactions rejected | 127 |
  | Acceptance rate | 99.17% |
  | Total volume | 45,678,901.23 SEK |
  | Average transaction | 2,997.30 SEK |
```

### Scenario 2: Portfolio balance report with overlimit detection

```gherkin
GIVEN the account master file contains 50,000 active accounts
  AND 23 accounts have balances exceeding their credit limits
WHEN the portfolio balance report runs
THEN the report shows total portfolio balance and utilization
  AND the 23 overlimit accounts are listed with balance and limit details
  AND if overlimit count exceeds 100, a critical alert is raised
```

### Scenario 3: Delinquency aging report

```gherkin
GIVEN accounts with outstanding balances exist in the portfolio
WHEN the delinquency aging report runs
THEN accounts are classified into aging buckets:
  | Bucket | Count | Balance (SEK) |
  | Current | 45,200 | 890,000,000.00 |
  | 30 days | 3,100 | 62,000,000.00 |
  | 60 days | 980 | 24,500,000.00 |
  | 90 days | 450 | 13,500,000.00 |
  | 120+ days | 270 | 10,000,000.00 |
  AND the total matches the portfolio balance report total
```

### Scenario 4: Anomaly detection in daily volumes

```gherkin
GIVEN the 30-day rolling average daily transaction count is 15,000
  AND today's transaction count is 11,500 (23.3% below average)
WHEN the daily operations summary evaluates volume deviation
THEN the report flags the volume deviation as a warning
  AND the deviation exceeds the 20% threshold
  AND the anomaly is noted for operations investigation
```

### Scenario 5: Management report SLA

```gherkin
GIVEN the daily transaction report (CBTRN03C) completes by 06:00
WHEN management reports are generated
THEN all management reports are available by 08:00
  AND the reports are accessible to authorized management personnel
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|---|---|---|---|
| FSA FFFS 2014:5 | Ch. 7 | Board and management must have adequate information systems for monitoring and decision-making | Management reports provide daily operational metrics, portfolio health, and risk indicators |
| FSA FFFS 2014:5 | Ch. 3 | Financial records must support governance and oversight | Report metrics are derived from auditable transaction and account data |
| DORA | Art. 11 | ICT risk management and monitoring | Operational anomaly detection and volume deviation alerts support ICT resilience monitoring |

## Edge Cases

1. **Zero-transaction day**: On days with no transactions (e.g., holidays), the operations summary should still be generated showing zero counts. The reject rate should be reported as N/A rather than division-by-zero.

2. **Account status transitions**: An account that transitions from active to closed during the reporting period should be counted in both the active and closed buckets for the aging report, with the balance assigned to the final status.

3. **Negative balances (credits)**: Accounts with negative balances (overpayment or credit refunds) should be excluded from utilization calculations and reported separately as credit-balance accounts.

4. **Concurrent batch and management reporting**: If management reports read from files being updated by CBTRN02C, data consistency may be compromised. The pipeline sequence must ensure management reports run after CBTRN02C and CBTRN03C have completed. See TRN-BR-009 for pipeline ordering.

5. **Historical comparison**: Month-over-month and year-over-year comparisons require access to historical report data. The current mainframe retains report files for comparison. The migrated system should store historical metrics in a time-series format for trend analysis.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. CRITICAL QUESTIONS: (1) Which management reports are currently generated by the mainframe batch system? (2) Are there dedicated COBOL programs for management reports, or are they derived from CBTRN03C output files? (3) What is the complete list of metrics and thresholds used for operational alerts? (4) How are management reports currently distributed — printed, emailed, or accessed via a portal? (5) Are delinquency aging buckets standard (30/60/90/120+) or customized for NordKredit? (6) What historical comparison periods are used for trend analysis?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
