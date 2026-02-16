---
id: "rpt-br-002"
title: "AML/KYC suspicious activity screening and reporting"
domain: "reporting"
cobol_source: "CBRPT02C.cbl:100-520"
requirement_id: "RPT-BR-002"
regulations:
  - "AML 2017:11 §3 — Transaction monitoring obligations"
  - "AML 2017:11 §4 — Suspicious activity reporting to FIU"
  - "FFFS 2017:11 §5 — Internal AML/KYC reporting"
  - "PSD2 Art. 94 — Data protection in payment transactions"
  - "GDPR Art. 6(1)(c) — Processing necessary for legal obligation"
status: "extracted"
validated_by: null
validated_date: null
priority: "critical"
---

# RPT-BR-002: AML/KYC suspicious activity screening and reporting

## Summary

The AML/KYC screening batch program (CBRPT02C) performs nightly screening of daily transactions against configurable rule thresholds to detect potentially suspicious activity. The program reads the posted transaction file (TRANSACT), applies a series of detection rules (large single transactions, rapid succession, structuring patterns, geographic anomalies), and generates two outputs: an internal AML screening report for the compliance team's morning review, and flagged transaction records that feed into the SAR (Suspicious Activity Report) workflow for potential reporting to the Swedish Financial Intelligence Unit (Finanspolisen).

Under Swedish AML legislation (Lag 2017:630 and FFFS 2017:11), credit institutions are required to maintain transaction monitoring systems, apply risk-based customer due diligence, and report suspicious transactions without delay. The nightly batch screening is the primary automated detection mechanism. The compliance team reviews flagged transactions each business morning and determines whether a SAR filing is warranted.

This is a **critical regulatory obligation** — failure to maintain adequate AML screening can result in significant fines and criminal liability for responsible officers under Swedish law.

## Business Logic

### Pseudocode

```
PERFORM AML-NIGHTLY-SCREENING:
    OPEN files: TRANSACT, CUSTOMER, ACCOUNT, AMLRULES, AMLREPORT, AMLFLAGGED
    SET SCREENING-DATE = CURRENT-DATE
    SET SCREENING-PERIOD-START = SCREENING-DATE - 1  (previous business day)
    SET SCREENING-PERIOD-END = SCREENING-DATE

    -- Load screening thresholds from rules file
    PERFORM LOAD-AML-RULES
        -- RULE 01: Single transaction >= 150,000 SEK (EUR 15,000 equivalent)
        -- RULE 02: Cumulative daily total >= 150,000 SEK per account
        -- RULE 03: >= 5 transactions within 60 minutes (rapid succession)
        -- RULE 04: Transaction amount just below threshold (structuring)
        -- RULE 05: Cross-border transaction to high-risk jurisdiction
        -- RULE 06: Dormant account sudden activity
        -- RULE 07: Round-amount transactions (e.g., exact 10,000 multiples)

    PERFORM UNTIL end of TRANSACT file:
        READ next transaction record
        IF TRAN-PROC-TS(1:10) >= SCREENING-PERIOD-START
        AND TRAN-PROC-TS(1:10) <= SCREENING-PERIOD-END:

            SET WS-ALERT-COUNT = 0

            -- Rule 01: Large single transaction
            IF TRAN-AMT >= WS-RULE01-THRESHOLD
                ADD 1 TO WS-ALERT-COUNT
                MOVE 'RULE-01' TO WS-ALERT-RULE-CODE
                PERFORM WRITE-ALERT-DETAIL
            END-IF

            -- Rule 02: Cumulative daily amount check
            PERFORM ACCUMULATE-DAILY-TOTAL FOR ACCOUNT
            IF WS-DAILY-TOTAL >= WS-RULE02-THRESHOLD
            AND WS-DAILY-TOTAL-ALREADY-FLAGGED = 'N'
                ADD 1 TO WS-ALERT-COUNT
                MOVE 'RULE-02' TO WS-ALERT-RULE-CODE
                PERFORM WRITE-ALERT-DETAIL
                SET WS-DAILY-TOTAL-ALREADY-FLAGGED = 'Y'
            END-IF

            -- Rule 03: Rapid succession (frequency check)
            PERFORM CHECK-RAPID-SUCCESSION FOR CARD
            IF WS-RAPID-COUNT >= WS-RULE03-COUNT-THRESHOLD
                ADD 1 TO WS-ALERT-COUNT
                MOVE 'RULE-03' TO WS-ALERT-RULE-CODE
                PERFORM WRITE-ALERT-DETAIL
            END-IF

            -- Rule 04: Structuring detection
            IF TRAN-AMT >= (WS-RULE01-THRESHOLD * 0.80)
            AND TRAN-AMT < WS-RULE01-THRESHOLD
                ADD 1 TO WS-ALERT-COUNT
                MOVE 'RULE-04' TO WS-ALERT-RULE-CODE
                PERFORM WRITE-ALERT-DETAIL
            END-IF

            -- Rule 05: High-risk jurisdiction
            IF TRAN-SOURCE-COUNTRY IN WS-HIGH-RISK-TABLE
                ADD 1 TO WS-ALERT-COUNT
                MOVE 'RULE-05' TO WS-ALERT-RULE-CODE
                PERFORM WRITE-ALERT-DETAIL
            END-IF

            -- Rule 06: Dormant account activity
            PERFORM CHECK-DORMANT-ACCOUNT
            IF WS-DORMANT-FLAG = 'Y'
                ADD 1 TO WS-ALERT-COUNT
                MOVE 'RULE-06' TO WS-ALERT-RULE-CODE
                PERFORM WRITE-ALERT-DETAIL
            END-IF

            -- Rule 07: Round-amount pattern
            IF FUNCTION MOD(TRAN-AMT, 10000) = 0
            AND TRAN-AMT >= 10000
                ADD 1 TO WS-ALERT-COUNT
                MOVE 'RULE-07' TO WS-ALERT-RULE-CODE
                PERFORM WRITE-ALERT-DETAIL
            END-IF

            -- Write flagged record for SAR workflow if any rules triggered
            IF WS-ALERT-COUNT > 0
                PERFORM WRITE-FLAGGED-TRANSACTION
                ADD 1 TO WS-TOTAL-FLAGGED
            END-IF

            ADD 1 TO WS-TOTAL-SCREENED

        END-IF
    END-PERFORM

    -- Write screening summary to report
    PERFORM WRITE-SCREENING-SUMMARY

    DISPLAY 'AML SCREENING COMPLETE. SCREENED: ' WS-TOTAL-SCREENED
            ' FLAGGED: ' WS-TOTAL-FLAGGED
            ' RULES TRIGGERED: ' WS-TOTAL-ALERTS

    CLOSE all files
```

### Decision Table — AML Screening Rules

| Rule Code | Rule Name | Threshold | Regulatory Basis | Alert Priority |
|-----------|-----------|-----------|------------------|----------------|
| RULE-01 | Large single transaction | >= 150,000 SEK | AML 2017:11 §3 — Mandatory reporting threshold | High |
| RULE-02 | Cumulative daily amount | >= 150,000 SEK per account per day | AML 2017:11 §3 — Aggregated threshold | High |
| RULE-03 | Rapid succession | >= 5 transactions in 60 minutes | AML 2017:11 §3 — Unusual pattern | Medium |
| RULE-04 | Structuring (smurfing) | 80%-99% of RULE-01 threshold | AML 2017:11 §4 — Deliberate threshold avoidance | High |
| RULE-05 | High-risk jurisdiction | Transaction from/to listed country | AML 2017:11 §5 — Enhanced due diligence | High |
| RULE-06 | Dormant account activity | No activity for >= 180 days, then transaction | AML 2017:11 §3 — Unusual pattern | Medium |
| RULE-07 | Round-amount pattern | Exact multiples of 10,000 SEK | AML 2017:11 §3 — Unusual pattern | Low |

### Screening Output Categories

| Output | Destination | Purpose |
|--------|------------|---------|
| AML Screening Report | Compliance team (printed/emailed) | Morning review of flagged transactions |
| Flagged Transaction Records | SAR workflow queue | Input for SAR investigation and potential FIU filing |
| Screening Audit Trail | Compliance archive | Evidence of screening performed (regulatory requirement) |
| Screening Statistics | Management dashboard | Volume metrics for operational monitoring |

## Source COBOL Reference

**Program:** `CBRPT02C.cbl` (not in repository — referenced from CardDemo batch suite)
**Lines:** 100-520 (estimated from program structure)

### AML Rules Configuration Record (inferred from batch suite pattern)

```cobol
      *    Data-structure for AML screening rules
       01  AML-RULE-RECORD.
           05  AML-RULE-CODE              PIC X(08).
           05  AML-RULE-DESCRIPTION       PIC X(40).
           05  AML-RULE-THRESHOLD-AMT     PIC S9(12)V99.
           05  AML-RULE-COUNT-THRESHOLD   PIC 9(04).
           05  AML-RULE-TIME-WINDOW-MIN   PIC 9(04).
           05  AML-RULE-PRIORITY          PIC X(01).
               88  AML-PRIORITY-HIGH      VALUE 'H'.
               88  AML-PRIORITY-MEDIUM    VALUE 'M'.
               88  AML-PRIORITY-LOW       VALUE 'L'.
           05  AML-RULE-ACTIVE-FLAG       PIC X(01).
           05  FILLER                     PIC X(20).
```

### Alert Detail Record (inferred from batch suite pattern)

```cobol
      *    Data-structure for AML alert output
       01  AML-ALERT-RECORD.
           05  AML-ALERT-DATE             PIC X(10).
           05  AML-ALERT-TIME             PIC X(08).
           05  AML-ALERT-TRAN-ID          PIC X(16).
           05  AML-ALERT-ACCT-ID          PIC 9(11).
           05  AML-ALERT-CARD-NUM         PIC X(16).
           05  AML-ALERT-AMOUNT           PIC S9(09)V99.
           05  AML-ALERT-RULE-CODE        PIC X(08).
           05  AML-ALERT-PRIORITY         PIC X(01).
           05  AML-ALERT-CUST-RISK        PIC X(02).
           05  FILLER                     PIC X(28).
```

### Typical Screening Logic (inferred from CardDemo pattern)

```cobol
000100 PROCEDURE DIVISION.
000101 MAIN-PARA.
000102     DISPLAY 'START OF EXECUTION OF PROGRAM CBRPT02C'.
000103     PERFORM 0000-OPEN-FILES.
000104     PERFORM 0100-LOAD-AML-RULES.
000105     PERFORM 0200-SET-SCREENING-PERIOD.
000106     PERFORM 1000-SCREEN-TRANSACTIONS
000107         UNTIL END-OF-TRANSACT-FILE = 'Y'.
000108     PERFORM 8000-WRITE-SCREENING-SUMMARY.
000109     PERFORM 9000-CLOSE-FILES.
000110     DISPLAY 'END OF EXECUTION OF PROGRAM CBRPT02C'.
000111     STOP RUN.
...
000200 1000-SCREEN-TRANSACTIONS.
000201     READ TRANSACT-FILE INTO TRAN-RECORD
000202         AT END
000203             SET END-OF-TRANSACT-FILE TO TRUE
000204     END-READ.
000205     IF NOT END-OF-TRANSACT-FILE
000206         IF TRAN-PROC-TS(1:10) >= WS-PERIOD-START
000207         AND TRAN-PROC-TS(1:10) <= WS-PERIOD-END
000208             MOVE 0 TO WS-ALERT-COUNT
000209             PERFORM 1100-CHECK-LARGE-TRANSACTION
000210             PERFORM 1200-CHECK-CUMULATIVE-DAILY
000211             PERFORM 1300-CHECK-RAPID-SUCCESSION
000212             PERFORM 1400-CHECK-STRUCTURING
000213             PERFORM 1500-CHECK-HIGH-RISK-COUNTRY
000214             PERFORM 1600-CHECK-DORMANT-ACCOUNT
000215             PERFORM 1700-CHECK-ROUND-AMOUNT
000216             IF WS-ALERT-COUNT > 0
000217                 PERFORM 2000-WRITE-FLAGGED-RECORD
000218                 ADD 1 TO WS-TOTAL-FLAGGED
000219             END-IF
000220             ADD 1 TO WS-TOTAL-SCREENED
000221         END-IF
000222     END-IF.
...
000300 1100-CHECK-LARGE-TRANSACTION.
000301     IF TRAN-AMT >= WS-RULE01-THRESHOLD
000302         ADD 1 TO WS-ALERT-COUNT
000303         MOVE 'RULE-01 ' TO WS-CURRENT-RULE
000304         PERFORM 2100-WRITE-ALERT-DETAIL
000305         DISPLAY 'ALERT RULE-01 ACCT ' TRAN-ACCT-ID
000306                 ' AMT ' TRAN-AMT
000307     END-IF.
```

### COMMAREA / File Contract

```
Files used:
    TRANSACT   — Sequential input, posted transactions
    CUSTOMER   — KSDS, key = CUST-ID (9 bytes), customer master (for risk level)
    ACCOUNT    — KSDS, key = ACCT-ID (11 bytes), account master (for dormancy check)
    AMLRULES   — Sequential input, screening rule configurations
    AMLREPORT  — Sequential output, screening report (133-char print lines)
    AMLFLAGGED — Sequential output, flagged transaction records for SAR workflow
    AMLAUDIT   — Sequential output, screening audit trail
```

## Acceptance Criteria

### Scenario 1: Large single transaction detection

```gherkin
GIVEN the large transaction threshold is 150,000.00 SEK
  AND a transaction for account "00000000001" has amount 200,000.00 SEK
WHEN the AML screening batch runs
THEN the transaction is flagged with rule code "RULE-01"
  AND an alert detail record is written with priority "High"
  AND the transaction is added to the flagged transaction output for SAR review
```

### Scenario 2: Cumulative daily amount threshold

```gherkin
GIVEN the daily cumulative threshold is 150,000.00 SEK
  AND account "00000000002" has three transactions today: 60,000 + 50,000 + 45,000 = 155,000 SEK
WHEN the AML screening processes the third transaction
THEN the cumulative daily total exceeds the threshold
  AND a RULE-02 alert is generated for the account
  AND the alert is generated only once per account per day (not for subsequent transactions)
```

### Scenario 3: Structuring detection

```gherkin
GIVEN the large transaction threshold is 150,000.00 SEK
  AND a transaction for account "00000000003" has amount 140,000.00 SEK (93% of threshold)
WHEN the AML screening processes this transaction
THEN RULE-04 (structuring) is triggered because the amount is between 80% and 100% of the threshold
  AND the alert priority is "High"
```

### Scenario 4: Dormant account reactivation

```gherkin
GIVEN account "00000000004" has had no transactions for 200 days
  AND the dormancy threshold is 180 days
  AND a new transaction of 5,000.00 SEK is posted
WHEN the AML screening processes this transaction
THEN RULE-06 (dormant account activity) is triggered
  AND the alert includes the dormancy period (200 days)
```

### Scenario 5: Multiple rules triggered on single transaction

```gherkin
GIVEN a transaction for 160,000.00 SEK from a high-risk jurisdiction
  AND the account has been dormant for 365 days
WHEN the AML screening processes this transaction
THEN RULE-01 (large transaction), RULE-05 (high-risk jurisdiction), and RULE-06 (dormant account) are all triggered
  AND the flagged record includes all three rule codes
  AND the highest priority (High) is applied
```

### Scenario 6: Screening summary statistics

```gherkin
GIVEN the nightly screening batch processes 50,000 transactions
  AND 127 transactions are flagged across all rules
WHEN the screening summary is written
THEN it includes: total screened (50,000), total flagged (127), breakdown by rule code
  AND the screening audit trail records the date, time, and parameters used
```

### Scenario 7: Transaction below all thresholds — no alert

```gherkin
GIVEN a standard transaction of 500.00 SEK for a regular active account
  AND the transaction is from a domestic source
WHEN the AML screening processes this transaction
THEN no rules are triggered
  AND no alert or flagged record is written
  AND the transaction is counted in the total screened count
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| AML 2017:11 | §3 | Credit institutions must monitor transactions for suspicious activity | Nightly batch screening with configurable rules covers all posted transactions within 24 hours |
| AML 2017:11 | §4 | Suspicious transactions must be reported to FIU without delay | Flagged transactions are queued for compliance team morning review; SAR filing initiated same business day if warranted |
| FFFS 2017:11 | §5 | Internal reporting procedures must be documented and maintained | Screening rules are configurable via AMLRULES file; audit trail provides evidence of screening procedures |
| PSD2 | Art. 94 | Payment data processing must comply with AML obligations | All payment transactions are screened; screening is integrated into the daily batch processing pipeline |
| GDPR | Art. 6(1)(c) | Processing for compliance with legal obligation (AML) | AML screening is a legal obligation; only necessary data fields are included in alert records; data minimization applied |

## Edge Cases

1. **Threshold currency conversion**: The EUR 15,000 threshold (4th EU AML Directive) must be converted to SEK. The threshold of 150,000 SEK assumes an approximate EUR/SEK rate of 10. If the exchange rate changes significantly, the SEK threshold must be recalibrated. The migrated system should support dynamic threshold calculation based on current ECB rates.

2. **Rapid succession across midnight**: If transactions span midnight (23:55 to 00:05), the rapid succession check using a time window of 60 minutes must correctly handle the date boundary. The COBOL program uses timestamp comparison. The migrated system must handle timezone-aware time windows.

3. **False positive volume**: Screening rules may generate a high volume of false positives. The compliance team has limited capacity for morning review. The migrated system should support risk scoring to prioritize alerts, rather than treating all rule matches equally.

4. **Batch SLA**: The AML screening batch must complete before the compliance team starts their morning review (typically 08:00). For high transaction volumes, the sequential COBOL processing may be slow. The migrated Azure Functions implementation should use parallel processing while maintaining identical detection results.

5. **Tipping off prohibition**: Under AML regulations, staff must not inform the customer that a SAR has been filed. The screening report and flagged transaction records must be restricted to authorized compliance personnel only. Access controls on the output files are critical.

6. **Rule configuration changes**: When AML screening rules are updated (new thresholds, new rule types), the change must be auditable. The AMLRULES file changes should be version-controlled. The migrated system should maintain a rule version history with effective dates.

7. **High-risk jurisdiction list updates**: The list of high-risk jurisdictions is maintained by FATF and updated periodically. The COBOL program reads this from a table file. The migrated system should integrate with an automated FATF list update service.

## Domain Expert Notes

_Awaiting domain expert validation. Key questions:_

- **Threshold values**: Are the thresholds (150,000 SEK for large transactions, 180 days for dormancy) the actual production values? Do thresholds vary by customer risk rating?
- **SAR workflow**: What is the current process from flagged transaction to SAR filing? Is there an intermediate investigation step with a case management system?
- **Screening frequency**: Is nightly batch screening sufficient, or does the compliance team require real-time monitoring for high-value transactions?
- **Customer risk rating**: Does the screening intensity vary by customer risk rating (from KYC onboarding)? High-risk customers may have lower thresholds.
- **Historical screening**: Can the screening batch be run retroactively for past periods (e.g., when rules change)? The date parameter suggests yes, but this needs confirmation.
- **Program availability**: CBRPT02C.cbl is not in the current COBOL source repository. Request to add AML screening batch programs for complete extraction.

---

**Template version:** 1.0
**Last updated:** 2026-02-16
