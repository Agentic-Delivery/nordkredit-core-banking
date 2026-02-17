---
id: "dep-br-007"
title: "Deposit account statement generation"
domain: "deposits"
cobol_source: "Dedicated program not yet in repository"
requirement_id: "DEP-BR-007"
regulations:
  - "FSA FFFS 2014:5 Ch. 7"
  - "PSD2 Art. 57"
  - "GDPR Art. 15"
  - "GDPR Art. 5(1)(a)"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# DEP-BR-007: Deposit account statement generation

## Summary

Account statements are periodic reports provided to depositors summarizing all transactions, interest accruals, and balance changes over a statement period. Swedish regulations and PSD2 require that customers receive clear, complete information about their deposit account activity. The statement generation batch runs monthly (or quarterly for certain products), reads all posted transactions for each account within the statement period, calculates opening and closing balances, summarizes interest earned, and produces formatted statement records for delivery (print or electronic).

The dedicated COBOL program for statement generation is not yet available in the repository. This rule is inferred from the transaction record structure (CVTRA05Y.cpy), the report generation patterns observed in CBTRN03C.cbl (daily transaction report), and Swedish regulatory requirements. The statement generation process is similar in structure to the daily report (TRN-BR-006) but operates on a longer period and includes interest summary information.

## Business Logic

### Pseudocode

```
PERFORM MONTHLY-STATEMENT-GENERATION:
    OPEN ACCOUNT-FILE (INPUT)
    OPEN TRANSACT-FILE (INPUT, browse)
    OPEN STATEMENT-FILE (OUTPUT)

    PERFORM UNTIL END-OF-ACCOUNT-FILE
        READ next ACCOUNT-RECORD

        IF ACCT-ACTIVE-STATUS = 'Y'
          AND account-type = DEPOSIT
            PERFORM GENERATE-ACCOUNT-STATEMENT
        END-IF
    END-PERFORM

PERFORM GENERATE-ACCOUNT-STATEMENT:
    -- Step 1: Determine statement period
    SET period-start = first day of previous month
    SET period-end = last day of previous month

    -- Step 2: Calculate opening balance
    COMPUTE opening-balance = ACCT-CURR-BAL
        - sum(transactions in current period)

    -- Step 3: Read all transactions in period
    STARTBR TRANSACT-FILE for account
    PERFORM UNTIL no more transactions in period
        READNEXT TRANSACT-FILE
        IF TRAN-ORIG-TS within period
            ADD to statement-detail-lines
            Classify: deposit, withdrawal, interest, fee
        END-IF
    END-PERFORM

    -- Step 4: Summarize
    COMPUTE total-deposits = sum of positive transactions
    COMPUTE total-withdrawals = sum of negative transactions
    COMPUTE interest-earned = sum of interest-type transactions
    SET closing-balance = ACCT-CURR-BAL (at period end)

    -- Step 5: Generate statement record
    WRITE statement-header (account, period, opening balance)
    WRITE statement-detail-lines (each transaction)
    WRITE statement-summary (totals, interest, closing balance)
    WRITE regulatory-disclosures (deposit guarantee info, rate info)
```

### Statement Structure

| Section | Contents |
|---------|----------|
| Header | Account number, statement period, customer name, opening balance |
| Transaction Details | Date, description, amount, running balance for each transaction |
| Interest Summary | Accrued interest, posted interest, applicable rate |
| Summary | Total deposits, total withdrawals, fees, closing balance |
| Regulatory Disclosures | Deposit guarantee coverage, applicable rate schedule, bank contact |

## Source COBOL Reference

**Program:** Dedicated statement generation program — not yet in repository.

The pattern is inferred from the daily transaction report generation:

```cobol
       2000-WRITE-REPORT-HEADER.
       2500-WRITE-REPORT-DETAIL.
       3000-WRITE-REPORT-TOTALS.
```
*(CBTRN03C.cbl — report generation paragraphs showing the header/detail/totals pattern. The monthly statement follows the same structure but spans a month rather than a single day.)*

The transaction record structure used for statement line items:

```cobol
       COPY CVTRA05Y.
```
*(CBTRN02C.cbl — posted transaction record layout (350 bytes). Each transaction in the statement period is read from this file to generate statement detail lines.)*

## Acceptance Criteria

### Scenario 1: Monthly statement with deposits and withdrawals

```gherkin
GIVEN a deposit account with the following transactions in January 2026:
  | Date       | Description          | Amount     |
  | 2026-01-05 | Salary deposit       | +45000.00  |
  | 2026-01-10 | ATM withdrawal       | -2000.00   |
  | 2026-01-15 | Transfer in          | +5000.00   |
  | 2026-01-25 | Rent payment         | -12000.00  |
  | 2026-01-31 | Interest posting     | +68.49     |
  AND opening balance on 2026-01-01 was 100000.00
WHEN the monthly statement is generated for January 2026
THEN the statement shows:
  | Opening Balance  | 100000.00 |
  | Total Deposits   | 50068.49  |
  | Total Withdrawals| 14000.00  |
  | Interest Earned  | 68.49     |
  | Closing Balance  | 136068.49 |
  AND all 5 transactions are listed in chronological order
```

### Scenario 2: Statement includes deposit guarantee disclosure

```gherkin
GIVEN a deposit account with closing balance of 800000.00 SEK
WHEN the monthly statement is generated
THEN the regulatory disclosure section includes:
  "Your deposits at NordKredit AB are covered by the Swedish deposit guarantee
   scheme up to SEK 1,050,000 per depositor."
  AND the current deposit guarantee coverage amount is shown
```

### Scenario 3: Zero-transaction period statement

```gherkin
GIVEN a deposit account with no transactions in February 2026
  AND the opening balance is 50000.00
  AND no interest was posted during the period
WHEN the monthly statement is generated
THEN the statement shows:
  | Opening Balance  | 50000.00 |
  | Total Deposits   | 0.00     |
  | Total Withdrawals| 0.00     |
  | Interest Earned  | 0.00     |
  | Closing Balance  | 50000.00 |
  AND no transaction detail lines are included
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FSA FFFS 2014:5 | Ch. 7 | Financial systems — institutions must generate periodic reports for customer accounts | Monthly/quarterly statement generation provides customers with complete account activity reports |
| PSD2 | Art. 57 | Information on individual payment transactions — payment service providers must provide information on each transaction | Each transaction is listed in the statement with date, description, amount, and running balance |
| GDPR | Art. 15 | Right of access — data subjects have the right to obtain a copy of their personal data being processed | Statements provide customers with a comprehensive record of their financial data for the period |
| GDPR | Art. 5(1)(a) | Lawfulness, fairness, transparency — processing must be transparent to the data subject | The statement includes clear disclosures about applicable rates, fees, and deposit guarantee coverage |

## Edge Cases

1. **Statement delivery**: Statements may be delivered by post (paper) or electronically (internet bank, PDF). The generation process creates the content; delivery channel selection is a separate process. The COBOL system likely generates print-formatted records; the migrated system should support both formats.

2. **Swedish character encoding**: Customer names and transaction descriptions containing Å, Ä, Ö must render correctly in statements. EBCDIC-to-UTF-8 conversion must preserve Swedish characters.

3. **Statement SLA**: Monthly statements must be completed by day 3 of the following month (per batch SLAs). For ~2 million deposit accounts, the Azure Functions implementation must parallelize statement generation to meet this deadline.

4. **Closed accounts**: Accounts closed during the statement period should receive a final statement showing the closing transaction and zero ending balance.

5. **Large transaction volume**: Some business deposit accounts may have hundreds of transactions per month. The statement must handle variable-length detail sections without truncation.

6. **Year-end tax summary**: Swedish tax reporting may require annual interest summaries (kontrolluppgift) in addition to monthly statements. This is a separate regulatory requirement but relies on the same transaction data.

## Domain Expert Notes

- **null** (null): Awaiting domain expert review. Key questions: (1) What is the statement frequency for each product type — monthly, quarterly, or annual? (2) Are statements generated for all active accounts or only on request? (3) What regulatory disclosures must be included in each statement? (4) Is there a separate year-end interest summary (kontrolluppgift) for tax reporting? (5) How are electronic statements delivered — internet banking portal, email, or both? (6) What is the COBOL report layout — print line width, page breaks, header/footer format?

---

**Template version:** 1.0
**Last updated:** 2026-02-17
