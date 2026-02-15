---
id: "BILL-BR-003"
title: "Batch statement generation produces text and HTML statements per card"
domain: "billing"
cobol_source: "CBSTM03A.CBL:262-924"
requirement_id: "BILL-BR-003"
regulations:
  - "FFFS 2014:5 Ch. 4 §5 — Statement Requirements"
  - "GDPR Art. 15 — Right of Access (statement contains personal data)"
  - "GDPR Art. 5(1)(f) — Integrity and Confidentiality"
  - "PSD2 Art. 57 — Information on Individual Payment Transactions"
status: "extracted"
validated_by: null
validated_date: null
priority: "high"
---

# BILL-BR-003: Batch statement generation produces text and HTML statements per card

## Summary

The statement generation batch job (CBSTM03A with subroutine CBSTM03B, invoked by CREASTMT.JCL) produces account statements in two formats: plain text and HTML. For each card in the cross-reference file, it retrieves customer information, account details, and all associated transactions, then generates a formatted statement showing customer name and address, account ID, current balance, FICO score, a transaction listing with individual amounts, and a total expenditure amount.

## Business Logic

### Processing Pipeline (CREASTMT.JCL)

```
Step 1 (DELDEF01): Delete and redefine TRXFL VSAM KSDS cluster
    - Keys: 32 bytes at offset 0 (card number + tran ID)
    - Record size: 350 bytes

Step 2 (STEP010): SORT transaction file
    - Input: TRANSACT.VSAM.KSDS
    - Sort by: card number (pos 263, 16 bytes, ascending)
              then tran ID (pos 1, 16 bytes, ascending)
    - Output record layout rearranged:
        Pos 1-16:    Card number (from pos 263)
        Pos 17-278:  Tran ID + rest of record (from pos 1-262)
        Pos 279-328: Remaining fields (from pos 279, 50 bytes)

Step 3 (STEP020): REPRO sorted sequential file into VSAM KSDS

Step 4 (STEP030): Delete previous statement reports (HTML and text)

Step 5 (STEP040): Execute CBSTM03A to produce statements
    - Input: TRNXFILE, XREFFILE, ACCTFILE, CUSTFILE
    - Output: STMTFILE (plain text, LRECL=80), HTMLFILE (HTML, LRECL=100)
```

### Pseudocode (CBSTM03A)

```
INITIALIZATION:
    Display JCL job name, step, and DD name information from TIOT
    OPEN OUTPUT: STMT-FILE, HTML-FILE
    INITIALIZE transaction table (51 cards x 10 transactions each)

FILE OPENING SEQUENCE (via ALTER/GO TO):
    OPEN TRNXFILE -> Read all transactions into memory table
    OPEN XREFFILE
    OPEN CUSTFILE
    OPEN ACCTFILE

LOAD TRANSACTIONS INTO MEMORY:
    8500-READTRNX-READ:
        FOR EACH transaction record from TRNXFILE:
            IF same card number as previous:
                INCREMENT transaction counter for this card
            ELSE:
                SAVE counter for previous card
                INCREMENT card counter
                RESET transaction counter to 1
            END-IF
            STORE card number, tran ID, and tran data in table
        END-FOR

MAIN PROCESSING:
    1000-MAINLINE:
        FOR EACH cross-reference record (XREFFILE, sequential):
            READ customer record (CUSTFILE, by XREF-CUST-ID)
            READ account record (ACCTFILE, by XREF-ACCT-ID)

            5000-CREATE-STATEMENT:
                Format customer name: first + middle + last
                Format address lines: addr1, addr2, addr3+state+country+zip
                Format account details: account ID, current balance, FICO score
                WRITE statement header (text and HTML)

            4000-TRNXFILE-GET (from in-memory table):
                RESET total-amount = 0
                FOR EACH card matching XREF-CARD-NUM:
                    FOR EACH transaction for this card:
                        6000-WRITE-TRANS:
                            Format: tran ID, description, amount
                            WRITE to text file and HTML file
                        ADD tran-amount TO total-amount
                    END-FOR
                END-FOR
                WRITE total expenditure line (text and HTML)
                WRITE end-of-statement markers

        END-FOR

    CLOSE all files
```

### Statement Layout (Plain Text, 80 columns)

```
*******************************START OF STATEMENT*******************************
<Customer Full Name>
<Address Line 1>
<Address Line 2>
<Address Line 3> <State> <Country> <ZIP>
--------------------------------------------------------------------------------
                                 Basic Details
--------------------------------------------------------------------------------
Account ID         :<Account ID>
Current Balance    :<Balance (9(9).99-)>
FICO Score         :<FICO Score>
--------------------------------------------------------------------------------
                              TRANSACTION SUMMARY
--------------------------------------------------------------------------------
Tran ID         Tran Details                                       Tran Amount
--------------------------------------------------------------------------------
<16-char ID>    <49-char description>                             $<amount>
...
--------------------------------------------------------------------------------
Total EXP:                                                        $<total>
********************************END OF STATEMENT********************************
```

### Decision Table

| Xref Record | Customer Found | Account Found | Transactions Found | Outcome |
|------------|---------------|--------------|-------------------|---------|
| Available | Yes | Yes | Yes | Full statement with transactions |
| Available | Yes | Yes | No (card not in table) | Statement with header only, zero total |
| Available | No (error) | — | — | ABEND: error reading CUSTFILE |
| Available | — | No (error) | — | ABEND: error reading ACCTFILE |
| EOF | — | — | — | End processing, close files |
| Error | — | — | — | ABEND: error reading XREFFILE |

## Source COBOL Reference

**Program:** `CBSTM03A.CBL`
**Lines:** 458-504 (Statement creation)

```cobol
       5000-CREATE-STATEMENT.
           INITIALIZE STATEMENT-LINES.
           WRITE FD-STMTFILE-REC FROM ST-LINE0.
           PERFORM 5100-WRITE-HTML-HEADER THRU 5100-EXIT.
           STRING CUST-FIRST-NAME DELIMITED BY ' '
                  ' ' DELIMITED BY SIZE
                  CUST-MIDDLE-NAME DELIMITED BY ' '
                  ' ' DELIMITED BY SIZE
                  CUST-LAST-NAME DELIMITED BY ' '
                  ' ' DELIMITED BY SIZE
                  INTO ST-NAME
           END-STRING.
           MOVE CUST-ADDR-LINE-1 TO ST-ADD1.
           MOVE CUST-ADDR-LINE-2 TO ST-ADD2.
           STRING CUST-ADDR-LINE-3 DELIMITED BY ' '
                  ' ' DELIMITED BY SIZE
                  CUST-ADDR-STATE-CD DELIMITED BY ' '
                  ' ' DELIMITED BY SIZE
                  CUST-ADDR-COUNTRY-CD DELIMITED BY ' '
                  ' ' DELIMITED BY SIZE
                  CUST-ADDR-ZIP DELIMITED BY ' '
                  ' ' DELIMITED BY SIZE
                  INTO ST-ADD3
           END-STRING.
           MOVE ACCT-ID TO ST-ACCT-ID.
           MOVE ACCT-CURR-BAL TO ST-CURR-BAL.
           MOVE CUST-FICO-CREDIT-SCORE TO ST-FICO-SCORE.
```

**Lines:** 416-456 (Transaction retrieval and total calculation)

```cobol
       4000-TRNXFILE-GET.
           PERFORM VARYING CR-JMP FROM 1 BY 1
             UNTIL CR-JMP > CR-CNT
             OR (WS-CARD-NUM (CR-JMP) > XREF-CARD-NUM)
               IF XREF-CARD-NUM = WS-CARD-NUM (CR-JMP)
                   MOVE WS-CARD-NUM (CR-JMP) TO TRNX-CARD-NUM
                   PERFORM VARYING TR-JMP FROM 1 BY 1
                     UNTIL (TR-JMP > WS-TRCT (CR-JMP))
                       MOVE WS-TRAN-NUM (CR-JMP, TR-JMP)
                         TO TRNX-ID
                       MOVE WS-TRAN-REST (CR-JMP, TR-JMP)
                         TO TRNX-REST
                       PERFORM 6000-WRITE-TRANS
                       ADD TRNX-AMT TO WS-TOTAL-AMT
                   END-PERFORM
               END-IF
           END-PERFORM.
           MOVE WS-TOTAL-AMT TO WS-TRN-AMT.
           MOVE WS-TRN-AMT TO ST-TOTAL-TRAMT.
```

**Lines:** 675-723 (Individual transaction line output)

```cobol
       6000-WRITE-TRANS.
           MOVE TRNX-ID TO ST-TRANID.
           MOVE TRNX-DESC TO ST-TRANDT.
           MOVE TRNX-AMT TO ST-TRANAMT.
           WRITE FD-STMTFILE-REC FROM ST-LINE14.
```

**Subroutine:** `CBSTM03B.CBL` — File I/O handler for TRNXFILE, XREFFILE, CUSTFILE, ACCTFILE

**JCL:** `CREASTMT.JCL`
**Input Files:** TRANSACT (sorted into TRNXFILE), XREFFILE (card cross-reference), ACCTFILE (account master), CUSTFILE (customer master)
**Output Files:** STMTFILE (plain text, 80-col), HTMLFILE (HTML format, 100-col)

**Copybooks:** `COSTM01.CPY` (Transaction layout for reporting), `CVACT03Y.cpy` (Card Cross-Reference), `CVACT01Y.cpy` (Account Record), `CUSTREC.cpy` (Customer Record)

## Acceptance Criteria

### Scenario 1: Standard statement with multiple transactions

```gherkin
GIVEN card "1234567890123456" is linked to customer "John A Smith" and account "00000012345"
  AND customer address is "123 Main St", "Apt 4B", "Seattle WA US 98101"
  AND account has current balance of $2,500.00 and FICO score "750"
  AND the following transactions exist for this card:
    | Tran ID | Description | Amount |
    | 0000000000000001 | Purchase at Store A | 100.00 |
    | 0000000000000002 | Purchase at Store B | 250.50 |
WHEN the statement generation batch runs
THEN a text statement is generated containing:
  - Header: "START OF STATEMENT"
  - Customer name: "John A Smith"
  - Address lines
  - Account ID: "00000012345"
  - Current Balance: "2500.00"
  - FICO Score: "750"
  - Transaction lines for each transaction with amounts
  - Total EXP: $350.50
  - Footer: "END OF STATEMENT"
  AND an equivalent HTML statement is generated
```

### Scenario 2: Statement with no transactions for a card

```gherkin
GIVEN card "9999888877776666" is linked to a valid customer and account
  AND no transactions exist in the transaction file for this card
WHEN the statement generation batch runs
THEN a statement is generated with customer and account details
  AND the transaction summary section shows no transaction lines
  AND Total EXP: $0.00
```

### Scenario 3: Multiple cards generate separate statements

```gherkin
GIVEN the cross-reference file contains cards for 3 different customers
WHEN the statement generation batch runs
THEN 3 separate statements are generated in both text and HTML format
  AND each statement contains only the transactions for that specific card
```

### Scenario 4: Transaction amount formatting

```gherkin
GIVEN a transaction with amount -150.75 (credit/refund)
WHEN the statement is generated
THEN the amount is displayed as "$   150.75-" (trailing minus, zero-suppressed with leading spaces)
```

### Scenario 5: Customer name formatting with STRING

```gherkin
GIVEN a customer with:
  | CUST-FIRST-NAME | "Jane" (padded with spaces to field width) |
  | CUST-MIDDLE-NAME | "M" (padded with spaces) |
  | CUST-LAST-NAME | "Johnson" (padded with spaces) |
WHEN the statement name line is formatted
THEN the name appears as "Jane M Johnson" (trimmed at first space, joined with single spaces)
```

## Regulatory Mapping

| Regulation | Article/Section | Requirement | How This Rule Satisfies It |
|------------|----------------|-------------|---------------------------|
| FFFS 2014:5 | Ch. 4 §5 | Credit card statements must be provided regularly with transaction details | The batch job generates statements with full transaction listing, amounts, and totals for each billing period. |
| GDPR | Art. 15 | Right of access to personal data | Statements include customer PII (name, address, FICO score). Access to statement output must be controlled. |
| GDPR | Art. 5(1)(f) | Personal data processed with appropriate security | Statement files (STMTFILE, HTMLFILE) contain PII and must be protected. The migration must ensure encrypted storage and access controls. |
| PSD2 | Art. 57 | Payment service providers must provide transaction details | Each statement lists individual transactions with ID, description, and amount, satisfying the itemized transaction requirement. |

## Edge Cases

1. **In-memory table limits**: The transaction table is dimensioned for 51 cards (`OCCURS 51 TIMES`) with 10 transactions each (`OCCURS 10 TIMES`). If more than 51 distinct cards or more than 10 transactions per card exist, the table will overflow. The migration should remove these fixed limits and use dynamic collections.

2. **ALTER and GO TO flow control**: CBSTM03A uses `ALTER` statements (lines 300-313) to modify `GO TO` targets at runtime, an obsolete COBOL pattern that creates complex control flow. The file opening sequence proceeds: TRNXFILE -> read all transactions -> XREFFILE -> CUSTFILE -> ACCTFILE -> main processing. The migration should use straightforward sequential logic.

3. **Mainframe control block addressing**: CBSTM03A accesses PSA, TCB, and TIOT control blocks (lines 235-291) to display the JCL job name and DD names. This is z/OS-specific and has no equivalent in the target environment. The migration should replace this with standard logging.

4. **Subroutine call pattern**: CBSTM03A calls CBSTM03B via `CALL 'CBSTM03B' USING WS-M03B-AREA` for all file I/O operations. The subroutine handles OPEN, CLOSE, READ (sequential), and READ-KEY (random) operations via an operation code. The migration should use direct repository/data access patterns.

5. **JCL SORT preprocessing**: Before CBSTM03A runs, the JCL sorts the transaction file by card number + transaction ID and rearranges the record layout (OUTREC FIELDS). The sort key becomes the first 32 bytes (card 16 + tran ID 16). The migration must replicate this ordering via SQL ORDER BY or equivalent.

6. **VSAM cluster management**: The JCL deletes and redefines the TRXFL VSAM cluster each run (DELDEF01 step). This ensures a clean working file. The migration should use temporary tables or equivalent transient storage.

7. **Dual output format**: Statements are produced simultaneously in text and HTML. The HTML includes inline CSS styles with specific colors and layout. The migration should consider whether HTML generation is still needed or if a modern template engine/PDF generator would be preferred.

8. **COMP and COMP-3 variables**: Counter variables (`CR-CNT`, `TR-CNT`, etc.) use `COMP` (binary) storage, and `WS-TOTAL-AMT` uses `COMP-3` (packed decimal, `PIC S9(9)V99`). The migration must account for the precision characteristics of these types.

9. **Transaction amount sign**: `ST-TRANAMT` is `PIC Z(9).99-` (trailing minus for negative) and `ST-TOTAL-TRAMT` is also `PIC Z(9).99-`. Negative amounts (refunds/credits) display with a trailing minus sign. The migration must preserve this display convention or adopt a modern equivalent.

## Domain Expert Notes

- **Pending validation**: This rule requires review by a domain expert to confirm: (1) whether the 51-card and 10-transaction limits are sufficient for production data or if they represent test constraints, (2) whether HTML statement output is still needed, and (3) the complete list of customer and account fields displayed.
- The FICO score is included on the statement, which may raise GDPR data minimization concerns. The migration should evaluate whether FICO score should be removed from customer-facing statements.
- The `ALTER`/`GO TO` pattern in CBSTM03A is considered poor practice in COBOL and makes the program difficult to maintain. The migration provides an opportunity to implement clean sequential logic.
- Statement generation is triggered by the CREASTMT JCL job, which currently runs as a batch job. The migration target is an Azure Function with a timer trigger (per CLAUDE.md), which must meet the "complete by day 3" SLA for monthly statements.

---

**Template version:** 1.0
**Last updated:** 2026-02-15
